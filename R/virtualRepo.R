##base packages are not available anywhere but the core R distribution
## so we will never find them and do not need to look. The version will always
## match the R version (XXX double check in case of patched)
pkgs <- installed.packages()
basepkgs = pkgs[ pkgs[,"Priority"] %in% "base", "Package"]






##' @rdname virtualRepo-funs
##' @param sinfo A sessionInfo object or character vector containing the text from printing such an object
##' @export
sessionRepo = function(sinfo = sessionInfo(),
    repo_dir, doi= NULL, dir,
    name = NULL , replace = FALSE, stoponfail = TRUE, GRepo = GRANRepo$repo,
    install = FALSE, libloc = NULL)
{
    if(!(is(sinfo, "sessionInfo") || is(sinfo, "character") || is(sinfo, "parsedSessionInfo"))) {
        stop("sinfo must be a character vector or sessionInfo object")
    }
    
       
    ##probably want to do better but for now this will do
    if(is.character(sinfo))
        sinfo = parseSessionInfoString(sinfo)
    else if(is(sinfo, "sessionInfo"))
        sinfo = parseSessionInfoString(capture.output(print(sinfo)))
    
        #name = substr(fastdigest(sinfo), 1, 32)
    
    Rvers = sinfo@version
    Rvers = gsub("([^\\.]*\\.[^\\.]+).*", "\\1",Rvers)
 
   # fils = getSessionPackages(sinfo, dir = dir, repo= GRepo, stoponfail)
    pkgdf = sinfoToPkgDF(sinfo)
    makeVirtualRepo(pkgdf, repo_dir, doi, dir, name, replace, stoponfail = stoponfail, GRepo, install, libloc)
    
}

sinfoToPkgDF = function(sinfo) {
       ##probably want to do better but for now this will do
    if(is.character(sinfo))
        sinfo = parseSessionInfoString(sinfo)
    else if(is(sinfo, "sessionInfo"))
        sinfo = parseSessionInfoString(capture.output(print(sinfo)))

    pkgs = rbind(sinfo@attached, sinfo@loaded)
    pkgs
}


##' @rdname virtualRepo-funs
##' @return for \code{getPkgVersions} and \code{getSessionPackages}, a character vector with the full path to each downloaded/built tar.gz file.
##' @export
getSessionPackages = function(sinfo, dir, GRepo = NULL, stoponfail = FALSE) {
    if(!(is(sinfo, "sessionInfo") || is(sinfo, "character") || is(sinfo, "parsedSessionInfo"))){
        stop("sinfo must be a character vector or sessionInfo object")
    }
    pkgs = sinfoToPkgDF(sinfo)
    getPkgVersions(pkgs, dir = dir, GRepo = GRepo, stoponfail = stoponfail)
}
##' @rdname virtualRepo-funs
##' @param pkgs A data.frame  of package versions to locate and/or build
##' @param pkgcol The column in \code{pkgs} or \code{pkgsdf} containing the package names
##' @param verscol The column in \code{pkgs} or \code{pkgsdf} containing the package versions
##' @export
getPkgVersions = function(pkgs, dir, GRepo = NULL, stoponfail = FALSE,pkgcol = "Package", verscol = "Version") {
  
    if(!(is.null(GRepo) || is(GRepo, "character") || is(GRepo, "GRANRepository"))){
        stop("if GRepo is specified it must be a GRANRepository object or directory path")
    } else if(is(GRepo, "character")) {
        GRepo = loadRepo(filename = file.path(GRepo, "repo.R"))
    }
    
    if(missing(dir) && !missing(GRepo) && !is.null(GRepo))
        dir = notrack(GRepo)
    
    if(missing(dir) || !is(dir, "character")) {
        stop("must specify dir or GRepo to identify where to download packages")
    }
    
    ## we will never find any version of base packages,
    ##they are not in any repo and only ship with R itself
    pkgs = pkgs[!pkgs[[pkgcol]] %in% basepkgs,]
    fils = mapply(locatePkgVersion, name = pkgs[[pkgcol]], version = pkgs[[verscol]],
        repo = list(GRepo), SIMPLIFY=FALSE)
    if(any(sapply(fils, function(x) length(x) == 0)))
    {
        msg = "Unable to locate the correct version of some packages."
        if(stoponfail)
            stop(msg)
        else
            warning(msg)
    }
    fils = unlist(fils)
    fils
}

##'makeVirtualRepo
##'
##' Create a virtual repository which contains only the exact packages specified
##' 
##' @title Create a virtual repository containing only the specified package versions versions 
##'
##'
##' Packages are located via the \code{getPkgVersions} function, which will look in the following places:
##' \enumerate{
##' \item{The \code{repo} repository and associated notrack directory}
##' \item{The current CRAN repository}
##' \item{The CRAN archives of previous source packages}
##' \item{The current Bioconductor repository}
##' \item{The Bioconductor SVN history}
##' \item{The SCM (SVN/git) history for a GRAN package}
##' }
##'
##' When found, package versions not already in the GRAN repository proper or notrack directory are built into the \code{repo}'s associated notrack directory.
##'
##' The repository is the constructed as a sibling to \code{repo}'s repository using only symbolic links. This allows many virtual repositories to contain the same versions of packages without physical file duplication.
##' @rdname virtualRepo-funs
##' @param pkgdf A data.frame containing the package names and versions to populate the repository with
##' @param repo_dir The base directory to create the virtual repository under. 
##' @param doi A DOI associated with the session info. If specified when name is NULL, the repository name is set to the doi with "/" replaced with "_".
##' @param dir  The directory to download/build package tarballs into during the search process
##' @param name The name of the repository to create. Defaults to a 32 character hash generated from \code{sinfo}
##' @param replace logical. Indicates whether the newly created virtual repository should overwrite any exists virtual repositories of the same name
##' @param stoponfail  logical. Indicates whether the function should throw an error if it is unable to retreive one or more of the specified package versions. Defaults to \code{TRUE}
##' @param GRepo (optional) a \code{GRANRepository} to act as a parent to the
##' virtual repository. If specified, this is used to: search for pkg versions, determine where to download/build newly located pkg versions, and set the parent directory of the virtual repo.
##' @param install should the packages be immediately installed into
##' \code{libloc}. Defaults to FALSE
##' @param libloc If packages are being installed, a library location to
##' contain only the packages for this set of package versions. In generally this should
##' *not* be your standard library location.
##' @param Rvers The R version to build into the repository structure, if desired. Defaults to no specific version (suitable for src packages).
##' @param pkgcol  Column in the dataframe that contains package name
##' @param verscol Column in the dataframe taht contains package version
##' @return for \code{makeVirtualRepo} and \code{sessionRepo}, the path to the created virtual repository
##' @author Gabriel Becker
##' @importFrom digest digest
##' @export

makeVirtualRepo = function(pkgdf, repo_dir, doi,  dir, name=NULL, replace=FALSE, stoponfail=TRUE, GRepo, install=FALSE, libloc = NULL, Rvers="", pkgcol = "Package", verscol = "Version") {

    if(!is.null(GRepo) && !is(GRepo, "character") && !is(GRepo, "GRANRepository")) {
        stop("if GRepo is specified it must be a GRANRepository object or directory path")
    }
    
    if(!missing(GRepo) && is(GRepo, "character")) {
        if(!file.exists(file.path(GRepo, "repo.R")))
            stop("GRepo was a character but doesn't appear to be the location of a repo.R file")
        else{ 
            GRepo = loadRepo(filename = file.path(GRepo, "repo.R"))
        }
    }
    
    if(missing(repo_dir)) {
        if(is.null(GRepo))
            repo_dir = tempdir()
        else
            repo_dir = dest_base(GRepo)
    }
    
    if(missing(dir))
    {
        if(is(GRepo, "GRANRepository"))
            dir = notrack(GRepo)
        else
            dir = tempdir()
    }
    
    if(is.null(name)) {
        if(!is.null(doi))
            name = gsub("/", "_", doi, fixed=TRUE)
        else {
            ord = order(pkgdf[, "Package"])
            name = digest(paste(pkgdf[ord, 1], #Package name
                pkgdf[ord, 2])) #Pacakge version
        }
    }
    repo_dir = gsub("/$", "", repo_dir)
    vrepoloc = file.path(repo_dir, name,  Rvers, "src", "contrib")
    if(file.exists(vrepoloc)) {
        if(replace)
        {
            warning(paste("Replacing existing repository at", vrepoloc,
                          "Disregard warnings about file.symlnk"))
        } else {
            warning("A virtual repository already exists with that name in the specified location. Returning that repository.")
            return(vrepoloc)
        }
    }

    fils = getPkgVersions(pkgs = pkgdf, dir = dir, GRepo = GRepo, stoponfail = stoponfail, pkgcol = pkgcol, verscol = verscol)
    
    if(!file.exists(vrepoloc))
        dir.create(vrepoloc, recursive=TRUE)

    
    
    file.symlink(from = fils, to = file.path(vrepoloc, basename(fils)))
    write_PACKAGES(dir = vrepoloc, addFiles = TRUE, latestOnly = FALSE,
                   type = "source")
    if(install)
    {
        if(is.null(libloc))
            stop("Cannot install packages if libloc is not specified")
        install.packages(available.packages(contriburl =
                                            paste0("file://",
                                                  vrepoloc), type="source")[,"Package"],
                                            lib = libloc, type = "source",
                         contriburl = paste0("file://", vrepoloc))
    }
    vrepoloc
} 


##' installPkgVersion
##'
##' Install an exact version of a single package
##'
##' @param name package name
##' @param version package version string
##' @param repo (optional) GRANRepository object to search
##' @param dir directory to download package into
##' @param libloc library to install into. Defaults to first
##' element of \code{.libPaths()}
##' @param ... Additional arguments to be passed to install.packages
##' @return The full path to the downloaded file , or NULL if unable to
##' locate the package
##' @note This function does *NOT* find old versions of the package's
##' dependencies. It is safer to create a full (temporary) virtual Repository
##' using \code{makeVirtualRepo} or \code{sessionRepo}
##' @author Gabriel Becker
##' @export
installPkgVersion = function(name, version, repo = NULL, 
    dir = if(is.null(repo)) tempdir() else notrack(repo),
    libloc = .libPaths()[1],
    ...) {
    fname = locatePkgVersion(name = name, repo = repo,
        version = version, dir = dir)
    if(is.null(fname))
        stop(paste("Unable to find version", version,
                   "of package", name))
    else
        install.packages(fname, lib = libloc, ...)
}


##' locatePkgVersion
##'
##' Locate and download/build the exact version of a single package.
##'
##' @param name package name
##' @param version package version string
##' @param repo (optional) GRANRepository object to search
##' @param dir directory to download package into
##' @return The full path to the downloaded file , or NULL if unable to
##' locate the package
##' @author Gabriel Becker
##' @export
locatePkgVersion = function(name, version, repo = NULL, dir = if(is.null(repo)) tempdir() else notrack(repo)) {
    ##There are %three places we might find what we need in increasing order of computational cost:
    ##1. Already in the parent repository (including the associated notrack directory)
    ##2. In the cran archives
    ##3. By wading backwards in an SCM repository (SVN/git)

    ##round 1: check the repo
    if(!is.null(repo)) { 
        fname = findPkgVersionInRepo(name, version, repo)
        if(length(fname) && file.exists(fname))
            return(fname)
    }

    ##round 2a: Look in CRAN repo and  archive
    fname = findPkgVersionInCRAN(name, version, repo, dir = dir)
    if(length(fname) && file.exists(fname))
        return(fname)

    ##round 2b: Look in bioc repo and SVN
    fname = findPkgVersionInBioc(name, version, repo, dir = dir)
    if(length(fname) && file.exists(fname))
        return(fname)
    
    ##round 3: if we have an SCM for this package from the GRAN manifest
    ## or external source list we will look there in the future.
    ## fname = findInSCM(name, version, repo, dir)
    
    warning(sprintf("Unable to locate version %s of package %s", version, name))
    NULL

}

##TODO: make this able to see into sibling GRAN repositories. Right now it
## will only look in repo itself.
findPkgVersionInRepo = function(name, version, repo)
{
    repdir = repobase(repo)

    tarname = paste0( name, "_", version, ".tar.gz")
    fname = c(list.files(destination(repo), pattern = tarname,
        full.names = TRUE, recursive = TRUE),
        list.files(notrack(repo), pattern = tarname, full.names=TRUE,
                   recursive = TRUE)
        )
    if(!length(fname))
        fname = NULL
    else
        fname = normalizePath2(fname[1])
    fname
}

findPkgVersionInCRAN = function(name, version, repo, dir)
{
    destpath = dir
    pkgs = as.data.frame(available.packages( fields = c("Package", "Version")))
    if(nrow(pkgs) && name %in% pkgs$Package)
    {
        pkg = pkgs[pkgs$Package == name,]
        if(pkg$Version == version){
           return(download.packages(name, destdir = destpath, )[1,2])
        }
    }
        

    tarname = paste0(name, "_", version, ".tar.gz")
    
    cranurl = paste("http://cran.r-project.org/src/contrib/Archive", name, tarname, sep = "/")

  
    if(!file.exists(destpath))
        dir.create(destpath, recursive = TRUE)
    destfile = file.path(destpath, tarname)
    res = tryCatch(download.file(cranurl, destfile), error = function(e) e)
    if(is(res, "error") || res > 0)
        destfile = NULL

    destfile
}
        
##' @importFrom BiocInstaller biocinstallRepos biocVersion
##XXX This will only find package versions that exist on the trunk! New pkg versions created on old branches after a new release are missed!!!
findPkgVersionInBioc = function(name, version, repo, dir)
{
    
    ret = .biocTryToDL(name, version, dir = dir)
    if(!is.null(ret$file))
        return(ret$file)
    else {
        br = ret$biocVers
        ##XXX need to figure out how to deal with bioc branches.
        ## they aren't at the package level so this is difficult

        if(!file.exists(dir))
            dir.create(dir, recursive=TRUE)
        oldwd = getwd()
        setwd(dir)
        on.exit(setwd(oldwd))
        
        commit = findSVNRev(name, version, destpath = dir, repo = repo, ret$biocVers)
        if(is.null(commit))
            return(NULL)
        pkgdir = file.path(dir, name)
        system_w_init(paste("R CMD build --no-build-vignettes --no-resave-data --no-manual", pkgdir), repo = repo)
        ret = normalizePath2(list.files(pattern  = paste0(name, "_", version, ".tar.gz"), full.names=TRUE))
        setwd(pkgdir)
        system_w_init("svn up", repo = repo) #this gets us back to the trunk

    }
    ret
}


## tries to download the file. Returns list with two elements (file:dl'ed file or NULL and versionToSearch:bioc version)
.biocTryToDL = function(name, version, repo, dir, verbose = FALSE) {
    
    destpath = dir
    ##    urls = contrib.url(biocinstallRepos())
    ##    urls = urls[-length(urls)]
    urls = contrib.url(highestBiocVers())
#    biocVers = as.character(biocVersion())
    biocVers = biocVersFromRepo(urls)
    
    pkgAvail = TRUE
    everAvail = FALSE
    ret = NULL
    while(!is.null(urls) && pkgAvail && is.null(ret)) {
        if(verbose)
            message(sprintf("Searching Bioc repository for release %s", biocVersFromRepo(urls)))
        
        pkgs = as.data.frame(available.packages(urls, fields = c("Package", "Version")), stringsAsFactors=FALSE)
        pkg = pkgs[pkgs$Package == name,]       
        
        pkgAvail = nrow(pkg) > 0
        
        if(pkgAvail) {
            everAvail = TRUE
            versAvail = pkg[,"Version"]       
            if(compareVersion(versAvail, version) < 0) {
                if(verbose)
                    message(sprintf("Bioc repo for release %s has package version %s, earlier than desired version %s", biocVersFromRepo(urls), versAvail, version))
                pkgAvail = FALSE
            } else if (compareVersion(versAvail, version) == 0) {
                                        #                ret = download.packages(name, destdir = destpath, repos = urls)[1,2]
                filname = paste0(name, "_", version, .getExt(pkg[1,"Repository"]))
                dstfile = file.path(dir, filname)
                ret = tryCatch(download.file(paste(pkg[1,"Repository"], filname, sep="/"), destfile=dstfile), error=function(x) x)
                if(!is(ret, "error") && ret == 0) {
                    ret = dstfile
                    if(verbose)
                        message(sprintf("FOUND package %s version %s in Bioc release repository %s", name, version, biocVersFromRepo(urls)))
                } else {
                    stop(paste("Package version found but error occured when trying to retrieve it:", ret))
                }
            } else {
                biocVers = biocVersFromRepo(urls)
                urls = decrBiocRepo(urls)
            }
            
        }
        
    }
    list(file = ret, biocVers = biocVers)
    
}

.getExt = function(repourl) {
    typ = gsub(".*/(.*)/contrib", "\\1", repourl)
    switch(typ,
           src = ".tar.gz",
           win = ".zip",
           mac = ".tgz")
}


findSVNRev = function(name, version, destpath, repo, biocVers="trunk")
{
    if(biocVers != biocVersFromRepo(highestBiocVers()) && biocVers != "trunk") {
##        addl_dir = paste0("BioC_", biocVers)
        biocVers = paste("branches/RELEASE", gsub(".", "_", biocVers, fixed=TRUE), sep="_")
    } else {
##        biocVers = "trunk"
        addl_dir = ""
    }

    pkgdir = file.path(destpath, name) ##file.path(destpath, addl_dir)
    repoloc = paste0("https://hedgehog.fhcrc.org/bioconductor/", biocVers, "/madman/Rpacks/", name)
    if(!file.exists(pkgdir)) {
        src = makeSource(url = repoloc, type = "svn", user = "readonly", password="readonly")
        ##ret = makePkgSourceDir(name = name, source = src, path = file.path(destpath, addl_dir), repo = repo)
        ret = makePkgSourceDir(name = name, source = src, path = destpath, repo = repo)
        
        if(!ret)
            return(NULL)
    }         

    oldwd = getwd()
    setwd(file.path(destpath,  name))
    on.exit(setwd(oldwd))
    system_w_init(paste("svn switch --ignore-ancestry", repoloc))
    
    cmd0 = "svn log -r 1:HEAD --limit 1 DESCRIPTION"
    revs = system_w_init(cmd0, intern=TRUE, repo = repo)
    minrev = as.numeric(gsub("r([[:digit:]]*).*", "\\1", revs[2])) #first line is -------------------
     cmd1 = "svn log -r HEAD:1 --limit 1 DESCRIPTION"
    revs2 = system_w_init(cmd1, intern=TRUE, repo= repo)
    maxrev = as.numeric(gsub("r([[:digit:]]*).*", "\\1", revs2[2]))
    
    currev = floor((maxrev+minrev)/2)
    
    commit = binRevSearch(version, currev = currev, maxrev = maxrev, minrev = minrev, found = FALSE)
    cmd2 = paste("svn switch --ignore-ancestry -r", commit, repoloc)
    system_w_init(cmd2, repo = repo)
    return(commit)
        
}
    
binRevSearch = function(version, currev, maxrev, minrev, repo, found = FALSE)
{
    cmd = paste("svn diff --revision", paste(currev, maxrev, sep=":"), "DESCRIPTION")
                                        #  revs = tryCatch(system_w_init(cmd, intern=TRUE, repo=repo), error=function(x) x)
    revs = tryCatch(system(cmd, intern=TRUE), error=function(x) x)
    
    revVersions = grep(".*[Vv]ersion:", revs, value=TRUE)
    if(is(revs, "error"))
        return(NULL)

    if(!length(revVersions)) {
        if(minrev == maxrev - 1) {
            if(found)
                return(minrev)
            else
                return(NULL)
        } else {
            return(binRevSearch(version, floor((minrev + currev )/2),  currev, minrev, repo, found = found))
        }
    }
        
    revVNums = gsub(".*:(.*)", "\\1", revVersions)
    afterInd = grep("+", revVersions, fixed=TRUE)
    after = revVNums[afterInd]
    before = revVNums[-afterInd]
    if(compareVersion(after, version) == 0)
        found = TRUE
    if(minrev == maxrev -1) {
        if(compareVersion(after, version) == 0)
            return(maxrev)
        else if (compareVersion(before, version) == 0)
            return(minrev)
        else
            return(NULL)
    } else if(compareVersion(before, version) == -1)
        return(binRevSearch(version, floor((currev + maxrev)/2), maxrev, currev, repo, found = found))
    else
        return(binRevSearch(version, floor((minrev + currev )/2),  currev, minrev, repo, found = found))
}
                                        #-1 is second is later, 1 if first is later
        
        ##svn log -q VERSION | grep ^r | awk '{print $1}' | sed -e 's/^r//' 
        
