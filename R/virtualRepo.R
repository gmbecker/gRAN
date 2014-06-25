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
    makeVirtualRepo(pkgdf, repo_dir, doi, dir, name, replace, stoponfail, GRepo, install, libloc)
    
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
##' 
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
        else
        name = digest(pkgdf[order(pkgdf[,1])])
    }
       vrepoloc = file.path(repo_dir, name,  Rvers, "src", "contrib")
    if(file.exists(vrepoloc)) {
        if(replace)
        {
            warning(paste("Replacing existing repository at", vrepoloc,
                          "Disregard warnings about file.symlnk"))
        } else {
            stop("A virtual repository already exists with that name.")
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
        
##' @importFrom BiocInstaller biocinstallRepos
##XXX This will only find package versions that exist on the trunk! New pkg versions created on old branches after a new release are missed!!!
findPkgVersionInBioc = function(name, version, repo, dir)
{
    destpath = dir
    urls = contrib.url(biocinstallRepos())
    urls = urls[-length(urls)]
    pkgs = as.data.frame(available.packages(urls, fields = c("Package", "Version")))
    pkg = pkgs[pkgs$Package == name,]
    if(!nrow(pkg))
        return(NULL)
    
    if(nrow(pkg) && pkg$Version == version)
    {
        ret = download.packages(name, destdir = destpath, repos = biocinstallRepos())[1,2]
    } else {
        src = makeSource(url = paste0("https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/", name), type = "gitsvn", user = "readonly", password="readonly")
        ret = makePkgSourceDir(name = name, source = src, path = destpath, repo = repo)
        if(!ret)
            return(NULL)
        oldwd = getwd()
        setwd(file.path(destpath, name))
        on.exit(setwd(oldwd))
        revs = tryCatch(system_w_init("git svn log -p DESCRIPTION",
            intern = TRUE, repo = repo), error = function(e) e)
        if(is(revs, "error") || !length(revs))
            return(NULL)

        revspots  = grep("^r([0-9]+)", revs)
        found = grep(paste0("\\+[Vv]ersion:.*", version), revs)
        if(!length(found))
            return(NULL)
        pos = max(revspots[revspots < found])
        commit = gsub("^(r[0-9]+).*", "\\1", revs[pos])
        gitcommit = system_w_init(paste("git svn find-rev", commit),
            intern=TRUE, repo = repo)
        cmd = paste0("git checkout ", gitcommit)
        res = system_w_init(cmd, intern=TRUE, repo = repo)
        if(is(res, "error")) {
            warning("git svn was unable to checkout the identified commit")
            return(NULL)
        }
        
        pkgdir = getwd()
        setwd(destpath)
        system_w_init(paste("R CMD build --no-build-vignettes --no-resave-data --no-manual", pkgdir), repo = repo)
        ret = normalizePath2(list.files(pattern  = paste0(name, "_", version, ".tar.gz"), full.names=TRUE))

        setwd(pkgdir)
        system_w_init("git checkout master", repo = repo)
    }
    ret
}


    ##svn log -q VERSION | grep ^r | awk '{print $1}' | sed -e 's/^r//' 
