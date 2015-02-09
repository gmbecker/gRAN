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
    if(!(is(sinfo, "sessionInfo") || is(sinfo, "character") ||
         is(sinfo, "parsedSessionInfo"))) {
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
    makeVirtualRepo(pkgdf, repo_dir, doi, dir, name,
                    replace, stoponfail = stoponfail, GRepo, install, libloc)
    
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


##' Create a virtual repository containing only the specified package versions versions 
##'
##' Create a virtual repository which contains only the exact packages specified
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

makeVirtualRepo = function(pkgdf,
    repo_dir,
    doi,
    dir,
    name=NULL,
    replace=FALSE,
    stoponfail=TRUE,
    GRepo,
    install=FALSE,
    libloc = NULL,
    Rvers="",
    pkgcol = "Package",
    verscol = "Version") {

    if(!is.null(GRepo) && !is(GRepo, "character") &&
       !is(GRepo, "GRANRepository")) {
        stop("if GRepo is specified it must be a GRANRepository",
             "object or directory path")
    }
    
    if(!missing(GRepo) && is(GRepo, "character")) {
        if(!file.exists(file.path(GRepo, "repo.R")))
            stop("GRepo was a character but doesn't appear to be the",
                 "location of a repo.R file")
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
            warning("A virtual repository already exists with that name in",
                    "the specified location. Returning that repository.")
            return(vrepoloc)
        }
    }

    fils = getPkgVersions(pkgs = pkgdf, dir = dir, GRepo = GRepo,
        stoponfail = stoponfail, pkgcol = pkgcol, verscol = verscol)
    
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

