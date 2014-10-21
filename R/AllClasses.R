setClass("PkgSource", representation(name = "character",location="character",
                                     branch = "character",
                                     subdir = "character", user = "character",
                                     password="character"))
setClass("SVNSource", contains = "PkgSource")
setClass("GitSource", contains = "PkgSource")
setClass("GithubSource", contains = "GitSource")
setClass("GitSVNSource", contains = "PkgSource")
setClass("CVSSource", contains = "PkgSource")
setClass("LocalSource", contains = "PkgSource")




##'@export
##'
setClass("PkgManifest", representation( manifest = "data.frame",
                                          dependency_repos = "character"))

##'@export
##' @import RCurl
PkgManifest = function(manifest, dep_repos = c(biocinstallRepos(), defaultGRAN()), ...) {
    if(is.character(manifest)) {
        if(is.url(manifest)) {
            fil = tempfile()
            download.file(manifest, method = "curl", fil)
            manifest  = fil
        }

        if(file.exists(manifest))
            manifest = read.table(manifest, header= TRUE, sep= ",", stringsAsFactors = FALSE, ...)
        else
            stop("invalid manifest")
    }

    new("PkgManifest", manifest = manifest, dependency_repos = dep_repos)
}

setClass("GithubPkgManifest", contains = "PkgManifest")



#manifest is a data.frame with the following columns:
##name, url, type, subdir, branch, extra
manifestBaseCols = c("name", "url", "type", "subdir", "branch", "extra")







##'@export
setClass("SessionManifest", representation(pkg_versions = "data.frame",
                                           pkg_manifest = "PkgManifest"))





##'@export
setClass("RepoBuildParam", representation(tempRepo = "character",
                                          baseDir = "character",
                                          tempCheckout = "character",
                                          errlog = "character",
                                          logfile = "character",
                                          rversion = "character",
                                          repoName="character",
                                          tempLibLoc = "character",
                                          checkWarnOk = "logical",
                                          checkNoteOk = "logical",
                                          extraFun = "function",
                                          auth = "character",
                                          dest_base = "character",
                                          dest_url = "character",
                                          shell_init = "character"))


##' @export
setClass("GRANRepository", representation(
    results = "data.frame",
    manifest = "PkgManifest",
    param = "RepoBuildParam"
))




##' GRANRepository
##'
##' A constructor for the \code{GRANRepository} class of S4 objects representing
##' individual repositories
##'
##' @param basedir The base directory. By default the temporary repository,
##' temporary install library, and package staging area will be located in
##' <basedir>/<subrepoName>/, while the  temporary source checkout will be in t
##' he basedir itself.
##' 
##' @param subrepoName The name of the repository, e.g. stable or devel
##' @param rversion The R executable to use when invoking R CMDs and package
##'   testing code
##' @param tempRepo Location to create the temporary repository
##' @param tempCheckout Location to create temporary checkouts/copies of package
##'   source code
##' @param errlog The file to append error output to during the building and
##'   testing processes
##' @param logfile The file to append summary log information to during building
##'   and testing
##' @param checkNoteOk logical. Whether packages that raise notes during
##'   R CMD check should be considered to have passed
##' @param checkWarnOk logical. Whether packages that raise warnings during
##'   R CMD check should be considered to have passed
##' @param tempLibLoc Location to create the temporary installed package library
##'   for use during the testing process
##' @param extraFun currently ignored
##' @param destination Base location (not including repository name) of the
##'   final repository to be built
##' @param auth character. Authentication information required to add packages
##'   to the manifest.
##' @param manifest data.frame. The manifest of package information associated
##'   with this repository. Defaults to an empty manifest.
##' @param dest_url The base URL the destination directory corresponds to. The
##' subrepository name will be appended to this to generate the URL used when
##' installing from the repository.
##' @param shell_init An optional shell script to source before invoking system
##' commands, e.g. a bashrc file. Ignored if "" or not specified.
##' @export
GRANRepository = function(basedir,
    subrepoName = "stable",
    rversion = "R",
    tempRepo = file.path(basedir, subrepoName, "tmprepo"),
    tempCheckout = file.path(basedir, "tmpcheckout"),
    errlog = file.path(basedir, subrepoName, paste0("GRAN-errors-", subrepoName,
        "-", Sys.Date(), ".log")),
    logfile = file.path(basedir, subrepoName, paste0("GRAN-log-", subrepoName,
        "-", Sys.Date(), ".log")),
    checkNoteOk = TRUE,
    checkWarnOk = TRUE,
    tempLibLoc = file.path(basedir, subrepoName, "LibLoc"),
    extraFun = function(...) NULL,
    destination = basedir,
    auth = "",
    manifest = emptyManifest,
    dest_url = paste0("file://", normalizePath2(destination)),
    shell_init = "")
{
    
    if(!file.exists(basedir))
        dir.create(basedir, recursive = TRUE)
    
    basedir = normalizePath2(basedir)

    prepDirStructure(basedir, subrepoName, tempRepo, tempCheckout, tempLibLoc,
                     destination)

    
    repo = new("GRANRepository", baseDir = basedir,
        subrepoName = subrepoName,
        rversion = rversion,
        tempRepo = normalizePath2(tempRepo),
        tempCheckout = normalizePath2(tempCheckout),
        errlog = errlog,
        logfile = logfile,
        checkNoteOk = checkNoteOk,
        checkWarnOk = checkWarnOk,
        tempLibLoc = normalizePath2(tempLibLoc),
        extraFun = extraFun,
        dest_base= normalizePath2(destination),
        auth = auth,
        manifest = manifest,
        dest_url = dest_url,
        shell_init = shell_init)

    if(!nrow(manifest))
        repo@manifest = readManifest(repo = repo)
    repo
}

prepDirStructure = function(basedir, subrepo, temprepo, tempcheckout,
    templibloc, destination) {
    if(!file.exists(file.path(basedir, subrepo, "src", "contrib")))
        dir.create(file.path(basedir, subrepo, "src", "contrib"),
                   recursive=TRUE)
    if(!file.exists(file.path(temprepo, "src", "contrib")))
        dir.create(file.path(temprepo, "src", "contrib"),
                   recursive = TRUE)
    if(!file.exists(tempcheckout))
        dir.create(tempcheckout, recursive = TRUE)
    if(!file.exists(templibloc))
        dir.create(templibloc, recursive = TRUE)
    if(!file.exists(file.path(basedir, subrepo, "staging")))
        dir.create(file.path(basedir, subrepo, "staging"),
                   recursive = TRUE)
    if(!file.exists(file.path(destination, subrepo, "src", "contrib")))
        dir.create(file.path(destination, subrepo, "src", "contrib"),
                   recursive = TRUE)
    if(!file.exists(file.path(destination, subrepo, "CheckResults")))
        dir.create(file.path(destination, subrepo, "CheckResults"),
                   recursive = TRUE)
    
}


##'@export
setClass("parsedSessionInfo", representation(version = "character",
                                             platform="character",
                                             attached = "data.frame",
                                             loaded = "data.frame"))

