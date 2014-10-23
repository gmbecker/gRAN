setClass("PkgSource", representation(name = "character",location="character",
                                     branch = "character",
                                     subdir = "character", user = "character",
                                     password="character"))
setClass("SVNSource", contains = "PkgSource")
setClass("GitSource", contains = "PkgSource")
setClass("GithubSource", contains = "GitSource")
setClass("CVSSource", contains = "PkgSource")
setClass("LocalSource", contains = "PkgSource")
setClass("CRANSource", contains = "PkgSource")



setAs("GitSource", "SVNSource",
      function(src) {
              if(!grepl("github", location(src)))
                  stop("Cannot convert non-github GitSource object to SVNSource")
              else {
                  url = gsub( "\\.git", "", location(src))
                  url = gsub("git://", "http://", url)
                  br = if(branch(src) == "master") "trunk" else branch(src)
                  makeSource(name = src@name, url = url, branch = br, subdir = subdir(src), user = "", password = "")
              }
          })






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

SessionManifest = function(manifest, versions) {
    new("SessionManifest", pkg_versions = versions, pkg_manifest = manifest)
}




##'@export
setClass("RepoBuildParam", representation(
    repoName = "character",
    tempRepo = "character",
    
    baseDir = "character",
    tempCheckout = "character",
    errlog = "character",
    logfile = "character",
    tempLibLoc = "character",
    checkWarnOk = "logical",
    checkNoteOk = "logical",
    extraFun = "function",
    auth = "character",
    dest_base = "character",
    dest_url = "character",
    shell_init = "character",
    installTest = "logical",
    checkTest = "logical",
    suspended = "character"))


##' @export
setClass("GRANRepository", representation(
    results = "data.frame",
#    manifest = "PkgManifest",
    manifest = "SessionManifest",
    param = "RepoBuildParam"
))




##' GRANRepository
##'
##' A constructor for the \code{GRANRepository} class of S4 objects representing
##' individual repositories
##'
##' @param manifest A PkgManifest object
##' @param results A data.frame containing previous build results
##' @param param A RepoBuildParam object controlling the location and behavior of
##' the repository being built
##' @param ... Passed through to the default value of \code{param}
##' @export
GRANRepository = function(manifest,
    results,
    param = RepoBuildParam(...),
    ...) {

    if(missing(results))
        results = ResultsRow(name = manifest_df(manifest)$name)
    new("GRANRepository", manifest = manifest, results = results, param = param)
}



##' RepoBuildParam
##' 
##' @param basedir The base directory. By default the temporary repository,
##' temporary install library, and package staging area will be located in
##' <basedir>/<subrepoName>/, while the  temporary source checkout will be in t
##' he basedir itself.
##' 
##' @param subrepoName The name of the repository, e.g. stable or devel
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
##' @param installTest logical. Should the install test be performed? Required
##' to build packages with vignettes, and for the check test
##' @param checkTest logical. Should R CMD check be run on the packages as a
##' cohort. Requires install test.
##' @export



RepoBuildParam = function(
    basedir,
    repo_name = "current",
    tempRepo = file.path(basedir, repo_name, "tmprepo"),
    tempCheckout = file.path(basedir, "tmpcheckout"),
    errlog = file.path(basedir, repo_name, paste0("GRAN-errors-", repo_name,
        "-", Sys.Date(), ".log")),
    logfile = file.path(basedir, repo_name, paste0("GRAN-log-", repo_name,
        "-", Sys.Date(), ".log")),
    checkNoteOk = TRUE,
    checkWarnOk = TRUE,
    tempLibLoc = file.path(basedir, repo_name, "LibLoc"),
    extraFun = function(...) NULL,
    destination = basedir,
    auth = "",
    dest_url = paste0("file://", normalizePath2(destination)),
    shell_init = "",
    installTest = TRUE,
    checkTest = TRUE)
{
    
    if(!file.exists(basedir))
        dir.create(basedir, recursive = TRUE)
    
    basedir = normalizePath2(basedir)

    prepDirStructure(basedir, repo_name, tempRepo, tempCheckout, tempLibLoc,
                     destination)


    if(checkTest && !installTest)
        stop("Cannot run check test without install test")
    
    repo = new("RepoBuildParam", baseDir = basedir,
        repoName = repo_name,
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
        dest_url = dest_url,
        shell_init = shell_init,
        installTest = installTest,
        checkTest = checkTest)
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

