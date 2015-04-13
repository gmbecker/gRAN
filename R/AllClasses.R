##' @import switchr




##' @rdname RepoBuildParam
##' @export
setClass("RepoBuildParam", representation(
    repo_name = "character",
    temp_repo = "character",
    
    base_dir = "character",
    temp_checkout = "character",
    errlog = "character",
    logfile = "character",
    tempLibLoc = "character",
    check_warn_ok = "logical",
    check_note_ok = "logical",
    extra_fun = "function",
    auth = "character",
    dest_base = "character",
    dest_url = "character",
    install_test = "logical",
    check_test = "logical",
    suspended = "character"),
         contains = "SwitchrParam")


##' @rdname GRANRepository
##' @export
setClass("GRANRepository", representation(
    results = "data.frame",
#    manifest = "PkgManifest",
    manifest = "SessionManifest",
    param = "RepoBuildParam"
))


setClass("GRANRepositoryv0.9", representation(tempRepo = "character",
                                          baseDir = "character",
                                          tempCheckout = "character",
                                          errlog = "character",
                                          logfile = "character",
                                          rversion = "character",
                                          subrepoName="character",
                                          tempLibLoc = "character",
                                          manifest = "data.frame",
                                          checkWarnOk = "logical",
                                          checkNoteOk = "logical",
                                          extraFun = "function",
                                          auth = "character",
                                          dest_base = "character",
                                          dest_url = "character",
                                          shell_init = "character"
                                          ))



updateGRANRepoObject = function(object, ...) {
              param = RepoBuildParam(basedir = object@baseDir,
                  temp_repo = object@tempRepo,
                  repo_name = object@subrepoName,
                  errlog = object@errlog,
                  logfile = object@logfile,
                  temp_checkout = object@tempCheckout,
                  check_note_ok = object@checkNoteOk,
                  check_warn_ok = object@checkWarnOk,
                  tempLibLoc = object@tempLibLoc,
                  extra_fun = object@extraFun,
                  destination = object@dest_base,
                  dest_url = object@dest_url,
                  shell_init = object@shell_init,
                  auth = object@auth,
                  ...)
              
              man = PkgManifest(manifest = object@manifest[,names(ManifestRow())])
              results = data.frame(name = manifest_df(man)$name,
                  object@manifest[,!names(object@manifest) %in% names(ManifestRow())],
                  stringsAsFactors = FALSE)
              vers = data.frame(name = manifest_df(man)$name, version = NA_character_,
                  stringsAsFactors = FALSE)
              GRANRepository(manifest = SessionManifest(manifest =man, versions = vers), results = results, param = param)
          }



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
##' @docType methods
##' @export
GRANRepository = function(manifest,
    results,
    param = RepoBuildParam(...),
    ...) {

    if(missing(results))
        results = ResultsRow(name = manifest_df(manifest)$name)
    if(is(manifest, "PkgManifest"))
        manifest = SessionManifest(manifest - manifest,
            versions = data.frame(name = manifest_df(manifest)$name,
                version = NA_character_,
                stringsAsFactors = FALSE))
    
    new("GRANRepository", manifest = manifest, results = results, param = param)
}



##' RepoBuildParam
##'
##' Constructor for  RepoBuildParam objects
##' 
##' @param basedir The base directory. By default the temporary repository,
##' temporary install library, and package staging area will be located in
##' <basedir>/<subrepoName>/, while the  temporary source checkout will be in t
##' he basedir itself.
##' 
##' @param repo_name The name of the repository, e.g. stable or devel
##' @param temp_repo Location to create the temporary repository
##' @param temp_checkout Location to create temporary checkouts/copies of package
##'   source code
##' @param errlog The file to append error output to during the building and
##'   testing processes. Specifying a non-default \code{logfun} overrides this argument.
##' @param logfile The file to append summary log information to during building
##'   and testing. Specifying a non-default \code{logfun} overrides this argument.
##' @param check_note_ok logical. Whether packages that raise notes during
##'   R CMD check should be considered to have passed
##' @param check_warn_ok logical. Whether packages that raise warnings during
##'   R CMD check should be considered to have passed
##' @param tempLibLoc Location to create the temporary installed package library
##'   for use during the testing process
##' @param extra_fun currently ignored
##' @param destination Base location (not including repository name) of the
##'   final repository to be built
##' @param auth character. Authentication information required to add packages
##'   to the manifest.
##' @param dest_url The base URL the destination directory corresponds to. The
##' subrepository name will be appended to this to generate the URL used when
##' installing from the repository.
##' @param shell_init An optional shell script to source before invoking system
##' commands, e.g. a bashrc file. Ignored if "" or not specified.
##' @param logfun A function to call to write the error and full log messages. The default is a closer which carries around the locations specified via \code{logfile} and \code{errfile}.
##' @param install_test logical. Should the install test be performed? Required
##' to build packages with vignettes, and for the check test
##' @param check_test logical. Should R CMD check be run on the packages as a
##' cohort. Requires install test.
##' @return A parameter object representing the options to use when building a package
##' repository from a GRANRepository object.
##' @rdname RepoBuildParam
##' @export



RepoBuildParam = function(
    basedir,
    repo_name = "current",
    temp_repo = file.path(basedir, repo_name, "tmprepo"),
    temp_checkout = file.path(basedir, "tmpcheckout"),
    errlog = file.path(basedir, repo_name, paste0("GRAN-errors-", repo_name,
        "-", Sys.Date(), ".log")),
    logfile = file.path(basedir, repo_name, paste0("GRAN-log-", repo_name,
        "-", Sys.Date(), ".log")),
    check_note_ok = TRUE,
    check_warn_ok = TRUE,
    tempLibLoc = file.path(basedir, repo_name, "LibLoc"),
    extra_fun = function(...) NULL,
    destination = basedir,
    auth = "",
    dest_url = paste0("file://", normalizePath2(destination)),
    shell_init = character(),
    logfun = function(...) writeGRANLog(..., logfile = logfile, errfile = errlog),
    install_test = TRUE,
    check_test = TRUE)
{
    
    if(!file.exists(basedir))
        dir.create(basedir, recursive = TRUE)
    
    basedir = normalizePath2(basedir)

    prepDirStructure(basedir, repo_name, temp_repo, temp_checkout, tempLibLoc,
                     destination)


    if(check_test && !install_test)
        stop("Cannot run check test without install test")
    
    repo = new("RepoBuildParam", base_dir = basedir,
        repo_name = repo_name,
        temp_repo = normalizePath2(temp_repo),
        temp_checkout = normalizePath2(temp_checkout),
        errlog = errlog,
        logfile = logfile,
        check_note_ok = check_note_ok,
        check_warn_ok = check_warn_ok,
        tempLibLoc = normalizePath2(tempLibLoc),
        extra_fun = extra_fun,
        dest_base= normalizePath2(destination),
        auth = auth,
        dest_url = dest_url,
        shell_init = shell_init,
        logfun = logfun,
        install_test = install_test,
        check_test = check_test)
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


