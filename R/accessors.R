##' logfile
##' 
##' Retrieve the path to the full logfile for a GRAN repository
##'
##' @docType methods
##' @aliases logfile-method,GRANRepository
##' @rdname GRANRepository-accessors
##' @title Log file location of a GRAN (sub) repository
##' @param repo a GRANRepository object
##' @return file location of the full logfile
##' @author Gabriel Becker
##' @export
setGeneric("logfile", function(repo) standardGeneric("logfile"))
##' @rdname GRANRepository-accessors
##' @aliases logfile,GRANRepository-method
setMethod("logfile", "GRANRepository", function(repo) {
    ret = param(repo)@logfile
    if(!file.exists(dirname(ret)))
        dir.create(dirname(ret), recursive = TRUE)
    ret})


##' errlogfile
##' Retrieve the path to the errors-only logfile for a GRAN repository
##'
##' @docType methods
##' @rdname errlogfile-methods
##' @title Log file location of a GRAN (sub) repository
##' @param repo a GRANRepository object
##' @return file location of the errors-only logfile
##' @author Gabriel Becker
##' @export
setGeneric("errlogfile", function(repo) standardGeneric("errlogfile"))
##' @rdname errlogfile-methods
##' @aliases errlogfile,GRANRepository-method
setMethod("errlogfile", "GRANRepository", function(repo) {
        ret = param(repo)@errlog
    if(!file.exists(dirname(ret)))
        dir.create(dirname(ret), recursive = TRUE)
    ret})

##' Retrieve the path to a GRAN (sub) repository
##' @rdname location-methods
##' @param repo A GRANRepository object.
##' @aliases location,GRANREpository-method
setMethod("location", "GRANRepository", function(repo) {
    ret = file.path(repobase(repo), "src", "contrib")
    
    if(!file.exists(ret))
        dir.create(ret, recursive=TRUE)
    normalizePath2(ret)
})


##'repobase
##' Generic accessor function to retreive the repo specific subdirectory within the base directory
##'
##' @rdname repobase-methods
##' @param repo a GRANRepository object
##' @return The path to the repository specific directory
##' @export
setGeneric("repobase", function(repo) standardGeneric("repobase"))
##' @rdname repobase-methods
##' @aliases repobase,GRANRepository-method
##' @export
setMethod("repobase", "GRANRepository", function(repo) {
    ret = file.path(param(repo)@base_dir, param(repo)@repo_name)
    if(!file.exists(ret))
        dir.create(ret, recursive=TRUE)
    normalizePath2(ret)
})


##' staging
##' Return the staging directory to be used when building the repository. If the directory does not exist it will be created.
##'
##' @rdname staging-methods
##' @param repo a GRANRepository object
##' @return The path to the repository specific directory
##' @export
setGeneric("staging", function(repo) standardGeneric("staging"))
##' @rdname staging-methods
##' @aliases staging,GRANRepository-method
##' @export
setMethod("staging", "GRANRepository", function(repo) {
    ret = file.path(repobase(repo), "staging")
    if(!file.exists(ret))
        dir.create(ret, recursive=TRUE)
    normalizePath2(ret)
})

##' temporary library
##'
##' @param repo A GRANRepository object
##' @docType methods
##' @rdname templib
##' @export
setGeneric("temp_lib", function(repo) standardGeneric("temp_lib"))
##' @rdname templib
##' @aliases temp_lib,GRANRepository
setMethod("temp_lib", "GRANRepository",
          function(repo) normalizePath2(param(repo)@tempLibLoc))

##' notrack
##' Return the directory which stores retreived versions of non-GRAN packages
##' for use in virtual repositories
##'
##' @param repo a GRANRepository object
##' @return The path to the notrack directory
##' @rdname notrack-methods
##' @aliases notrack,GRANRepository-method
##' @docType methods
setMethod("notrack", "GRANRepository",
          function(repo) file.path(repobase(repo), "notrack"))

##' destination
##' Return the full path to the contrib directory for the final repository
##' deployment.
##'
##' @rdname destination-methods
##' @param repo a GRANRepository object
##' @return For destination, the full path to the contrib directory the packages
##' will be deployed to
##' @docType methods
##' @export
setGeneric("destination", function(repo) standardGeneric("destination"))
##' @rdname destination-methods
##' @aliases destination,GRANRepository-method
##' @export
setMethod("destination","GRANRepository",
          function(repo) file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name,  "src", "contrib"))


##' dest_base
##' Return the full path to the contrib directory for the final repository
##' deployment.
##'
##' @rdname dest_base-methods
##' @param repo a GRANRepository object
##' @return For dest_base, the base directory the repository will reside in
##' @docType methods
##' @export
setGeneric("dest_base", function(repo) standardGeneric("dest_base"))
##' @rdname dest_base-methods
##' @aliases dest_base,GRANRepository-method
##' @export
setMethod("dest_base","GRANRepository",
          function(repo) file.path(normalizePath2(param(repo)@dest_base)))


##' check_result_dir
##' Return the path where check results for packages will be deployed for use in
##' the build report.
##'
##' @rdname check_result_dir-methods
##' @param repo a GRANRepository object
##' @return The directory where check results should be deployed for use in the
##' build report
##' @docType methods
##' @export
setGeneric("check_result_dir", function(repo) standardGeneric("check_result_dir"))
##' @rdname check_result_dir-methods
##' @aliases check_result_dir,GRANRepository-method
##' @export
setMethod("check_result_dir","GRANRepository",
          function(repo) file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "CheckResults" ))


##' install_result_dir
##' Return the path where instal results for packages will be deployed for use in
##' the build report.
##'
##' @rdname install_result_dir-methods
##' @param repo a GRANRepository object
##' @return The directory where install results should be deployed for use in the
##' build report
##' @docType methods
##' @export
setGeneric("install_result_dir", function(repo) standardGeneric("install_result_dir"))
##' @rdname install_result_dir-methods
##' @aliases install_result_dir,GRANRepository-method
##' @export
setMethod("install_result_dir","GRANRepository",
          function(repo) file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "InstallResults" ))






##' repo_url
##' Return the url that the repository will be served at. This is the web
##' address corresponding to repository, suitable for calling contrib.url.  e.g
##' http://www.website.com/gran/current/
##'
##' @rdname repo_url-methods
##' @param repo a GRANRepository object
##' @return For destination, the full path to the contrib directory the packages
##' will be deployed to
##' @docType methods
##' @export
setGeneric("repo_url", function(repo) standardGeneric("repo_url"))
##' @rdname repo_url-methods
##' @aliases repo_url,GRANRepository-method
##' @export
setMethod("repo_url","GRANRepository",
          function(repo) paste(param(repo)@dest_url,
                               param(repo)@repo_name,
                               sep="/"))
##' @rdname repo_url-methods
##' @aliases repo_url,NULL
##' @export
setMethod("repo_url","NULL", function(repo) NULL)


##' checkout_dir
##' Return the directory that package sources will be checked-out into
##' for use in the build process
##'
##' @rdname checkout_dir-methods
##' @param repo a GRANRepository object
##' @return For destination, the full path to the contrib directory the packages
##' will be deployed to
##' @docType methods
##' @export
setGeneric("checkout_dir", function(repo) standardGeneric("checkout_dir"))
##' @rdname checkout_dir-methods
##' @aliases checkout_dir,GRANRepository-method
##' @export
setMethod("checkout_dir","GRANRepository",
          function(repo) param(repo)@temp_checkout)

##' @rdname checkout_dir-methods
##' @aliases checkout_dir,NULL
##' @export
setMethod("checkout_dir","NULL", function(repo) NULL)



setMethod("manifest<-", "GRANRepository",
          function(x, value ) {
              x@manifest = value
              x
          })


setMethod("manifest", "GRANRepository", function(x) x@manifest)

## only get manifest rows for pkgs in the 'session' by default
## override with session_only=FALSE if desired
setMethod("manifest_df", "GRANRepository", function(x, ...) manifest_df(manifest(x), ...))

setMethod("manifest_df<-", "GRANRepository", function(x, value) {
    manifest_df(manifest(x)) = value
    x
    })
setMethod("versions_df", "GRANRepository", function(x) versions_df(manifest(x)))
setMethod("versions_df<-", "GRANRepository", function(x, value) {
    versions_df(manifest(x)) = value
    x
    })

##' Repository build results
##'
##' @param x A GRANRepository object
##' @return A data.frame of build results
##' @docType methods
##' @rdname reporesults
##' @export
setGeneric("repo_results", function(x) standardGeneric("repo_results"))
##' @rdname reporesults
##' @aliases repo_results,GRANRepository
setMethod("repo_results", "GRANRepository", function(x) x@results)

##' ##'@export
##' @rdname reporesults
##' @param value The new results data.frame
setGeneric("repo_results<-", function(x, value) standardGeneric("repo_results<-"))
##' @rdname reporesults
##' @aliases repo_results<-,GRANRepository

setMethod("repo_results<-", "GRANRepository", function(x, value) {
    x@results = value
    x
    })

##' Extract parameter object
##' @param x An object with an associated paramater
##' @docType methods
##' @export
setGeneric("param", function(x) standardGeneric("param"))
##' @rdname param
##' @aliases param,GRANRepository
setMethod("param", "GRANRepository",
          function(x) x@param)

##' @param value A new parameter object
##' @rdname param
##' @export
setGeneric("param<-", function(x, value) standardGeneric("param<-"))
##' @rdname param
##' @aliases param<-,GRANRepository
setMethod("param<-", "GRANRepository",
          function(x, value){
              x@param = value
              x
          })

##' Get or set individual parameters on a GRANRepository object
##' @param x A GRANRepository object
##' @details These functions get or set individual repository build parameters
##' on a GRANRepository object.
##' @seealso \code{\link{RepoBuildParam}}
##' @docType methods
##' @rdname GRANparams
##' @export
setGeneric("repo_name", function(x) standardGeneric("repo_name"))
##' @rdname GRANparams
##' @aliases repo_name,GRANRepository
setMethod("repo_name", "GRANRepository",
          function(x) param(x)@repo_name)


##' @rdname GRANparams
##' @export
setGeneric("temp_repo", function(x) standardGeneric("temp_repo"))
##' @rdname GRANparams
##' @aliases temp_repo,GRANRepository
setMethod("temp_repo", "GRANRepository",
          function(x) param(x)@temp_repo)



##' @rdname GRANparams
##' @export
setGeneric("check_warn_ok", function(x) standardGeneric("check_warn_ok"))
##' @rdname GRANparams
##' @aliases check_warn_ok,GRANRepository
setMethod("check_warn_ok", "GRANRepository",
          function(x)  param(x)@check_warn_ok)

##'@rdname GRANparams
##' @export
setGeneric("check_note_ok", function(x) standardGeneric("check_note_ok"))
##' @rdname GRANparams
##' @aliases check_note_ok,GRANRepository
setMethod("check_note_ok", "GRANRepository",
          function(x)  param(x)@check_note_ok)

##' @rdname GRANparams
##' @export
setGeneric("suspended_pkgs", function(x) standardGeneric("suspended_pkgs"))
##' @rdname GRANparams
##' @aliases suspended_pkgs,GRANRepository
setMethod("suspended_pkgs", "GRANRepository",
          function(x) param(x)@suspended)

##' @rdname GRANparams
##' @param value The new parameter value
##' @export
setGeneric("suspended_pkgs<-", function(x, value) standardGeneric("suspended_pkgs<-"))
##' @rdname GRANparams
##' @aliases suspended_pkgs<-,GRANRepository
setMethod("suspended_pkgs<-", "GRANRepository",
          function(x, value) {
              param(x)@suspended = value
              x
              })





##'@rdname GRANparams
##' @aliases sh_init_script,GRANRepository
##' @export
setMethod("sh_init_script", "GRANRepository",
          function(x) param(x)@shell_init)
##'@rdname GRANparams
##' @aliases sh_init_script<-,GRANRepository
##'@export

setMethod("sh_init_script<-", "GRANRepository",
          function(x, value) {
              param(x)@shell_init= value
              x
              })








##' @rdname GRANparams
##' @export
setGeneric("extra_fun", function(x) standardGeneric("extra_fun"))
##' @rdname GRANparams
##' @export
setMethod("extra_fun", "GRANRepository",
          function(x)  param(x)@extra_fun)

##' @rdname GRANparams
##' @export
setGeneric("check_test_on", function(x) standardGeneric("check_test_on"))
##' @rdname GRANparams
##' @aliases check_test_on,RepoBuildParam
setMethod("check_test_on", "RepoBuildParam", function(x) x@check_test)
##' @rdname GRANparams
##' @aliases check_test_on,GRANRepository
setMethod("check_test_on", "GRANRepository", function(x) check_test_on(param(x)))

##' @rdname GRANparams
##' @export
setGeneric("install_test_on", function(x) standardGeneric("install_test_on"))
##' @rdname GRANparams
##' @aliases install_test_on,RepoBuildParam

setMethod("install_test_on", "RepoBuildParam", function(x) x@install_test)
##' @rdname GRANparams
##' @aliases install_test_on,GRANRepository
setMethod("install_test_on", "GRANRepository", function(x) install_test_on(param(x)))


##' @rdname GRANparams
##' @aliases logfun,GRANRepository
##' @export
setMethod("logfun", "GRANRepository",
          function(x) logfun(param(x)))


##' @rdname GRANparams
##' @aliases logfun<-,GRANRepository
##' @export
setMethod("logfun<-", "GRANRepository",
          function(x, value) {
              p = param(x)
              logfun(p) = value
              param(x) = p
              x
              })


##' addPkg
##'
##' Add a package to the manifest for a GRANRepository
##' @param x A GRANRepository object
##' @param \dots passed to manifest method for addPkg
##' @param rows data.frame or unspecified. passed to manifest method for addPkg
##' @param versions data.frame passed to manifest method for addPkg
##' @return \code{x} with the specified package(s) added to the associated manifest
##' @export
##' @importMethodsFrom switchr addPkg
setMethod("addPkg", "GRANRepository",
          function(x, ..., rows, versions) {
              if(any(manifest_df(rows)$name %in% manifest_df(x)$name))
                  stop("Some of the packages to be added already appear in the repo manifest")
              manifest(x) = addPkg(manifest(x), ..., rows = rows, versions = versions)
              new = which(!manifest_df(x)$name %in% repo_results(x)$name)
              oldres = repo_results(x)
              newres = ResultsRow(name = manifest_df(x)$name[new])
              oldres = oldres[,names(newres)]
              repo_results(x) = rbind(oldres, newres)
              x
          })










##' @rdname GRANparams
##' @return logical indicating whether GRANBase should be installed
##' from CRAN during the repository build process. If FALSE, or if GRANBase
##' can't be found on your CRAN mirror, GRANBase will be checked out and built
##' from github.
##'
##' @docType methods
##' @export
setGeneric("use_cran_granbase", function(x) stop("This object doesn't contain repository build parameters"))

##' @rdname GRANparams
##' @docType methods
##' @export
setGeneric("use_cran_granbase<-", function(x, value) stop("This object doesn't contain repository build parameters"))



##'@rdname GRANparams
##' @aliases use_cran_granbase,GRANRepository
##' @export
setMethod("use_cran_granbase", "GRANRepository",
          function(x) param(x)@use_cran_granbase)
##'@rdname GRANparams
##' @aliases use_cran_granbase<-,GRANRepository
##'@export

setMethod("use_cran_granbase<-", "GRANRepository",
          function(x, value) {
              param(x)@use_cran_granbase= value
              x
              })

##' @rdname GRANparams
##' @aliases use_cran_granbase,RepoBuildParam
##' @export
setMethod("use_cran_granbase", "RepoBuildParam",
          function(x) x@use_cran_granbase)
##'@rdname GRANparams
##' @aliases use_cran_granbase<-,RepoBuildParam
##'@export

setMethod("use_cran_granbase<-", "RepoBuildParam",
          function(x, value) {
              x@use_cran_granbase= value
              x
              })
