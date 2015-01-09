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

##' @export
setGeneric("temp_lib", function(repo) standardGeneric("temp_lib"))
setMethod("temp_lib", "GRANRepository",
          function(repo) normalizePath2(param(repo)@tempLibLoc))

##' notrack
##' Return the directory which stores retreived versions of non-GRAN packages
##' for use in virtual repositories
##'
##' @rdname notrack-methods
##' @param repo a GRANRepository object
##' @return The path to the notrack directory
setGeneric("notrack", function(repo) standardGeneric("notrack"))
##' @rdname notrack-methods
##' @aliases notrack,GRANRepository-method
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
##' @export
setGeneric("check_result_dir", function(repo) standardGeneric("check_result_dir"))
##' @rdname check_result_dir-methods
##' @aliases check_result_dir,GRANRepository-method
##' @export
setMethod("check_result_dir","GRANRepository",
          function(repo) file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "CheckResults" ))



##' repo_url
##' Return the url that the repository will be served at. This is the web
##' address corresponding to repository, suitable for calling contrib.url.  e.g
##' http://www.website.com/gran/current/
##'
##' @rdname repo_url-methods
##' @param repo a GRANRepository object
##' @return For destination, the full path to the contrib directory the packages
##' will be deployed to
##' @export
setGeneric("repo_url", function(repo) standardGeneric("repo_url"))
##' @rdname repo_url-methods
##' @aliases repo_url,GRANRepository-method
##' @export
setMethod("repo_url","GRANRepository",
          function(repo) paste(param(repo)@dest_url,
                               param(repo)@repo_name,
                               sep="/"))

setMethod("repo_url","NULL", function(repo) NULL)


##' checkout_dir
##' Return the directory that package sources will be checked-out into
##' for use in the build process
##'
##' @rdname checkout_dir-methods
##' @param repo a GRANRepository object
##' @return For destination, the full path to the contrib directory the packages
##' will be deployed to
##' @export
setGeneric("checkout_dir", function(repo) standardGeneric("checkout_dir"))
##' @rdname checkout_dir-methods
##' @aliases checkout_dir,GRANRepository-method
##' @export
setMethod("checkout_dir","GRANRepository",
          function(repo) param(repo)@temp_checkout)

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

##' @export
setGeneric("repo_results", function(x) standardGeneric("repo_results"))
setMethod("repo_results", "GRANRepository", function(x) x@results)

##' ##'@export
setGeneric("repo_results<-", function(x, value) standardGeneric("repo_results<-"))
setMethod("repo_results<-", "GRANRepository", function(x, value) {
    x@results = value
    x
    })

##'@export
setGeneric("param", function(x) standardGeneric("param"))
setMethod("param", "GRANRepository",
          function(x) x@param)

##'@export
setGeneric("param<-", function(x, value) standardGeneric("param<-"))
setMethod("param<-", "GRANRepository",
          function(x, value){
              x@param = value
              x
          })

##' @export
setGeneric("repo_name", function(x) standardGeneric("repo_name"))
setMethod("repo_name", "GRANRepository",
          function(x) param(x)@repo_name)


##' @export
setGeneric("temp_repo", function(x) standardGeneric("temp_repo"))
setMethod("temp_repo", "GRANRepository",
          function(x) param(x)@temp_repo)



##'@export
setGeneric("check_warn_ok", function(x) standardGeneric("check_warn_ok"))
setMethod("check_warn_ok", "GRANRepository",
          function(x)  param(x)@check_warn_ok)

##'@export
setGeneric("check_note_ok", function(x) standardGeneric("check_note_ok"))
setMethod("check_note_ok", "GRANRepository",
          function(x)  param(x)@check_note_ok)

##' @export
setGeneric("suspended_pkgs", function(x) standardGeneric("suspended_pkgs"))
setMethod("suspended_pkgs", "GRANRepository",
          function(x) param(x)@suspended)

##'@export
setGeneric("suspended_pkgs<-", function(x, value) standardGeneric("suspended_pkgs<-"))
setMethod("suspended_pkgs<-", "GRANRepository",
          function(x, value) {
              param(x)@suspended = value
              x
              })





##'@export
setMethod("sh_init_script", "GRANRepository",
          function(x) param(x)@shell_init)

##'@export

setMethod("sh_init_script<-", "GRANRepository",
          function(x, value) {
              param(x)@shell_init= value
              x
              })




##'@export
setGeneric("extra_fun", function(x) standardGeneric("extra_fun"))
setMethod("extra_fun", "GRANRepository",
          function(x)  param(x)@extra_fun)

setGeneric("check_test_on", function(x) standardGeneric("check_test_on"))
setMethod("check_test_on", "RepoBuildParam", function(x) x@check_test)
setMethod("check_test_on", "GRANRepository", function(x) check_test_on(param(x)))


setGeneric("install_test_on", function(x) standardGeneric("install_test_on"))
setMethod("install_test_on", "RepoBuildParam", function(x) x@install_test)
setMethod("install_test_on", "GRANRepository", function(x) install_test_on(param(x)))


##' @export
setMethod("logfun", "GRANRepository",
          function(x) logfun(param(x)))


##' @export
setMethod("logfun<-", "GRANRepository",
          function(x, value) {
              p = param(x)
              logfun(p) = value
              param(x) = p
              x
              })



setMethod("addPkg", "GRANRepository",
          function(x, ..., rows, versions) {
              manifest(x) = addPkg(manifest(x), ..., rows = rows, versions = versions)
              x
          })


