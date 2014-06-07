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
    ret = repo@logfile
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
        ret = repo@errlog
    if(!file.exists(dirname(ret)))
        dir.create(dirname(ret), recursive = TRUE)
    ret})

##' location generic
##' Retreive the directory associated with an object
##' @rdname location-methods
##' @return a character containing the associated path
##' @author Gabriel Becker
##' @param repo a GRANRepository object
##' @docType methods
##' @export
setGeneric("location", function(repo) standardGeneric("location"))

##' Retrieve the path to a GRAN (sub) repository
##' @rdname location-methods
##' @aliases location,GRANREpository-method
setMethod("location", "GRANRepository", function(repo) {
    ret = file.path(repobase(repo), "src", "contrib")
    
    if(!file.exists(ret))
        dir.create(ret, recursive=TRUE)
    normalizePath2(ret)
})

##' Retreive the local directory associated with a PkgSource object for a package in a GRAN manifest
##' @rdname location-methods
##' @aliases location,PkgSource-method
##' @export
setMethod("location", "PkgSource", function(repo) repo@location)

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
    ret = file.path(repo@baseDir, repo@subrepoName)
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

##' repoManifest
##' Return the location of the manifest file associated with the repository
##'
##' @rdname repoManifest-methods
##' @param repo a GRANRepository object
##' @return The path to the manifest file
setGeneric("repoManifest", function(repo) standardGeneric("repoManifest"))
##' @rdname repoManifest-methods
##' @aliases repoManifest,GRANRepository-method
##' @export
setMethod("repoManifest", "GRANRepository", function(repo)
          file.path(repo@baseDir, "manifest.dat"))


setGeneric("manLockFile", function(repo) standardGeneric("manLockFile"))
setMethod("manLockFile", "GRANRepository",
          function(repo) gsub(".dat", ".out", fixed = TRUE, repoManifest(repo)))

##' LibLoc
##' Return the temporary library location associated with the repository
##'
##' @rdname LibLoc-methods
##' @param repo a GRANRepository object
##' @return The path to the repository specific library location
setGeneric("LibLoc", function(repo) standardGeneric("LibLoc"))
##' @rdname LibLoc-methods
##' @aliases LibLoc,GRANRepository-method
setMethod("LibLoc", "GRANRepository",
          function(repo) normalizePath2(repo@tempLibLoc))

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
          function(repo) file.path(normalizePath2(repo@dest_base),
                                   repo@subrepoName,  "src", "contrib"))


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
          function(repo) file.path(normalizePath2(repo@dest_base)))


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
          function(repo) file.path(normalizePath2(repo@dest_base),
                                   repo@subrepoName, "CheckResults" ))



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
          function(repo) paste(repo@dest_url,
                               repo@subrepoName,
                               sep="/"))

