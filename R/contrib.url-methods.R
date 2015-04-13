##' contrib.url
##' A generic for contrib.url so that available.packages et al can
##' interact with GRANRepository objects.
##' @param repos An object representing an R package repository, or the location thereof.
##' @param type The type of packages to extract the contrib url for.
##' @docType methods
##' @rdname contrib.url2
##' @export
setGeneric("contrib.url", contrib.url)
##' @rdname contrib.url2
##' @aliases contrib.url,GRANRepository
setMethod("contrib.url", "GRANRepository", function(repos, type) {
    contrib.url(repos = repo_url(repos), type = type)
})

##' available.packages
##' A generic for available.packages and a method for GRANRepository objects
##' @param contriburl The object representing the repository. Pre-generic this was the contrib URL for the repository
##' @param method Passed to base available.packages function
##' @param fields passed to base available.packages function
##' @param type passed to base available.packages function
##' @param filters passed to base available.packages function
##' @docType methods
##' @rdname available.packages2
##' @export
setGeneric("available.packages", available.packages)
##' @rdname available.packages2
##' @aliases available.packages,GRANRepository
setMethod("available.packages", "GRANRepository",
          function (contriburl, 
                    method, fields = NULL, type = getOption("pkgType"),
                    filters = NULL)
      {
          available.packages(contrib.url(contriburl, type), method = method,
                             fields = fields, type = type,
                             filters = filters)
      })
