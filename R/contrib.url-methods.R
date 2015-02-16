##' contrib.url
##' A generic for contrib.url so that available.packages et al can
##' interact with GRANRepository objects.
##' @param repos A repository to extract the contrib url from
##' @param type The type of package repository it is
##' @docType methods
##' @export
##' @rdname contriburl
setGeneric("contrib.url", contrib.url)
##'@rdname contriburl
##' @aliases contrib.url,GRANRepository
setMethod("contrib.url", "GRANRepository", function(repos, type) {
    contrib.url(repos = repo_url(repos), type = type)
})

##' available.packages
##' A generic for available.packages and a method for GRANRepository objects
##' @param contriburl The repository or contrib url
##' @param method See base documentation
##' @param fields See base documentation
##' @param type The type of packages to query
##' @param filters See base documetnation
##' @rdname availpkgs
##' @docType methods
##' @export
setGeneric("available.packages", available.packages)
##' @rdname availpkgs
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
