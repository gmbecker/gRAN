##' contrib.url
##' A generic for contrib.url so that available.packages et al can
##' interact with GRANRepository objects.
##' @export
setGeneric("contrib.url", contrib.url)

setMethod("contrib.url", "GRANRepository", function(repos, type) {
    contrib.url(repos = repo_url(repos), type = type)
})

##' available.packages
##' A generic for available.packages and a method for GRANRepository objects
##' @export
setGeneric("available.packages", available.packages)
setMethod("available.packages", "GRANRepository",
          function (contriburl, 
                    method, fields = NULL, type = getOption("pkgType"),
                    filters = NULL)
      {
          available.packages(contrib.url(contriburl, type), method = method,
                             fields = fields, type = type,
                             filters = filters)
      })
