#' contrib.url
#' A generic for contrib.url so that available.packages et al can
#' interact with GRANRepository objects.
#' @param repos A repository to extract the contrib url from
#' @param type The type of package repository it is
#' @docType methods
#' @export
#' @rdname contriburl
setGeneric("contrib.url", contrib.url)
#'@rdname contriburl
#' @aliases contrib.url,GRANRepository
setMethod("contrib.url", "GRANRepository", function(repos, type) {
    contrib.url(repos = repo_url(repos), type = type)
})

#' available.packages
#' A generic for available.packages and a method for GRANRepository objects
#' @param contriburl The repository or contrib url
#' @param method See base documentation
#' @param fields See base documentation
#' @param type The type of packages to query
#' @param filters See base documetnation
#' @param repos Character string for the repository to query. GRANRepository
#' objects should be passed to the contriburl argument.
#' @rdname availpkgs
#' @docType methods
#' @export
setGeneric("available.packages", function(contriburl, method, fields = NULL, type = getOption("pkgType"), filters = NULL, repos = NULL) standardGeneric("available.packages"))

#' @rdname availpkgs
#' @aliases available.packages,ANY
setMethod("available.packages", "ANY",
          function (contriburl,
                    method, fields = NULL, type = getOption("pkgType"),
                    filters = NULL,
                    repos = NULL)
          {
              args = list(contriburl = contriburl, fields = fields, type = type, filters = filters)
              if("repos" %in% names(formals(utils::available.packages)))
                  args$repos = repos
              else if(!is.null(repos))
                  args$contriburl = contrib.url(repos, type= type)
              if(!missing(method))
                  args$method = method

              do.call(utils::available.packages, args)
          })


#' @rdname availpkgs
#' @aliases available.packages,GRANRepository
setMethod("available.packages", "GRANRepository",
          function (contriburl,
                    method, fields = NULL, type = getOption("pkgType"),
                    filters = NULL,
                    repos = NULL)
      {
          available.packages(contrib.url(contriburl, type), method = method,
                             fields = fields, type = type,
                             filters = filters,
                             repos = NULL)
      })
