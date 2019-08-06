#'makeRepo
#'
#' Make a package repository containing a specified set of packages from
#' various sources
#'
#' @param x The object containing the information necessary to create the repository
#' @param cores The number of cores on the local machine to use during building
#' @param build_pkgs The names of the packages to (re) build and test within the
#' repository. Defaults to \code{NULL} which builds all packages in the manifest
#' @param scm_auth A named list containing the information necessary to check
#' out package sources. The list elements (assumed to be a character vector of
#' length 2, user then password) are applied when the name is contained in a
#' package's url
#' @param constrained_build Whether to constrain side-effects of build
#' @param ... Additional arguments, typically used for the construction of a
#' \code{GRANRepository} object if one does not already exist.
#' @return A GRANRepository object which has used to create a repository.
#'
#' @docType methods
#' @rdname makerepo
#' @references Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
#' @export

setGeneric("makeRepo", function(x, cores = 1,
                                build_pkgs = NULL,
                                scm_auth = list("bioconductor.org" =
                                    c("readonly", "readonly")),
                                constrained_build = FALSE,
                                ...)
           standardGeneric("makeRepo"))
