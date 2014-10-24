#setGeneric("makePkgSourceDir",
#           function(name, source, path = tempdir(), branch = "master",
#                    subdir = "./", repo)
#           standardGeneric("makePkgSourceDir"))
##' @export
setGeneric("makePkgDir",
           function(name, source, path = tempdir(), latest_only = FALSE, repo = NULL, forceRefresh = FALSE) standardGeneric("makePkgDir"))

##' lazyRepo
##'
##' Create a lazy repository for installing directly from a package
##' manifest. Most users will want to call \code{Install} directly,
##' which will call this as needed behind the scenes.
##'
##' @param pkgs The packages to install
##' @param manifest The manifest to use
##' @param version Specific versions of the packages to install. Should be a
##' vector of the same length as \code{pkgs}. Defaults to NA (any version)
##' for all packages.
##' @param dir The directory packages should be downloaded/checkedout/built into
##' @param rep_path The path of the final repository
##' @param get_suggests Whether suggested packages should be included
##' in the lazy repository. Defaults to FALSE
##' @param verbose Should extra information be printed to the user during
##' the construction process
##' @param scm_auths Named list of username/password credentials for checking
##' out package sources from one or more sources listed in \code{manifest}
##'
##' @return A path to the populated lazy repository, suitable for 'coercing' to
##' a url and installing from.
##' @export
##'
setGeneric("lazyRepo",
           function(pkgs,
                    manifest,
                    versions = rep(NA, times = length(pkgs)),
                    dir = tempdir(),
                    rep_path = file.path(dir, "repo"),
                    get_suggests = FALSE,
                    verbose = FALSE,
                    scm_auths = list(bioconductor = c("readonly", "readonly"))) standardGeneric("lazyRepo"))


##'makeRepo
##'
##' Make a package repository containing a specified set of packages from
##' various sources
##'
##' @param x The object containing the information necessary to create the repository
##' @param cores The number of cores on the local machine to use during building
##' @param build_pkgs The names of the packages to (re) build and test within the
##' repository. Defaults to \code{NULL} which builds all packages in the manifest
##' @param scm_auth A named list containing the information necessary to check
##' out package sources. The list elements (assumed to be a character vector of
##' length 2, user then password) are applied when the name is contained in a
##' package's url
##' @param ... Additional arguments, typically used for the construction of a
##' \code{GRANRepository} object if one does not already exist.
##' @return A GRANRepository object which has used to create a repository.
##'
##' @export



setGeneric("makeRepo", function(x, cores = 3L,
                                build_pkgs = NULL,
                                scm_auth = list("bioconductor.org" =
                                    c("readonly", "readonly")),
                                ...)
           standardGeneric("makeRepo"))
##' @export
setGeneric("addPkg", function(x, name, version = NA,...) standardGeneric("addPkg"))
##' @export
setGeneric("gotoVersCommit", function(dir, src, version, repo = NULL) standardGeneric("gotoVersCommit"))
