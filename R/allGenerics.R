#setGeneric("makePkgSourceDir",
#           function(name, source, path = tempdir(), branch = "master",
#                    subdir = "./", repo)
#           standardGeneric("makePkgSourceDir"))

setGeneric("makePkgDir",
           function(name, source, path = tempdir(), latest_only = FALSE, repo = NULL, forceRefresh = FALSE) standardGeneric("makePkgDir"))

##' @export
setGeneric("lazyRepo",
           function(pkgs,
                    manifest,
                    versions = rep(NA, times = length(pkgs)),
                    dir = tempdir(),
                    rep_path = file.path(dir, "repo"),
                    get_suggests = FALSE,
                    verbose = FALSE,
                    scm_auths = list(bioconductor = c("readonly", "readonly"))) standardGeneric("lazyRepo"))


setGeneric("makeRepo", function(x, cores = 3L, ...,
                                scm_auth = list("bioconductor.org" = c("readonly", "readonly")))
           standardGeneric("makeRepo"))
