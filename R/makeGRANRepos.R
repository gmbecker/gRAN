

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
##' @docType methods
##' @rdname makeRepo
##' @export



setGeneric("makeRepo", function(x, cores = 3L,
                                build_pkgs = NULL,
                                scm_auth = list("bioconductor.org" =
                                    c("readonly", "readonly")),
                                ...)
           standardGeneric("makeRepo"))

##' @rdname makeRepo
##' @aliases makeRepo,PkgManifest
##' @export
setMethod("makeRepo", "PkgManifest",
          function(x, cores = 3L, build_pkgs = NULL,
                   scm_auth = list("bioconductor.org" =
                       c("readonly", "readonly")),
                   ...
                   ) {

              vers = data.frame(name= manifest_df(x)$name,
                  version = NA, stringsAsFactors = FALSE)
              sessMan = SessionManifest(manifest = x,
                  versions = vers)
             
              makeRepo(sessMan, cores = cores, scm_auth = scm_auth,
                       build_pkgs = build_pkgs,
                       ...)
          })


##' @rdname makeRepo
##' @aliases makeRepo,SessionManifest
##' @export

setMethod("makeRepo", "SessionManifest",
          function(x, cores = 3L, build_pkgs = NULL, 
                   scm_auth = list("bioconductor.org" =
                       c("readonly", "readonly")),
                   ...
                   ) {

              repo = GRANRepository(manifest = x, param = RepoBuildParam(...))
              makeRepo(repo, cores = cores, scm_auth = scm_auth,
                       build_pkgs = build_pkgs, ...)
          })




##' @rdname makeRepo
##' @aliases makeRepo,GRANRepository
##' @export

setMethod("makeRepo", "GRANRepository",
          function(x, cores = 3L, build_pkgs = NULL,  
                   scm_auth = list("bioconductor.org" =
                       c("readonly", "readonly")),
                    ...) {
              repo = x
              if(file.exists(destination(repo)))
                  repo2 = tryCatch(loadRepo(paste(destination(repo), "repo.R",
                      sep="/")), error = function(x) NULL)
              else 
                  repo2 = suppressWarnings(tryCatch(loadRepo(paste(repo_url(repo), "repo.R", sep="/")),
                      error = function(x) NULL))
              if(!is.null(repo2) ) {
                  res = repo_results(repo)
                  res2 = repo_results(repo2)
                  if(any(!is.na(res$lastAttempt)) && (max(res$lastAttempt, na.rm=TRUE) < max(res2$lastAttempt,
                                              na.rm=TRUE))) {
                      message("Loading latest results from specified repository")
                      repo = repo2
                  }
              }
              repo = init_results(repo)
              if(!is.null(build_pkgs)) {
                  repo_results(repo)$building =
                      manifest_df(repo)$name %in% build_pkgs
                  suspended_pkgs(repo) = setdiff(suspended_pkgs(repo),
                                    build_pkgs)
              }

              print(paste("Building", sum(getBuilding(repo)), "packages"))
              ##package, build thine self!
              repo = GRANonGRAN(repo)
              ##do checkouts
              print(paste("Starting makeSrcDirs", Sys.time()))
              print(paste("Building", sum(getBuilding(repo)), "packages"))
              repo = makeSrcDirs(repo, cores = cores, scm_auth = scm_auth)
              ##add reverse dependencies to build list
              repo = addRevDeps(repo)
              ##do checkouts again to grab reverse deps
              repo = makeSrcDirs(repo, cores = cores, scm_auth = scm_auth)
              ##build temp repository
              print(paste("Starting buildBranchesInRepo", Sys.time()))
              print(paste("Building", sum(getBuilding(repo)), "packages"))
              ##if we have a single package specified we want to build it with or without a version bump
              repo = buildBranchesInRepo( repo = repo, temp = TRUE, cores = cores, incremental = is.null(build_pkgs))
              ##test packges
              print(paste("Invoking package tests", Sys.time()))
              print(paste("Building", sum(getBuilding(repo)), "packages"))
              repo = doPkgTests(repo, cores = cores)
              ##copy successfully built tarballs to final repository
              print(paste("starting migrateToFinalRepo", Sys.time()))
              print(paste("Built", sum(getBuilding(repo)), "packages"))
              repo = migrateToFinalRepo(repo)
              
              finalizeRepo(repo)
              repo
          })

##' @rdname makeRepo
##' @aliases makeRepo,character
##' @export

setMethod("makeRepo", "character",
          function(x, cores = 3L, build_pkgs = NULL,  
                   scm_auth = list("bioconductor.org" =
                       c("readonly", "readonly")),
                   ...) {

              if(!grepl("^(http|git|.*repo\\.R)", x))
                  x2 = list.files(x, pattern = "repo\\.R", full.names = TRUE,
                      recursive=TRUE)[1]
              else
                  x2 = x
              repo = loadRepo(x2)
              if(is.null(repo))
                  stop("There doesn't seem to be a repo.R file at associated",
                       "with the location",
                       x)
              makeRepo(repo, cores = cores, build_pkgs = build_pkgs,
                       scm_auth = scm_auth, ...)
          })

