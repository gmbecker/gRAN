##' @rdname makerepo
##' @aliases makeRepo,PkgManifest
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


##' @rdname makerepo
##' @aliases makeRepo,SessionManifest

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




##' @rdname makerepo
##' @aliases makeRepo,GRANRepository
setMethod("makeRepo", "GRANRepository",
          function(x, cores = 3L, build_pkgs = NULL,
                   scm_auth = list("bioconductor.org" =
                                       c("readonly", "readonly")),
                   ...) {
    if(!haveGit()) {
        message("Your system does not appear to have Git available. Returning NULL from makeRepo")
        return(NULL)
    }
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
            warning("Loading latest results from specified repository")
                      repo = repo2
        }
    }
    repo = init_results(repo)
    if(!is.null(build_pkgs)) {
        repo_results(repo)$building =
                             manifest_df(repo)$name %in% build_pkgs
        suspended_pkgs(repo) = setdiff(suspended_pkgs(repo),
                                       build_pkgs)
    } else {
        repo_results(repo)$building = !manifest_df(repo)$name %in% suspended_pkgs(repo)
        repo_results(repo)$suspended <- manifest_df(repo)$name %in% suspended_pkgs(repo)
    }


    message(paste("Building", sum(getBuilding(repo)), "packages"))
    ##package, build thine self!
    repo = GRANonGRAN(repo)
    ##do checkouts
    message(paste("Starting makeSrcDirs", Sys.time()))
    message(paste("Building", sum(getBuilding(repo)), "packages"))
    repo = makeSrcDirs(repo, cores = cores, scm_auth = scm_auth)
    ##add reverse dependencies to build list
    repo = addRevDeps(repo)
    ##do checkouts again to grab reverse deps
    repo = makeSrcDirs(repo, cores = cores, scm_auth = scm_auth)
    ##build temp repository
    message(paste("Starting buildBranchesInRepo", Sys.time()))
    message(paste("Building", sum(getBuilding(repo)), "packages"))
    ##if we have a single package specified we want to build it with or without a version bump
    repo = buildBranchesInRepo( repo = repo, temp = TRUE, cores = cores, incremental = is.null(build_pkgs))
    ##test packges
    message(paste("Invoking package tests", Sys.time()))
    message(paste("Building", sum(getBuilding(repo)), "packages"))
    repo = doPkgTests(repo, cores = cores)
    ##copy successfully built tarballs to final repository
    message(paste("starting migrateToFinalRepo", Sys.time()))
    message(paste("Built", sum(getBuilding(repo)), "packages"))
    repo = suppressWarnings(migrateToFinalRepo(repo))

    finalizeRepo(repo)
    repo
})

##' @rdname makerepo
##' @aliases makeRepo,character

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
