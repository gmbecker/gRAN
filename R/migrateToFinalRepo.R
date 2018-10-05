migrateToFinalRepo = function(repo)
{
    repoLoc <- destination(repo)
    man <- manifest_df(repo)
    bman <- getBuildingManifest(repo = repo)

    ## I think this is not necessary because the bug was coming from somewhere else.
    ## might still be a good idea eventually though...
    ##  clearTmpRepoFailedPkgs(repo)

    ## if they aren't being tested at all, we don't build them twice.

    if(all(getBuildingResults(repo)$status == "ok - not tested")) {
        stagingLoc <- file.path(temp_repo(repo), "src", "contrib")
        clearstage <- FALSE
    }  else {
        stagingLoc <- staging(repo)
        clearstage <- TRUE
    }

    repo = markFailedRevDeps(repo)
    logfun(repo)("NA", paste("Migrating", sum(getBuilding(repo)),
      "successfully built and tested packages to final repository at", repoLoc))


    if(!nrow(bman))
        return(repo)
    tars <- list.files(stagingLoc, pattern = "\\.tar.*")
    tars <- tars[sapply(tars, function(tr)
        any(sapply(bman$name, function(nm) grepl(paste0("^",nm, "_"), tr))))]

    #copy files
    out  <- tryCatch(file.copy(from = file.path(stagingLoc, tars),
          to = file.path(repoLoc, tars), overwrite = TRUE), error=function(x) x)
    if(is(out, "error"))
    {
      logfun(repo)("CRITICAL FAILURE",
        c("Copying built packages to final repo failed. Error message:", out),
        type="both")
      repo_results(repo)$status[getBuilding(repo)] = "GRAN FAILURE"
      return(repo)
    } else if (any(!out)) {
      logfun(repo)("CRITICAL FAILURE",
        c("Copying built packages to final repo failed for some pkgs. Pkgs:", out),
        type="both")
      repo_results(repo)$status[getBuilding(repo)][!out] = "GRAN FAILURE"
      return(repo)
    }

    oldwd <- getwd()
    setwd(repoLoc)
    on.exit(setwd(oldwd))
    updateArchive(repo)
    createMeta(repo)
    update_PACKAGES(type="source", latestOnly = TRUE, verbose = TRUE, logfun = logfun(repo),
                    strict = FALSE)
    repo <- updateResults(repo)
    dummy <- pkgHTML(repo)
    if(clearstage) {
        out = tryCatch(unlink(list.files(stagingLoc,
                                          pattern = paste0("(PACKAGES|Rcheck|",
                                                       "\\.tar\\..*$",
                                                       ")"), full.names=TRUE),
                                                       recursive = TRUE),
                                          error = function(x) x)
        if(is(out, "error")) {
            logfun(repo)("NA", c(paste("Unable to remove tarballs/zips,",
                "PACKAGES files, and Rcheck directories from staging directory",
                "after deployment: "), out$message), type="both")
        }
    }
    return(repo)
}

clearTmpRepoFailedPkgs = function(repo) {

    remFailedPkgTballs(repo)
    remOldPkgTballs(repo)
    trim_PACKAGES(temp_repo(repo))
}


remFailedPkgTballs = function(repo) {
    res = repo_results(repo)
    failed = res$building & !isOkStatus(repo = repo)
    flpkgs = res$name[failed]
    tarbls = file.path(temp_repo(repo),"src", "contrib",  paste0(flpkgs, "_",
                                            res$version[failed], ".tar.gz"))
    tarbls = tarbls[file.exists(tarbls)]
    if(length(tarbls)>0) {
        logfun(repo)(NA, msg = paste("Removing", length(tarbls),
        "tarballs from temporary repo for packages which failed during the buidl process"))
        file.remove(tarbls)

    }
}

remOldPkgTballs = function(repo) {

    tballs = list.files(file.path(temp_repo(repo), "src/contrib"),
                        pattern = paste0(".+_[[:digit:]]+\\.[[:digit:]]+(\\.|-)[[:digit:]]\\",
                                         ".tar.gz"),
                        full.names=TRUE)
    stuff = strsplit(basename(tballs), split="(\\.|-|_)")
    stuffdf = do.call(rbind, lapply(stuff, as.data.frame))
    stuffdf

}


#' @importFrom tools package_dependencies
markFailedRevDeps = function(repo) {
    bman = getBuildingManifest(repo)
    rdpkgs = package_dependencies(bman$name, which = c("Depends", "Imports", "LinkingTo"),
        db = installed.packages(temp_lib(repo), noCache = TRUE), recursive= TRUE)
    keep = sapply(rdpkgs, function(x, bman) {
        length(rdpkgs) == 0 || all(rdpkgs %in% bman$name)
    }, bman = bman)

    rempkgs = bman[!keep, "package"]
    if(!all(keep)) {
        sapply(rempkgs, function(x) logfun(repo)(x, paste("One or more package",
        "dependencies failed to build. Not deploying package."), type = "both"))
        repo_results(repo)$status[manifest_df(repo)$name %in% rempkgs] = "Dependency build failure"
    }
    repo
}
