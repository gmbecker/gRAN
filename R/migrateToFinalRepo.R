migrateToFinalRepo = function(repo)
{
    repoLoc = destination(repo)
    stagingLoc = staging(repo)

    man = manifest_df(repo)
    bman = getBuildingManifest(repo = repo)

    repo = markFailedRevDeps(repo)
    logfun(repo)("NA", paste("Migrating", sum(getBuilding(repo)), "successfully built and tested packages to final repository at", repoLoc))

    
    if(!nrow(bman))
        return(repo)
    tars = list.files(stagingLoc, pattern = "\\.tar.*")
    tars = tars[sapply(tars, function(tr)
        any(sapply(bman$name, function(nm) grepl(paste0("^",nm, "_"), tr))))]

    
    #copy files
    out  = tryCatch(file.copy(from = file.path(stagingLoc, tars), to = file.path(repoLoc, tars), overwrite = TRUE), error=function(x) x)
    if(is(out, "error"))
    {
        logfun(repo)("CRITICAL FAILURE", c("Copying built packages into final repository failed. Error message:", out), type="both")
        repo_results(repo)$status[getBuilding(repo)] = "GRAN FAILURE"
        return(repo)
    } else if (any(!out)) {
        logfun(repo)("CRITICAL FAILURE", c("Copying built packages into final repository failed for some packages. Packages", out), type="both")
        repo_results(repo)$status[getBuilding(repo)][!out] = "GRAN FAILURE"
        return(repo)
    }
    out = tryCatch(file.remove(list.files(stagingLoc, pattern = "\\.tar.*", full.names=TRUE)), error=function(x) x)

    if(is(out, "error"))
    {
        logfun(repo)("NA", c("Unable to remove tarballs from staging directory after deployment: ", out$message), type="both")
    }

    
    oldwd = getwd()
    setwd(repoLoc)
    on.exit(setwd(oldwd))
    write_PACKAGES( type="source")
    repo = updateResults(repo)

    repo
}

##' @importFrom tools package_dependencies
markFailedRevDeps = function(repo) {
    bman = getBuildingManifest(repo)
    rdpkgs = package_dependencies(bman$name, which = c("Depends", "Imports", "LinkingTo"),
        db = installed.packages(temp_lib(repo), noCache = TRUE), recursive= TRUE)
    keep = sapply(rdpkgs, function(x, bman) {
        length(rdpkgs) == 0 || all(rdpkgs %in% bman$name)
    }, bman = bman)
    
    rempkgs = bman[!keep, "package"]
    if(!all(keep)) {
        sapply(rempkgs, function(x) logfun(repo)(x, "One or more package dependencies failed to build. Not deploying package.", type = "both"))
        repo_results(repo)$status[manifest_df(repo)$name %in% rempkgs] = "Dependency build failure"
    }
    repo
}
    
    


   
