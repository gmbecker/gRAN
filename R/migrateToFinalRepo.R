migrateToFinalRepo = function(repo)
{
    repoLoc = destination(repo)
    stagingLoc = staging(repo)

    man = repo@manifest
    bman = getBuildingManifest(repo = repo)
    
    writeGRANLog("NA", paste("Migrating", sum(getBuilding(repo)), "successfully built and tested packages to final repository at", repoLoc), repo = repo)

    
    if(!nrow(bman))
        return(repo)
    tars = list.files(stagingLoc, pattern = "\\.tar.*")
    tars = tars[sapply(tars, function(tr)
        any(sapply(bman$name, function(nm) grepl(paste0("^",nm, "_"), tr))))]

    
    #copy files
    out  = tryCatch(file.copy(from = file.path(stagingLoc, tars), to = file.path(repoLoc, tars), overwrite = TRUE), error=function(x) x)
    if(is(out, "error"))
    {
        writeGRANLog("CRITICAL FAILURE", c("Copying built packages into final repository failed. Error message:", out), type="both", repo=repo)
        repo@manifest$status[getBuilding(repo)] = "GRAN FAILURE"
        return(repo)
    } else if (any(!out)) {
        writeGRANLog("CRITICAL FAILURE", c("Copying built packages into final repository failed for some packages. Packages", out), type="both", repo=repo)
        repo@manifest$status[getBuilding(repo)][!out] = "GRAN FAILURE"
        return(repo)
    }
    out = tryCatch(file.remove(list.files(stagingLoc, pattern = "\\.tar.*", full.names=TRUE)), error=function(x) x)

    if(is(out, "error"))
    {
        writeGRANLog("NA", c("Unable to remove tarballs from staging directory after deployment: ", out$message), repo = repo, type="both")
    }

    
    oldwd = getwd()
    setwd(repoLoc)
    on.exit(setwd(oldwd))
    write_PACKAGES( type="source")
    repo = updateManifest(repo)

    repo
}
    



   
