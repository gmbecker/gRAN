
ResultsRow = function(name = NA,
    building = TRUE,
    status = "ok",
    lastAttempt = NA,
    lastAttemptVersion = NA,
    lastAttemptStatus = NA,
    lastbuilt = NA,
    lastbuiltversion = NA,
    lastbuiltstatus = NA,
    buildReason = NA,
    maintainer  = NA,
    suspended  = FALSE) {
    data.frame(name = name, status = status,
               lastAttempt = lastAttempt,
               lastAttemptStatus = lastAttemptStatus,
               lastbuilt = lastbuilt,
               lastbuiltversion = lastbuiltversion,
               lastbuiltstatus = lastbuiltstatus,
               maintainer = maintainer,
               suspended = suspended)
}





updateResults = function(repo)
{
    fullres = repo_results(repo)
    tried = fullres$building
    ##succeeded = getBuilding(repo)
    succeeded = isOkStatus(status = fullres$status, repo = repo) & tried
    time = as.character(Sys.time())
    fullres$lastAttempt[tried] = time
    fullres$lastAttemptStatus[tried] = fullres$status[tried]
    fullres$lastAttemptVersion[tried] = fullres$version[tried]
    fullres$lastbuilt[succeeded] = time
    fullres$lastbuiltversion[succeeded] = fullres$version[succeeded]
    fullres$lastbuiltstatus[succeeded] = fullres$status[succeeded]

    repo_results(repo) = fullres
    repo
    
    
}
