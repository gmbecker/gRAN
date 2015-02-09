
ResultsRow = function(name = NA,
    building = TRUE,
    status = "ok",
    version = "0.0-0",
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
               version = version,
               lastAttempt = lastAttempt,
               lastAttemptStatus = lastAttemptStatus,
               lastbuilt = lastbuilt,
               lastbuiltversion = lastbuiltversion,
               lastbuiltstatus = lastbuiltstatus,
               maintainer = maintainer,
               suspended = suspended, building = building,
               stringsAsFactors = FALSE)
}


init_results = function(repo) {
    if(is.null(repo_results(repo)) || !nrow(repo_results(repo)))
        repo_results(repo) = ResultsRow(name = manifest_df(repo)$name)
    else {
        df = repo_results(repo)
        df$status = ifelse(df$suspended, NA_character_, "ok")
        repo_results(repo) = df
    }
        
    repo
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
