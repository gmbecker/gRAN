init_results <- function(repo) {
    if(is.null(repo_results(repo)) || !nrow(repo_results(repo)))
        repo_results(repo) = ResultsRow(name = manifest_df(repo)$name)
    else {
        df = repo_results(repo)
        missingcols = setdiff(names(ResultsRow()), names(df))
        for(col in missingcols) {
            df[[col]] = ResultsRow()[[col]]
        }
        df$status = ifelse(df$suspended, NA_character_, "ok")
        df = df[,names(ResultsRow())]
        repo_results(repo) = df
    }
    repo
}

updateResults <- function(repo) {
    fullres = repo_results(repo)
    tried = fullres$building
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

resetResults <- function(repo) {
    res = repo_results(repo)
    res2 = ResultsRow(name = res$name, suspended= res$suspended,
        maintainer = res$maintainer)
    repo_results(repo) = res2
    repo
}
