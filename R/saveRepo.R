
saveRepoFiles = function(repo)
    {
        repolock = file.path(repobase(repo), "repo.R.LOCK")
        if(file.exists(repolock))
            {
                last = strftime(readLines(repolock))
                if(difftime(Sys.time(), last, units = "mins") < 3) {
                    warning("repo.R is locked by another build process. Unable to save repo.")
                    return()
                }
                
            }
        #convert it to a list so we don't rely on GRANBase when loading in GRAN
        cat(format(Sys.time()), file = repolock)
        saveRepo(repo = repo, filename = normalizePath2(file.path(repobase(repo), "repo.R")))
        saveRepo(repo = repo, filename = normalizePath2(file.path(destination(repo), "repo.R")))
        file.remove(repolock)
        NULL
    }

finalizeRepo = function(repo)
    {
        manifestHTML(repo)
        saveRepoFiles(repo)
        if(!file.exists(file.path(repobase(repo), "getGRAN.R")))
            file.copy(file.path(repobase(repo), "GRAN", "inst", "scripts", "getGRAN.R"), file.path(repobase(repo), "getGRAN.R"))
        
        repo
        
    }
