updateGit = function(dir, source, repo)
{
    oldwd = getwd()
    on.exit(setwd(oldwd))
    setwd(dir)
    cmd = paste("git checkout", branch(source), "; git pull origin", branch(source))
    out = tryCatch(system_w_init(cmd, intern=TRUE, repo = repo),
        error = function(x) x)
    if(errorOrNonZero(out))
    {
        writeGRANLog(basename(dir), paste("git update failed! cmd:", cmd),
                     type = "both", repo = repo)
        return(FALSE)
    }

    TRUE
}


gitChangeBranch = function(codir, branch, repo) {
    oldwd = getwd()
    on.exit(setwd(oldwd))
    setwd(codir)
    cmd=paste("git checkout", branch)
    writeGRANLog(basename(codir), paste("GIT: Switching to branch", branch,
                                        "in checked out local copy."),
                 repo = repo)
    out = tryCatch(system_w_init(cmd, intern=TRUE, repo = repo),
        error = function(x) x)
    if(errorOrNonZero(out)) {
        writeGRANLog(basename(codir), c("GIT: switching to branch failed."),
                     type = "both", repo = repo)
        writeGRANLog(basename(codir), c("git error output:", out), type="error",
                     repo = repo)
        FALSE
    } else {
        TRUE
    }
}
