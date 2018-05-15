saveRepoFiles <- function(repo) {
  repolock = file.path(repobase(repo), "repo.R.LOCK")
  if (file.exists(repolock)) {
    last = strftime(readLines(repolock))
    if (difftime(Sys.time(), last, units = "mins") < 3) {
      warning("repo.R is locked by another build process. Unable to save repo.")
      return()
    }

  }
  # convert it to a list so we don't rely on GRANBase when loading in GRAN
  cat(format(Sys.time()), file = repolock)
  saveRepo(repo = repo, filename = normalizePath2(file.path(repobase(repo), "repo.R"),
                                                  mustWork = FALSE))
  saveRepo(repo = repo, filename = normalizePath2(file.path(destination(repo),
                                                            "repo.R"),
                                                  mustWork = FALSE))
  file.remove(repolock)
  NULL
}

finalizeRepo <- function(repo, cores = 1L) {
  buildReport(repo, cores = cores)
  saveRepoFiles(repo)
  if (!file.exists(file.path(repobase(repo), "getGRAN.R"))) {
    file.copy(file.path(repobase(repo), paste0("GRAN", repo_name(repo)), "inst",
      "scripts", "getGRAN.R"), file.path(repobase(repo), "getGRAN.R"))
  }
  if (email_notify(repo)) {
      logfun(repo)("NA", "Sending email notifications...")
      emailNotifier(repo)
      logfun(repo)("NA", "Completed email delivery")
  }
  repo
}
