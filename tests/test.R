suppressPackageStartupMessages(library(GRANBase))
testpkgs <- list.files(system.file("testpkgs", package = "GRANBase"),
                       full.names = TRUE)
man <- PkgManifest(name = basename(testpkgs),
                  url = testpkgs, type = "local")
repdir <- file.path(tempdir(), "repos")
if(!file.exists(repdir)) dir.create(repdir)
repo <- makeRepo(man,
                repo_name= "stable",
                basedir = repdir,
                destination = repdir,
                cores = 1L,
                check_test = FALSE)


clear_repo(repo)

## regression test for when repo.R in repo url has been cleared
## but in memory one has not

repo3 = makeRepo(repo, check_test = FALSE)
