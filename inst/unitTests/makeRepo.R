library(GRANBase)
ghman = GithubManifest("gmbecker/rpath",
                        "hadley/lazyeval",
                        "hadley/dplyr",
                        "rstudio/ggvis")
repo = makeRepo(ghman, cores = 3, basedir = "~/testrepo")
