library(GRANBase)

  ghman = GithubManifest("gmbecker/rpath", "hadley/lazyeval",
      "hadley/dplyr", "rstudio/ggvis")

res = lazyRepo("ggvis", manifest = ghman)

  Install("ggvis", ghman)
