##' Install packages in GRAN along with their dependencies
##'
##' Installs packags in GRAN along with their dependencies
##' @title Install packages from GRAN
##' @param package character vector of packages to install from GRAN
##' @param repos a list of repos to search in addition to the repos GRAN
##'   searches by default
##' @param type The package type; defaults to 'both' on the Mac so
##' that source packages are attempted.
##' @param ... Additional parameters that will be passed to
##' @param gRAN GRANRepository object to use. Defaults to the default
##'   repository for the current installation, or NULL if no default repository
##'   exists.
##' @return TRUE if successful 
##' @author Cory Barr and Gabriel Becker
##' @importFrom BiocInstaller biocinstallRepos
##' @export
GRAN <- function(package,
                 repos=c(BiocInstaller::biocinstallRepos(),
                     "http://R-Forge.R-project.org"),
                 type = if (onTheMac()) "both" else "source",
                 ...,
                 gRAN = GRANRepo$repo)
{
   
  ##TODO: get GRAN URL from config file
  if(!is.null(gRAN)) {
      granURL = repo_url(gRAN)
  } else {
      stop(paste('GRANRepo is NULL. Use source("<url-to-repo>/getGRAN.R") to',
                 "get a copy of the GRAN package which understands where your",
                 "GRAN repo of choice is, or specify a GRANRepository object"))
  }
  repos = c(GRAN=granURL, repos)
  install.packages(package, repos=repos, type=type, ...)

  invisible(TRUE)
}


onTheMac <- function(x) {
  grepl("darwin", R.version$platform)
}
