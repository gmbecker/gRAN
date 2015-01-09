##' @importMethodsFrom switchr install_packages
##' @export

setMethod("install_packages", c(pkgs = "character", repos= "GRANRepository"), function(pkgs, repos, verbose, ...) {
    install_packages(pkgs = pkgs, repos = c(repo_url(repos), biocinstallRepos()),
            verbose = verbose, ...)
})
