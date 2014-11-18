##' @importMethodsFrom switchr Install
##' @export

setMethod("Install", c(pkgs = "character", repos= "GRANRepository"), function(pkgs, repos, verbose, ...) {
    Install(pkgs = pkgs, repos = c(repo_url(repos), biocinstallRepos()),
            verbose = verbose, ...)
})
