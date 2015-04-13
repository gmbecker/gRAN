##' Install packages 'directly' from a GRANRepository object.
##' @docType methods
##' @rdname install_packages_GRANRepo
##' @param pkgs The list of packages to install
##' @param repos a GRANRepository object
##' @param verbose Should verbose logging messages be displayed to the user
##' @param ... passed directly to \code{install.packages}
##' @param versions ignored when repos is a GRANRepository object.
##' @importMethodsFrom switchr install_packages
##' @aliases install_packages,character,GRANRepository
##' @export

setMethod("install_packages", c(pkgs = "character", repos= "GRANRepository"), function(pkgs, repos, verbose, ...) {
    install_packages(pkgs = pkgs, repos = c(repo_url(repos), biocinstallRepos()),
            verbose = verbose, ...)
})
