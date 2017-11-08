#' install_packages method for GRANRepository objects
#'
#' @param pkgs Character vector of package names to install
#' @param repos A GRANRepository
#' @param versions Ignored
#' @param verbose Whether verbose/debugging information should be printed
#' @param \dots Passed (eventually) to install.packages
#' @details This method calls directly down to the character,character
#' method of install_packages in switchr
#' @docType methods
#' @export
#' @importMethodsFrom switchr install_packages

setMethod("install_packages", c(pkgs = "character", repos= "GRANRepository"),
    function(pkgs, repos,versions, verbose, ...) {
        install_packages(pkgs = pkgs, repos = c(repo_url(repos), defaultRepos()),
                         verbose = verbose, ...)
})
