myGRANenv = new.env()
##' defaultGRAN
##'
##' Get the GRAN repository object for the associated repo
##' 
##' @return The GRANRepository object encapsulated by this package
##' @export
defaultGRAN = function() {
    if(is.null(myGRANenv$repo))
        myGRANenv$repo = loadRepo(system.file("myrepo.R", package = pkgname))
    myGRANenv$repo
}

##' defaultGRANURL
##'
##' Get the URL for the associated repo
##' @return A character value with the URL for the default GRAN repository in
##' this package
##' @export
defaultGRANURL = function() repo_url(defaultGRAN())
