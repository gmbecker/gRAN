## Beware! I don't know how fragile this is.
inGRAN = function()
{
    ns = environment(getMyGRAN)
    if(!isNamespace(ns))
        return(FALSE)
    pkgname = get("spec", envir = get(".__NAMESPACE__.", envir = ns))["name"]
    pkgname == "GRAN"
}


getMyGRAN = function()
{
    GRANRepo = NULL
    ##if we are in the GRAN package (created via GRANonGRAN.R) then we have a specific repo to use.
    if(inGRAN()) {
        GRANRepo = loadRepo(system.file("myrepo.R", package="GRAN"))
        GRANRepo@manifest = readManifest(repo = GRANRepo)
    }
    GRANRepo
}

##' GRANRepo
##'
##' An environment populated at load time with a GRANRepository object in GRANRepo$repo
##' @export
GRANRepo = new.env()

GRANRepo$repo = NULL

##' defaultGRAN
##'
##' Return the default GRAN repository for this package, or NULL.
##' @export
defaultGRAN = function() GRANRepo$repo

##' currentPackage
##'
##' Returns the name of the GRAN package loaded (GRAN or GRANBase)
##' @export
currentPackage = function() if(inGRAN()) "GRAN" else "GRANBase"
