
##' @export
setGeneric("Install", function(pkgs, repos, verbose = FALSE, ...) standardGeneric("Install"))

setMethod("Install", c("character", "character"), function(pkgs, repos, verbose, ...) install.packages(pkgs, repos = repos, ...))

setMethod("Install", c(pkgs = "character", repos= "PkgManifest"), function(pkgs, repos, verbose, ...) {

    ghrepo= lazyRepo(pkgs, repos, verbose = verbose)
    avail1 = available.packages(ghrepo)
    avail2 = available.packages(contrib.url(depRepos(repos)))
    new = !avail2[,"Package"] %in% avail1[,"Package"]
    avail = rbind(avail1, avail2[new,])
    install.packages(pkgs, available = avail, ...)
})
    
    
     
