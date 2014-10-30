
##' @export
setGeneric("Install", function(pkgs, repos, versions = NULL, verbose = FALSE, ...) standardGeneric("Install"))

setMethod("Install", c("character", "character"), function(pkgs, repos, versions, verbose, ...) {

    man = PkgManifest(manifest = ManifestRow(), dep_repos = repos)
    if(!is.null(versions))
        man = SessionManifest(pkg_manifest = man, pkg_versions = version)
    Install(pkgs, repos = man, verbose = verbose, ...)
    
})



setMethod("Install", c(pkgs = "character", repos= "PkgManifest"), function(pkgs, repos, verbose, ...) {

    ghrepo= lazyRepo(pkgs, repos, verbose = verbose)
    avail1 = available.packages(ghrepo)
    avail2 = available.packages(contrib.url(depRepos(repos)))
    new = !avail2[,"Package"] %in% avail1[,"Package"]
    avail = rbind(avail1, avail2[new,])
    oldinst = installed.packages()
    install.packages(pkgs, available = avail, ...)
    newinst = installed.packages()

    ## determine which packages were actually installed.
    newinds = !newinst[,"Package"] %in% oldinst[,"Package"]
    upinds = !newinds && newinst[, "Version"] != oldinst[newinst[,"Package"], "Version"]
    inds = newinds | upinds
    installedpkgs = newinst[inds, "Package"]
    annotateDESCs(installedpkgs, repos)
    installedpkgs
})




    
    
     
