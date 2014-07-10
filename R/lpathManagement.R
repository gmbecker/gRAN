## These are silly helper functions. Not even sure they're necessary, but I suppsoe they can't hurt and give the appearance of "switching R environments"

Renvs= new.env()

##' @export
compEnvFromRepo = function(repo, pkgs = available.packages(contrib.url(repo))[,"Package"], libloc, name, exclude.site = TRUE, switchTo = FALSE, deps_repos = c(defaultGRAN(), biocinstallRepos()), download.dir = NULL, ...) {
    ##need to make sure any dependencies that live in the site lib get installed if the environment is intended to be self-sufficient (exclude.site=TRUE)
    if(!file.exists(libloc))
        dir.create(libloc, recursive=TRUE)
    oldlp = .libPaths()
    if(exclude.site)
        .libPaths2(unique(.Library))
    else 
        .libPaths2(unique(c(.Library.site, .Library)))
    on.exit(.libPaths(oldlp))

    install.packages(pkgs, lib = libloc, repos = unique(c(repo, deps_repos)), destdir = download.dir, INSTALL_opts = sprintf("--library=%s", libloc), ...)
    .libPaths(oldlp)
    on.exit(NULL)
    ret = RComputingEnv(name, libpaths = libloc, exclude.site = exclude.site)
    if(switchTo)
        switchToCompEnv(ret)
    ret
}
    
        
##' @export
setGeneric("switchToCompEnv", function(Renv, reverting = FALSE, ...) standardGeneric("switchToCompEnv"))
##' @export
setMethod("switchToCompEnv", "RComputingEnv", function(Renv, reverting=FALSE, ...) {
        if(is.null(Renvs$stack)) {
            paths = .libPaths()
            paths = paths[!paths %in% c(.Library.site, .Library)]
            Renvs$stack = list(original = RComputingEnv("original", paths, exclude.site=FALSE))
        }
        
        if(!Renv@exclude.site)
            .libPaths(library_paths(Renv))
        else
            .libPaths2(c(library_paths(Renv), .Library))

        if(!reverting)
            Renvs$stack = c(Renv, Renvs$stack)
        else
            Renvs$stack = Renvs$stack[-1]
        announce(Renv, reverted = reverting)
        invisible(Renv)
    })
##' @export
setGeneric("announce", function(Renv, reverted=FALSE) standardGeneric("announce"))
##' @export
setMethod("announce", "RComputingEnv", function(Renv, reverted=FALSE) {
    message(sprintf("%s to the '%s' computing environment. %d packages are currently available. Packages installed in your site library ARE %ssuppressed.\n To switch back to your previous environment type revertCompEnv()", ifelse(reverted, "Reverted", "Switched"), Renv@name, nrow(Renv@packages), ifelse(Renv@exclude.site, "", "NOT ")))
})

##' @export
setGeneric("library_paths", function(Renv) standardGeneric("library_paths"))
##' @export
setMethod("library_paths", "RComputingEnv", function(Renv) {
    Renv@libpaths
})

##' @export
setMethod("show", "RComputingEnv", function(object) {
    cat(paste(sprintf("An RComputingEnv object defining the '%s' computing environment", object@name),
              "\n\n\t", sprintf("Primary library location(s): %s", paste(object@libpaths, collapse=";")),
              "\n\t", sprintf("Packages: %d packages installed in %d directories (including R's base library)", nrow(object@packages), length(unique(object@packages$LibPath))),
              "\n\t", paste0("This environment DOES ", ifelse(object@exclude.site, "NOT ", ""), "combine with the current site library location when loaded."),
              "\n\n"))
})

##' @export
setGeneric("packages", function(Renv) standardGeneric("packages"))
##' @export
setMethod("packages", "RComputingEnv", function(Renv) Renv@packages)
##' @export
revertCompEnv = function() {
    if(length(Renvs$stack) < 2) {
        warning("No previous computing environment to revert to. Computing environment will remain unchanged")
        return(NULL)
    }
    switchToCompEnv(Renvs$stack[[2]], reverting=TRUE)
}

##' @export   
currentCompEnv = function() {
            if(is.null(Renvs$stack)) {
                lp = .libPaths()
                lp = lp[!(lp %in% .Library | lp %in% .Library.site)]
                Renvs$stack = list(original = RComputingEnv("original", lp , exclude.site=FALSE))
            }
            Renvs$stack[[1]]
        }


##scary and bad!!!!
.libPaths2 = function(fulllp) {
    fun = function(x) .lib.loc <<- unique(x)
    environment(fun) = environment(.libPaths)
    fun(fulllp)
}

    
