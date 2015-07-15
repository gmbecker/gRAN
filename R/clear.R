##' Clear packages and temporary files  from repo build process
##'
##' These are convenience functions which clears the intermediate
##' files generated during the build process. This is important
##' when, e.g., building a repository for the first time with
##' a new version of R.
##'
##' @details \code{clear_repo} removes packages deployed into the destination repository
##' and updates the PACKAGES and PACKAGES.gz files. \code{clear_temp_fils} clears
##' intermediate files from the library location used during building, the temporary
##' repository, the package staging area, and the store of install- and check-results.
##'
##' @param repo GRANRepository - The repository to clean
##' @param checkout logical - Should the checkouts of packages also be cleared.
##' Generally this is not necessary (default: FALSE)
##'
##' @return logical scalar indicating full success (TRUE) or one or more failures (FALSE). The repository log will contain more detailed information about failures.
##' @author Gabriel Becker
##' @rdname clear
##' @export
clear_temp_fils = function(repo, checkout = FALSE) {
    dirs = c( "temporary library" = temp_lib(repo),
        "temporary repo" = temp_repo(repo),
        "install results" = install_result_dir(repo),
        "check results" = check_result_dir(repo),
        "staging area" = staging(repo))

    res = mapply(.clearhelper, dirs, repo = list(repo), dirlab = names(dirs))
    if(checkout)
        res = c(res, .clearhelper(checkout_dir(repo), repo, "checkout directory"))
    all(res)
}    
    
    
        
.clearhelper = function(dir, repo, dirlab) {
    fils = list.files(dir, include.dirs=TRUE, no..=TRUE, full.names=TRUE)
    if(!file.exists(dir) || !length(fils)) {
        logfun(repo)("NA", sprintf("Not clearing non-existent or empty %s (%s)",
                                   dirlab, dir))
        return(TRUE)
    }

    logfun(repo)("NA", sprintf("Clearing %d files/directories from %s (%s)",
                               length(fils), dirlab, dir))
    res = unlink(fils, recursive=TRUE)
    if(any(res>0))
        logfun(repo)("NA", sprintf("Failed to clear %d files/directories from %s (%s)",
                                   sum(res>0), dirlab, dir), type = "error")
    all(res==0)
}

##' @rdname clear
##' @param all Should temporary artifacts from the build process also be cleared
##' (via automatically calling clear_temp_fils). Defaults to TRUE
##' @param archivedir
##' @export
clear_repo = function(repo, all = TRUE, checkout = FALSE, archivedir = NULL) {
    if(all)
        res = clear_temp_fils(repo = repo, checkout = checkout)
    else
        res = logical()
    d = destination(repo)
    if(!is.null(archivedir)) {
        if(!file.exists(archivedir))
            dir.create(archivedir)
        
        fils = list.files(d, pattern = "tar.gz", full.names=TRUE)
        logfun(repo)("NA", sprintf("Found %d deployed packages. Copying to archive before clearing repository.", length(fils)))
        file.copy(fils, archivedir, overwrite = FALSE)
    }
    res = c(res, .clearhelper(d, repo, "deployed packages"))
    write_PACKAGES(d)
    res
}
