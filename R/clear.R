#' Clear packages and temporary files  from repo build process
#'
#' These are convenience functions which clears the intermediate
#' files generated during the build process. This is important
#' when, e.g., building a repository for the first time with
#' a new version of R.
#'
#' @details \code{clear_repo} removes packages deployed into the destination repository,
#' updates the PACKAGES and PACKAGES.gz files, and resets the build results within the
#' GRANRepository object. \code{clear_temp_fils} clears
#' intermediate files from the library location used during building, the temporary
#' repository, the package staging area, and the store of install- and check-results.
#'
#' @param repo GRANRepository - The repository to clean
#' @param checkout logical - Should the checkouts of packages also be cleared.
#' Generally this is not necessary (default: FALSE)
#'
#' @return The GRANRepository object, ready to be rebuilt.
#' @author Gabriel Becker
#' @rdname clear
#' @export
clear_temp_fils = function(repo, checkout = FALSE) {
    dirs = c( "temporary library" = temp_lib(repo),
              "temporary repo" = temp_repo(repo),
              "install results" = install_result_dir(repo),
              "check results" = check_result_dir(repo),
              "staging area" = staging(repo),
              "covr reports" = coverage_report_dir(repo),
              "package docs" = pkg_doc_dir(repo),
              "single pkglogs" = pkg_log_dir(repo))

    res = mapply(.clearhelper, dirs, repo = list(repo), dirlab = names(dirs))
    if(checkout)
        res = c(res, .clearhelper(checkout_dir(repo), repo, "checkout directory"))
    if(!all(res))
        warning("Not all clearing steps succeeded. See the relevant GRAN log for more information")
    repo
}



.clearhelper <- function(dir, repo, dirlab) {
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
                                   sum(res>0), dirlab, dir), type = "both")
    all(res==0)
}

#' @rdname clear
#' @param all logical - Should temporary artifacts from the build process also be cleared
#' (via automatically calling clear_temp_fils). Defaults to TRUE
#' @param archivedir character - Optional. A directory where build packages
#' deployed to the repository will be archived. Package versions already in
#' the archive will not be overwritten.
#' @export
clear_repo = function(repo, all = TRUE, checkout = FALSE, archivedir = NA) {
    if(is.na(archivedir)) {
        archivedir <- backup_archive(repo)
        if(!file.exists(archivedir))
            dir.create(archivedir)
    }
    if(all)
        res = clear_temp_fils(repo = repo, checkout = checkout)
    else
        res = logical()
    d = destination(repo)
    if(!is.null(archivedir)) {
        fils = list.files(d, pattern = builtPkgExt(),
                          full.names = TRUE, recursive = TRUE)
        logfun(repo)("NA", sprintf("Found %d deployed packages. Copying to archive before clearing repository.", length(fils)))
        file.copy(fils, archivedir, overwrite = FALSE)
    }
    res = .clearhelper(d, repo, "deployed packages")
    write_PACKAGES(d)
    if(!res)
        warning("Failed to fully clear packages from deployed repository")
    repo = resetResults(repo)
    saveRepoFiles(repo)
    repo
}
