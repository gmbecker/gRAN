#' Create Windows binary builds (only works on Windows machines)
#'
#'
#' @title Make Windows binary packages
#' @param repo A GRANRepository object
#' @param cores Number of cores to use during build process. defaults to (parallel:::detectCores() - 1)
#' @param virtualstore Windows directory where built binary packages are initially stored
#' @return None
#' @author Dinakar Kulkarni
#' @importFrom tools write_PACKAGES

makeWinBins <- function(repo,
                        cores = (parallel:::detectCores() - 1),
                        virtualstore = file.path(Sys.getenv("LOCALAPPDATA"),
                                                 "VirtualStore")) {
    bindir <- windowsbindir(repo)
    srcbuilddir <- destination(repo)

    # Clean up the Windows binary builds directory
    updateArchive(repo,
                  repodest = windowsbindir,
                  archive = file.path(windowsbindir, "Archive"),
                  ext = "\\.zip$")

    # Make binary builds
    srcbuilds <- list.files(srcbuilddir,
                            pattern = "\\.tar\\..*$",
                            full.names = TRUE)
    install_opts <- c("--build", "--compile-both")

    install.packages(srcbuilds,
                     type = "source",
                     repos = NULL,
                     INSTALL_opts = install_opts,
                     lib = temp_lib(repo),
                     Ncpus = cores)

    dummy <- mcmapply2(function(srcpkg) {
              binpkg <- paste0(gsub("\\.tar\\..*$",
                                    "", basename(srcpkg)), ".zip")
              file.rename(from = file.path(virtualstore, binpkg),
                          to = file.path(windowsbindir, binpkg))
    }, pkgName = srcbuilds, mc.cores = cores)

    # Create PACKAGES files
    write_PACKAGES(windowsbindir)
}
