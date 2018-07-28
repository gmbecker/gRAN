#' Create Windows binary builds (only works on Windows machines)
#'
#'
#' @title Make Windows binary packages
#' @param repo A GRANRepository object
#' @param cores Number of cores to use during build process
#' @param virtualstore Windows VM directory where built binaries are stored
#' @return None
#' @author Dinakar Kulkarni
#' @importFrom tools write_PACKAGES

makeWinBins <- function(repo,
                        cores = 1,
                        virtualstore = file.path(Sys.getenv("LOCALAPPDATA"),
                                                 "VirtualStore")) {
    bindir <- windowsbindir(repo)
    srcbuilddir <- destination(repo)

    # Clean up the Windows binary builds directory
    updateArchive(repo,
                  repodest = bindir,
                  archive = file.path(bindir, "Archive"),
                  ext = "\\.zip$")

    oldwd = setwd(staging(repo))
    on.exit(setwd(oldwd))
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
        if (file.exists(file.path(virtualstore, binpkg))) {
            file.rename(from = file.path(virtualstore, binpkg),
                        to = file.path(bindir, binpkg))
        } else if (file.exists(file.path(getwd(), binpkg))) {
            file.rename(from = file.path(getwd(), binpkg),
                        to = file.path(bindir, binpkg))
        } else {
            message(getwd(), " is the working dir and no zips were created here")
        }
    }, srcpkg = srcbuilds, mc.cores = cores)

    # Create PACKAGES files
    write_PACKAGES(bindir)
}
