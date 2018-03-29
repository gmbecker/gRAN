#' Create package metadata files
#'
#' @title Create package metadata files
#' @param repo A GRAN repo object
#' @param repodest The repo destination
#'    (something that looks like BASE_REPO_DIR/src/contrib)
#' @param metadir The directory containing metadata files
#' @param archive_dir Directory containing package archive
#' @author Dinakar Kulkarni
createMeta <- function (repo, repodest = destination(repo),
                        metadir = metadatadir(repo),
                        archive_dir = archivedir(repo)) {
  if (!file.exists(metadir)) {
    if (!dir.create(metadir, recursive = TRUE)) {
      stop("Metadata directory not found and could not be created\n",
            call. = FALSE)
    }
  }

  # Create the current.rds file
  ext <- "\\.tar\\..*$"
  tarballs <- list.files(repodest, pattern = ext, full.names = FALSE)
  current_info <- file.info(tarballs)
  saveRDS(current_info, file = file.path(metadir, "current.rds"))

  # Create the archive.rds file
  archived_pkgs <- list.dirs(archive_dir,
                            full.names = FALSE)
  archived_info <- list()
  for (pkg in archived_pkgs) {
    archived_info[[pkg]] <- file.info(list.files(file.path(archive_dir, pkg),
                                                 pattern = ext,
                                                 full.names = FALSE,
                                                 include.dirs = TRUE))
  }
  saveRDS(archived_info, file = file.path(metadir, "archive.rds"))

  invisible(NULL)
}
