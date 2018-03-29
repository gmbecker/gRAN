#' Create package metadata files
#'
#' @title Create package metadata files
#' @importFrom jsonlite serializeJSON
#' @param repo A GRAN repo object
#' @param repodest The repo destination
#'    (something that looks like BASE_REPO_DIR/src/contrib)
#' @param metadir The directory containing metadata files
#' @param archive_dir Directory containing package archive
#' @param serialize_json Serialize the RDS metadata files as JSON
#' @author Dinakar Kulkarni
createMeta <- function (repo, repodest = destination(repo),
                        metadir = metadatadir(repo),
                        archive_dir = archivedir(repo),
                        serialize_json = FALSE) {
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

  if (serialize_json) {
    # Convert to JSON
    current_info_json <- serializeJSON(current_info, pretty = TRUE)
    current_info_json_file <- file.path(metadir, "current.json")
    archived_info_json <- serializeJSON(archived_info, pretty = TRUE)
    archived_info_json_file <- file.path(metadir, "archive.json")
    # Write JSON
    write(current_info_json, current_info_json_file)
    write(archived_info_json, archived_info_json_file)
  }

  invisible(NULL)
}
