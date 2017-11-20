#' Move older versions of packages into the repo Archive
#'
#' @title Move older package sources to the Archive directory
#' @param repo A GRAN repo object
#' @param repodest The repo destination
#'    (something that looks like BASE_REPO_DIR/src/contrib)
#' @param archive The Archive directory where older packages will be stored
#' @author Dinakar Kulkarni
updateArchive <- function (repo, repodest = destination(repo),
                           archive = archivedir(repo)) {
  if (!file.exists(archive)) {
    if (!dir.create(archive, recursive = TRUE)) {
      stop("Repo Archive not found and could not be created\n", call. = FALSE)
    }
  }

  # Identify which packages are new vs which are old
  ext <- "\\.tar\\..*$"
  tarballs <- list.files(repodest, pattern = ext, full.names = FALSE)
  noextfiles <- gsub(ext, "", tarballs)
  pkgs <- sapply(strsplit(tarballs, "_", fixed = TRUE), "[", 1L)
  verstxt <- gsub("[a-zA-Z0-9\\.]*_", "", noextfiles)
  vers <- package_version(verstxt)
  df <- data.frame(file = tarballs, package = pkgs,
                   version = vers, stringsAsFactors = FALSE)
  df <- df[order(df$package, df$version, decreasing = TRUE), ]
  df$newest <- !duplicated(df$package)
  df <- df[order(df$package, df$version, decreasing = FALSE), ]
  old <- df[!df[, "newest"], ]

  # Create individual package archive dirs
  createPkgArchive <- function(pkgarchive) {
    pkgarchivedir <- file.path(repodest, "Archive", pkgarchive)
    if (!file.exists(pkgarchivedir)) {
      if (!dir.create(pkgarchivedir, recursive = TRUE)) {
        stop("Package archive directory for ", pkgarchive,
             " not found and couldn't be created\n", call. = FALSE)
      }
    }
  }
  sapply(unique(old$package), createPkgArchive)

  # Move the packages to the archive
  file.rename(file.path(repodest, old$file),
              file.path(archive, old$package, old$file))
  invisible(NULL)
}
