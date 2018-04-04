#' Move older versions of packages into the repo Archive
#'
#' @title Move older package sources to the Archive directory
#' @param repo A GRAN repo object
#' @param repodest The repo destination
#'    (something that looks like BASE_REPO_DIR/src/contrib)
#' @param archive The Archive directory where older packages will be stored
#' @param ext Regex describing the file extension of the built packages
#' @author Dinakar Kulkarni
updateArchive <- function (repo, repodest = destination(repo),
                           archive = archivedir(repo),
                           ext = "\\.tar\\..*$") {
  if (!file.exists(archive)) {
    if (!dir.create(archive, recursive = TRUE)) {
      stop("archive not found and could not be created\n", call. = FALSE)
    }
  }

  # Identify which packages are new vs which are old
  builtpkgs <- list.files(repodest, pattern = ext, full.names = FALSE)
  noextfiles <- gsub(ext, "", builtpkgs)
  pkgs <- sapply(strsplit(builtpkgs, "_", fixed = TRUE), "[", 1L)
  verstxt <- gsub("[a-zA-Z0-9\\.]*_", "", noextfiles)
  vers <- package_version(verstxt)
  df <- data.frame(file = builtpkgs, package = pkgs,
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
