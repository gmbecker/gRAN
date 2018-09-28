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
  if(!length(builtpkgs))
    return(NULL)
  noextfiles <- gsub(ext, "", builtpkgs)
  pkgs <- sapply(strsplit(builtpkgs, "_", fixed = TRUE), "[", 1L)
  verstxt <- gsub("[a-zA-Z0-9\\.]*_", "", noextfiles)
  vers <- package_version(verstxt)
  df <- data.frame(file = builtpkgs, package = pkgs,
                   ##                 version = vers, stringsAsFactors = FALSE)
                   ## we are using compareVersion which
                   ## doesn't support package_version objs
                   version = verstxt, stringsAsFactors = FALSE)
  #df <- df[order(df$package, df$version, decreasing = TRUE), ]
  newestinds <- findNewestRows(df)
  df$newest <- FALSE
  df$newest[newestinds] <- TRUE
#  df <- df[order(df$package, df$version, decreasing = FALSE), ]
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

.newestPkgVersion = function(inds, df) {
    if(length(inds) ==1)
        return(inds)
    
    curind = inds[1]
    ## head(x, -1) is all but the last element
    ## pass through the versions once to get the latest one
    for(i in head(seq(along = inds), -1)) {
        if(compareVersion(df$version[ inds[ i ] ],
                          df$version[ inds[ i + 1 ] ]) < 0)
            curind = inds[i+1]
        }
    curind
}

findNewestRows = function(df) {
    indsbypkg = split(seq(along = df[[1]]), df$package)
    res = lapply(indsbypkg, function(inds)  .newestPkgVersion(inds, df))
    unlist(res)
}
