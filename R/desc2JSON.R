#' Build JSON of DESCRIPTION file
#'
#' Create a JSON version of the package's DESCRIPTION file. Useful for
#' interfacing with other applications.
#'
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom jsonlite toJSON
#' @param repo A GRAN repo object
#' @param suffix String that is appended to the end of the JSON file
#' @return None
desc2JSON <- function(repo, suffix = "-description.json") {
  destination <- param(repo)@dest_base
  subrepo <- repo_name(repo)
  pkgs <- repo_results(repo)$name
  for (pkg in pkgs) {
    check_dir <- file.path(staging(repo), paste0(pkg, '.Rcheck'))
    if (file.exists(file.path(check_dir, "DESCRIPTION"))) {
      descr_df <- generateDescInfo(file.path(check_dir, pkg))
      descr_df$id <- encode_string(descr_df$Package)
      descr_df$granrepo <- subrepo
      desc_json <- toJSON(descr_df, pretty = TRUE)
      outfile <- file.path(destination, subrepo, "PkgDocumentation", pkg,
                           paste0(descr_df$Package, suffix))
      write(desc_json, outfile)
    }
  }
  NULL
}
