#' Create JSON representation of package information
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom jsonlite toJSON
#' @param repo A GRAN repo object
#' @param pkg_name Name of the GRAN package
#' @param descr_df data.frame representation of DESCRIPTION file
#' @param scm_df data.frame representation of GRAN manifest object
#' @param docdir Directory where the JSON doc will be written
#' @param suffix Suffix for the JSON file
#' @return None. Write JSON file to disk
#' @seealso \code{\link{manifest_df}} for generating scm_df and
#'    \code{\link{generateDescInfo}} for generating descr_df.
createJSON <- function(repo, pkg_name, descr_df, scm_df, docdir,
                       suffix = paste0("_", descr_df$Version, ".json")) {
  reponame <- paste0("GRAN", repo_name(repo))

  ## TODO
  # Convert Imports, Depends and Suggests to JSON arrays

  descr_df$id <- encode_string(paste0(reponame, descr_df$Package))
  descr_df$gran_repo <- reponame

  # Get SCM info:
  scm_info <- scm_df[scm_df$name == pkg_name, ]
  descr_df$scm_url <- scm_info$url
  descr_df$scm_type <- scm_info$type
  descr_df$scm_branch <- scm_info$branch
  descr_df$scm_subdir <- scm_info$subdir
  descr_df$r_version <- R.version$version.string
  #descr_df$bioc_version <- BiocInstaller::biocVersion()

  # Add repo result information
  bldresults <- repo_results(repo)
  bldresults <- bldresults[bldresults$name == pkg_name, ]
  descr_df$last_attempt_version <- bldresults$lastAttemptVersion
  descr_df$last_attempt_status <- bldresults$lastAttemptStatus
  descr_df$last_attempt_date <- bldresults$lastAttempt
  descr_df$last_built_version <- bldresults$lastbuiltversion
  descr_df$last_built_status <- bldresults$lastbuiltstatus
  descr_df$last_built_date <- bldresults$lastbuilt
  descr_df$is_suspended <- bldresults$suspended

  # Get documentation URL locations:
  doc_url <- paste0(repo_url(repo), "/PkgDocumentation/", pkg_name)
  descr_df$pkgdocs_url <- doc_url
  descr_df$pkg_sticker <- paste0(doc_url, "/", pkg_name, ".png")

  # Convert to JSON
  desc_json <- toJSON(descr_df, pretty = TRUE)
  # Make sure JSON is not in the form of a list,
  # Trim leading and trailing list markers i.e. '[' and ']'
  desc_json <- substring(desc_json, 2)
  desc_json <- substr(desc_json, 1, nchar(desc_json) - 1)

  # Write JSON
  json_outfile <- file.path(docdir, paste0(pkg_name, suffix))
  logfun(repo)(pkg_name, "Writing package metadata JSON file")
  write(desc_json, json_outfile)
}
