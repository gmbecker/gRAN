#' Create JSON representation of package information
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom jsonlite toJSON
#' @param repo A GRAN repo object
#' @param pkg_name Name of the GRAN package
#' @param descr_df data.frame representation of DESCRIPTION file
#' @param scm_df data.frame representation of GRAN manifest object
#' @param docdir Directory where the JSON doc will be written
#' @param rev_deps data.frame representing pkg_name's reverse deps
#' @param suffix Suffix for the JSON file
#' @return None. Write JSON file to disk
#' @seealso \code{\link{manifest_df}} for generating scm_df and
#'    \code{\link{generateDescInfo}} for generating descr_df.
createJSON <- function(repo, pkg_name, descr_df, scm_df, docdir, rev_deps,
                       suffix = paste0("_", descr_df$Version, ".json")) {
  reponame <- paste0("GRAN", repo_name(repo))

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
  doc_url <- paste0(repo_url(repo), basename(pkg_doc_dir(repo)), pkg_name)
  descr_df$pkgdocs_url <- doc_url
  descr_df$pkg_sticker <- paste0(doc_url, "/", pkg_name, ".png")

  # Convert the data.frame to a list
  descr_list <- lapply(as.list(descr_df), as.vector)

  # Get reverse dependencies
  reverse_deps <- lapply(as.list(rev_deps), as.vector)

  # Combine these lists
  combo_list <- append(descr_list, reverse_deps)

  # Convert comma-separated strings to vectors
  if ("Imports" %in% names(combo_list))
    combo_list$Imports <- .stringToVec(combo_list$Imports)
  if ("Depends" %in% names(combo_list))
    combo_list$Depends <- .stringToVec(combo_list$Depends)
  if ("Suggests" %in% names(combo_list))
    combo_list$Suggests <- .stringToVec(combo_list$Suggests)
  if ("Enhances" %in% names(combo_list))
    combo_list$Enhances <- .stringToVec(combo_list$Enhances)
  if ("LinkingTo" %in% names(combo_list))
    combo_list$LinkingTo <- .stringToVec(combo_list$LinkingTo)
  if ("ReverseImports" %in% names(combo_list))
    combo_list$ReverseImports <- .stringToVec(combo_list$ReverseImports)
  if ("ReverseDependencies" %in% names(combo_list))
    combo_list$ReverseDependencies <- .stringToVec(combo_list$ReverseDependencies)
  if ("ReverseLinkingTo" %in% names(combo_list))
    combo_list$ReverseLinkingTo <- .stringToVec(combo_list$ReverseLinkingTo)
  if ("ReverseSuggests" %in% names(combo_list))
    combo_list$ReverseSuggests <- .stringToVec(combo_list$ReverseSuggests)
  if ("ReverseEnhances" %in% names(combo_list))
    combo_list$ReverseEnhances <- .stringToVec(combo_list$ReverseEnhances)

  # Convert to JSON
  desc_json <- toJSON(combo_list, pretty = TRUE)
  
  # Write JSON
  json_outfile <- file.path(docdir, paste0(pkg_name, suffix))
  logfun(repo)(pkg_name, "Writing package metadata JSON file")
  write(desc_json, json_outfile)
}

.stringToVec <- function(x) {
  unlist(strsplit(gsub("[[:blank:]]", "",x), ","))
}
