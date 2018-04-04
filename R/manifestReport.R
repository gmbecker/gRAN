#' manifestReport
#'
#' Build a package manifest report for a GRAN repository
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom htmlTable htmlTable
#' @importFrom jsonlite toJSON
#' @importFrom tools file_path_sans_ext
#' @param repo A GRANRepository object
#' @param theme CSS+JS theme. bootstrap, foundation, semanticui or jqueryui
#' @param reportfile File path of the HTML report
#' @param jsonrpt Whether to create a JSON version of the manifest report
#' @return None
#' @export
manifestReport <- function(repo, theme = "bootstrap",
                           reportfile = file.path(destination(repo),
                                                 "manifest.html"),
                           jsonrpt = TRUE) {
  title <- paste0("<title>GRAN", repo_name(repo), " Manifest</title>")
  header <- paste0("<h1>Build manifest for GRAN",
                              repo_name(repo), "</h1>")
  cnames <- c("name", "url", "type", "branch", "subdir")
  scm_df <- manifest_df(repo)[, cnames]
  # Don't include suspended packages
  scm_df <- scm_df[!(scm_df$name %in% suspended_pkgs(repo)), ]

  table_header <- c("Package Name", "SCM Repo", "SCM Type",
                    "Branch", "Package Subdirectory in SCM Repo")
  manifest_html <- htmlTable(scm_df,
                        header = table_header,
                        css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
                        css.class = "table-hover table-striped table-bordered",
                        css.table = "margin-left:10px; margin-right:10px;",
                        label = "manifestreport",
                        align = "l",
                        escape.html = FALSE)
  manifest_html <- gsub("border-bottom: 2px solid grey;", "", manifest_html)

  # Create CSS, JS tags
  css_tag <- paste0("<link rel=\"stylesheet\" type=\"text/css\"",
                    " href=\"assets/css/", theme, ".min.css\">",
                    "<link rel=\"stylesheet\" type=\"text/css\"",
                    " href=\"assets/css/dataTables.", theme, ".min.css\">")
  js_tag <- paste0("<script type=\"text/javascript\" language=\"javascript\"
                   src=\"assets/js/jquery.js\"></script>",
                   "<script type=\"text/javascript\" language=\"javascript\"
                   src=\"assets/js/", theme,".min.js\"></script>",
                   "<script type=\"text/javascript\" language=\"javascript\"
                   src=\"assets/js/jquery.dataTables.min.js\"></script>",
                   "<script type=\"text/javascript\" language=\"javascript\"
                   src=\"assets/js/dataTables.", theme, ".min.js\"></script>")
  ds_script <- paste("<script type=\"text/javascript\" charset=\"utf-8\">
                     $(document).ready(function() {
                     $('#manifestreport').DataTable();
                     } );
                     </script>")

  #
  # Create a JSON version of the report if specified
  if (jsonrpt) {
    json_outfile <- paste0(file_path_sans_ext(reportfile), ".json")
    write(toJSON(scm_df, pretty = TRUE), json_outfile)
    dl_json_link <- paste0("<p><a href=\"", basename(json_outfile),
                           "\">View manifest as JSON</a><p>")
  } else {
    dl_json_link <- ""
  }

  # Construct final HTML
  final_html <- paste("<!doctype html>
                       <html> <head>", title, css_tag, js_tag, ds_script,
                       "<body style=\"padding: 20px;\"></head>",
                       header, manifest_html, "<br/>", dl_json_link,
                       "</body></html>")
  write(final_html, reportfile)
}