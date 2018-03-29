#' manifestHTML
#'
#' Create a build report for a repository reflecting the latest build
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom htmlTable htmlTable
#' @importFrom jsonlite toJSON
#' @importFrom tools file_path_sans_ext
#' @param repo A GRANRepository object
#' @param theme CSS+JS theme. bootstrap, foundation, semanticui or jqueryui
#' @param reportfile Where the report should be located
#' @param riskrpt Whether to build the risk report
#' @param jsonrpt Whether to create a JSON version of the build report
#' @return None
#' @export
manifestHTML <- function(repo, theme = "bootstrap",
                reportfile = file.path(destination(repo), "buildreport.html"),
                riskrpt = FALSE,
                jsonrpt = TRUE) {

  # Overall Build Stats
  title <- paste0("<title>GRAN", repo_name(repo), " Build Report</title>")
  summary_header <- paste0("<h2>Overall Build Stats for GRAN",
                          repo_name(repo), "</h2>")
  results_df <- repo_results(repo)
  results_df <- results_df[!(results_df$name %in% suspended_pkgs(repo)), ]
  lastattempt <- results_df$lastAttemptStatus
  attmptab <- as.matrix(table(lastattempt))
  attmpthtml <- htmlTable(attmptab,
                        css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
                        css.class = "table-hover table-bordered table-striped",
                        css.table = "margin-left:10px;margin-right:10px;",
                        align = "l",
                        escape.html = FALSE)

  # Get detailed build report
  cnames <- c("name", "lastAttemptVersion", "lastAttemptStatus", "lastAttempt",
              "lastbuiltversion", "lastbuiltstatus", "lastbuilt", "maintainer")
  tmpman <- repo_results(repo)[, cnames]

  # Don't show suspended pkgs in result
  tmpman <- tmpman[!(tmpman$name %in% suspended_pkgs(repo)), ]
  tmpman[is.na(tmpman)] = "NA"

  # Create a JSON version of the report if specified
  if (jsonrpt) {
    json_outfile <- paste0(file_path_sans_ext(reportfile), ".json")
    write(toJSON(tmpman, pretty = TRUE), json_outfile)
    dl_json_link <- paste0("<p><a href=\"", json_outfile,
                                  "\">View build report as JSON</a><p>")
  } else {
    dl_json_link <- ""
  }

  # Calculate test Coverage
  if(check_test_on(repo)) {
    covr <- testCoverage(repo)
    tmpman$coverage <- covr$coverage[match(tmpman$name, covr$name)]
  } else {
    tmpman$coverage <- NULL
  }
  covg_report <- file.path("..", "..", "CovrReports", paste0(tmpman$name,
                            "-covr-report.html"))
  tmpman$coverage <- paste0("<a href='", covg_report, "'>", tmpman$coverage, "</a>")
  tmpman$coverage[!file.exists(file.path(destination(repo), covg_report))] = ""

  # Assign badges and log links to the latest build status
  # Also make logs human-readable
  tmpman$Chronicles <- ""
  log_header <- paste0("<html><meta charset=\"UTF-8\"><link rel=\"stylesheet\"",
                       "type=\"text/css\" href=\"../src/contrib/assets/css/",
                       theme, ".min.css\"><pre><code>")
  log_closer <- "</code></pre></html>"
  for (i in 1:length(tmpman$lastAttemptStatus)) {
    # Prettify the logs
    checkrep <- file.path("..", "..", "CheckResults",
                          paste0(tmpman$name[i], "_CHECK.log"))
    pkglog <- file.path("..", "..", "SinglePkgLogs",
                        paste0(tmpman$name[i], ".log"))
    install_results <- file.path("..", "..", "InstallResults",
                                 paste0(tmpman$name[i], ".out"))
    x_loc <- file.path(destination(repo), checkrep)
    if (file.exists(x_loc) && !grepl(log_closer,
                                    readChar(x_loc, file.info(x_loc)$size))) {
      cat(log_closer, file = x_loc, append = TRUE, sep = "\n")
      lines <- readLines(x_loc, -1, warn = FALSE)
      lines[1] <- paste(log_header, lines[1], sep = "\n")
      writeLines(lines, x_loc)
    }
    y_loc <- file.path(destination(repo), pkglog)
    if (file.exists(y_loc)) {
      lines <- readLines(y_loc, -1, warn = FALSE)
      lines <- gsub(log_closer, "", lines)
      lines <- gsub(log_header, "", lines)
      lines <- rev(lines)[1:500] # Retain only last 500 lines
      lines <- rev(lines)
      lines[1] <- paste(log_header, lines[1], sep = "\n")
      lines <- lines[!is.na(lines)]
      writeLines(lines, y_loc)
      cat(log_closer, file = y_loc, append = TRUE, sep = "\n")
    }
    z_loc <- file.path(destination(repo), install_results)
    if (file.exists(z_loc) && !grepl(log_closer,
                                    readChar(z_loc, file.info(z_loc)$size))) {
      cat(log_closer, file = z_loc, append = TRUE, sep = "\n")
      lines <- readLines(z_loc, -1, warn = FALSE)
      lines[1] <- paste(log_header, lines[1], sep = "\n")
      writeLines(lines, z_loc)
    }

    # Create badges
    status <- tmpman$lastAttemptStatus[i]
    #oldstatus <- tmpman$lastbuiltstatus[i]
    tmpman$lastAttemptStatus[i] <- buildBadge(status, tmpman$name[i])
    #tmpman$lastbuiltstatus[i] <- buildBadge(oldstatus, tmpman$name[i])

    # Create hrefs for email IDs
    tmpman$maintainer[i] <- emailTag(tmpman$maintainer[i])

    # Build history
    tmpman$Chronicles[i] <- createURL(pkglog, "Build log")

    # Package documentation
    pkg_doc <- file.path("..", "..", "PkgDocumentation",
                         tmpman$name[i], "index.html")
    if (file.exists(file.path(destination(repo), pkg_doc))) {
      tmpman$name[i] <- createURL(pkg_doc, tmpman$name[i])
      # Add test coverage badge to spash page
      if (!tmpman$coverage[i] == "") {
        lines <- readLines(file.path(destination(repo), pkg_doc), warn = FALSE)
        covbadge <- paste("<p>Test Coverage:", tmpman$coverage[i], "</p>")
        lines <- gsub("<tcplaceholder/>", covbadge, lines)
        write(lines, file.path(destination(repo), pkg_doc))
      }
    }
  }

  # Create HTML table from tmpman
  build_header <- "<h2>Build details</h2>"
  table_header <- c("Package Name", "Last Attempt Version", "Last Attempt Status",
                "Last Attempt Date", "Last Built Version", "Last Built Status",
                "Last Built Date", "Maintainer", "Coverage", "Build History")
  build_html <- htmlTable(tmpman,
                        header = table_header,
                        css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
                        css.class = "table-hover table-striped table-bordered",
                        css.table = "margin-left:10px; margin-right:10px;",
                        label = "builddetails",
                        align = "l",
                        escape.html = FALSE)
  build_html <- gsub("border-bottom: 2px solid grey;", "", build_html)

  # Add JS, CSS to the report page
  assets_folder <- system.file2("assets", package = "GRANBase")
  file.copy(assets_folder, destination(repo), recursive = TRUE)
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
                  				$('#builddetails').DataTable();
                  			} );
                  		</script>")

  # Link to risk report
  risk_rpt_html <- ""
  risk_rpt <- file.path(destination(repo), "update-risk.html")
  if(riskrpt) {
    buildRiskReport(repo, report_file = risk_rpt, theme = theme)
  }
  if(file.exists(risk_rpt)) {
  risk_rpt_html <- paste("<hr><p>Update Risk Report for all site packages:",
                         createURL(risk_rpt, label = "Risk Report"), "</p>")
  }

  # Construct final HTML
  final_html <- paste("<!doctype html>
                      <html> <head>", title, css_tag, js_tag, ds_script,
                      "<body style=\"padding: 20px;\"></head>",
                      summary_header, attmpthtml, "<br/>", build_header,
                      build_html, risk_rpt_html, dl_json_link, "</body></html>")
  write(final_html, reportfile)
  NULL
}
