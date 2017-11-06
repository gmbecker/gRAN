#'buildRiskReport
#'
#' Generates an HTML report identifying which packages have updates available,
#' and which of the specified important packages may be effected by
#' installing those new versions.
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @title Build risk-assessment for proposed package updates
#' @inheritParams identifyRisk
#' @importFrom htmlTable htmlTable
#' @param file File to write the resulting HTML report into.
#' @param theme CSS theme. bootstrap, foundation, semanticui or jqueryui
#' @return none. Writes HTML report with risk assessment
#' @export
buildRiskReport = function(repo, to_update = old.packages(repos = repo_urls),
    important_pkgs = installed.packages(lib.loc = liblocs)[,"Package"],
    liblocs = .libPaths(),
    repo_urls = getOption("repos"),
    file = file.path(destination(repo), "update-risk.html"),
    theme = "bootstrap")
{
  if(class(to_update) == "matrix") {
    oldmat = to_update
    to_update = to_update[,"Package"]
  } else {
    oldmat = old.packages(lib.loc = liblocs, repos = repo_urls)
    oldmat = oldmat[oldmat[,"Package"] %in% to_update]
  }

  oldmat = as.data.frame(oldmat[,c("Package", "Installed", "Built", "ReposVer",
                                   "Repository")], stringsAsFactors = FALSE)
  if (!is.null(readPkgsNEWS(oldmat))) {
    oldmat = cbind(oldmat, readPkgsNEWS(oldmat))
  }

  risks = identifyRisk(repo, to_update= to_update, important_pkgs = important_pkgs)
  oldmat$ImpPkgsAffected = sapply(risks$splash_damage, function(vec, imp)
                                  sum(imp %in% vec), imp = important_pkgs)
  rownames(oldmat) <- NULL

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
                  				$('#updatedetails').DataTable();
                          $('#riskdetails').DataTable();
                  			} );
                  		</script>")
  title <- paste("<title>Update Risk Assessment:", if(!is.null(repo)) repo_name(repo),
                 Sys.Date(), "</title>")

  if (ncol(oldmat) == 10) {
    update_header <- "<h2>Packages with updates available:</h2>"
    table_headers <- c("Package", "Installed Version", "Build with R",
        "Repository Version", "Repository", "Bugfixes", "User-visible changes",
        "Deprecations", "Total listed changes", "Number of pkgs affected")
    update_html <- htmlTable(as.matrix(oldmat), header = table_headers,
                          css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
                          css.class = "table table-striped table-hover",
                          css.table = "margin-left:10px;margin-right:10px;",
                          align="l", label = "updatedetails")
    update_html <- gsub("border-bottom: 2px solid grey;", "", update_html)
  } else {
    update_header <- ""
    update_html <- ""
  }


  danger_df <- suppressWarnings(as.data.frame(do.call(rbind, risks$in_danger)))
  cols <- colnames(danger_df)
  danger_df$RisksTo<-rownames(danger_df)
  danger_df$Package <- apply(danger_df[ , cols, drop=F], 1, paste, collapse = ",")
  danger_df$Package <- sapply(strsplit(danger_df$Package ,","),
                                    function(x) paste(unique(x), collapse=", "))
  danger_df <- danger_df[ ,!(names(danger_df) %in% cols)]
  rownames(danger_df) <- NULL

  danger_header <- "<hr><h2>Packages at risk:</h2>"
  danger_html <- suppressWarnings(htmlTable(danger_df,
              header = c("Package at risk:", "When these packages are updated:"),
              css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
              css.class = "table table-striped table-hover",
              css.table = "margin-left:10px;margin-right:10px;",
              align="l", label = "riskdetails"))
  danger_html <- gsub("border-bottom: 2px solid grey;", "", danger_html)

  final_html <- paste("<html><head>",title, css_tag, js_tag, ds_script, "</head>",
              update_header, update_html, danger_header, danger_html, "</html>")
  write(final_html, file)
}

#' identifyRisk
#'
#' Identify packages which could possibly be effected by updating the specified
#' list of packages to their latest versions.
#' @param to_update vector of package names which may be updated, or a matrix
#' output from \code{old.packages}. Defaults to all packages which are out of date
#' @param important_pkgs list of packages to check for risk of change cascades
#' from updating the packages in \code{to_update}. Defaults to all installed packages
#' @param liblocs the library locations to look for installed packages
#' @param repo_urls The repositories to check for new versions of packages
#' @param repo The name of a GRAN repository to use. Assumes that a
#' a package named GRAN<repo> is available to load.
#'
#' @return A list containing two named lists: splash_damage and in_danger.
#' splash_damage lists the packages potentially affected by updating each
#' package in \code{to_update}. in_danger lists the packages from
#' \code{to_update} that affect each package in \code{important_pkgs}
#' (packages which are unaffected are omitted).
#' @author Gabriel Becker
#' @importFrom tools dependsOnPkgs
#' @importFrom utils installed.packages old.packages
#' @export
identifyRisk = function(repo, to_update = old.packages(repos = repo_urls),
    liblocs = .libPaths(),
    important_pkgs = installed.packages(lib.loc = liblocs)[,"Package"],
    repo_urls = getOption("repos"))
{
    if(!is.null(repo))
        repo_urls = c(repo_url(repo), repo_urls)
    if(is(to_update, "matrix"))
        to_update = to_update[,"Package"]

    splash_damage = sapply(to_update, dependsOnPkgs, simplify=FALSE)

    in_danger= vector("list", length(important_pkgs))
    names(in_danger) = important_pkgs
    for(i in important_pkgs)
        {
            in_danger[[i]] = names(splash_damage)[sapply(splash_damage,
                                            function(vec, i) i %in% vec, i = i)]
        }

    in_danger = tryCatch(
                      {in_danger[sapply(in_danger, function(x) length(x) > 0)]},
                      error = function(e) {list()})

    list(splash_damage = splash_damage, in_danger = in_danger)
}

#'readPkgsNEWS
#'
#' Attempts to generate a per-package summary of risky-to-ignore changes for
#' updatable packages.
#'
#' @title Read and summarize the NEWS files for packages at risk (updatable)
#' @param df A data.frame or matrix of out-of-date packages currently
#' installed, with columns Package, Installed (installed version), and
#' Repository (contriburl of repo with newer version). Other columns are i
#' gnored.
#' @param oldlib The currently library to compare against latest avaialble
#' versions
#' @param tmplib A temporary library directory to install new versions of the
#' packages into so that their NEWS files can be accessed.
#' @param repos A character vector of the repositories to search for newer
#' versions of packages installed in \code{oldlib}
#' @param newlib An already populated 'new' library to compare against
#' \code{oldlib} instead of retrieving new package versions from \code{repos}
#' @return A data.frame with 3 counts for each updatable package: bugfixes,
#' u_visible_changes (user visible changes) and deprec (deprecation and defunct
#' entries). All counts are NA if the package does not have parsable NEWS.
#' @importFrom utils news
#' @export
readPkgsNEWS = function(df, oldlib = .libPaths(), tmplib = file.path(tempdir(),
                      "libloc"), repos = unique(df$Repository), newlib = NULL) {
    if(is.matrix(df))
        df = as.data.frame(df, stringsAsFactors=FALSE)
    if(is.null(newlib)) {
        tmp=TRUE
        newlib = tmplib
        if(!file.exists(tmplib))
            dir.create(tmplib, recursive=TRUE)
    } else {
        tmp = FALSE
        tmplib = newlib
    }
    prevlp = .libPaths()
    .libPaths(newlib)
    on.exit(.libPaths(prevlp))
    if(tmp) {
        install.packages(df$Package, lib = tmplib, contriburl = repos)
    }
   
    if (!(is.null(df$Package) || is.null(df$Installed) )) { ##|| is.null(df$Repository))) {
      newsres = t(mapply(innerReadNEWS, pkg = df$Package, instver = df$Installed,
                       repo = df$Repository, newlib = tmplib, oldlib = list(oldlib)))
      if(tmp)
          unlink(tmplib)
      return(as.data.frame(newsres))
    } else return(NULL)
}

globalVariables("Version")
innerReadNEWS = function(pkg, instver, repo, newlib, oldlib) {
    .libPaths(c(newlib))
    on.exit(.libPaths(oldlib))
    newsdf = tryCatch(as.data.frame(news(Version > instver, package=pkg,
                                        lib.loc = newlib)), error=function(x) x)
    if(is(newsdf, "error") || !nrow(newsdf)) {
        return(data.frame(bugfixes = NA, u_visible_changes = NA, deprecs = NA,
                          total_listed_changes = NA))
    }
    bugs = sum(newsdf$Category == "BUG FIXES")
    u_visible_changes = length(grep("USER.VISIBLE.CHANGES", newsdf$Category,
                                    ignore.case=TRUE))
    deprecs = length(grep("(DEPRECATE|DEFUNCT)", newsdf$Category,
                          ignore.case=TRUE))
    data.frame(bugfixes = bugs, u_visible_changes = u_visible_changes,
               deprecs = deprecs, total_listed_changes = nrow(newsdf))
}
