#' Create HTML splash pages for packages
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom htmlTable htmlTable
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom jsonlite toJSON
#' @importFrom markdown markdownToHTML
#' @param repo A gRAN repo object
#' @param splashname Filename for the HTML splash page
#' @param theme CSS theme. bootstrap, foundation, semanticui or jqueryui
#' @return None
#' @export
pkgHTML <- function(repo,
                    splashname = "index.html",
                    theme = "bootstrap") {
  logfun(repo)("NA", paste("Creating HTML splash pages for",
                            sum(repo_results(repo)$building), "packages"))

  # Build splash pages only for building packages
  bres <- subset(repo_results(repo), repo_results(repo)$building == TRUE
                                  & !is.na(repo_results(repo)$buildReason)
                                  & repo_results(repo)$status != "up-to-date"
                                  & !is.na(repo_results(repo)$lastbuiltversion))
  if(nrow(bres) == 0) {
      logfun(repo)("NA", "No packages to create HTML splash pages for")
      return(repo)
  }
  bres <- subset(bres, !(grepl("^GRAN", bres$name)))

  # Get manifest_df for SCM info
  scm_df <- manifest_df(repo)

  # Copy the stylesheet
  stylesheet <- paste0("<link rel=\"stylesheet\" type=\"text/css\"
        href=\"../../src/contrib/assets/css/", theme, ".min.css\">")

  # Begin creation of package HTML page
  for (pkg_name in bres$name) {
    # Create and initialize necessary directories
    docdir <- file.path(pkg_doc_dir(repo), pkg_name)
    dir.create(docdir, showWarnings = FALSE, recursive = TRUE)
    check_dir <- file.path(staging(repo), paste0(pkg_name, '.Rcheck'))

    # There may or may not be an Rcheck dir
    if (file.exists(check_dir)) {

      # Create description info table
      logfun(repo)("NA", paste("Found check directory at", check_dir,
                                "for", pkg_name))
      if (file.exists(file.path(check_dir, pkg_name, "DESCRIPTION"))) {
        descr_df <- generateDescInfo(file.path(check_dir, pkg_name))

        # Get reverse dependencies
        revdeps <- reversals(pkg_name)

        # Create JSON of the DESCRIPTION file
        createJSON(repo, pkg_name, descr_df, scm_df, docdir, revdeps)

        # Exclude these fields from the splash page
        descr_df <- descr_df[ , !(names(descr_df) %in%
                                  c("Authors@R", "Collate"))]
        if("URL" %in% colnames(descr_df)) {
          descr_df$URL <- createHyperlink(descr_df$URL)
        }
        if("BugReports" %in% colnames(descr_df)) {
          descr_df$BugReports <- createHyperlink(descr_df$BugReports)
        }
        if("Suggests" %in% colnames(descr_df)) {
          descr_df$Suggests <- extPkgURL(descr_df$Suggests)
        }
        if("Imports" %in% colnames(descr_df)) {
          descr_df$Imports <- extPkgURL(descr_df$Imports)
        }
        desc_header <- "<br/><h4>Details</h4><hr>"
        desc_html <- htmlTable(t(descr_df),
                        css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
                        css.class = "table table-striped table-hover",
                        css.table = "margin-left:10px;margin-right:10px;",
                        align="l",
                        rnames = names(descr_df),
                        escape.html = FALSE)
        logfun(repo)("NA", paste("Created DESCRIPTION info for", pkg_name))
      } else {
        desc_header <- ""
        desc_html <- ""
      }

      # Create HTML for reverse deps
      if (!(is.data.frame(revdeps) && ncol(revdeps)==0)) {
        revdeps_header <- "<br/><h4>Reverse Dependencies</h4><hr>"
        revdeps_html <- htmlTable(t(revdeps),
                        css.cell = ("padding-left: 0.5em; padding-right: 0.5em"),
                        css.class = "table table-striped table-hover",
                        css.table = "margin-left:10px;margin-right:10px;",
                        align="l",
                        rnames = names(revdeps),
                        escape.html = FALSE)
        logfun(repo)("NA", paste("Created revdep info for", pkg_name))
      } else {
        revdeps_header <- ""
        revdeps_html <- ""
        logfun(repo)("NA", paste("No revdep info available for", pkg_name))
      }

      # Create sticker for the package
      createSticker(pkg_name, destination = docdir)

      # Create package intro
      status <- bres$lastAttemptStatus[bres$name == pkg_name]
      description <- as.character(descr_df$Description)
      maintainer <- emailTag(as.character(descr_df$Maintainer))
      authors <- emailTag(as.character(descr_df$Author))
      title <- as.character(descr_df$Title)
      intro <- paste0("<h2>", pkg_name, ": ",title, "</h2><hr> ",
              "<img src=\"", pkg_name, ".png\" height=\"160\" align=\"right\">",
                      "<strong><p>", description, "</p></strong> ",
                      "<p>GRAN Release: ",
                      "<a href=\"../../src/contrib/buildreport.html\">",
                      "<span class=\"label label-primary\">GRAN",
                              repo_name(repo), "</span></a>", "</p> ",
                      "<p>Build status: ", buildBadge(status, pkg_name), "</p>",
                      "<tcplaceholder/>",
                      "<p>Authors: ", authors, "</p> ",
                      "<p>Maintainer: ", maintainer, "</p> ")
      logfun(repo)("NA", paste("Created package intro for", pkg_name))

      # Installation instructions
      installn <- paste0("<br/><h4>Installation</h4><hr>",
                         "<p>To install this package, start R and enter:</p>",
                         #"<pre><code>install.packages(\"", pkg_name,
                         #"\", repos = \"", param(repo)@dest_url, "/",
                         #repo_name(repo), "\")</code></pre>")
                         "<pre><code><p>source(\"", param(repo)@dest_url,
                         "/getGRAN-", repo_name(repo),
                         ".R\")</p>", "<p>library(GRAN", repo_name(repo),")</p>",
                         "<p>install_packages(\"", pkg_name, "\"",
                         ", type=\"source\")</p></code></pre>")

      logfun(repo)("NA", paste("Created installation info for", pkg_name))

      # Copy reference manual, vignettes and NEWS into pkg docs dir
      reference_man <- file.path(check_dir, paste0(pkg_name, '-manual.pdf'))
      if (file.exists(reference_man)) {
        file.copy(reference_man, docdir)
        manref_url <- paste("<p>Reference Manual:",
                            createHyperlink(paste0(pkg_name, '-manual.pdf'),
                                      label = paste0(pkg_name, '-manual.pdf')),
                            "</p>")
        doc_header <- "<br/><h4>Package Documentation</h4><hr>"
      } else {
        manref_url <- ""
        doc_header <- ""
      }

      check_docs <- file.path(check_dir, pkg_name, 'doc')
      if (file.exists(check_docs)) {

        # Create link for PDF Vignettes files
        rnw_files <- list.files(check_docs, pattern = "\\.Rnw", full.names = TRUE)
        if (length(rnw_files) > 0) {
          vign_vec <- c()
          for (rnw_file in rnw_files) {
            pdf_file <- paste0(file_path_sans_ext(rnw_file), ".pdf")
            file.copy(pdf_file, docdir)
            vign_url <- createHyperlink(basename(pdf_file),
                                        label = basename(pdf_file))
            vign_vec <- append(vign_vec, vign_url)
          }
          pdf_vign_header <- paste("<p>PDF Vignettes:", paste(vign_vec,
                                                      collapse = ", "), "</p>")
          logfun(repo)("NA", paste("Created PDF Vignette info for", pkg_name))
        } else pdf_vign_header <- ""

        # Create link for HTML Vignettes files
        rmd_files <- list.files(check_docs, pattern = "\\.Rmd", full.names = TRUE)
        if (length(rmd_files) > 0) {
          vign_vec2 <- c()
          for (rmd_file in rmd_files) {
            html_file <- paste0(file_path_sans_ext(rmd_file), ".html")
            file.copy(html_file, docdir)
            vign_url2 <- createHyperlink(basename(html_file),
                                         label = basename(html_file))
            vign_vec2 <- append(vign_vec2, vign_url2)
          }
          html_vign_header <- paste("<p>HTML Vignettes:", paste(vign_vec2,
                                                      collapse = ", "), "</p>")
          logfun(repo)("NA", paste("Created HTML Vignette info", pkg_name))
        } else html_vign_header <- ""

        # Create link for NEWS files
        news_files <- list.files(file.path(check_docs, '..'),
                                  pattern = "NEWS*", full.names = TRUE)
        if (length(news_files) > 0) {
          news_vec <- c()
          for (news_file in news_files) {
            # Convert NEWS.md into HTML
            if(file_ext(news_file) == "md") {
              markdownToHTML(file = news_file,
                             output = file.path(docdir, basename(news_file)))
            } else {
              file.copy(news_file, docdir)
            }
            news_url <- createHyperlink(basename(news_file),
                                        label = basename(news_file))
            news_vec <- append(news_vec, news_url)
          }
          news_header <- paste("<p>NEWS files:", paste(news_vec,
                                                      collapse = ", "), "</p>")
          logfun(repo)("NA", paste("Created NEWS info", pkg_name))
        } else news_header <- ""

        # Create link for README files
        readme_files <- list.files(file.path(check_docs, '..', '..',
                                             '00_pkg_src', pkg_name),
                                  pattern = "README*", full.names = TRUE)
        if (length(readme_files) > 0) {
          readme_vec <- c()
          for (readme_file in readme_files) {
            # Convert README.md into HTML
            if(file_ext(readme_file) == "md") {
              markdownToHTML(file = readme_file,
                             output = file.path(docdir, basename(readme_file)))
            } else {
              file.copy(readme_file, docdir)
            }
            readme_url <- createHyperlink(basename(readme_file),
                                          label = basename(readme_file))
            readme_vec <- append(readme_vec, readme_url)
          }
          readme_header <- paste("<p>README files:",
                                 paste(readme_vec, collapse = ", "), "</p>")
          logfun(repo)("NA", paste("Created README info", pkg_name))
        } else readme_header <- ""

        # Create link for package source
        src_build_url <- ""
        src_build <- file.path(destination(repo),
                               paste0(pkg_name, "_", descr_df$Version, ".tar.gz"))
        if (file.exists(src_build)) {
          src_build_url <- paste("<p>Package source:",
                              createHyperlink(src_build,
                                              label = basename(src_build)),
                              "</p>")
        } else src_build_url <- ""

        # Create link for package archive
        pkg_archive_dir <- file.path(archivedir(repo), pkg_name)
        if (file.exists(pkg_archive_dir)) {
          pkg_archive_url <- paste("<p>Old sources:",
                              createHyperlink(pkg_archive_dir,
                                              label = "Archive"),
                              "</p>")
        } else pkg_archive_url <- ""
      } else {
        pdf_vign_header <- ""
        html_vign_header <- ""
        news_header <- ""
        readme_header <- ""
        src_build_url <- ""
        pkg_archive_url <- ""
      }

      # Create HTML snippet for documentation, NEWS, vignettes

      doc_content <- paste(readme_header, manref_url, pdf_vign_header,
                           html_vign_header, news_header, src_build_url,
                           pkg_archive_url)

      # Construct final HTML
      final_html <- paste0("<html><head>", "<title>", pkg_name, " on GRAN",
                          repo_name(repo), "</title>", stylesheet,
                          "<body style=\"padding: 20px;\"> </head>", intro,
                          installn, desc_header, desc_html, revdeps_header,
                          revdeps_html, doc_header, doc_content, "</body></html>")

      # Save the previous splash page
      # If we're doing this, we'll need to do this for the remaining docs also.
      # CRAN doesn't do this.
      # Besides, if old sources are available from the archive then
      # the required docs can be obtained from that.
      # One could store the docs & sources into a CMS via separate mechanisms.
      #if (file.exists(file.path(docdir, splashname))) {
      #  file.rename(from = file.path(docdir, splashname),
      #              to = file.path(docdir,
      #                             paste0(pkg_name,
      #                                    "_",
      #                                    descr_df$Version,
      #                                    ".html")))
      #}

      # Create the HTML spash page
      logfun(repo)("NA", paste("Writing final pkg HTML info", pkg_name))
      write(final_html, file = file.path(docdir, splashname))
    }
  }

  logfun(repo)("NA", paste0("Completed HTML splash page creation for ",
               length(bres$name), " packages."))
  NULL
}

#' Converts a DESCRIPTION file to a data.frame
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param pkg_path The path preceding the location of the DESCRIPTION file
#' @param encoding If there is an Encoding field, to what encoding should
#'     re-encoding be attempted? The other values are as used by iconv, so the
#'     default "" indicates the encoding of the current locale
#' @return If a DESCRIPTION file for the given package is found and can
#'     successfully be read, this function returns a data.frame ontaining
#'     of the fields as headers and the tags as rows
generateDescInfo <- function(pkg_path, encoding = "") {
  retval <- list()
  if (file.exists(file <- file.path(pkg_path, "DESCRIPTION"))) {
    dcf <- read.dcf(file = file)
    desc <- as.list(dcf[1, ])
  } else {
    file <- ""
    return(paste("No DESCRIPTION file found for", pkg_path))
  }
  if (nzchar(file)) {
    enc <- desc[["Encoding"]]
    if (!is.null(enc) && !is.na(encoding)) {
      if (missing(encoding) && Sys.getlocale("LC_CTYPE") == "C")
        encoding <- "ASCII//TRANSLIT"
      newdesc <- try(lapply(desc, iconv, from = enc, to = encoding))
      if (!inherits(newdesc, "try-error"))
        desc <- newdesc
      else
        warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible",
        call. = FALSE)
    }
    retval[names(desc)] <- desc
  }
  attr(retval, "file") <- file
  return(as.data.frame(t(as.matrix(unlist(retval))), header = TRUE))
}

#' Generate reverse dependency info as data.frame
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param pkg_name The name of the package (string)
#' @return data.frame containing package reverse dependency info
reversals <- function(pkg_name) {
  ReverseImports <- extPkgURL(relatedPkgs(pkg_name, "Imports"))
  ReverseDependencies <- extPkgURL(relatedPkgs(pkg_name, "Depends"))
  ReverseLinkingTo <- extPkgURL(relatedPkgs(pkg_name, "LinkingTo"))
  ReverseSuggests <- extPkgURL(relatedPkgs(pkg_name, "Suggests"))
  ReverseEnhances <- extPkgURL(relatedPkgs(pkg_name, "Enhances"))
  df <- data.frame(ReverseImports, ReverseDependencies, ReverseLinkingTo,
                  ReverseSuggests, ReverseEnhances)
  df <- Filter(function(x) !(all(x=="")), df)
  df
}

#' Generate package's external URL after validation info as data.frame
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom RCurl url.exists
#' @param pkg_name The name of the package (string)
#' @return Package external URL
determinePkgURL <- function(pkg_name) {
  cran_url <- paste0("https://cran.r-project.org/package=", pkg_name)
  bioc_home <- "https://www.bioconductor.org/packages/release"
  bioc_soft_url <- paste0(bioc_home, "/bioc/html/", pkg_name, ".html")
  bioc_exp_url <- paste0(bioc_home, "/data/experiment/html/", pkg_name, ".html")
  bioc_ano_url <- paste0(bioc_home, "/data/annotation/html/", pkg_name, ".html")
  if (url.exists(cran_url)) {
    url <- createHyperlink(cran_url, pkg_name)
  } else if (url.exists(bioc_soft_url)) {
    url <- createHyperlink(bioc_soft_url, pkg_name)
  } else if (url.exists(bioc_exp_url)) {
    url <- createHyperlink(bioc_exp_url, pkg_name)
  } else if (url.exists(bioc_ano_url)) {
    url <- createHyperlink(bioc_ano_url, pkg_name)
  } else {
    url <- pkg_name
  }
  url
}

#' Wrapper for determinePkgURL. Deprecate due to performance issues.
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param desc_field The DESCRIPTION field which has mutiple package names
#' @param as_string Return as string
#' @return Package external URL
extPkgURL_old <- function(desc_field, as_string = TRUE) {
  items <- string2list(desc_field)
  itemList <- list()
  for (i in items) {
    i <- gsub( " *\\(.*?\\) *", "", i)
    itemList[[length(itemList) + 1]] <- determinePkgURL(i)
  }
  if (as_string == FALSE && length(items) == 1) {
    itemList
  } else {
    paste(itemList, collapse = ', ')
  }
}

#' Wrapper for determinePkgURL
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param desc_field The DESCRIPTION field which has mutiple package names
#' @param as_string Return as string
#' @return Package external URL
extPkgURL <- function(desc_field, as_string = TRUE) {
  items <- string2list(desc_field)
  itemList <- list()
  for (i in items) {
    i <- gsub( " *\\(.*?\\) *", "", i)
    itemList[[length(itemList) + 1]] <- i
  }
  if (as_string == FALSE && length(items) == 1) {
    itemList
  } else {
    paste(itemList, collapse = ', ')
  }
}

#' Wrapper for dependsOnPkgs
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom tools dependsOnPkgs
#' @param pkg_name The name of the package (string)
#' @param relation What type of reverse dependency?
#' @return String of reverse dependencies
relatedPkgs <- function(pkg_name, relation = "LinkingTo") {
  paste(dependsOnPkgs(pkg_name, dependencies = relation, recursive = FALSE),
        collapse = ', ')
}

#' Converts delimited string to list
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom stringi stri_replace_all_charclass
#' @param string Input string
#' @param separator The delimiter
#' @return Processed list
string2list <- function(string, separator = ",") {
  as.list(strsplit(stri_replace_all_charclass(string, "\\p{WHITE_SPACE}", ""),
          separator)[[1]])
}

#' Creates a href tag
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param string URL string
#' @param label Label for href
#' @return href tag
createHyperlink <- function(string, label = "") {
  if (label == "" || is.null(label)) {
    label <- gsub("(/+)", "\\1&#8203;", string)
  }
  paste0("<a href='", string, "'>", label, "</a>")
}

#' Create a mailto tag for email IDs
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param item A string with the email ID
#' @return href tag
emailTag <- function(item) {
  item <- gsub("[\r\n]", " ", item)
  item <- gsub("[<]", "<a href=\"mailto:", item)
  item <- gsub("[>]", "\">", item)
  emailIDs <- unlist(regmatches(item, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", item)))
  for (i in emailIDs) {
    item <- gsub(paste0("", i ,"\">"), paste0(i, "\">", i, "</a>"), item)
  }
  item
}

#' Create Badges for build status
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param status The build status for the package
#' @param pkg_name Name of the package
#' @return Badge href tag
buildBadge <- function(status, pkg_name) {
  # Create badges for build status
  checkrep <- file.path("..", "..", "CheckResults",
                        paste0(pkg_name, "_CHECK.log"))
  pkglog <- file.path("..", "..", "SinglePkgLogs",
                      paste0(pkg_name, ".log"))
  install_results <- file.path("..", "..", "InstallResults",
                               paste0(pkg_name, ".out"))
  if (is.na(status) || status == "NA") {
    label <- "label-default"
    log <- ""
  } else if (status == "ok") {
    label <- "label-primary"
    log <- pkglog
  } else if (status == "build failed" || status == "source checkout failed") {
    label <- "label-danger"
    log <- pkglog
  } else if (status == "check fail" || status == "Unable to check - missing tarball") {
    label <- "label-danger"
    log <- checkrep
  } else if (status == "check note(s)") {
    label <- "label-info"
    log <- checkrep
  } else if (status == "check warning(s)") {
    label <- "label-warning"
    log <- checkrep
  } else if (status == "install failed") {
    label <- "label-danger"
    log <- install_results
  } else if (status == "up-to-date") {
    label <- "label-success"
    log <- install_results
  } else if (status == "ok - not tested") {
    label <- "label-primary"
    log <- pkglog
  } else if (status == "GRAN FAILURE" || status == "Dependency build failure") {
    label <- "label-danger"
    log <- pkglog
  } else {
    label <- "label-default"
    log <- pkglog
  }
  paste("<a href=\"", log, "\"><span class=\"label",
        label, "\">", status, "</span></a>")
}

