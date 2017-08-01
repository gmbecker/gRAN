#' Create HTML splash pages for packages
#' @importFrom htmlTable htmlTable
#' @importFrom tools file_path_sans_ext
#' @param repo A gRAN repo object
#' @param suffix Append a suffix to the HTML splash page
#' @param theme CSS theme. bootstrap, foundation, semanticui or jqueryui
#' @return None
#' @export
pkgHTML <- function(repo, suffix = "-index.html", theme = "bootstrap") {
  logfun(repo)("NA", paste0("Creating HTML splash pages for ",
                            sum(repo_results(repo)$building), " packages"),
                            type = "full")

  bres <- subset(repo_results(repo), repo_results(repo)$building == TRUE
                                    & !is.na(repo_results(repo)$buildReason)
                                    & repo_results(repo)$status != "up-to-date")
  if(nrow(bres) == 0) {
      logfun(repo)("NA", "No packages to create HTML splash pages for",
                   type = "full")
      return(repo)
  }
  bres <- subset(bres, !(grepl("^GRAN", bres$name)))

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
      logfun(repo)(pkg_name, paste0("Found check directory at ",
                                check_dir), type = "full")
      if (file.exists(file.path(check_dir, pkg_name, "DESCRIPTION"))) {
        descr_df <- generateDescInfo(file.path(check_dir, pkg_name))
        descr_df <- descr_df[ , !(names(descr_df) %in% c("Authors@R","Collate"))]
        if("URL" %in% colnames(descr_df)) {
          descr_df$URL <- createURL(descr_df$URL)
        }
        if("BugReports" %in% colnames(descr_df)) {
          descr_df$BugReports <- createURL(descr_df$BugReports)
        }
        if("Suggests" %in% colnames(descr_df)) {
          descr_df$Suggests <- extPkgURL(descr_df$Suggests)
        }
        if("Imports" %in% colnames(descr_df)) {
          descr_df$Imports <- extPkgURL(descr_df$Imports)
        }
        desc_header <- "<br/><h4>Details</h4><hr>"
        desc_html <- htmlTable(t(descr_df),
                        css.cell = ("padding-left: 1.5em; padding-left: 1.5em"),
                        css.class = "table table-striped table-hover",
                        css.table = "margin-left:10px;margin-right:10px;",
                        align="l", rnames = names(descr_df))
        logfun(repo)(pkg_name, "Created DESCRIPTION info", type = "full")
      } else {
        desc_header <- ""
        desc_html <- ""
      }
      # Reverse dependencies
      revdeps <- reversals(pkg_name)
      if (!(is.data.frame(revdeps) && ncol(revdeps)==0)) {
        revdeps_header <- "<br/><h4>Reverse Dependencies</h4><hr>"
        revdeps_html <- htmlTable(t(revdeps),
                          css.cell = ("padding-left: 1.5em; padding-left: 1.5em"),
                          css.class = "table table-striped table-hover",
                          css.table = "margin-left:10px;margin-right:10px;",
                          align="l", rnames = names(revdeps))
        logfun(repo)(pkg_name, "Created revdep info", type = "full")
      } else {
        revdeps_header <- ""
        revdeps_html <- ""
        logfun(repo)(pkg_name, "No revdep info available", type = "full")
      }

      # Create package intro
      status <- bres$lastAttemptStatus[bres$name == pkg_name]
      description <- as.character(descr_df$Description)
      maintainer <- emailTag(as.character(descr_df$Maintainer))
      authors <- emailTag(as.character(descr_df$Author))
      title <- as.character(descr_df$Title)
      intro <- paste0("<h2>", pkg_name, ": ",title, "</h2><hr> ",
                      "<strong><p>", description, "</p></strong> ",
                      "<p>GRAN Release: ",
                      "<a href=\"../../src/contrib/buildreport.html\">",
                      "<span class=\"label label-primary\">",
                              repo_name(repo), "</span></a>", "</p> ",
                      "<p>Build status: ", buildBadge(status, pkg_name), "</p>",
                      "<tcplaceholder/>",
                      "<p>Authors: ", authors, "</p> ",
                      "<p>Maintainer: ", maintainer, "</p> ")
      logfun(repo)(pkg_name, "Created package intro", type = "full")

      # Installation instructions
      installn <- paste0("<br/><h4>Installation</h4><hr>",
      "<p>To install this package, start R and enter:</p>",
      "<pre><code><p>library(GRAN", repo_name(repo),")</p>",
      "<p>install_packages(\"", pkg_name, "\", type=\"source\")</p></code></pre>")
      logfun(repo)(pkg_name, "Created installation info", type = "full")

      # Copy reference manual, vignettes and NEWS into destination
      reference_man <- file.path(check_dir, paste0(pkg_name, '-manual.pdf'))
      if (file.exists(reference_man)) {
        file.copy(reference_man, docdir)
        manref_url <- createURL(file.path("..", "..", "PkgDocumentation",
                                pkg_name, paste0(pkg_name, '-manual.pdf')),
                                label = paste0(pkg_name, '-manual.pdf'))
      } else manref_url <- ""

      check_docs <- file.path(check_dir, pkg_name, 'doc')
      if (file.exists(check_docs)) {
        # Create link for PDF Vignettes files
        rnw_files <- list.files(check_docs, pattern = "\\.Rnw", full.names = TRUE)
        if (length(rnw_files) > 0) {
          vign_vec <- c()
          for (rnw_file in rnw_files) {
            pdf_file <- paste0(file_path_sans_ext(rnw_file), ".pdf")
            file.copy(pdf_file, docdir)
            vign_url <- createURL(file.path("..", "..", "PkgDocumentation",
                                  pkg_name, basename(pdf_file)),
                                  label = basename(pdf_file))
            vign_vec <- append(vign_vec, vign_url)
          }
          pdf_vign_header <- paste("<p>PDF Vignettes:", paste(vign_vec,
                                                      collapse = ", "), "</p>")
          logfun(repo)(pkg_name, "Created PDF Vignette info", type = "full")
        } else pdf_vign_header <- ""
        # Create link for HTML Vignettes files
        rmd_files <- list.files(check_docs, pattern = "\\.Rmd", full.names = TRUE)
        if (length(rmd_files) > 0) {
          vign_vec2 <- c()
          for (rmd_file in rmd_files) {
            html_file <- paste0(file_path_sans_ext(rmd_file), ".html")
            file.copy(html_file, docdir)
            vign_url2 <- createURL(file.path("..", "..", "PkgDocumentation",
                                   pkg_name, basename(html_file)),
                                   label = basename(html_file))
            vign_vec2 <- append(vign_vec2, vign_url2)
          }
          html_vign_header <- paste("<p>PDF Vignettes:", paste(vign_vec2,
                                                      collapse = ", "), "</p>")
          logfun(repo)(pkg_name, "Created HTML Vignette info", type = "full")
        } else html_vign_header <- ""
        # Create link for NEWS files
        news_files <- list.files(file.path(check_docs, '..'),
                                  pattern = "NEWS*", full.names = TRUE)
        if (length(news_files) > 0) {
          news_vec <- c()
          for (news_file in news_files) {
            file.copy(news_file, docdir)
            news_url <- createURL(file.path("..", "..", "PkgDocumentation",
                                   pkg_name, basename(news_file)),
                                   label = basename(news_file))
            news_vec <- append(news_vec, news_url)
          }
          news_header <- paste("<p>PDF Vignettes:", paste(news_vec,
                                                      collapse = ", "), "</p>")
          logfun(repo)(pkg_name, "Created NEWS info", type = "full")
        } else news_header <- ""
      } else {
        pdf_vign_header <- ""
        html_vign_header <- ""
        news_header <- ""
      }

      # Create HTML snippet for documentation, NEWS, vignettes
      doc_header <- "<br/><h4>Package Documentation</h4><hr>"
      doc_content <- paste("<p>Reference Manual:", manref_url, "</p>",
                            pdf_vign_header, html_vign_header, news_header)

      # Construct final HTML
      final_html <- paste("<html><head>", "<title>", pkg_name, "on", repo_name(repo),
                          "</title>", stylesheet, "</head>", intro, installn,
                          desc_header, desc_html, revdeps_header, revdeps_html,
                          doc_header, doc_content, "</html>")

      # Create the HTML spash page
      logfun(repo)(pkg_name, "Writing final pkg HTML info", type = "full")
      write(final_html, file = file.path(docdir, paste0(pkg_name, suffix)))
    }
  }

  logfun(repo)("NA", paste0("Completed HTML splash page creation for ",
               length(bres$name), " packages."), type = "full")
  NULL
}

#' Converts a DESCRIPTION file to a data.frame
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
    url <- createURL(cran_url, pkg_name)
  } else if (url.exists(bioc_soft_url)) {
    url <- createURL(bioc_soft_url, pkg_name)
  } else if (url.exists(bioc_exp_url)) {
    url <- createURL(bioc_exp_url, pkg_name)
  } else if (url.exists(bioc_ano_url)) {
    url <- createURL(bioc_ano_url, pkg_name)
  } else {
    url <- pkg_name
  }
  url
}

#' Wrapper for determinePkgURL. Deprecate due to performance issues.
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
#' @importFrom tools dependsOnPkgs
#' @param pkg_name The name of the package (string)
#' @param relation What type of reverse dependency?
#' @return String of reverse dependencies
relatedPkgs <- function(pkg_name, relation = "LinkingTo") {
  paste(dependsOnPkgs(pkg_name, dependencies = relation,
                             recursive = FALSE),
        collapse = ', ')
}

#' Converts delimited string to list
#' @importFrom stringi stri_replace_all_charclass
#' @param string Input string
#' @param separator The delimiter
#' @return Processed list
string2list <- function(string, separator = ",") {
  as.list(strsplit(stri_replace_all_charclass(string,
                    "\\p{WHITE_SPACE}", ""), separator)[[1]])
}

#' Creates a href tag
#' @param string URL string
#' @param label Label for href
#' @return href tag
createURL <- function(string, label = "") {
  if (label == "" || is.null(label)) {
    label <- gsub("(/+)", "\\1&#8203;", string)
  }
  paste0("<a href='", string, "'>", label, "</a>")
}

#' Create a mailto tag for email IDs
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
