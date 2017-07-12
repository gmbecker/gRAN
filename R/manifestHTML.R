##' manifestHTML
##'
##' Create a build report for a repository reflecting the latest build
##' @param repo a GRANRepository object.
##' @importFrom hwriter hwrite
##'
manifestHTML <- function(repo) {
    doc <- openPage("buildreport.html", dirname = destination(repo))
    lastattempt <- repo_results(repo)$lastAttemptStatus
    attmptab <- as.matrix(table(lastattempt))
    hwrite(attmptab, page = doc)
    hwrite("<br/>", page = doc)

    cnames <- c("name", "lastAttemptVersion", "lastAttemptStatus", "lastAttempt",
                "lastbuiltversion", "lastbuiltstatus", "lastbuilt", "maintainer")
    tmpman <- repo_results(repo)[, cnames]
    covr <- testCoverage(repo)
    tmpman$coverage <- covr$coverage[match(tmpman$name, covr$name)]
    checkrep <- file.path("..", "..", "CheckResults", paste0(tmpman$name, "_CHECK.log"))
    pkglog <- file.path("..", "..", "SinglePkgLogs", paste0(tmpman$name, ".log"))
    covg_report <- file.path("..", "..", "CovrReports", paste0(tmpman$name, "-covr-report.html"))

    tmpman[is.na(tmpman)] = "NA"
    tmpman$CheckResult <- paste0("<a href='", checkrep, "'>check log</a>")
    tmpman$SinglePkgLog <- paste0("<a href='", pkglog, "'>single package log</a>")
    tmpman$coverage <- paste0("<a href='", covg_report, "'>", tmpman$coverage, "</a>")
    tmpman$CheckResult[!file.exists(file.path(destination(repo), checkrep))] = ""
    tmpman$coverage[!file.exists(file.path(destination(repo), covg_report))] = ""
    hwrite(tmpman, page = doc, col.names = TRUE)
    closePage(doc, splash = FALSE)
}
