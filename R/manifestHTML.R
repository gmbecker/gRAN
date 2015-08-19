##' manifestHTML
##'
##' Create a build report for a repository reflecting the latest build
##' @param repo a GRANRepository object.
##' @importFrom XML newXMLNode
##' @importFrom XML addChildren
##' @importFrom XML htmlParse
##' @importFrom XML saveXML
##' @importFrom hwriter hwrite
##' 
manifestHTML = function(repo)
    {
        doc = newXMLNode("html")
        body = newXMLNode("body", parent = doc)
        summary = newXMLNode("div", "Build summary:", parent = body)
        suppressWarnings(addChildren(summary, htmlParse(hwrite(as.matrix(table(repo_results(repo)$lastAttemptStatus))))))
        tmpman = repo_results(repo)[,c("name", "lastAttemptVersion",
            "lastAttemptStatus", "lastAttempt",  "lastbuiltversion",
            "lastbuiltstatus", "lastbuilt", "maintainer")]
        checkrep = file.path("..", "..", "CheckResults", paste0(tmpman$name,
            "_CHECK.log"))

        tmpman[is.na(tmpman)] = "NA"
        tmpman$CheckResult = paste0("<a href='", checkrep, "'>check log</a>")
        tmpman$CheckResult[!file.exists(file.path(destination(repo), checkrep))] = ""
        suppressWarnings(addChildren(body, htmlParse(hwrite(tmpman))))
       # saveXML(doc, file = file.path(repobase(repo), "buildreport.html"), prefix = "<!DOCTYPE html>")
        saveXML(doc, file = file.path(destination(repo), "buildreport.html"), prefix = "<!DOCTYPE html>")
    }
