##' manifestHTML
##'
##' Create a build report for a repository reflecting the latest build
##' @param repo a GRANRepository object.
##' @importFrom hwriter hwrite
##' 
manifestHTML = function(repo)
    {
        doc = openPage("buildreport.html", dirname = destination(repo))
        lastattempt = repo_results(repo)$lastAttemptStatus
        attmptab =  as.matrix(table(lastattempt))
        hwrite(attmptab, page = doc)
        hwrite("<br/>", page = doc)
        
        tmpman = repo_results(repo)[,c("name", "lastAttemptVersion",
            "lastAttemptStatus", "lastAttempt",  "lastbuiltversion",
            "lastbuiltstatus", "lastbuilt", "maintainer")]
        checkrep = file.path("..", "..", "CheckResults", paste0(tmpman$name,
            "_CHECK.log"))
        pkglog = file.path("..", "..", "SinglePkgLogs", paste0(tmpman$name,
            ".log"))

        tmpman[is.na(tmpman)] = "NA"
        tmpman$CheckResult = paste0("<a href='", checkrep, "'>check log</a>")
        tmpman$PackageLog =  paste0("<a href='", pkglog, "'>single package log</a>")
        tmpman$CheckResult[!file.exists(file.path(destination(repo), checkrep))] = ""
        tmpmanvec = as.character(as.matrix(tmpman))
        hwrite(tmpmanvec, dim = dim(tmpman), page = doc)
        closePage(doc, splash=FALSE)
    }
