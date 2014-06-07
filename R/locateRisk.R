##' identifyRisk
##'
##' Identify packages which could possibly be effected by updating the specified list of packages to their latest versions.
##' @param to_update vector of package names which may be updated, or a matrix output from \code{old.packages}. Defaults to all packages which are out of date
##' @param important_pkgs list of packages to check for risk of change cascades from updating the packages in \code{to_update}. Defaults to all installed packages
##' @param liblocs the library locations to look for installed packages
##' @param repo_urls The repositories to check for new versions of packages
##' @param gRAN  A GRANRepository object. Defaults to the repository associated
##' with the version of the GRAN package in use (or NULL if there isn't one).
##'
##' @return A list containing two named lists: splash_damage and in_danger. splash_damage lists the packages potentially affected by updating each package in \code{to_update}. in_danger lists the packages from \code{to_update} that affect each package in \code{important_pkgs} (packages which are unaffected are omitted).
##' @author Gabriel Becker
##' @importFrom tools dependsOnPkgs
##' @export

identifyRisk = function(to_update = old.packages(repos = repo_urls),
    important_pkgs = installed.packages(lib.loc = liblocs)[,"Package"],
    liblocs = .libPaths(),
    repo_urls = getOption("repos"), gRAN = GRANRepo$repo)
{
    if(!is.null(gRAN))
        repo_urls = c(repo_url(gRAN), repo_urls)
    if(is(to_update, "matrix"))
        to_update = to_update[,"Package"]
    
    splash_damage = sapply(to_update, dependsOnPkgs, simplify=FALSE)
    
    in_danger= vector("list", length(important_pkgs))
    names(in_danger) = important_pkgs
    for(i in important_pkgs)
        {
            in_danger[[i]] = names(splash_damage)[sapply(splash_damage, function(vec, i) i %in% vec, i = i)]
        }

    in_danger = in_danger[sapply(in_danger, function(x) length(x)  >0)]
    list(splash_damage = splash_damage, in_danger = in_danger)
}

##'buildRiskReport
##'
##' Generates an HTML report identifying which packages have updates available, and which of the specified important packages may be effected by installing those new versions.
##' @title Build risk-assessment for proposed package updates
##' @inheritParams identifyRisk
##' @param file file to write the resulting HTML report into.
##' @return the
##' @importFrom hwriter openPage
##' @importFrom hwriter closePage
##' @importFrom hwriter hwrite
##' @export
buildRiskReport = function(to_update = old.packages(repos = repo_urls),
    important_pkgs = installed.packages(lib.loc = liblocs)[,"Package"],
    liblocs = .libPaths(),
    repo_urls = getOption("repos"),
    file = "update-risk.html")
{
  if(class(to_update) == "matrix") {
      oldmat = to_update
      to_update = to_update[,"Package"]
  } else {
      oldmat = old.packages(lib.loc = liblocs, repos = repo_urls)
      oldmat = oldmat[oldmat[,"Packages"] %in% to_update]
  }

  oldmat = as.data.frame(oldmat[,c("Package", "Installed", "Built", "ReposVer", "Repository")])


  risks = identifyRisk(to_update= to_update, important_pkgs = important_pkgs)
  oldmat$ImpPkgsAffected = sapply(risks$splash_damage, function(vec, imp) sum(imp %in% vec), imp = important_pkgs)
  ##use hwriter for now, something better will come later
  css_file = system.file("js_css", "RiskReport.css", package = currentPackage())
  css = paste(readLines(css_file), collapse = "\n")
  pg =  openPage(filename = file, title  = paste("Update Risk Assessment", Sys.Date()), css = css)


  
  
  row_classes = ifelse(oldmat$ImpPkgsAffected == 0, "update_safe", "update_risky")
  hwrite("Packages with updates available", page = pg, br=TRUE)
  hwrite(as.matrix(oldmat), row.class = row_classes, page = pg, br=TRUE)

  mapply(function(nm, vec, pg) {
      hwrite(paste("Risks to", nm), page = pg, br=TRUE, heading=2)

      #hmakeTag("p", data = vec, class="risk_list", newline=TRUE)
     hwrite(vec, page = pg, table=FALSE)
  }, nm = names(risks$in_danger), vec = risks$in_danger, pg = list(pg))
#  hwrite(risks$in_danger, page = pg, br=TRUE)
  closePage(pg)
}
