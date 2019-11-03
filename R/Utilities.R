system.file2 <- function(..., package = "GRANBase") {
    ret = tryCatch(system.file(..., package = package), error = function(e) e)

    if(!is.null(ret) && !is(ret, "error") && nchar(ret))
        return(ret)
    instp = installed.packages()
    if(!package %in% instp[,"Package"])
        return("")
    ind = which(instp[,"Package"] == package)[1]
    path = file.path(instp[ind,"LibPath"], package, ...)
    if(file.exists(path))
        return(path)
    else
        return("")
}

getPkgNames <- function(path)
{
    path = normalizePath2(path)
    if(length(path) > 1)
        return(sapply(path, getPkgNames))
    if(file.info(path)$isdir && file.exists(file.path(path, "DESCRIPTION")))
        read.dcf(file.path(path, "DESCRIPTION"))[1,"Package"]
    else if (grepl(".tar", path, fixed=TRUE))
        gsub(basename(path), "([^_]*)_.*", "\\1")
}

getCheckoutLocs <- function(codir, manifest = manifest_df(repo),
    branch = manifest$branch, repo)
{
    mapply(function(basepath, subdir, scm_type, branch, name)
      tryCatch(getPkgDir(basepath = basepath, subdir = subdir,
                         scm_type = scm_type, branch = branch,
                         name = name), 
               error = function(e) {
                 warning("Failed to getPkgDir for ",
                         "pkg ", name, ", ",
                         "basepath ", basepath, ", ",
                         "branch ", branch, ", ",
                         "subdir ", subdir, ", ",
                         "scm_type ", scm_type, ", ",
                         "with error: ", e)
                 NA
                 }),
      basepath = codir, 
      subdir = manifest$subdir,
      scm_type = manifest$type, 
      branch = branch,
      name = manifest$name)
}

#' @importFrom desc desc_get_maintainer
getMaintainers <- function(codir, manifest = manifest_df(repo),
    branch = manifest$branch, repo) {
    sapply(getCheckoutLocs(codir, manifest = manifest), function(x) {
        if(!file.exists(file.path(x,"DESCRIPTION")))
            NA
        else {
            ## some github packages don't know how to construct
            ## DESCRIPTION files ... *mumble*
            tryCatch(desc_get_maintainer(file.path(x, "DESCRIPTION")),
                     error = function(x) NA)
        }
    })
}

getCOedVersions <- function(codir, manifest = manifest_df(repo),
    branch = manifest$branch, repo) {
    locs = getCheckoutLocs(codir, manifest = manifest,
        branch = branch, repo = repo)

    vers = lapply(unname(locs), function(x) {
                      dsc = tryCatch(readLines(file.path(x, "DESCRIPTION"),
                                            warn = FALSE), error= function(e) e)
                      if(is(dsc, "error")) {
                          v = NA_character_
                          names(v) = basename(x)
                          return(v)
                      }

                      vline = grep("^[V|v]ersion:.*", dsc, value = TRUE)
                      v = gsub("^[V|v]ersion: (.*)$", "\\1", vline)
                      pline = grep("^[P|p]ackage:.*", dsc, value = TRUE)
                      names(v) = gsub(".*: (.*)", "\\1", pline)
                      v
                  })
    unlist(vers)
}

isOkStatus <- function(status= repo_results(repo)$status,
    repo)
{
    #status can be NA when the package isn't being built at all
    !is.na(status) & (status == "ok" | status == "ok - not tested" |
                      (check_warn_ok(repo) & status == "check warning(s)") |
                      (check_note_ok(repo) & status == "check note(s)"))
}

install.packages2 <- function(pkgs, repos, lib,  ..., param = SwitchrParam(),
    outdir = tempdir())
{

    if(!file.exists(outdir))
        dir.create(normalizePath(outdir), recursive=TRUE)
    wd = getwd()
    on.exit(setwd(wd))
    setwd(outdir)
    ## the keep_outputs=dir logic doesn't work, the files just
    ##end up in both locations!
    ##install.packages(pkgs, ..., keep_outputs=outdir)
    avail = available.packages(contrib.url(repos, type = "source"))
    install.packages(pkgs = pkgs, repos = repos,
                     lib = lib, ..., keep_outputs=TRUE)
    ret = sapply(pkgs, function(p)
    {
        if(! p %in% avail[,"Package"])
            return("unavailable")
        fil = file.path(outdir, paste0(p, ".out"))
        if(!file.exists(fil))
            return("output missing")
        tmp = readLines(fil)
        ## for some reason this isn't always the last line, e.g. from histry 0.2.1

        ## NOTE: Removed R CMD INSTALL output for histry 0.2.1 that was here. 
        ##       Asterisks in the output were causing problems for roxygen.

        ## outcome = tmp[length(tmp)]
        donemsg = paste0("* DONE (", p, ")")
        if(any(grepl(donemsg, tmp, fixed = TRUE)))
            "ok"
        else
            fil
    })
    ret
}

getBuilding <- function(repo, results= repo_results(repo))
{
    results$building & isOkStatus( repo = repo)
}

getBuildingManifest <- function(repo, results = repo_results(repo),
    manifest = manifest_df(repo))
{
    manifest[getBuilding(repo, results),]
}

getBuildingResults <- function(repo, results = repo_results(repo))
{
    results[getBuilding(repo, results),]
}

trim_PACKAGES <- function(dir) {

    pkgs = read.dcf(file.path(dir, "PACKAGES"))
    pkgsdf = as.data.frame(pkgs)
    if("File" %in% names(pkgsdf))
        fils = pkgsdf$Files
    else {
        fils = file.path(dir, paste0(pkgsdf$Package, "_",
                                     pkgsdf$Version, ".tar.gz"))
    }
    missing = !file.exists(fils)
    pkgsdf = pkgsdf[!missing,]
    out <- file(file.path(dir, "PACKAGES"), "wt")
    outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
    write.dcf(pkgsdf, file = out)
    write.dcf(pkgsdf, file = outgz)
    close(out)
    close(outgz)
    invisible(pkgs[missing, "Package"])
}

haveGit <- function() nchar(Sys.which("git")) > 0

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#' Returns the difference between 2 data frames
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom dplyr anti_join
#' @param new_df The new dataframe which you want to compare
#' @param old_df An older dataframe of the same structure
#' @return Differences as a dataframe of the same structure
#' @seealso \code{\link[dplyr]{anti_join}}
#' @note This function is not intended for direct use by the end user.
deltaDF <- function(new_df, old_df) {
  delta <- suppressWarnings(suppressMessages(anti_join(new_df, old_df)))
  return(delta)
}

#' Checks whether an email ID is valid
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @param email_id Email ID as a string
#' @return Boolean
#' @note This function is not intended for direct use by the end user.
isValidEmail <- function(email_id) {
	grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(email_id), ignore.case=TRUE)
}

#' Get the OS Type
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @return OS Type
#' @note This function is not intended for direct use by the end user.
getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' Convert string to numeric representation
#' @param x String
#' @return Numeric representation of string
#' @note This function is not intended for direct use by the end user.
encode_string <- function(x) {
  tolower(paste(strtoi(charToRaw(as.character(x)), 16L), collapse = ""))
}

globalVariables("getRversion")
getRversion2 <- function() {
    if(exists("getRversion"))
        getRversion()
    else
        paste(R.version$major, R.version$minor)
}

#' Protect against binary incompatibility in R versions (3.4- <-> 3.5+)
#' @param repo GRANRepository being built
#' @return \code{repo}, after clearing the temporary library location
#' if packages in it were built using a different R version
checkAndFixLibLoc = function(repo) {
    libloc <- temp_lib(repo)
    inst <- installed.packages(lib.loc = libloc)
    if(dim(inst)[1] == 0)
        return(repo)
    
    bldvrs <- inst[,"Built"]
    currvers <-  getRversion2()
    allbvers = unique(bldvrs)
    if(length(allbvers) > 1 || allbvers != currvers) {
        message("Found mismatching R versions in LibLoc. Clearing temporary library.")
        clear_temp_files(repo, checkout = FALSE, logs = FALSE)
    }
    return(repo)
}

update_pkgs_logfun = function(repo, pkg = "NA" ) {
    fun = logfun(repo)
    force(fun)
    function(msg) {
        fun(pkg, msg)
    }
}

## Remove a package from a GRANRepository object.
.removePkg <- function(repo, package) {
    not_found <- TRUE
    ## pkg_manifest and pkg_version
    pm <- manifest_df(repo)
    if (package %in% pm$name) {
        manifest_df(repo) <- pm[pm$name != package,]
        not_found <- FALSE
    }
    pv <- versions_df(manifest(repo))
    if (package %in% pv$name) {
        versions_df(manifest(repo)) <- pv[pv$name != package,]
        not_found <- FALSE
    }
    ## results
    res <- repo_results(repo)
    if (package %in% res$name) {
        repo_results(repo) <- res[res$name != package,]
        not_found <- FALSE
    }
    ## suspended
    suspended <- suspended_pkgs(repo)
    if (package %in% suspended) {
        suspended_pkgs(repo) <- suspended[suspended != package]
        not_found <- FALSE
    }

    if (not_found)
        message(paste0("'", package, "' not found in repo object"))

    repo
}
