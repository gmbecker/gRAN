#Package, build thine self
GRANonGRAN = function(repo)
{
    logfun(repo)("GRAN", paste("Creating repository specific GRAN package and",
                                "installing it into the GRAN repository at",
                                destination(repo)))
#    babyGRAN = file.path(repobase(repo), "GRANpkg")
    if(file.exists(file.path(repobase(repo), "GRAN")))
        unlink(file.path(repobase(repo), "GRAN"), recursive=TRUE)
    babyGRAN = file.path(repobase(repo), "GRAN")
    dir.create(file.path(babyGRAN, "inst","scripts"), recursive = TRUE)
    GRANRepo = repo
    res = file.copy(system.file("GRAN", package="GRANBase"),
              normalizePath2(repobase(repo)), recursive=TRUE,
              overwrite=TRUE)
    if(any(!res))
        stop("copy failed")
    saveRepo(GRANRepo,
         filename = file.path(repobase(repo), "GRAN", "inst", "myrepo.R"))
    code = paste("getGRAN = function(...) {",
        sprintf("install.packages('GRAN', ..., repos = c('%s', getOption('repos')))", repo_url(repo)),
        "};", "getGRAN(type='source')", collapse = "\n")
    cat(code, file = file.path(babyGRAN, "inst", "scripts", "getGRAN.R"))
    cat(code, file = file.path(dest_base(repo), paste0("getGRAN-", repo_name(repo), ".R")))
    DESC = readLines(file.path(babyGRAN, "DESCRIPTION"))
    DESC[1] = "Package: GRAN"
    writeLines(DESC, con = file.path(babyGRAN, "DESCRIPTION"))

    if("GRAN" %in% manifest_df(repo)$name) {
        granInd = which(repo_results(repo)$name == "GRAN")
        repo_results(repo)[granInd,] = ResultsRow(name = "GRAN")
        manifest_df(repo)[granInd,] = ManifestRow(name="GRAN",
                             url = babyGRAN, type="local", subdir = ".",
                             branch = "master")
    } else {
        repo = addPkg(repo, name="GRAN", url = babyGRAN, type="local",
            subdir = ".")
    }
    if(!"switchr" %in% manifest_df(repo, session_only=FALSE)$name)
        repo = addPkg(repo, name = "switchr", url="http://github.com/gmbecker/switchr", type = "git")
    repo
    
        
}

##'Transform a GRANRepository object into a list
##'
##' Utility to transform a GRANRepository object into a list
##' so that repos saved using GRANBase can be loaded by GRAN
##' without requiring GRANBase
##'
##' @param repo repository
##' @return a list suitable for use with RepoFromList
##' @export
RepoToList = function(repo) {

    sl = names(getSlots(class(repo)))
    l = lapply(sl, function(x) slot(repo, x))
    names(l) = sl
    l
}

##'Create a GRANRepository object from a list
##'
##' @param rlist A list with entries that are slot name-value
##' pairs for a GRANRepository object
##' @return a GRANRepository object
##' @export
RepoFromList = function(rlist) {
    do.call(new, c("GRANRepository", rlist))
}

##'Backwards compatible load utility
##'
##' Load a repository serialized to an R code file
##'
##' @param filename The file to load
##' @export
loadRepo = function(filename) {
    res = tryCatch(dget(filename), error = function(e)e)
    if(is(res, "error")) {
        txt = readLines(filename)
        txt2 = gsub("GRANRepository", "GRANRepositoryv0.9", txt)
        res = dget(textConnection(txt2))
        res = updateGRANRepoObject(res)
    }
    ##refresh closure for log function
    logfun(res) = function(pkg, msg, type = "full") writeGRANLog(pkg, msg, type,
              logfile = logfile(res), errfile = errlogfile(res))
    res
}
            
        
##'Backwards compatible save utility
##'
##' serialize a repository to a file so that it does not require GRANBase
##' to load
##'
##' @param repo The GRANRepository object to save
##' @param filename The destination file
##' @return NULL
##' @export
saveRepo = function(repo, filename) {
   dput(repo, filename)
}
    
