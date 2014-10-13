#Package, build thine self
GRANonGRAN = function(repo)
{
    writeGRANLog("GRAN", paste("Creating repository specific GRAN package and",
                                "installing it into the GRAN repository at",
                                destination(repo)), repo = repo)
#    babyGRAN = file.path(repobase(repo), "GRANpkg")
    babyGRAN = file.path(repobase(repo), "GRAN")
    if(!file.exists(file.path(babyGRAN, "inst", "scripts")))
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
    cat(code, file = file.path(dest_base(repo), paste0("getGRAN-", repo@subrepoName, ".R")))
    DESC = readLines(file.path(babyGRAN, "DESCRIPTION"))
    DESC[1] = "Package: GRAN"
    writeLines(DESC, con = file.path(babyGRAN, "DESCRIPTION"))
    manrow = ManifestRow(name="GRAN", url = babyGRAN, type="local",
        subdir=".", subrepo = repo@subrepoName, building=TRUE, extra = "",
        status="ok", branch = "trunk", lastbuiltversion = "0.0-0",
        version = "0.0-0", lastAttemptStatus="never built",
        buildReason = "vbump")
    granrow = repo@manifest[which(repo@manifest$name == "GRAN"), ]
    if(nrow(granrow) && granrow$type == "local" && granrow$url == babyGRAN) {
        repo@manifest$building[repo@manifest$name == "GRAN"] = TRUE
        repo@manifest$status[repo@manifest$name == "GRAN"] = "ok"
        ##force there always to be a "version bump"
        repo@manifest$version = "0.0-0"
    } else 
        repo@manifest = merge(repo@manifest, manrow,
            by = intersect(names(repo@manifest), names(manrow)),
            all.x=TRUE, all.y=TRUE)
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
    dget(filename)
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
    
