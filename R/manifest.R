setMethod("addPkg", "PkgManifest",
          function(x, name, version = NA,...) {
              if(!name %in% manifest_df(x)$name) {
                  manifest_df(x) = rbind(manifest_df(x),
                                 ManifestRow(name = name, ...))
              } else {
                  warning("package already in manifest")
              }
              x
          })

setMethod("addPkg", "GRANRepository",
          function(x, name, version = NA, ...) {
              if(!name %in% manifest_df(x)$name) {
                  manifest(x) = addPkg(manifest(x), name, version, ...)
                  repo_results(x) = rbind(repo_results(x),
                                  ResultsRow(name = name))
           
              } else {
                  warning("package already in manifest")
              }
              x
          })

setMethod("addPkg", "SessionManifest",
          function(x, name, version = NA, ...) {
              if(!name %in% manifest_df(x)$name) {
                  manifest(x) = addPkg(manifest(x), name,
                              version = version, ... )
                  versions(x) = rbind(versions(x),
                                  cbind(name = name, version=version))
              } else {
                  warning("package already in manifest")
              }
              x
              })

                  


emptyManifest = data.frame(name = character(),
    url = character(),
    type = character(),
    branch = character(),
    subdir = character(),

    extra = character(),
    stringsAsFactors = FALSE
    )
    

##'ManifestRow
##'
##' Create one or more rows of manifest
##'
##' @param name name of the package
##' @param url location of the package sources
##' @param type type of location (svn, git, local, etc)
##' @param subrepo name of the repository to build the package in
##' @param branch name of the branch to use to build the package
##' @param subdir subdirectory to use to build the package
##' @param extra currently ignored. extra commands for building or
##' installing the package
##' @param status status of the current build (typically starts as 'ok')
##' @param building whether the package is being/should be built in this
##' building cycle
##' @param version Version of the package.
##' @param lastAttempt Date of the last attempted build (or NA)
##' @param lastAttemptVersion version used in last attempted build (or NA)
##' @param lastAttemptStatus result of the last attempted build (or NA)
##' @param lastbuilt Date of the last successful build (or NA)
##' @param lastbuiltversion version built duirng last successful build (or NA)
##' @param lastbuiltstatus result of the last successful build (or NA)
##' @param buildReason Reason for building the package. typically NA
##' @param maintainer The maintainer of the package
##' @param suspended Whether building of the package is suspended (without
##' removal from the manifest). Defaults to FALSE
##' @return A valid GRAN manifest data.frame
##' @export
ManifestRow = function(name = NA,
    url = NA,
    type = NA,
    branch = NA,
    subdir = ".",
    extra = NA
    ) {

    if(is.na(type) && !is.na(url))
        type = .inferType(url)
    if(is.na(branch) && !is.na(type))
        branch = .inferDefaultBranch(branch, type)
    data.frame(name = name, url = url, type = type,
           branch = branch, subdir = subdir, extra = extra,
           stringsAsFactors = FALSE)
}

Manifest = function(..., dep_repos = c(biocinstallRepos(), repo_url(defaultGRAN()))) {
    rows = mapply(ManifestRow, ..., SIMPLIFY=FALSE)
    PkgManifest(manifest = do.call(rbind.data.frame, rows), dep_repos = dep_repos)
}

##XXX can't specify non-defaults in a lot of the columns


##' @export
GithubManifest = function( ..., pkgrepos = as.character(list(...))) {

    names = gsub(".*/(.*)(.git){0,1}$", "\\1", pkgrepos)
    res =Manifest(url = paste0("git://github.com/", pkgrepos, ".git"),
             type = "git", branch = "master", name = names)
    as(res, "GithubPkgManifest")
}
    


addToManifest = function(repo, row)
{
    mfile = repoManifest(repo)
    locked = lockManifest(repo)
    if(locked)
    {
        man = read.table(mfile, header=TRUE, stringsAsFactors = FALSE, sep=",")
        ind = which(man$name == row$name & man$subrepo == repo_name(repo))
        if(length(ind))
        {
            warning("Package was already in the specified GRAN repository")
            ret = FALSE
        } else {
            writeGRANLog("NA", paste("Adding new package", row$name, "to this repository"), repo = repo)
            
            man = rbind(man, row)
            write.table(man, file = mfile, sep=",")
            unlockManifest(repo)
            ret = TRUE
        }
    } else {
        stop("unable to lock manifest file. manifest overflow overflwo handling not implemented yet")
        warning("Unable to lock manifest file. Writing to manifest overflow")
    }
    ret
}
        
gitregex = "^(git:.*|http{0,1}://(www.){0,1}(github|bitbucket)\\.com.*|.*\\.git)$"


.inferType = function(urls) {
    types = character(length(urls))
    gits = grep(gitregex, urls)
    types[gits] = "git"
    types
}

.inferDefaultBranch= function(branch, type) {
    switch(type,
           git = "master",
           svn = "trunk",
           NA)
}
           

##' readManifest 
##'
##' Read a package or session manifest from a remote or local directory
##'
##' @param uri The location of the manifest directory (path or URL)
##' @param local Whether the manifest is a local directory or a URL
##' @param archive Not currently supported
##' @return A PackageManifest object, or a SessionManifest object if the
##' manifest directory contains a pkg_versions.dat file.
##' @importFrom RCurl url.exists
##' @export
readManifest = function(uri, local = !url.exists(uri), archive = FALSE) {
    if(archive)
        stop("support for archived manifest directories is forthcoming")
    if(!local) {
        dir = tempdir()
        download.file(paste(uri, "pkg_locations.dat", sep ="/"),
                      file.path(dir, "pkg_locations.dat"))
        download.file(paste(uri, "dep_repos.txt", sep ="/"),
                      file.path(dir, "dep_repos.txt"))
        if(url.exists(paste(uri, "pkg_versions.dat", sep="/"))) {
            download.file(paste(uri, "dep_repos.dat", sep ="/"),
                          file.path(dir, "pkg_versions.dat"))
        }
    }
    pkgman = read.table(file.path(dir, "pkg_locations.dat"), header = TRUE,
        sep = "\t")
    deprepos = readLines(file.path(dir, "dep_repos.txt"))
    manifest = PkgManifest(manifest = pkgman, dep_repos = deprepos)
    if(file.exists(file.path(dir, "pkg_versions.dat"))) {
        vers = read.table(file.path(dir, "pkg_versions.dat"),
            header = TRUE, sep="\t")
        SessionManifest(pkg_versions = vers,
                        pkg_manifest = manifest)
    } else {
        manifest
    }
}
