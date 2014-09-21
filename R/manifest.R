emptyManifest = data.frame(name = character(),
    url = character(),
    type = character(),
    subrepo = character(),
    branch = character(),
    subdir = character(),

    extra = character(),
    status = character(),
    building = logical(),
    version = character(),
    lastAttempt = character(),
    lastAttemptVersion = character(),
    lastAttemptStatus = character(),
    lastbuilt = character(),
    lastbuiltversion = character(),
    lastbuiltstatus = character(),
    buildReason = character(),
    suspended = logical(),
    maintainer = character(),
    stringsAsFactors = FALSE
    )
    

##'makeManifestRow
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
    subrepo = "current",
    branch = NA,
    subdir = ".",
    extra = NA,
    status = NA,
    building = NA,
    version = NA,
    lastAttempt = NA,
    lastAttemptVersion = NA,
    lastAttemptStatus = NA,
    lastbuilt = NA,
    lastbuiltversion = NA,
    lastbuiltstatus = NA,
    buildReason = NA,
    maintainer  = NA,
    suspended  = FALSE
    ) {

    if(is.na(type) && !is.na(url))
        type = .inferType(url)
    if(is.na(branch) && !is.na(type))
        branch = .inferDefaultBranch(branch, type)
    data.frame(name = name, url = url, type = type, subrepo = subrepo,
           branch = branch, subdir = subdir, extra = extra, status = status,
           version = version, lastAttempt = lastAttempt,
           lastAttemptVersion = lastAttemptVersion,
           lastAttemptStatus = lastAttemptStatus, lastbuilt = lastbuilt,
           lastbuiltversion = lastbuiltversion,
           lastbuiltstatus = lastbuiltstatus, buildReason = buildReason,
           suspended=suspended, building = building, maintainer = maintainer,
           stringsAsFactors = FALSE)
}

Manifest = function(..., dep_repos = c(biocinstallRepos(), defaultGRAN())) {
    rows = mapply(ManifestRow, ..., SIMPLIFY=FALSE)
    PkgManifest(manifest = do.call(rbind.data.frame, rows), dep_repos = dep_repos)
}

##XXX can't specify non-defaults in a lot of the columns
GithubManifest = function( ..., pkgrepos = as.character(list(...))) {

    names = gsub(".*/(.*)(.git){0,1}$", "\\1", pkgrepos)
    res =Manifest(url = paste0("https://github.com/", pkgrepos, ".git"),
             type = "git", branch = "master", name = names)
    as(res, "GithubPkgManifest")
}
    

readManifest =function(file = repoManifest(repo), repo, returnFull = FALSE)
{
    ##during the initial build of the repository the GRAN package needs to be loadable before the manifest exists!
    if(file.exists(file))
        man = read.table(file, stringsAsFactors=FALSE, header=TRUE, sep=",")
    else
        man = emptyManifest

    if(nrow(man) && !returnFull)
        man = man[man$subrepo == repo@subrepoName,]
    man
}

updateManifest = function(repo)
{
    fullman = repo@manifest
    tried = fullman$building
    ##succeeded = getBuilding(repo)
    succeeded = isOkStatus(status = fullman$status, repo = repo) & tried
    time = as.character(Sys.time())
    fullman$lastAttempt[tried] = time
    fullman$lastAttemptStatus[tried] = fullman$status[tried]
    fullman$lastAttemptVersion[tried] = fullman$version[tried]
    fullman$lastbuilt[succeeded] = time
    fullman$lastbuiltversion[succeeded] = fullman$version[succeeded]
    fullman$lastbuiltstatus[succeeded] = fullman$status[succeeded]

    reminds = which(names(fullman) %in% c("status", "building"))
    if(lockManifest(repo))
        {
            write.table(fullman[,-reminds], file = repoManifest(repo), col.names=TRUE, sep = ",")
            unlockManifest(repo)
        } else {
            stop("Unable to lock manifest. manifest overflow handling not yet supported")
            warning("Unable to lock manifest file, writing to overflow manifest")
            write.table(fullman, file= file.path(paste0(repoManifest(repo), ".overflow")), append=TRUE, col.names=TRUE, sep=",")
        }
    repo@manifest = fullman
    repo
    
    
}

lockManifest = function(repo)
{
##    lfile = file.path(repobase(repo), "manifest.LOCK")
    lfile = manLockFile(repo)
    ret = FALSE
    if(!file.exists(lfile)) {
        cat(as.character(Sys.time()), file = lfile)
        ret = TRUE
    } else {
        tm = strftime(readLines(lfile))
        now =  Sys.time()
        if (difftime(now, tm, units="mins") > 5)
        {
            cat(now, file=lfile, append=FALSE)
            ret = TRUE
        }
    }

    if(!ret)
    {
        writeGRANLog("NA", "GRAN FAILURE: unable to lock manifest file", type="both", repo = repo)
    }
    ret
}

unlockManifest = function(repo)
{
    ##lfile = file.path(reepobase(repo), "manifest.LOCK")
    lfile = manLockFile(repo)
    if(file.exists(lfile))
        file.remove(lfile)
}


addToManifest = function(repo, row)
{
    mfile = repoManifest(repo)
    locked = lockManifest(repo)
    if(locked)
    {
        man = read.table(mfile, header=TRUE, stringsAsFactors = FALSE, sep=",")
        ind = which(man$name == row$name & man$subrepo == repo@subrepoName)
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
           
