
makeSrcDirs = function(repo, cores = 3L, scm_auth)
{
    binds = getBuilding(repo = repo)
    
    manifest = getBuildingManifest(repo = repo)
    scm_auth = replicate(scm_auth, n=nrow(manifest), simplify=FALSE)
    sources = mapply(makeSource, url = manifest$url,
        type = manifest$type, scm_auth= scm_auth, branch = manifest$branch,
        subdir = manifest$subdir, prefer_svn = TRUE, name = manifest$name)
    path = checkout_dir(repo)
    versions = versions_df(repo)
    res <- mapply(#mcmapply2(
        function(nm, src,  repo, path, version) {
            ##hack for now, supports github but not other git repos
            ## eventually I need to write gotoVersCommit for git checkouts
            if(!is.na(version) && is(src, "GitSource"))
                src = as(src, "SVNSource")
                
            ret = makePkgDir(name = nm, source = src, path =path,
                latest_only = FALSE, repo=repo, forceRefresh=FALSE)
            if(ret && !is.na(version)  && file.exists(file.path(path, nm))) {
                gotoVersCommit(file.path(path, nm), src = src, repo = repo)
            }
            ret
        },
                     
        nm = manifest$name,
        version = versions$version,
        src = sources,
        path = path,
        repo = list(repo))#,
#       mc.cores = cores)
    res = unlist(res)
    if(!is.logical(res)) 
        print(res)
    repo_results(repo)$status[binds][!res] = "source checkout failed"
    repo
}

