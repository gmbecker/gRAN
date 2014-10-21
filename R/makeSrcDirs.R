
makeSrcDirs = function(repo, cores = 3L, scm_auth)
{
    binds = getBuilding(repo = repo)
    
    manifest = getBuildingManifest(repo = repo)
    scm_auth = replicate(scm_auth, n=nrow(manifest), simplify=FALSE)
    sources = mapply(makeSource, url = manifest$url, type = manifest$type, scm_auth= scm_auth, branch = manifest$branch, subdir = manifest$subdir)
    path = checkout_dir(repo)
    res <- mcmapply2(function(nm, src,  repo, path)  makePkgDir(name = nm, source = src, path =path,  latest_only = FALSE, repo=repo),
                     nm = manifest$name,
                     src = sources,
                     path = path,
                     repo = list(repo),
                     mc.cores = cores)
    res = unlist(res)
    if(!is.logical(res)) 
        print(res)
    manifest_df(repo)$status[binds][!res] = "source checkout failed"
    repo
}

