
makeSrcDirs = function(repo, cores = 3L, scm_auth)
{
    binds = getBuilding(repo = repo)

    manifest = getBuildingManifest(repo = repo)
    scm_auth = replicate(scm_auth, n=nrow(manifest), simplify=FALSE)
    sources = mapply(makeSource, url = manifest$url,
        type = manifest$type, scm_auth= scm_auth, branch = manifest$branch,
        subdir = manifest$subdir,  name = manifest$name)
    path = checkout_dir(repo)
    versions = versions_df(repo)[binds,]
    res <- mapply(function(nm, src,  repo, path, version) {
            ret = makePkgDir(name = nm, source = src, path =path,
                latest_only = TRUE, param = param(repo), forceRefresh=FALSE)
            if(ret && !is.na(version)  && file.exists(file.path(path, nm))) {
                gotoVersCommit(file.path(path, nm), version = version, src = src,
                               param = param(repo))
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
    fullres = repo_results(repo)
    fullres$status[binds][!res] = "source checkout failed"
    vrs = getCOedVersions(path, manifest = manifest, repo = repo)
    inds = fullres$name %in% names(vrs)

    fullres$version[inds] = vrs[match(fullres$name[inds],
                                  names(vrs))]
    repo_results(repo) = fullres
    repo
}
