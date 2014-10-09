testpkgs = list.files(system.file("testpkgs", package = currentPackage()), full.names = TRUE)
man = ManifestRow(name = basename(testpkgs), url = testpkgs, 
    type = "local", subrepo = "current", subdir=".")
repdir = file.path(tempdir(), "repos")
dir.create(repdir)
repo = makeSingleGRANRepo(man, subRepoName = "current", baseDir = repdir,
    dest_base = repdir, dest_url = paste0("file://", normalizePath(repdir)))

testGRANPkg = function() {
    libdir = tempdir()
    GRAN("GRAN", gRAN = repo, lib = libdir)
    library(GRAN, lib.loc = libdir)
    repo2 <<- GRAN:::makeSingleGRANRepo(repo = repo)
}

testonlyBuild = function() {
    if(!exists("repo2"))
        stop("This test can only be run after testGRANPkg")

    repo3 = GRAN:::makeSingleGRANRepo(repo = repo2, onlyBuild = "toypkg", subRepoName = "current")
    checkEquals(nrow(repo2@manifest), nrow(repo3@manifest))
}

testGithubSrc = function() {
    ghman = GithubManifest("gmbecker/fastdigest")
    repo4 = repo
    repo4@manifest = merge(repo4@manifest, ghman@manifest, by = intersect(names(ghman@manifest), names(repo4@manifest)), all = TRUE)
    repo5 = makeSingleGRANRepo(repo = repo4, subRepoName = "current")
}
