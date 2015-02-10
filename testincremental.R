library(GRANBase)
man = GithubManifest("gmbecker/rpath", "gmbecker/fastdigest")

##repo = makeSingleGRANRepo(manifest = man@manifest, subRepoName = "current", baseDir = "~/tempRepos", dest_base = "~/tempRepos", dest_url = paste0("file:///home/beckerg4/tempRepos"))
repo = makeRepo(man, repo_name = "current", basedir = "~/tempRepos")

repo2 = makeRepo(repo)

repo2 = makeSingleGRANRepo(repo = repo2, subRepoName = "current")
