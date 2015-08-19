library(RUnit)
library(switchr)
library(GRANBase)
pkg = list.files(system.file("testpkgs", package = "GRANBase"), full.names = TRUE)[1]
man = PkgManifest(name = basename(pkg), 
    url =  pkg, type = "local")
repdir = file.path(tempdir(), "repos")
dir.create(repdir)
repo = makeRepo(man, repo_name= "test", basedir = repdir,
    destination = repdir,
    cores = 1L, install_test = TRUE, check_test = FALSE)

test_clear = function() {
    fils = list.files(c(temp_lib(repo), file.path(temp_repo(repo), "src", "contrib"), staging(repo)),
        include.dirs=TRUE, no.. = TRUE, full.names=TRUE)
    message("Found ", length(fils), " temporary artifacts from the build process. Clearing")
    res = clear_temp_fils(repo)
    fils2 = list.files(c(temp_lib(repo), temp_repo(repo), staging(repo)),
        include.dirs=TRUE, no.. = TRUE, full.names=TRUE)
    checkEquals(0, length(fils2), paste("clear_temp_fils missed the following files:", fils, sep="\n\t"))
    arch = file.path(tempdir(), "archive")
    if(!file.exists(arch))
        dir.create(arch)
    av = available.packages(repo)
    resbef = repo_results(repo)
    message(sprintf("Clearing %d packages deployed into repository.", nrow(av)))
    repo = clear_repo(repo, archivedir = arch)
    resaf = repo_results(repo)
    av2 = available.packages(repo)
    checkEquals(0, nrow(av2), sprintf("%d packages remained in repository after clearing", nrow(av2)))
    checkTrue(all(paste0(av[,"Package"], "_", av[,"Version"], ".tar.gz") %in% list.files(arch, pattern = "tar.gz")), "Some packages failed to be copied into the archive")
    checkEquals(resbef$name, resaf$name, "Results data.frame does not contain the same packages in the same order")
    checkTrue(all(is.na(resaf[,4:9])), sprintf("One or more of the rows %s were not reset to NA", paste(names(resbef)[3:9], collapse=", ")))
    checkTrue(all(resaf$version == "0.0-0"), "One or more versions not correctly reset to 0.0-0")
    
}

test_clear()
