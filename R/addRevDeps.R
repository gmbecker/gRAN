#right now we only look for and care about reverse dependencies
# in GRAN packages so we only look in repo@tempLibLoc
addRevDeps <- function(repo)
{
    ##reset and recalculate reverse dependencies every time. Expensive
    ##but protects us against packages being added or removed from
    ##manfiest
    repo_results(repo)$revDepOf = ""
    repo_results(repo)$revdepends = ""

    if(is.null(repo_results(repo)$buildReason))
        repo_results(repo)$buildReason = ifelse(repo_results(repo)$building, "vbump", "")
    else {
        repo_results(repo)$buildReason[is.na(repo_results(repo)$buildReason)] = ""
        repo_results(repo)$buildReason[repo_results(repo)$building] = "vbump"
    }

    logfun(repo)("NA", paste("Checking for reverse dependencies to packages",
                             "with version bumps."))
    manifest = repo_results(repo)
    ##if this is the first time we are building the repository all the packages
    ##will be being built, so no need to check rev deps
    ## ignore suspended packages when checking if everything is being built
    if(!file.exists(temp_lib(repo)) || all(getBuilding(repo) |
                                           manifest$name %in% suspended_pkgs(repo)))    {
        logfun(repo)("NA", paste("All packages are being built, skipping",
                                 "reverse dependency check."))
        return(repo)
    }

    pkgs = manifest$name[getBuilding(repo)]
    revdeps = sapply(pkgs, dependsOnPkgs, dependencies = "all",
        lib.loc = temp_lib(repo), simplify=FALSE)
    if(!length(unlist(revdeps)) || all(revdeps %in% pkgs)) {
        logfun(repo)("NA", "No reverse dependencies detected")
        return(repo)
    }
    if(length(revdeps) > 0)
    {
        for(pkgname in names(revdeps)) {
            rdeps = revdeps[[pkgname]]
            if(length(rdeps) && !all(is.na(rdeps)))
            {
                rows = sapply(rdeps, function(pkg) {
                    x = which(pkg == manifest$name)
                    if(!length(x))
                        x = numeric()
                    x
                })
                rows = unlist(rows)
                prefix = ifelse(nchar(manifest$revDepOf[rows]) > 0, ",", "")

                manifest$revDepOf[rows] = paste0(manifest$revDepOf[rows],
                                     prefix, pkgname)
            }
            manifest$revdepends[manifest$name == pkgname] = paste(rdeps,
                                   collapse = " , ")
        }
    }


    rdepBuildPkgs = manifest$buildReason != "vbump" & !(manifest$name %in% suspended_pkgs(repo)) & grepl(paste0("(", paste(pkgs, collapse="|"), ")"), manifest$revDepOf)
    manifest[rdepBuildPkgs, "buildReason"] = "is rdep"
    manifest[rdepBuildPkgs, "building"] = TRUE

    logfun(repo)("NA", paste0("Detected ", sum(rdepBuildPkgs),
                              " packages that are reverse dependencies of",
                              "packages with version bumps:\n\t",
                              paste(manifest$name[rdepBuildPkgs],
                                    collapse=" , ")))

    repo_results(repo) = manifest
    repo
}
