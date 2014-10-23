#right now we only look for and care about reverse dependencies
# in GRAN packages so we only look in repo@TempLibLoc
addRevDeps = function(repo)
{

    ##reset and recalculate reverse dependencies every time. Expensive
    ##but protects us against packages being added or removed from
    ##manfiest
    manifest_df(repo)$revDepOf = ""
    manifest_df(repo)$revdepends = ""

    if(is.null(manifest_df(repo)$buildReason))
        manifest_df(repo)$buildReason = ifelse(repo_results(repo)$building, "vbump", "")
    else {
        manifest_df(repo)$buildReason[is.na(manifest_df(repo)$buildReason)] = ""
        manifest_df(repo)$buildReason[repo_results(repo)$building] = "vbump"
    }
    
    writeGRANLog("NA", paste("Checking for reverse dependencies to packages",
                             "with version bumps."), repo = repo)
    manifest = manifest_df(repo)
    ##if this is the first time we are building the repository all the packages
    ##will be being built, so no need to check rev deps
    if(!file.exists(temp_lib(repo)) || all(getBuilding(repo)))    {
        writeGRANLog("NA", paste("All packages are being built, skipping",
                                 "reverse dependency check."), repo = repo)
        return(repo)
    }
    pkgs = manifest_df(repo)$name[getBuilding(repo)]
    revdeps = sapply(pkgs, dependsOnPkgs, dependencies = "all",
        lib.loc = temp_lib(repo), simplify=FALSE)
    if(!length(unlist(revdeps))) {
        writeGRANLog("NA", "No reverse dependencies detected", repo = repo)
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


    rdepBuildPkgs = manifest$buildReason != "vbump" & !manifest$suspended & grepl(paste0("(", paste(pkgs, collapse="|"), ")"), manifest$revDepOf)
    manifest[rdepBuildPkgs, "buildReason"] = "is rdep"
    manifest[rdepBuildPkgs, "building"] = TRUE 
    
    writeGRANLog("NA", paste0("Detected ", sum(rdepBuildPkgs),
                              " packages that are reverse dependencies of",
                              "packages with version bumps:\n\t",
                              paste(manifest$name[rdepBuildPkgs],
                                    collapse=" , ")), repo = repo)

    if(sum(rdepBuildPkgs) >0) {
        writeGRANLog("NA",
                     "Building reverse dependencies in temporary repository.",
                     repo = repo)
        tmprepo = repo
        manifest_df(tmprepo) = manifest[rdepBuildPkgs,]
        manifest_df(tmprepo)$building = TRUE
        manifest_df(tmprepo)$status="ok"
        tmprepo = buildBranchesInRepo(repo = tmprepo, temp = TRUE,
           # incremental = FALSE)
            incremental = TRUE)
        manifest[rdepBuildPkgs, ] = manifest_df(tmprepo)
    }
    manifest_df(repo) = manifest
    repo
}

