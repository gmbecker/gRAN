#right now we only look for and care about reverse dependencies
# in GRAN packages so we only look in repo@TempLibLoc
addRevDeps = function(repo)
{

    ##reset and recalculate reverse dependencies every time. Expensive
    ##but protects us against packages being added or removed from
    ##manfiest
    repo@manifest$revDepOf = ""
    repo@manifest$revdepends = ""
    
    ##if(is.null(repo@manifest$revDepOf))
    ##    repo@manifest$revDepOf = ""
    ##else
    ##    repo@manifest$revDepOf[is.na(repo@manifest$revDepOf)] = ""
    ##if(is.null(repo@manifest$revdepends))
    ##    repo@manifest$revdepends = ""
    
    if(is.null(repo@manifest$buildReason))
        repo@manifest$buildReason = ifelse(repo@manifest$building, "vbump", "")
    else {
        repo@manifest$buildReason[is.na(repo@manifest$buildReason)] = ""
        repo@manifest$buildReason[repo@manifest$building] = "vbump"
    }
    
    writeGRANLog("NA", paste("Checking for reverse dependencies to packages",
                             "with version bumps."), repo = repo)
    manifest = repo@manifest
    ##if this is the first time we are building the repository all the packages
    ##will be being built, so no need to check rev deps
    if(!file.exists(repo@tempLibLoc) || all(getBuilding(repo)))
    {
        writeGRANLog("NA", paste("All packages are being built, skipping",
                                 "reverse dependency check."), repo = repo)
        return(repo)
    }
    pkgs = repo@manifest$name[getBuilding(repo)]
    revdeps = sapply(pkgs, dependsOnPkgs, dependencies = "all",
        lib.loc = repo@tempLibLoc, simplify=FALSE)
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
##                prefix = ifelse(!is.na(manifest$revDepOf), ",", "")
                manifest$revDepOf[rows] = paste0(manifest$revDepOf[rows],
                                     prefix, pkgname)
            }
            manifest$revdepends[manifest$name == pkgname] = paste(rdeps,
                                   collapse = " , ")
        }
    }


    ##manifest$revdepends[pkgs] = sapply(revdeps, function(x)
    ##paste(x, collapse = ", "))
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
        tmprepo@manifest = manifest[rdepBuildPkgs,]
        tmprepo@manifest$building = TRUE
        tmprepo@manifest$status="ok"
        tmprepo = buildBranchesInRepo(repo = tmprepo, temp = TRUE,
            incremental = FALSE)
        manifest[rdepBuildPkgs, ] = tmprepo@manifest
    }
    repo@manifest = manifest
    repo
}

