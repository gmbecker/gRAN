##' create the tarballs in the new repo from the svn branch locs
##'
##' 
##' @title Build SVN Checkouts Into Repository Directory 
##' @param repo a GRANRepository object
##' @param cores number of cores to use during build process. defaults to 1
##' @param temp logical. whether we are building the temp or final version of the repository
##' @param incremental logical. whether packages should only be rebuilt if the version number has increased. Default is TRUE
##' @param manifest data.frame containing a GRAN manifest of pkgs to build. Defaults to the full manifest associated with repo
##' @return a list with success and fail elements containing the directories which succeeded and failed the build
##' @author Cory Barr, Gabriel Becker
##' @importFrom tools write_PACKAGES
buildBranchesInRepo <- function( repo, cores = 1, temp=FALSE,
                                incremental = TRUE, manifest = manifest_df(repo)) {
  
    binds = getBuilding(repo = repo)
    if(!sum(binds))
    {
        logfun(repo)("NA", "No packages to build in buildBranchesInRepo.")
        return(repo)
    }
    manifest = getBuildingManifest(repo = repo)
    results = getBuildingResults(repo = repo)
    svnCheckoutsLoc = getCheckoutLocs(checkout_dir(repo), manifest,
        manifest$branch)
    if(temp) {
        repoLoc = temp_repo(repo)
        if(!grepl("src/contrib", repoLoc, fixed=TRUE))
        {
            if(!grepl(repo_name(repo), repoLoc, fixed=TRUE))
                repoLoc = file.path(repoLoc, repo_name(repo), "src", "conbrib")
            else
                repoLoc = file.path(repoLoc, "src", "contrib")
        }
        opts = "--no-build-vignettes --no-manual --no-resave-data"
    } else {
        repoLoc = staging(repo)
        opts = "--resave-data"
    }
    
    if(!file.exists(repoLoc))
        dir.create(repoLoc, recursive=TRUE)
    startDir <- getwd()
    on.exit(setwd(startDir))
    setwd(repoLoc)
    logfun(repo)("NA", paste0("Attempting to build ", sum(results$building), " into ", repoLoc))

    if(temp) {
        ## Only build  packages into the temp repo that aren't already there.
        ## Not doing this was causing unreasonably slow times when
        ## not very many packages ended up being actually built
        oldvers = character(length(svnCheckoutsLoc))
        names(oldvers) = results$name
        avl = tryCatch(available.packages(makeFileURL( repoLoc), filters= "duplicates"),
            error = function(x) x)
        if(is(avl, "error"))
            oldvers = rep(NA, times = nrow(results))
        else {
            inds = match(avl[,"Package"], names(oldvers))
            inds = inds[!is.na(inds)]
            pkgs = names(oldvers)[inds]
            oldvers[pkgs] = avl[pkgs,"Version"]
            oldvers[!nchar(oldvers)] = NA
        }
    } else {
        oldvers = as.character(results$lastbuiltversion)
    }

    vers_restrict = subset(versions_df(repo), versions_df(repo)$name %in% manifest$name)
    
    
    res <- mcmapply2(#svnCheckoutsLoc,
                    ##res <- mapply(
                    function (checkout, repo, opts, oldver, vers_restr, incremental) {
                        ## incremental build logic. If incremental == TRUE,
                        ## we only rebuild if the package version number has bumped.
                        vnum = read.dcf(file.path(checkout, "DESCRIPTION"))[1,"Version"]
                        pkg = getPkgNames(checkout)

                        if(!is.na(vers_restr) && vnum != vers_restr) {
                            logfun(repo)(pkg, paste("Wrong version number for pkg",
                                                    pkg, "Needed", vers_restr,
                                                    "have", vnum, "error earlier",
                                                    "in build process?"),
                                         type = "both")
                            ret = "wrong version"
                            names(ret) = vnum
                            return(ret)
                        }


                        ## we don't support changing the version restriction backward, should we?
                        if(is.na(oldver) || compareVersion(vnum, oldver) == 1 )
                        {
                            logfun(repo)(pkg, paste0("Had version ", oldver,
                                                     ". Building new version ", vnum))
                        } else if (incremental) {
                            logfun(repo)(pkg, paste0("Package up to date at version ", vnum, ". Not rebuilding."))
                            ret = "up-to-date"
                            names(ret) = vnum
                            return(ret)
                        } else {
                            logfun(repo)(pkg, paste0("Forcing rebuild of version ", vnum, "."))
                        }

                        if(pkg == "GRANBase" && !grepl("--no-build-vignettes", opts))
                           opts = paste(opts, "--no-build-vignettes")
                        command <- paste("R CMD build", checkout, opts )
                        if(!temp)
                            command = paste0("R_LIBS_USER=", temp_lib(repo), " ", command) 
                        out = tryCatch(system_w_init(command, intern = TRUE,
                            param = param(repo)), error = function(x) x)
                        if(is(out, "error") || ("status" %in% attributes(out) && attr(out, "status") > 0) || !file.exists(paste0(pkg, "_", vnum, ".tar.gz"))) {
                            type = if(temp) "Temporary" else "Final"
                            logfun(repo)(pkg, paste(type,"package build failed. R CMD build returned non-zero status"), type ="both")
                            logfun(repo)(pkg, c("R CMD build output for failed package build:", out), type="error")
                            ret = "failed"
                        } else {
                            #XXX we want to include the full output when the build succeeds?
                            logfun(repo)(pkg, "Sucessfully built package.", type="full")
                            ret = "ok"
                        }
                        names(ret) = vnum
                        ret
                    },
                     checkout = svnCheckoutsLoc,
                     repo = list(repo), opts = opts,
                     mc.cores=cores,
                                        #XXX shouldn't need the as.character...
                     oldver = oldvers,
                     incremental = incremental,
                     vers_restr = vers_restrict$version,
                    USE.NAMES=FALSE

                    )
    versions = names(res)
    res = unlist(res)
    ## at the temp stage we don't want to include anything
    ## from the list of potential builds, we just want
    ## to avoid unnecessary building within the temporary
    ## repository
    if(temp) {
        res[res=="up-to-date"] = "ok"
        res[versions == as.character(results$lastbuiltversion)] = "up-to-date"
    }
    built = res == "ok"

    ##We can only ever get the "up-to-date" return status if incremental=FALSE, so
    ## this is safe
    sameversion = res == "up-to-date"
    ##this may need to be more sophisticated later if we are building binaries for mac/win?
    write_PACKAGES(".", type="source")
    if(any(res == "failed")) {
        warning("Warning: not all packages were succesfully built")
    }

    res2 = res[!sameversion]
    results$status[!sameversion] = ifelse(res2=="ok", "ok", "build failed")
    results$status[sameversion] = "up-to-date"
    results$version[built] = versions[built]
    results$maintainer = getMaintainers(checkout_dir(repo),
                           manifest = manifest)
    repo_results(repo)[binds,] = results
    repo

}
