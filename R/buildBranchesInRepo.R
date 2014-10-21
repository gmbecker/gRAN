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
buildBranchesInRepo <- function( repo, cores = 1, temp=FALSE, incremental = TRUE, manifest = manifest_df(repo)) {
  
    binds = getBuilding(repo, manifest)       
    if(!sum(binds))
    {
        writeGRANLog("NA", "No packages to build in buildBranchesInRepo.", repo = repo)
        return(repo)
    }
    manifest = getBuildingManifest(repo, manifest) #manifest[binds,]
    svnCheckoutsLoc = getCheckoutLocs(checkout_dir(repo), manifest, manifest$branch)
    if(temp) {
        repoLoc = repo@tempRepo
        if(!grepl("src/contrib", repoLoc, fixed=TRUE))
        {
            if(!grepl(repo@subrepoName, repoLoc, fixed=TRUE))
                repoLoc = file.path(repoLoc, repo@subrepoName, "src", "conbrib")
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
    writeGRANLog("NA", paste0("Attempting to build ", sum(manifest$building), " into ", repoLoc), repo = repo)

    if(temp) {
        ## Only build  packages into the temp repo that aren't already there.
        ## Not doing this was causing unreasonably slow times when
        ## not very many packages ended up being actually built
        oldvers = character(length(svnCheckoutsLoc))
        names(oldvers) = manifest$name
        avl = tryCatch(available.packages(paste0("file://", repoLoc), filters= "duplicates"),
            error = function(x) x)
        if(is(avl, "error"))
            oldvers = rep(NA, times = nrow(manifest))
        else {
            inds = match(avl[,"Package"], names(oldvers))
            inds = inds[!is.na(inds)]
            pkgs = names(oldvers)[inds]
            oldvers[pkgs] = avl[pkgs,"Version"]
            oldvers[!nchar(oldvers)] = NA
        }
    } else {
        oldvers = as.character(manifest$lastbuiltversion)
    }
        
    res <- mcmapply2(#svnCheckoutsLoc,
                    ##res <- mapply(
                    function (checkout, repo, opts, oldver, incremental) {
                        #incremental build logic. If incremental == TRUE, we only rebuild if the package version number has bumped.
                        vnum = read.dcf(file.path(checkout, "DESCRIPTION"))[1,"Version"]
                        pkg = getPkgNames(checkout)
                        if(is.na(oldver) || compareVersion(vnum, oldver) == 1 )
                        {
                            writeGRANLog(pkg, paste0("Had version ", oldver, ". Building new version ", vnum), repo = repo)
                        } else if (incremental) {
                            writeGRANLog(pkg, paste0("Package up to date at version ", vnum, ". Not rebuilding."), repo = repo)
                            ret = "up-to-date"
                            names(ret) = vnum
                            return(ret)
                        } else {
                            writeGRANLog(pkg, paste0("Forcing rebuild of version ", vnum, "."), repo = repo)
                        }

                        command <- paste("R CMD build", checkout, opts )
                        if(!temp)
                            command = paste0("R_LIBS_USER=", LibLoc(repo), " ", command) 
                        out = tryCatch(system_w_init(command, intern = TRUE,
                            repo = repo), error = function(x) x)
                        if(is(out, "error") || ("status" %in% attributes(out) && attr(out, "status") > 0) || !file.exists(paste0(pkg, "_", vnum, ".tar.gz"))) {
                            type = if(temp) "Temporary" else "Final"
                            writeGRANLog(pkg, paste(type,"package build failed. R CMD build returned non-zero status"), type ="both", repo=repo)
                            writeGRANLog(pkg, c("R CMD build output for failed package build:", out), type="error", repo = repo)
                            ret = "failed"
                        } else {
                            #XXX we want to include the full output when the build succeeds?
                            writeGRANLog(pkg, "Sucessfully built package.", type="full", repo= repo)
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
                    USE.NAMES=FALSE

                    )
    versions = names(res)
    res = unlist(res)
    ## at the temp stage we don't want to include anything
    ## from the list of potential builds, we just want
    ## to avoid unnecessary building within the temporary
    ## repository
    if(temp)
        res[res=="up-to-date"] = "ok"
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
    manifest$status[!sameversion] = ifelse(res2=="ok", "ok", "build failed")
    manifest$status[sameversion] = "up-to-date"
    manifest$version[built] = versions[built]
    manifest$maintainer = getMaintainers(checkout_dir(repo),
                           manifest = manifest)
    manifest_df(repo)[binds,] = manifest
    repo

}
