
doPkgTests = function(repo, cores = 3L)
{

    logfun(repo)("NA", paste0("Beginning testing of GRAN packages before migration to final repository using ", cores, " cores: ", paste(manifest_df(repo)$name, collapse = " , ")), type = "full")

     logfun(repo)("NA", paste0("Performing 'extra' commands before installation. ", paste(manifest_df(repo)$name, collapse = " , ")), type = "full")

    repo = doExtra(repo)
    
    if(is.null(repo_results(repo)$building))
        repo_results(repo)$building = TRUE



    if(install_test_on(repo)) {
        ##        repo = installTest(repo, cores = cores)
        repo = installTest(repo, cores = cores)
        repo = buildBranchesInRepo(repo, temp=FALSE,
            incremental = TRUE, ## want to skip testing if pkg already passed
            cores = cores)
    }
    if(check_test_on(repo))
        repo = checkTest(repo, cores = cores)
    else
        repo_results(repo)$status[repo_results(repo)$status == "ok"] = "ok - not tested"
    repo

}

installTest = function(repo, cores = 3L)
{
##    if(!install_test_on(repo))
##        return(repo)
    logfun(repo)("NA", paste0("Attempting to install packages (",
                              sum(repo_results(repo)$building),
                              ") from temporary repository into temporary package library."),
                 type = "full")

    manifest = manifest_df(repo)

    oldops = options()
    options(warn = 1)
    on.exit(options(oldops))
    loc = temp_lib(repo)
    if(!file.exists(loc))
        dir.create(loc, recursive=TRUE)
    binds  = getBuilding(repo = repo)
    bres = getBuildingResults(repo = repo)
    if(!nrow(bres)) {
        logfun(repo)("NA", "No packages to install during installTest",
                     type ="full")
        return(repo)
    }

    reps = c(makeFileURL(temp_repo(repo)),
            defaultRepos(),
            "http://R-Forge.R-project.org")
    oldlp = .libPaths()
    .libPaths2(loc, exclude.site=TRUE)
    on.exit(.libPaths(oldlp))
    
    if(!file.exists(install_result_dir(repo)))
        dir.create(install_result_dir(repo))

    granpkgs = bres[grepl("^GRAN", bres$name),]
    bres = bres[!grepl("^GRAN", bres$name),]
    
    res = install.packages2(bres$name, lib = loc,
        repos = reps,
        type = "source", dependencies=TRUE, ## Ncpus = cores, problems with installing deps?
        param = param(repo),
        outdir = install_result_dir(repo))
    success = processInstOut(names(res), res, repo)
    ## why was I removing some of the output logs? Doesn't seem like we want this.
    ##cleanupInstOut(res)
    
    logfun(repo)("NA", paste0("Installation successful for ", sum(success), " of ", length(success), " packages."), type = "full")

    #update the packages in the manifest we tried to build with success/failure
    repo_results(repo)$status[binds][!success] = "install failed"
    repo
    
}


processInstOut = function(pkg, out, repo)
{
    if(length(out) > 1)
        return(unlist(mapply( processInstOut, repo = list(repo), pkg = pkg, out = out)))

    if(out == "ok") {
        logfun(repo)(pkg, paste0("Successfully installed package ", pkg, " from temporary repository"))
        ret = TRUE
    } else if (out == "unavailable") {
        logfun(repo)(pkg, paste("Package", pkg, "unavailable in temporary repository. Likely package name mismatch between manifest and DESCRIPTION file"), type = "both")
        ret = FALSE
    } else if (out == "output missing") {
        logfun(repo)(pkg, paste("Package", pkg, "was not successfully installed and no",
                                "output is available. Likely culprit: failure during",
                                "installation of one of it's dependencies?"), type = "both")
        ret = FALSE
    } else {
        logfun(repo)(pkg, paste0("Installation of ", pkg, " from temporary repository failed"), type="both")
        logfun(repo)(pkg, c("Installation output:", readLines(out)), type = "error")
        ret = FALSE
    }
    ret
}
    
cleanupInstOut = function(out)
{
    torem = out[out!="ok"]
    file.remove(torem)

}


checkTest = function(repo, cores = 3L)
{
##    if(!check_test_on(repo)) {
##        repo_results(repo)$status[repo_results(repo)$status == "ok"] = "ok - not tested"
##        return(repo)
##    }
    oldwd = getwd()
    setwd(staging(repo))
    on.exit(setwd(oldwd))
    logfun(repo)("NA", paste0("Running R CMD check on remaining packages (", sum(getBuilding(repo = repo)), ") using R at ", R.home(), "."), type = "full")
    manifest = manifest_df(repo)
    binds  = getBuilding(repo = repo)
    bres = getBuildingResults(repo = repo)
    if(!nrow(bres))
        return(repo)
    #pat = paste0("(", paste(bres$name, collapse="|"), ")_.*\\.tar.gz")
    #tars = list.files(pattern = pat)
    expectedTars = file.path(staging(repo), paste0(bres$name,"_", bres$version, ".tar.gz"))
    tars = expectedTars[file.exists(expectedTars)]
##    tars = unlist(mapply(function(nm, vr) list.files(pattern = paste0(nm, "_", vr, ".tar.gz")), nm = bres$name, vr = bres$version))
    if(!identical(expectedTars, tars)){
        missing = !file.exists(expectedTars)
        ##        missing = sapply(bres$name,
        ##            function(x) !any(grepl(paste0("^",x, "_"), tars, fixed=TRUE)))
        logfun(repo)("NA", c("Tarballs not found for these packages during check test:", paste(bres$name[missing], collapse = " , ")), type = "both")
        repo_results(repo)$status[manifest_df(repo)$name %in% bres$name[missing]] = "Unable to check - missing tarball"
        bres  = bres[!missing,]
        binds[missing] = FALSE
    }
    #tars = tars[order(bres$name)]
##    ord = mapply(function(nm, vr) grep(paste0(nm, "_", vr), tars), nm = bres$name, vr = bres$version)
    
  ##  tars = tars[unlist(ord)]
    outs = mcmapply2(
        function(nm, tar, repo)
        {
            if(grepl("^GRAN", nm)) {
                logfun(repo)(nm, paste("Not checking", nm, "package to avoid recursion problems"))
                return(c(paste("*", nm, "not checked to prevent recursion"),
                         "* DONE",
                         "* Status: OK"))
            }
            
            logfun(repo)(nm, paste("Running R CMD check on ", tar))
            ## We built the vignettes during this round of building, so if the pkg is going to
            ##fail on building vignettes it will have already happened by this point
            cmd = paste0('R_LIBS="', temp_lib(repo),  '" R_HOME="',
                R.home(),'" R CMD check ', tar, " --no-build-vignettes")
            env = c(paste0('R_LIBS="', temp_lib(repo), '"'),
                    paste0('R_HOME="', R.home(), '"'))
            args = c("check", tar, "--no-build-vignettes")
            cmd = file.path(R.home("bin"), "Rcmd")
                    
            out = tryCatch(system_w_init(cmd, args = args, env = env,
                                         intern=TRUE, param = param(repo)),
                error=function(x) x)
            out

    },  nm = bres$name, tar = tars,repo = list(repo), mc.cores = cores,
        SIMPLIFY=FALSE)
    
    success = mapply(function(nm, out, repo) {
        if(errorOrNonZero(out) || any(grepl("ERROR", out, fixed=TRUE))) {
            logfun(repo)(nm, "R CMD check failed.", type = "both")
            outToErrLog = TRUE
     
            ret = "check fail"
        } else {
            numwarns = length(grep("WARNING", out)) - 1 ##-1 to account for the WARNING count
            numnotes = length(grep("NOTE", out)) - 1
            license = any(grepl("Non-standard license", out))
            ##Nonstandard but standardizable licence is a NOTE
            ##Nonstandard and non-standardizable license is a WARNING
            licIsWarning = license && any(grepl("Standardizable: TRUE", out))
            ##non-standard license
            if(numwarns - licIsWarning > 0) {

                logfun(repo)(nm, "R CMD check raised warnings.", type = "both")
                outToErrLog = TRUE
                ret = "check warning(s)"
            } else if (numnotes - !licIsWarning > 0) {
                logfun(repo)(nm, "R CMD check raised notes.", type = "both")
                outToErrLog = TRUE
                ret = "check note(s)"
            } else {
                logfun(repo)(nm, "R CMD check passed.", type = "full")
                outToErrLog = FALSE
                ret = "ok"
            }
        }
        cat(paste(out, collapse="\n"), file = file.path(check_result_dir(repo),
                                           paste0(nm, "_CHECK.log")))
        if(outToErrLog)
            logfun(repo)(nm, c("R CMD check output:", out), type="error")
        ret
        
    }, nm = names(outs), out = outs, repo = list(repo))
  
    
    success = unlist(success)

    logfun(repo)("NA", paste0(sum(isOkStatus(status = success, repo = repo)), " of ", length(success), " packages passed R CMD check"))
    repo_results(repo)$status[binds] = success
  ##  repo_results(repo)$building[binds] = (success == "ok")
    repo
}


doExtra = function(repo)
{
    ##TODO!!!
    return(repo)
}
