##'@importFrom brew brew
invokePkgTests_old= function( repo, dir = file.path(tempdir(), repo_name(repo)), cores = 3L)
{
    if(!file.exists(dir))
        dir.create(dir, recursive=TRUE)
    testscript = file.path(dir, "GRANtestscript.R")
    tmpmanfile = file.path(dir, "tempmanifest.dat")
    write.table(manifest_df(repo), file = tmpmanfile, sep=",")
    repfile = file.path(dir, "repo.R")
    saveRepo(repo, filename = repfile)
    libpath = .libPaths()[1]
    ##silliness to allow for GRAN/GRANBase dichotomy
    sentry = search()[grep("package:(GRAN.*)", search()) [1] ]
    pkg = gsub("package:(GRAN.*)", "\\1", sentry)
    brew(system.file("templates", "testPkgs.brew", package=pkg), output = testscript)
    cmd = paste0( R.home(),"script --no-restore --no-save ", testscript)
    writeGRANLog("NA", paste("Attempting to invoke package testing via command",
                             cmd), type = "full", repo = repo)
    res = tryCatch(system_w_init(cmd, intern=TRUE, repo=repo), error=function(x) x)

    
    if(is(res, "error"))
    {
        writeGRANLog("NA", c("CRITICAL GRAN FAILURE! Failed to invoke package testing in external R session:", res), type="both", repo = repo)
        manifest_df(repo)$status[manifest_df(repo)$building] = "GRAN FAILURE"
    }  else {
        repo = loadRepo(repfile)
        writeGRANLog("NA", "Package testing complete", repo = repo)

    }
 
    repo
}

invokePkgTests = function( repo, dir = file.path(tempdir(), repo_name(repo)), cores = 3L) {
    doPkgTests(repo = repo, cores = cores)
}

doPkgTests = function(repo, cores = 3L)
{

    writeGRANLog("NA", paste0("Beginning testing of GRAN packages before migration to final repository using ", cores, " cores: ", paste(manifest_df(repo)$name, collapse = " , ")), type = "full", repo = repo)

     writeGRANLog("NA", paste0("Performing 'extra' commands before installation. ", paste(manifest_df(repo)$name, collapse = " , ")), type = "full", repo = repo)

    repo = doExtra(repo)
    
    if(is.null(manifest_df(repo)$building))
        manifest_df(repo)$building = TRUE

    repo = installTest(repo, cores = cores)
    repo = checkTest(repo, cores = cores)
    repo

}


installTest = function(repo, cores = 3L)
{
    writeGRANLog("NA", paste0("Attempting to install packages (",
                              sum(manifest_df(repo)$building),
                              ") from temporary repository into temporary package library."),
                 type = "full", repo = repo) 

    manifest = manifest_df(repo)

    oldops = options()
    options(warn = 1)
    on.exit(options(oldops))
    #  loc = file.path(tempdir(), paste("GRANtmplib", repo_name(repo), sep="_"))
    loc = temp_lib(repo)
    if(!file.exists(loc))
        dir.create(loc, recursive=TRUE)
    binds  = getBuilding(repo = repo)
    bman = getBuildingManifest(repo = repo)
    if(!nrow(bman)) {
        writeGRANLog("NA", "No packages to install during installTest",
                     type ="full", repo = repo)
        return(repo)
    }

    oldlp = .libPaths()
    .libPaths(loc)
    on.exit(.libPaths(oldlp))
    res = install.packages2(bman$name, lib = loc, repos = c(paste0("file://",repo@tempRepo), BiocInstaller::biocinstallRepos(), "http://R-Forge.R-project.org"), type = "source", dependencies=TRUE)
    success = processInstOut(names(res), res, repo)
    cleanupInstOut(res)
    
    writeGRANLog("NA", paste0("Installation successful for ", sum(success), " of ", length(success), " packages."), type = "full", repo = repo)

    #update the packages in the manifest we tried to build with success/failure
    manifest_df(repo)$status[binds][!success] = "install failed"
    repo
    
}


processInstOut = function(pkg, out, repo)
{
    if(length(out) > 1)
        return(unlist(mapply( processInstOut, repo = list(repo), pkg = pkg, out = out)))

    if(out == "ok") {
        writeGRANLog(pkg, paste0("Successfully installed package ", pkg, " from temporary repository"), repo = repo)
        ret = TRUE
    } else if (out == "unavailable") {
        writeGRANLog(pkg, paste("Package", pkg, "unavailable in temporary repository. Likely package name mismatch between manifest and DESCRIPTION file"), repo = repo, type = "both")
        ret = FALSE
    } else {
        writeGRANLog(pkg, paste0("Installation of ", pkg, " from temporary repository failed"), repo = repo, type="both")
        writeGRANLog(pkg, c("Installation output:", readLines(out)), type = "error", repo = repo)
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
    repo = buildBranchesInRepo(repo, temp=FALSE,
        #incremental=FALSE,
        incremental = TRUE, ## want to skip testing if pkg already passed
        cores = cores)
    oldwd = getwd()
    setwd(staging(repo))
    on.exit(setwd(oldwd))
    writeGRANLog("NA", paste0("Running R CMD check on remaining packages (", sum(getBuilding(repo = repo)), ") using R at ", R.home(), "."), type = "full", repo = repo)
    manifest = manifest_df(repo)
    binds  = getBuilding(repo = repo)
    bman = getBuildingManifest(repo = repo)
    if(!nrow(bman))
        return(repo)
    #pat = paste0("(", paste(bman$name, collapse="|"), ")_.*\\.tar.gz")
    #tars = list.files(pattern = pat)
    tars = unlist(mapply(function(nm, vr) list.files(pattern = paste0(nm, "_", vr, ".tar.gz")), nm = bman$name, vr = bman$version))
    if(length(tars) < nrow(bman)) {
        missing = sapply(bman$name, function(x) !any(grepl(x, tars, fixed=TRUE)))
        writeGRANLog("NA", c("Tarballs not found for these packages during check test:", paste(bman$name[missing], collapse = " , ")), type = "both", repo = repo)
        #tars = tars[order(bman$name[!missing])]
        manifest_df(repo)$status[manifest_df(repo)$name %in% bman$name[missing]] = "Unable to check - missing tarball"
        bman  = bman[!missing,]
        binds[binds] = binds[binds] & !missing
    }
    #tars = tars[order(bman$name)]
    ord = mapply(function(nm, vr) grep(paste0(nm, "_", vr), tars), nm = bman$name, vr = bman$version)
    
    tars = tars[unlist(ord)]
    outs = mcmapply2( function(tar, nm, repo) {
        writeGRANLog(nm, paste("Running R CMD check on ", tar), repo = repo)
        ## We built the vignettes during this round of building, so if the pkg is going to
        ##fail on building vignettes it will have already happened by this point
        cmd = paste0("R_LIBS='", LibLoc(repo), "'  ", R.home(), " CMD check ", tar, " --no-build-vignettes")
        out = tryCatch(system_w_init(cmd, intern=TRUE, repo = repo),
            error=function(x) x)
        out
    }, tar = tars, nm = bman$name, repo = list(repo), mc.cores = cores,
        SIMPLIFY=FALSE)
    
    success = mapply(function(nm, out, repo) {
        if(errorOrNonZero(out) || any(grepl("ERROR", out, fixed=TRUE))) {
            writeGRANLog(nm, "R CMD check failed.", type = "both", repo = repo)
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

                writeGRANLog(nm, "R CMD check raised warnings.", type = "both", repo = repo)
                outToErrLog = TRUE
                ret = "check warning(s)"
            } else if (numnotes - !licIsWarning > 0) {
                writeGRANLog(nm, "R CMD check raised notes.", type = "both", repo = repo)
                outToErrLog = TRUE
                ret = "check note(s)"
            } else {
                writeGRANLog(nm, "R CMD check passed.", type = "full", repo = repo)
                outToErrLog = FALSE
                ret = "ok"
            }
        }
        cat(paste(out, collapse="\n"), file = file.path(check_result_dir(repo),
                                           paste0(nm, "_CHECK.log")))
        if(outToErrLog)
            writeGRANLog(nm, c("R CMD check output:", out), type="error", repo = repo)
        ret
        
    }, nm = names(outs), out = outs, repo = list(repo))
  
    
    success = unlist(success)

    writeGRANLog("NA", paste0(sum(isOkStatus(status = success, repo = repo)), " of ", length(success), " packages passed R CMD check"), repo=repo)
    manifest_df(repo)$status[binds] = success
  ##  manifest_df(repo)$building[binds] = (success == "ok")
    repo
}


doExtra = function(repo)
{
    ##TODO!!!
    return(repo)
    fun = repo@extraFun
    bman = getBuildingManifest(repo)
    res = mapply(function(nm, extra, fun) 
    {
        rets = tryCatch(fun(extra), error=function(x) x)
        if(is(rets, "error") || (is.logical(rets) && !rets))
        {
            writeGRANLog(nm, paste("Unable to perform extra instructions (",extra, "). Extra function returned: ", rets), type="both", repo = repo)
            "extra instructions failed"
        } else {
            "ok"
        }
    }, nm = bman$name, extra = bman$extra, fun = list(fun))

    manifest_df(repo)$status[getBuilding(repo)] = res
    repo
}
