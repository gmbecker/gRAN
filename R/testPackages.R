doPkgTests <- function(repo, cores = 1, constrained_build = FALSE)
{
  logfun(repo)(
    "NA",
    paste0(
      "Beginning testing of GRAN packages before migration to ",
      "final repository using ",
      cores,
      " cores: ",
      paste(manifest_df(repo)$name, collapse = " , ")
    ),
    type = "full"
  )

  if (is.null(repo_results(repo)$building))
    repo_results(repo)$building = TRUE

  if (install_test_on(repo)) {
    repo = installTest(repo, cores = cores, constrained_build = constrained_build)
    repo = buildBranchesInRepo(repo,
                               temp = FALSE,
                               incremental = TRUE,
                               ## want to skip testing if pkg already passed
                               cores = cores)
  }

  if (check_test_on(repo))
    repo <- checkTest(repo, cores = cores)
  else
    repo_results(repo)$status[repo_results(repo)$status == "ok"] <-
      "ok - not tested"

  repo
}

#' @importFrom utils update.packages
installTest <- function(repo, cores = 1, constrained_build)
{
  logfun(repo)(
    "NA",
    paste0(
      "Attempting to install packages (",
      sum(repo_results(repo)$building),
      ") from temporary repository into temporary package library."
    ),
    type = "full"
  )

  manifest = manifest_df(repo)

  oldops = options()
  options(warn = 1)
  on.exit(options(oldops))
  loc = temp_lib(repo)
  if (!file.exists(loc))
    dir.create(loc, recursive = TRUE)
  binds  = getBuilding(repo = repo)
  bres = getBuildingResults(repo = repo)
  if (!nrow(bres)) {
    logfun(repo)("NA", "No packages to install during installTest",
                 type = "full")
    return(repo)
  }

  reps = c(makeFileURL(temp_repo(repo)),
           defaultRepos(),
           "http://R-Forge.R-project.org")
  oldlp = .libPaths()
  .libPaths2(loc, exclude.site = TRUE)
  on.exit(.libPaths(oldlp))

  ## insttmplogs = staging_logs(repo)
  ## if(!file.exists(insttmplogs))
  ##     dir.create(insttmplogs, recursive=TRUE)

  ## we don't install any GRAN packages
  ##binds is in repo_results row space!!! and it's a logical
  ## so we need to set elements to false, NOT remove them!
  rresgraninds = grep("^GRAN", repo_results(repo)$name)
  binds[rresgraninds] = FALSE
  ##bres is in an entirely different space because the packages that failed
  ## earlier steps (e.g., build fail, or were up to date) are excluded.
  ## it's a data.frame, so we need to remove the rows we don't want, unlike above
  bres <- subset(bres,!(grepl("^GRAN", bres$name)))

  ## protect ourselves here by asserting dimension conformity was preserved:
  ## bres should contain 1 row for each TRUE in binds
  stopifnot(nrow(bres) == sum(binds))

  # If everything is building fine, bres will
  # not result in anything. So no need to anything beyond this point
  if(nrow(bres) == 0) {
    return(repo)
  }

  # Update packages in the temporary library,
  # given that we always want to test with the latest available packages
  if (!constrained_build) {
    update.packages(lib.loc = loc, repos = dep_repos(repo),
                    ask = FALSE, instlib = loc)
  }

  res = install.packages2(bres$name,
                          lib = loc,
                          repos = reps,
                          type = "source",
                          dependencies = TRUE,
                          Ncpus = cores,
                          param = param(repo),
                          outdir = staging_logs(repo))
  success = processInstOut(names(res), res, repo)
  if (length(success) != nrow(bres) ||
      length(success) != sum(binds))
    stop("length mismatch between Install test output and packages tested")
  ## default is the staging_logs dir when repo specified, which is correct here
  cleanupInstOut(repo = repo)

  ## if these dimensions don't conform the recycling rule screws us.
  ## source of long-lived install results misreporting bug.
  stopifnot(length(binds) == nrow(repo_results(repo)))

  logfun(repo)("NA",
               paste0(
                 "Installation successful for ",
                 sum(success),
                 " of ",
                 length(success),
                 " packages."
               ),
               type = "full")
  ##update the packages in the manifest we tried to build with success/failure
  repo_results(repo)$status[binds][!success] = "install failed"
  repo
}


processInstOut = function(pkg, out, repo)
{
  if (length(out) > 1)
    return(unlist(mapply(
      processInstOut,
      repo = list(repo),
      pkg = pkg,
      out = out
    )))

  if (out == "ok") {
    logfun(repo)(pkg,
                 paste0(
                   "Successfully installed package ",
                   pkg,
                   " from temporary repository"
                 ))
    ret = TRUE
  } else if (out == "unavailable") {
    logfun(repo)(
      pkg,
      paste(
        "Package",
        pkg,
        "unavailable in temporary repository.",
        "Likely package name mismatch between manifest and DESCRIPTION file"
      ),
      type = "both"
    )
    ret = FALSE
  } else if (out == "output missing") {
    logfun(repo)(
      pkg,
      paste(
        "Package",
        pkg,
        "was not successfully installed and no",
        "output is available. Likely culprit: failure during",
        "installation of one of it's dependencies?"
      ),
      type = "both"
    )
    ret = FALSE
  } else {
    logfun(repo)(pkg,
                 paste0("Installation of ", pkg,
                        " from temporary repository failed"),
                 type = "both")
    logfun(repo)(pkg, c("Installation output:", readLines(out)), type = "error")
    ret = FALSE
  }
  ret
}

## Make sure that old install logs aren't around to gum up the works.
cleanupInstOut = function(outdir = staging_logs(repo), repo)
{
  ## dirs = list.dirs(outdir, recursive=FALSE)
  ## res = file.rename(dirs, file.path(install_result_dir(repo), basename(dirs)))
  ## if(!all(res))
  ##     stop("file renaming appears to have failed")
  ## invisible(NULL)
  instlogs = list.files(outdir, pattern = ".*\\.out", full.names = TRUE)
  if (!file.exists(install_result_dir(repo)))
    dir.create(install_result_dir(repo), recursive = TRUE)

  res = file.copy(
    normalizePath(instlogs),
    install_result_dir(repo),
    overwrite = TRUE,
    copy.date = TRUE
  )
  if (!all(res))
    stop("install log copying failed.")
  file.remove(instlogs)
  invisible(NULL)
}


.innerCheck <- function(nm, tar, repo) {
  if (grepl("^GRAN", nm)) {
    logfun(repo)(nm,
                 paste("Not checking", nm, "package to avoid recursion problems"))
    return(c(paste("*", nm, "not checked to prevent recursion")))
  }
  logfun(repo)(nm, paste("Running R CMD check on ", tar))
  ## We built the vignettes during this round of building, so if the pkg is going to
  ##fail on building vignettes it will have already happened by this point
  cmd = paste0(
    'R_LIBS="',
    temp_lib(repo),
    '" R_HOME="',
    R.home(),
    '" R CMD check ',
    tar,
    " --no-build-vignettes"
  )
  env = paste0('R_LIBS=', shQuote(temp_lib(repo)))
  args = c("CMD", "check", tar, "--no-build-vignettes")
  cmd = file.path(R.home("bin"), "R")
  out = tryCatch(
    system_w_init(
      cmd,
      args = args,
      env = env,
      intern = TRUE,
      param = param(repo)
    ),
    error = function(x)
      x
  )
  out
}


checkTest <- function(repo, cores = 1)
{
  oldwd = getwd()
  setwd(staging(repo))
  on.exit(setwd(oldwd))
  logfun(repo)(
    "NA",
    paste0(
      "Running R CMD check on remaining packages (",
      sum(getBuilding(repo = repo)),
      ") using R at ",
      R.home(),
      "."
    ),
    type = "full"
  )
  manifest = manifest_df(repo)
  ## binds is indices now, NOT TRUE/FALSE!!!
  binds  = which(getBuilding(repo = repo))
  bres = getBuildingResults(repo = repo)
  if (!nrow(bres)) {
    return(repo)
  }
  expectedTars = file.path(staging(repo),
                           paste0(bres$name, "_", bres$version, ".tar.gz"))
  tars = expectedTars[file.exists(expectedTars)]
  if (!identical(expectedTars, tars)) {
    missing = !file.exists(expectedTars)
    ##        missing = sapply(bres$name,
    ##            function(x) !any(grepl(paste0("^",x, "_"), tars, fixed=TRUE)))
    logfun(repo)("NA",
                 c(
                   "Tarballs not found for these packages during check test:",
                   paste(bres$name[missing], collapse = " , ")
                 ),
                 type = "both")
    repo_results(repo)$status[manifest_df(repo)$name %in% bres$name[missing]] <-
      "Unable to check - missing tarball"
    bres  = bres[!missing, ]
    binds = binds[!missing]
  }

  outs = mcmapply2(
    function(nm, tar, repo)
      tryCatch(
        .innerCheck(nm = nm, tar = tar, repo = repo),
        error = function(x)
          x
      )
    ,
    nm = bres$name,
    tar = tars,
    repo = list(repo),
    mc.cores = cores,
    SIMPLIFY = FALSE,
    mc.preschedule = FALSE
  )
  if (length(outs) != nrow(bres))
    stop("Fatal error. I didn't get check output for all checked packages.")

  success <- mapply(function(nm, out, repo) {
      if (errorOrNonZero(out) || any(grepl("Status: [[:digit:]]+ ERROR",
                                           out, fixed = FALSE))) {
        logfun(repo)(nm, "R CMD check failed.", type = "both")
        outToErrLog = TRUE

        ret = "check fail"
      } else {
        ##-1 to account for the Status count
        numwarns <- length(grep("\\.\\.\\. WARNING$", out)) - 1
        numnotes <- length(grep("\\.\\.\\. NOTE$", out)) - 1
        okstatus <- any(grepl("^Status: OK", out))
        notchecked <- any(grepl("not checked to prevent recursion", out))
        license <- any(grepl("Non-standard license", out))
        ##Nonstandard but standardizable license is a NOTE
        ##Nonstandard and non-standardizable license is a WARNING
        licIsWarning = license && any(grepl("Standardizable: TRUE", out))
        ##non-standard license
        if (numwarns - licIsWarning > 0 && !okstatus) {
          logfun(repo)(nm, "R CMD check raised warnings.", type = "warn")
          outToErrLog = TRUE
          ret = "check warning(s)"
        } else if (numnotes - !licIsWarning > 0 && !okstatus && !notchecked) {
          logfun(repo)(nm, "R CMD check raised notes.", type = "warn")
          outToErrLog = TRUE
          ret = "check note(s)"
        } else if (notchecked) {
          logfun(repo)(nm, "Not doing R CMD check", type = "full")
          outToErrLog = FALSE
          ret = "ok - not tested"
        } else {
          logfun(repo)(nm, "R CMD check passed.", type = "full")
          outToErrLog = FALSE
          ret = "ok"
        }
      }
      cat(paste(out, collapse = "\n"),
          file = file.path(check_result_dir(repo),
                           paste0(nm, "_CHECK.log")))
      if (outToErrLog)
        logfun(repo)(nm, c("R CMD check output:", out), type = "warn")
      ret

    }, nm = names(outs), out = outs, repo = list(repo))


  success = unlist(success)
  if (length(success) != length(binds)) {
    stop("FATAL ERROR: Only got ",
         length(success), " results from ", length(binds), " check tests.")
  }


  logfun(repo)("NA", paste0(
    sum(isOkStatus(status = success, repo = repo)),
    " of ",
    length(success),
    " packages passed R CMD check"
  ))
  repo_results(repo)$status[binds] = success

  repo
}


#' Calculate and generate package code test coverage reports
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom covr package_coverage percent_coverage report
#' @importFrom utils write.table read.table
#' @importFrom dplyr intersect
#' @param repo A gRAN repo object
#' @param cores How many CPU cores to use?
#' @return repo A gRAN repo object with updated code coverage info
#' @export
testCoverage <- function(repo, cores = 1) {
    if(!requireNamespace("DT")) {
        logfun(repo)("NA", type ="both",
            msg = "Unable to generate test coverage reports without the DT package. Skipping.")
        warning("Unable to generate test coverage reports without the suggested DT package. Skipping.")
    }

        
    logfun(repo)("NA",
                 paste0(
                   "Creating test coverage reports for ",
                   sum(repo_results(repo)$building),
                   " packages"
                 ))

    # Determine lib checkout location
    loc <- checkout_dir(repo)
    dir.create(loc, recursive = TRUE, showWarnings = FALSE)

    ## bres <- subset(repo_results(repo),
    ##                repo_results(repo)$building == TRUE
    ##                & !is.na(repo_results(repo)$buildReason)
    ##                & repo_results(repo)$status != "up-to-date")
    bres <- getBuildingResults(repo)
    mandf <- getBuildingManifest(repo)
    if (nrow(bres) == 0) {
      logfun(repo)("NA", "No packages to check test coverage for")
      return(repo)
    }

    bres <- subset(bres,!(grepl("^GRAN", bres$name)))
    mandf <- subset(mandf,!(grepl("^GRAN", mandf$name)))
    ## protective assertion
    stopifnot(identical(bres[["name"]], mandf[["name"]]))
    coverageDir <- coverage_report_dir(repo)

    # Begin test coverage calculations
    coverage <- suppressWarnings(mcmapply2(function(pkgName, subdir) {
        pkgDir <- file.path(loc, pkgName, subdir)
        stopifnot(file.exists(file.path(pkgDir, "DESCRIPTION")))
      if (file.exists(pkgDir)) {
        logfun(repo)(pkgName, "Calculating test coverage")
        pkgCovg <- tryCatch(package_coverage(path = pkgDir),
                            error = function(e) NULL)
        percentCovg <- tryCatch(percent_coverage(pkgCovg),
                                error = function(e) NULL)
        if(is.null(percentCovg) || is.nan(percentCovg)) {
            label <- "label-danger"
        } else if (percentCovg > 90) {
          label <- "label-success"
        } else if (percentCovg > 75) {
          label <- "label-warning"
        } else {
          label <- "label-danger"
        }
        cvgrptfile <- file.path(coverageDir, paste0(pkgName, "-covr-report.html"))
        dummy <- tryCatch(report(pkgCovg, file = cvgrptfile, browse = FALSE),
                          error = function(e) NULL)
        logfun(repo)(pkgName, "Completed test coverage")
        if (is.numeric(percentCovg) && !is.nan(percentCovg)) {
          paste("<span class=\"label", label, "\">",
                paste(round(percentCovg, digits = 2), "%"), "</span>")
        } else {
          "<span class=\"label label-default\">Details</span>"
        }
      }
    }, pkgName = bres$name, subdir = mandf$subdir, mc.cores = cores))

    logfun(repo)("NA", paste("Completed test coverage reports for",
                             length(bres$name),"packages."))
    covg <- as.data.frame(coverage)
    covg$name <- rownames(covg)

    # Covr calulcation preservation
    covr_hist <- file.path(destination(repo), ".prevcovr.df")
    if (!file.exists(covr_hist)) {
      write.table(covg, file = covr_hist, row.names = FALSE)
    } else {
      old_covg <- as.data.frame(read.table(covr_hist, header = TRUE))
      combo <- rbind(old_covg, covg)
      latest <- combo[!(combo$name %in% old_covg$name &
                        combo$coverage %in% old_covg$coverage), ]
      common <- tryCatch(suppressWarnings(intersect(old_covg, covg)),
                        error = function(e)
                        NULL)
      remnants <- old_covg[!(old_covg$name %in% covg$name), ]
      combo <- rbind(latest, common, remnants)
      covg <- combo
      write.table(covg, file = covr_hist, row.names = FALSE)
    }

    return(covg)
}
