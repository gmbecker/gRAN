
system.file2 = function(..., package = "GRANBase") {
    ret = tryCatch(system.file(..., package = package), error = function(e) e)

    if(!is.null(ret) && !is(ret, "error") && nchar(ret))
        return(ret)
    instp = installed.packages()
    if(!package %in% instp[,"Package"])
        return("")
    ind = which(instp[,"Package"] == package)[1]
    path = file.path(instp[ind,"LibPath"], package, ...)
    if(file.exists(path))
        return(path)
    else
        return("")
}


##' writeGRANLog
##'
##' Utility function which writes gran logs
##' @param pkg The name of the package the log is about
##' @param msg The log message, collapsed if length>1
##' @param type "full", "error", or "both" indicating which log(s) the message
##' should be written to
##' @param logfile The location of the full log file to write/append to
##' @param errfile the location of the error log file to write/append to
##' @note This function is not intended for direct use by the end user.
##' @export
writeGRANLog = function(pkg, msg, type = "full", logfile, errfile)
{
    
    dt = date()
    
    if(type == "error")
    {
        targ = errfile
        err = " ERROR "
    } else if (type == "both") {
        targ = c(logfile, errfile)
        err = " ERROR "
    } else {
        targ = logfile
        err = character()
    }

    
    fullmsg = paste("\n",err, "pkg:", pkg, "(", dt, ") - ",
        paste(paste0("\t",msg), collapse="\n\t"))
    sapply(targ, function(x) cat(fullmsg, append=TRUE, file=x))
}

getPkgNames = function(path)
{
    path = normalizePath2(path)
    if(length(path) > 1)
        sapply(path, getPkgNames)
    if(file.info(path)$isdir && file.exists(file.path(path, "DESCRIPTION")))
        read.dcf(file.path(path, "DESCRIPTION"))[1,"Package"]
    else if (grepl(".tar", path, fixed=TRUE))
        gsub(basename(path), "([^_]*)_.*", "\\1")
}


getCheckoutLocs = function(codir, manifest = manifest_df(repo),
    branch = manifest$branch, repo)
{
    mapply(getPkgDir, basepath = codir, subdir = manifest$subdir,
           scm_type = manifest$type, branch = branch, name = manifest$name)
}

getMaintainers = function(codir, manifest = manifest_df(repo),
    branch = manifest$branch, repo) {
    sapply(getCheckoutLocs(codir, manifest = manifest), function(x) {
        if(!file.exists(file.path(x,"DESCRIPTION")))
            NA
        else {
            ## some github packages don't know how to construct
            ## DESCRIPTION files ... *mumble*
            tryCatch(read.dcf(file.path(x, "DESCRIPTION"))[,"Maintainer"],
                     error = function(x) NA)
        }
    })
}

getCOedVersions = function(codir, manifest = manifest_df(repo),
    branch = manifest$branch, repo) {
    locs = getCheckoutLocs(codir, manifest = manifest,
        branch = branch, repo = repo)

    vers = lapply(unname(locs), function(x) {
                      dsc = tryCatch(readLines(file.path(x, "DESCRIPTION")), error= function(e) e)
                      if(is(dsc, "error")) {
                          v = NA_character_
                          names(v) = basename(x)
                          return(v)
                      }
                          
                      vline = grep("^[V|v]ersion:.*", dsc, value = TRUE)
                      v = gsub("^[V|v]ersion: (.*)$", "\\1", vline)
                      pline = grep("^[P|p]ackage:.*", dsc, value = TRUE)
                      names(v) = gsub(".*: (.*)", "\\1", pline)
                      v
                  })
    unlist(vers)
}


isOkStatus = function(status= repo_results(repo)$status,
    repo)
{
    #status can be NA when the package isn't being built at all
    !is.na(status) & (status == "ok" | status == "ok - not tested" |
                      (check_warn_ok(repo) & status == "check warning(s)") |
                      (check_note_ok(repo) & status == "check note(s)"))
}

install.packages2 = function(pkgs, repos, lib,  ..., param = SwitchrParam(),
    outdir = tempdir())
{

    if(!file.exists(outdir))
        dir.create(normalizePath(outdir), recursive=TRUE)
    wd = getwd()
    on.exit(setwd(wd))
    setwd(outdir)
    ## the keep_outputs=dir logic doesn't work, the files just
    ##end up in both locations!
    ##install.packages(pkgs, ..., keep_outputs=outdir)
     avail = available.packages(contrib.url(repos, type = "source"))
    ## args = list(pkgs = pkgs, repos = repos, lib = lib, ...,
    ##     INSTALL_opts = sprintf("-l %s", lib),
    ##     keep_outputs = outdir)
    

    ## tmpfile = tempfile(fileext=".rda")
    ## save(args, outdir, file =tmpfile)
    ## code = sprintf("load('%s'); wd = setwd(outdir);on.exit(setwd(wd)); .libPaths(args$lib); do.call(install.packages, args)", tmpfile)
    ## codefile = tempfile(pattern="instcode", fileext=".R")
    ## cat(code, file= codefile)
    ## cmd = paste0("R_LIBS_SITE=", lib, " R_LIBS_USER=",lib, " R", " --no-save <", codefile)
    ## system_w_init(cmd, param = param)
        install.packages(pkgs = pkgs, repos = repos,
                         INSTALL_opts = sprintf("-l %s", lib), lib = lib,
                         ..., keep_outputs=TRUE)
    ret = sapply(pkgs, function(p)
    {
        if(! p %in% avail[,"Package"])
            return("unavailable")
        fil = file.path(outdir, paste0(p, ".out"))
        tmp = readLines(fil)
        outcome = tmp[length(tmp)]
        if(grepl("* DONE", outcome, fixed=TRUE))
            "ok"
        else
            fil
    })
    ret
}


getBuilding = function(repo, results= repo_results(repo))
{
    results$building & isOkStatus( repo = repo)
}

getBuildingManifest = function(repo, results = repo_results(repo),
    manifest = manifest_df(repo))
{
    manifest[getBuilding(repo, results),]
}


getBuildingResults = function(repo, results = repo_results(repo))
{
    results[getBuilding(repo, results),]
}


## update_PACKAGES = function (dir = ".", fields = NULL, type = c("source", "mac.binary", 
##     "win.binary"), verbose = FALSE, unpacked = FALSE, subdirs = FALSE, 
##     latestOnly = TRUE, addFiles = FALSE) 
## {
##     if(!file.exists(file.path(dir, "PACKAGES"))) {
##         ret = write_PACKAGES(dir = dir, fields = fields, type = type, verbose = verbose,
##                        unpacked = unpacked, subdirs = subdirs,
##                        latestOnly = latestOnly, addFiles = addFiles)
##         return(ret)
##     }
##     exst = read.dcf(file.path(dir, "PACKAGES"))
##     allfields = colnames(exst)
    
##     if (missing(type) && .Platform$OS.type == "windows") 
##         type <- "win.binary"
##     type <- match.arg(type)
##     nfields <- 0
##     out <- file(file.path(dir, "PACKAGES"), "wt")
##     outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
##     paths <- ""
##     if (is.logical(subdirs) && subdirs) {
##         owd <- setwd(dir)
##         paths <- list.dirs(".")
##         setwd(owd)
##         paths <- c("", paths[paths != "."])
##     }
##     else if (is.character(subdirs)) 
##         paths <- c("", subdirs)
##     for (path in paths) {
##         this <- if (nzchar(path)) 
##             file.path(dir, path)
##         else dir
##         desc <- dodbstuff(this, type, 
##             verbose, unpacked, existing = exst)
##         if (length(desc)) {
##             Files <- names(desc)
##             fields <- names(desc[[1L]])
##             desc <- matrix(unlist(desc), ncol = length(fields), 
##                 byrow = TRUE)
##             colnames(desc) <- fields
##             if (addFiles) 
##                 desc <- cbind(desc, File = Files)
##             if (latestOnly) 
##                 desc <- .remove_stale_dups(desc)
##             license_info <- analyze_licenses(desc[, "License"])
##             desc[, "License"] <- ifelse(license_info$is_standardizable, 
##                 license_info$standardization, NA)
##             for (i in seq_len(nrow(desc))) {
##                 desci <- desc[i, !(is.na(desc[i, ]) | (desc[i, 
##                   ] == "")), drop = FALSE]
##                 write.dcf(desci, file = out)
##                 if (nzchar(path)) 
##                   cat("Path: ", path, "\n", sep = "", file = out)
##                 cat("\n", file = out)
##                 write.dcf(desci, file = outgz)
##                 if (nzchar(path)) 
##                   cat("Path: ", path, "\n", sep = "", file = outgz)
##                 cat("\n", file = outgz)
##             }
##             nfields <- nfields + nrow(desc)
##         }
##     }
##     close(out)
##     close(outgz)
##     invisible(nfields)
## }


## dodbupdate = function (dir, type = c("source", "mac.binary", 
##     "win.binary"), verbose = getOption("verbose"), unpacked = FALSE, existing)  {
##     type <- match.arg(type)
##     package_pattern <- switch(type, source = ".*_.*(\\.tar\\..*)$", 
##         mac.binary = ".*_.*(\\.tgz)$", win.binary = ".*_.*(\\.zip)$")
##     files <- list.files(dir, pattern = package_pattern)
##     fileext = gsub(package_pattern, "\\1", files[1])
##     files <- files[!sapply(files,
##                           function(x)
##                           x %in% paste0(existing$Package, "_",
##                                          existing$Version,
##                                          fileext))]
                                                    
##     if (!length(files)) 
##         return(list())
##     fields <- colnames(existing)
##     packages <- sapply(strsplit(files, "_", fixed = TRUE), "[", 
##         1L)
##     db <- vector(length(files), mode = "list")
##     names(db) <- files
##     op <- options(warn = -1)
##     on.exit(options(op))
##     if (verbose) 
##         message("Processing packages:")
##     if (type == "win.binary") {
##         files <- file.path(dir, files)
##         for (i in seq_along(files)) {
##             if (verbose) 
##                 message(paste(" ", files[i]))
##             con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
##             temp <- tryCatch(read.dcf(con, fields = fields)[1L, 
##                 ], error = identity)
##             if (inherits(temp, "error")) {
##                 close(con)
##                 next
##             }
##             db[[i]] <- temp
##             close(con)
##         }
##     }
##     else {
##         dir <- file_path_as_absolute(dir)
##         files <- file.path(dir, files)
##         cwd <- getwd()
##         if (is.null(cwd)) 
##             stop("current working directory cannot be ascertained")
##         td <- tempfile("PACKAGES")
##         if (!dir.create(td)) 
##             stop("unable to create ", td)
##         on.exit(unlink(td, recursive = TRUE), add = TRUE)
##         setwd(td)
##         for (i in seq_along(files)) {
##             if (verbose) 
##                 message(paste(" ", files[i]))
##             p <- file.path(packages[i], "DESCRIPTION")
##             temp <- try(utils::untar(files[i], files = p))
##             if (!inherits(temp, "try-error")) {
##                 temp <- tryCatch(read.dcf(p, fields = fields)[1L, 
##                   ], error = identity)
##                 if (!inherits(temp, "error")) {
##                   if ("NeedsCompilation" %in% fields && is.na(temp["NeedsCompilation"])) {
##                     l <- utils::untar(files[i], list = TRUE)
##                     temp["NeedsCompilation"] <- if (any(l == 
##                       file.path(packages[i], "src/"))) 
##                       "yes"
##                     else "no"
##                   }
##                   temp["MD5sum"] <- md5sum(files[i])
##                   db[[i]] <- temp
##                 }
##             }
##             unlink(packages[i], recursive = TRUE)
##         }
##         setwd(cwd)
##     }
##     if (verbose) 
##         message("done")
##     db = cbind(db, as.matrix(existing))
##     db = db[order(db[,"Package"]),]
##     db
## }
