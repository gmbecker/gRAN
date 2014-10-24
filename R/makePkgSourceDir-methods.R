#setMethod("makePkgSourceDir", c(name = "ANY", source = "SVNSource"), function(name, source, path, repo) {
setMethod("makePkgDir", c(name = "ANY", source = "SVNSource"),
          function(name, source, path, latest_only = FALSE, repo,
                   forceRefresh = FALSE) {
    oldwd = getwd()
    on.exit(setwd(oldwd))
    if(!file.exists(path))
        dir.create(path, recursive = TRUE)
    setwd(path)
    
    if(missing(name))
        name = basename(location(source))
    
    opts = character()
    if(length(source@user) && nchar(source@user))
        opts = paste(opts, "--username", source@user)
    if(length(source@password) && nchar(source@password))
        opts = paste(opts, "--password", source@password)
    
    #did we already check it out?
    if(file.exists(name) && file.exists(name, ".svn") && !forceRefresh)
    {
        writeGRANLog(name, "Existing temporary checkout found at this location. Updating", repo =  repo)
        up = updateSVN(file.path(path, name), source, repo)
        #if(!up)
        #    return(FALSE)
    } else {
        ## clean up the directory if it was created from some other type of source
        if(file.exists(name))
            unlink(name, recursive = TRUE)
        cmd = paste("svn co", location(source), name, opts)
        writeGRANLog(name, paste("Attempting to create temporary source",
                                  "directory from SVN repo", location(source),
                                 "(branch", source@branch, "; cmd", cmd, ")"),
                     repo = repo)        
        out = tryCatch(system_w_init(cmd, repo = repo), error = function(x) x)
        if(is(out, "error"))
        {
            msg = c(paste("Temporary SVN checkout failed. cmd:", cmd), out$message)
            writeGRANLog(name, msg, type="error", repo = repo)
            return(FALSE)
        }
    }
    rtdir = file.path(path, name)

    ret = !is.null(findPkgDir(rtdir, branch(source), source@subdir, repo = repo))

    #success log
    if(ret)
    {
        writeGRANLog(name,
                     paste("Temporary source directory successfully created:",
                           ret),
                     repo = repo)
    }
    ret
})


setMethod("makePkgDir", c(name = "ANY", source = "GithubSource"),
          function(name, source, path, latest_only = FALSE,  repo, forceRefresh = FALSE)
      {
          if(latest_only) {
              ##https://github.com/gmbecker/ProteinVis/archive/IndelsOverlay.zip
              ## for IndelsOverlay branch
              zipUrl = paste0(gsub("\\.git", "", location(source)), "/archive/",
                  branch(source), ".zip")
              zipUrl = gsub("git://", "http://", zipUrl, fixed=TRUE)
              zpfile = normalizePath(file.path(path,
                  paste(name, "-", branch(source), ".zip", sep = "")))
              if(!file.exists(zpfile) || forceRefresh)
                  success = download.file(zipUrl, zpfile, method = "wget")
              else
                  success = 0
              if(success > 0)
                  stop("Unable to get package zip file from git")
              uzdir = file.path(path, paste(name, branch(source), sep="-"))
              unzip(zpfile, exdir = path)
              uzdir
          } else {
              source = as(source, "SVNSource", strict = TRUE)
              makePkgDir(name, source, path, latest_only, repo, forceRefresh)
          }
      })

#setMethod("makePkgSourceDir", c(name = "ANY", source = "GitSource"), function(name, source, path,  repo) {
setMethod("makePkgDir", c(name = "ANY", source = "GitSource"),
          function(name, source, path, latest_only = FALSE,  repo, forceRefresh=FALSE)
      {
          oldwd = getwd()
          on.exit(setwd(oldwd))
          if(!file.exists(path))
              dir.create(path, recursive = TRUE)
          setwd(path)
          sdir = location(source)
          if(file.exists(name) &&
             file.exists(file.path(name, ".git"))) {
              writeGRANLog(name, "Existing temporary checkout found at this location. Updating", repo =  repo)
              up = updateGit(file.path(path, name), source, repo)
          } else {
              if(file.exists(name))
                  unlink(name)
              cmd = paste("git clone", sdir, name, ";cd", name, "; git checkout", branch(source))
              res = tryCatch(system_w_init(cmd, intern=TRUE, repo = repo),
                  error=function(x) x)
              if(is(res, "error") || (!is.null(attr(res, "status")) && attr(res, "status") > 0))
              {
                  writeGRANLog(name, paste("Failed to check out package source using command:", cmd),
                               type="both", repo = repo)
                  writeGRANLog(name, res, type="error", repo = repo)
                  return(FALSE)
              }
              writeGRANLog(name, paste0("Successfully checked out package source from ",
                                        sdir, " on branch ", branch(source)),
                           repo = repo)
          }
          rtdir = file.path(path, name)
          ret = !is.null(findPkgDir(rtdir, branch(source), source@subdir, repo=repo))
                                        #success log
          if(ret)
          {
              writeGRANLog(name, paste("Temporary source directory successfully created:", ret), repo = repo)
          }
          ret
      })
                                        #stub for everyone else
setMethod("makePkgDir", c(name = "ANY", source = "ANY"),
          function(name, source, path, latest_only, repo, forceRefresh = FALSE) {
    warning("Source type not supported yet.")
    FALSE
})

#setMethod("makePkgSourceDir", c(source="LocalSource"), function(name, source, path, latest_only = FALSE, repo) {
setMethod("makePkgDir", c(source="LocalSource"),
          function(name, source, path,  latest_only, repo, forceRefresh) {
    oldwd = getwd()
    on.exit(setwd(oldwd))
    if(!file.exists(path))
        dir.create(path, recursive = TRUE)
    setwd(path)
    
    if(missing(name))
        name = basename(location(source))
    
    writeGRANLog(name, "Copying local source directory into temporary checkout directory.", repo = repo)
    
   # ok= file.copy(location(source), file.path(path, name), recursive = TRUE)
    ok = file.copy(normalizePath2(location(source)), file.path(path), recursive=TRUE)
    if(basename(location(source)) != name)
    {
        writeGRANLog(name, "Renamed copied directory to package name.", repo = repo)
        ok = file.rename(file.path(path, basename(location(source))), file.path(path, name))
    }
    ret = normalizePath2(file.path(path, name, source@subdir))
    
                                        #success log
    if(ok)
    {
        writeGRANLog(name, paste("Temporary source directory successfully created:", ret), repo = repo)
    }
    ok
    
})
