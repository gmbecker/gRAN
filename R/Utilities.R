checkIsPkgDir = function (dir)
{
    fils = list.files(dir)
    any(grepl("^DESCRIPTION$", fils))
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
    targs = 
    
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

findPkgDir = function(rootdir, branch, subdir, repo)
{
                   
    ret = NULL
    name = basename(rootdir)
    #does it have the trunk, branches, tags layout?
    if(checkStdSVN(rootdir))
    {
        if(is.null(branch) || branch %in% c("master", "trunk"))
        {
            ret = file.path(rootdir, "trunk")
        } else {
            ret = file.path(rootdir, "branches", branch)
        }
    } else if(is.null(branch) || branch %in% c("master", "trunk")) {
        ret = rootdir
    } else {
        warning(paste0("The svn repository at ", location(source),
                       " does not appear to have branches. ",
                       "Unable to process this source."))
        logfun(repo)(name, paste("The SVN repository does not appear to have",
                                 "branches and a non-trunk/non-master branch",
                                 "was selected"), type="both")
        return(NULL)
    }

    ret = file.path(ret, subdir)
    ##we somehow got a return file that doesn't exist on the file system.
    ##This is a problem with GRAN logic, not with packages/user activity
    if(!file.exists(ret))
    {
        logfun(repo)(name, paste("Unable to find subdirectory", subdir,
                                 "in branch", branch), type="both")
        warning(paste0("Constructed temporary package directory",ret,
                       " doesn't appear to  exist after svn checkout. ",
                       "Missing branch?"))
        return(NULL)
    }
    
    ##Find a package. First look in ret, then in ret/package and ret/pkg
    ##we could be more general and allow people to specify subdirectories...
    if(!checkIsPkgDir(ret))
    {
        logfun(repo)(name, paste("Specified branch/subdirectory combination",
                                 "does not appear to contain an R package"),
                                  type="both")
        ret = NULL
    }
    ret
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

makeUserFun = function(scm_auth, url)
    {
        ind = sapply(names(scm_auth), function(pat) grepl(pat, url, fixed=TRUE))
        if(any(ind))
            scm_auth[[which(ind)]][1]
        else
            ""

    }

makePwdFun = function(scm_auth, url)
    {
        ind = sapply(names(scm_auth), function(pat) grepl(pat, url, fixed=TRUE))
        if(any(ind))
            scm_auth[[which(ind)]][2]
        else
            ""
       
    }


makeSource = function(url, type, user, password, scm_auth, prefer_svn = FALSE, ...) {
    type = tolower(type)
    if(missing(user))
        user = makeUserFun(scm_auth = scm_auth, url = url)
    if(missing(password))
        password = makePwdFun(scm_auth= scm_auth, url = url)
    if(type == "git" && grepl("github", url))
        type = "github"

    ret = switch(type,
           svn  = new("SVNSource", location = url, user = user,
               password = password, ...),
           local = new("LocalSource", location = url, user = user,
               password= password, ...),
           git = new("GitSource", location = url, user = user,
               password = password,  ...),
           github = new("GithubSource", location = url, user = user,
               password = password, ...),
        cran = new("CRANSource", location = url, user = "", password = ""),
        bioc = new("BiocSource", locatoin = url, user = "readonly", password = "readonly"),
           stop("unsupported source type")
           )
    if( (type=="git" || type == "github") && is.na(ret@branch))
        ret@branch = "master"
    else if (type=="svn" && is.na(ret@branch))
        ret@branch = "trunk"
    if(is(ret, "GitSource") && prefer_svn) {
        ret2 = tryCatch(as(ret, "SVNSource"), error = function(x) x)
        if(!is(ret2, "error"))
            ret = ret2
    }
    ret
}

getPkgDir = function(basepath,name,  subdir, scm_type, branch)
{

    
    basepath = normalizePath2(basepath)
    if(!file.exists(file.path(basepath, name)))
        stop("directory not found")
    ##svn
    if(file.exists(file.path(basepath, name, ".svn")))
    {
        if(checkStdSVN(file.path(basepath, name)))
        {
            if(is.na(branch) || branch == "trunk" || branch == "master")
                brdir = "trunk"
            else
                brdir = file.path("branches", branch)

        } else {
            brdir = "."

        }
    } else ## git or local, neither have explicit dirs for branching
        brdir = "."

    normalizePath2(file.path(basepath,name, brdir, subdir))
}


#system(..., intern=TRUE) throws an error if the the command fails,
#and has attr(out, "status") > 0 if the called program returns non-zero status.
errorOrNonZero = function(out)
{
    if(is(out, "error") ||
       (!is.null(attr(out, "status")) && attr(out, "status") > 0))
        TRUE
    else
        FALSE
}

isOkStatus = function(status= repo_results(repo)$status,
    repo)
{
    #status can be NA when the package isn't being built at all
    !is.na(status) & (status == "ok" | status == "ok - not tested" |
                      (check_warn_ok(repo) & status == "check warning(s)") |
                      (check_note_ok(repo) & status == "check note(s)"))
}

install.packages2 = function(pkgs, repos, ...)
{
    outdir = tempdir()
    wd = getwd()
    on.exit(setwd(wd))
    setwd(outdir)
    ## the keep_outputs=dir logic doesn't work, the files just
    ##end up in both locations!
    ##install.packages(pkgs, ..., keep_outputs=outdir)
    avail = available.packages(contrib.url(repos))
    install.packages(pkgs, repos, ..., keep_outputs=TRUE)
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


normalizePath2 = function(path, follow.symlinks=FALSE)
    {
        
        if(follow.symlinks || Sys.info()["sysname"]=="Windows")
            normalizePath(path)
        else {
            if(substr(path, 1, 1) == "~")
                path = path.expand(path)
            ##paths starting with / for example
            else if(substr(path, 1, 1) == .Platform$file.sep)

                path  = path
            else if (substr(path, 1, 2) == "..") {
                tmppath = getwd()
                while(substr(path, 1, 2) == "..") {
                    tmppath = dirname(tmppath)
                    path = substr(path, 3, nchar(path))
                    if(substr(path, 1, 1) == .Platform$file.sep)
                        path = substr(path, 2, nchar(path))
                }
                path = file.path(tmppath, path)
            } else if(grepl("^\\.*[[:alnum:]]", path))
                path = file.path(getwd(), path)
            else if (substr(path, 1,1) == ".")
                path = file.path(getwd(), substr(path,2, nchar(path)))
            path = gsub(paste(rep(.Platform$file.sep, 2), collapse=""), .Platform$file.sep, path, fixed=TRUE)
            path
            
        }
    }
