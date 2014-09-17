##' makeGRANRepos
##' Fire build procoess for a GRAN instance (all subrepositories)
##'
##' 
##' @title Build a set of GRAN repositories
##' @param manifest a data.frame containing the name, url, type of checkout, subdir, subrepo, branch, and extra. This single manifest should contain information for all repositories to be built. Overridden if repos are specified
##' @param Rlocs named character vector of the copies of the R executable to use for each of the subRepos. e.g. c(stable="R", dev = "/path/to/Rdevel/bin/R")
##' @param tmpRepoBase the base directory for the temporary intermediate repository
##' @param baseDir the base directory to use when building each repository.
##' @param tmpCheckout the directory the package sources will be checkedout into from version control before being built for each repository
##' @param logfiles character vector indicating the paths that the summary logfile should be written to for each repository
##' @param errfiles character vector indicating the paths that the error output logfile should be written to for each repository
##' @param tempLibLocs The temporary library location to use for each repository
##' @param checkWarnOk logical vector specifying whether packages that pass check with warnings should be included in each resulting respository
##' @param checkNoteOk logical vector specifying whether packages that pass check with notes should be included in  each resulting repository
##' @param cores integer indicating how many cores should be used to build the repositories
##' @param repos A list of existing GRANRepository objects to use when building the repositories.
##' @param auth character value indicating authentication required to add to manifest
##' @param scm_auths named list. Names are regular expressions which match scm (SVN, git) repositories. Elements are character vectors providing the username and password to checkout from the repository. Default is to use readonly:readonly for bioconductor.
##' @param dest_bases Base directory (or directories) to contain each named repository.
##' @param dest_urls The URLs where the repositories will be hosted. Defaults to file://<dest_bases>.
##' @param shell_inits The path to a file for the shell to source before running
##' system commands. E.g. a bashrc file.
##' @return A list of GRANRepository objects for the created repositoriesxs
##' @author Gabriel Becker, heavily adapted from a previous version by Cory Barr
##' @family buildRepos
##' @export
makeGRANRepos <- function(
    manifest = read.table(file.path(baseDir, subRepos, "GRANmanifest.dat"), header = TRUE, sep=",",
        stringsAsFactors=FALSE),
    Rlocs = c(stable = "R", dev = "Rdevel"),
    tmpRepoBase = "./tmpRepos",
    baseDir= "./GRANRepos",
    tmpCheckout = "./tmpSourceDirs",
    logfiles = file.path(baseDir, subRepos, paste0("GRANLog-", Sys.Date(), ".log")),
    errfiles = file.path(baseDir, subRepos, paste0("GRANErrors-", Sys.Date(), ".log")),
    tempLibLocs = file.path(baseDir, subRepos, "LibLoc"),
    checkWarnOk = FALSE,
    checkNoteOk = TRUE,
    cores = 3L,
    repos = list(NULL), #this will replicate out in the mapply
    auth = "",
    scm_auths = list(list()),
    dest_bases = normalizePath2(baseDir),
    dest_urls = paste0("file://", normalizePath2(dest_bases)),
    shell_inits = ""
    
    )
{
    if(!identical(repos, list(NULL))) {
        manis = lapply(repos, function(x) x@manifest)
        subRepos = sapply(repos, function(x) x@subrepoName)
    } else {
        if(is(manifest, "character"))
            manifest = read.table(manifest, header = TRUE, stringsAsFactors = FALSE)
        
        
        manis = split(manifest, manifest$subrepo)
        subRepos = names(manis)
    }
    
    #repos = file.path(baseDir, subRepos)
    tmprepos = file.path(tmpRepoBase, subRepos)
    tmpcheckouts = file.path(tmpCheckout, subRepos)
    if(missing(logfiles))
        logfiles = file.path(baseDir, subRepos, paste0("GRAN-log_", Sys.Date(), ".log"))
    if(missing(errfiles))
        errfiles = file.path(baseDir, subRepos, paste0("GRAN-errors_", Sys.Date(), ".log"))

    scm_auths = replicate(scm_auths, n = length(subRepos), simplify=FALSE)

    repoObs = mapply(makeSingleGRANRepo, tempRepo = tmprepos, baseDir = baseDir, tempCheckout = tmpcheckouts, logfile = logfiles, errlog = errfiles, rversion = Rlocs, subRepoName = subRepos, tempLibLoc = tempLibLocs, manifest = manis, checkWarnOk = checkWarnOk, checkNoteOk = checkNoteOk, cores = cores, repo = repos, auth = auth, scm_auth = scm_auths, dest_base = dest_bases, dest_url = dest_urls, shell_init = shell_inits)

       repoObs

}

##' makeSingleGRANRepo
##'
##' Create or rebuild a single GRAN repository or subset of packages therin.
##'
##' \code{makeSingleGRANRepo} creates or updates a single GRAN repository by performing the following steps:
##' \enumerate{
##' \item{Create a new GRANRepository object (if necessary) and set the \code{building} column of the manifest to \code{TRUE} for the packages to be built}
##' \item{Add a copy of the GRAN(Base) package to the manifest that uses the specified or newly created repository by default}
##' \item{Creates or updates checkouts of package source code for packages being built. If \code{onlyBuild} was NULL, any packages whose version number has not changed since the last successfull build are removed from the list of packages being built.}
##' \item{Searches the manifest for any packages which are reverse dependencies to the packages being built, and sets them to be built as well. (This step is skipped if all packages in the manifest are being built).}
##' \item{Builds tarballs of the packages whose source was successfully obtained/updated and uses them to create a temporary repository}
##' \item{Uses the temporary repository to install the packages which were successfully built for the temporary repository (and any dependencies) into the temporary library location for the repository. External dependencies are retreived from the Bioconductor CRAN and Bioc* repositories.}
##' \item{Using the libloc above, rebuilds tarballs for packages which successfully installed with vignettes and data resaving into a staging area}
##' \item{Performs R CMD check on the packages (tarballs) which were successfully rebuilt}
##' \item{Migrates the tarballs which passed check (based on the \code{checkWarnOk} and \code{checkNoteOk} settings for the repository) into the final destination repository.}
##' \item{Updates the manifest associated with the GRANRepo object to reflect this build cycle (lastbuilt* and lastAttempt* columns)}
##' \item{Creates build report from the updated manifest}
##' \item{Saves the manifest and the repo object}
##' }
##' @param manifest Manifest to use
##' @param subRepoName name of the repository to build
##' @param baseDir base directory for the building process
##' @param dest_base base directory for the resulting final repository
##' @param onlyBuild NULL (default) or a character vector naming packages to build. If non-null, packages not listed in this vector will not be built unless they are reverse dependencies of listed packages. Packages listed here are rebuilt even if the version number hasn't changed.
##' @param cores integer Number of cores to use during the build and testing processes
##' @param repo NULL (default) or a \code{GRANRepository} object. If specified, this repository object will be used rather than creating a new one.
##' @param scm_auth named list. Names are regular expressions which match scm (SVN, git) repositories. Elements are character vectors providing the username and password to checkout from the repository. Default is to use readonly:readonly for bioconductor.org.
##' @param ... Additional arguments which will be passed to \code{GRANRepository} to construct a new \code{GRANRepository} object. Ignored if \code{repo} is non-NULL.
##'
##' @return A \code{GRANRepository} object representing the created or updated repository.
##' @author Gabriel Becker
##' @family buildRepos
##' @export
makeSingleGRANRepo = function(
    manifest,
    subRepoName,
    baseDir,
    dest_base,
    onlyBuild = NULL,
    cores = 3L,
    repo = NULL,
    scm_auth = list("bioconductor.org" = c("readonly", "readonly")),
    ...

    ) {

    if(!(is.null(repo) || is(repo, "character") || is(repo, "GRANRepository"))) {
        stop("repo must be a GRANRepository object or the path of a directory containing a file named repo.R")
    } 

    ##can pass in a character containing the url of the (existing) repository
    if(is(repo, "character") )
    {
        repo = tryCatch(loadRepo(file.path(repo, "repo.R")), error = function(e) NULL)
    }

    if(missing(manifest) && !is.null(repo))
        manifest = repo@manifest

    if(is.null(repo)) {
        if(!(is(manifest, "character") || is(manifest, "data.frame"))) {
            stop("manifest must be a data.frame or the path of a directory containing a file named manifest.dat")
        }
        
        if(!is(dest_base, "character")) {
            stop("dest_base must be a character value")
        }
        
        if(!is(scm_auth, "list") || any(sapply(scm_auth, function(x) length(x) != 2))) {
            stop("scm_auth must be a list of two-element character vectors")
        }
    }
    if(!is(cores, "numeric") || cores <0 || cores != floor(cores)) {
        stop("cores must be a non-negative integer")
    }

    if(!is.null(onlyBuild) && !is(onlyBuild, "character")) {
        stop("onlyBuild must be NULL or a character vector of package names")
    }
        
        


    
    if(!is.null(repo))
        manifest = readManifest(repo = repo)
    
    if(is(manifest, "character"))
        manifest = read.table(manifest, header = TRUE, stringsAsFactors = FALSE, sep=",")

    if(!is(manifest, "data.frame"))
        stop("manifest doesn't seem to be or point to a GRAN manifest")
    if(is.null(manifest$suspended))
        manifest$suspended = FALSE
    
    #we start out trying to build everything in the manifest unless there are already instructions
    if( is.null(onlyBuild)) {
        manifest$building = TRUE
        manifest$building = manifest$building & (is.na(manifest$suspended) | !manifest$suspended)
        manifest$buildReason = ""
    } else {
        manifest$building = ifelse(manifest$name %in% onlyBuild, TRUE, FALSE)
        manifest$buildReason = ifelse(manifest$building, "forced", "")
        manifest$suspended[manifest$building] = FALSE
    }
    
    manifest$status[manifest$building] = "ok"

    if(is.null(repo)) {
        repo = GRANRepository(basedir = baseDir,
            subrepoName = subRepoName,
            destination = dest_base,
            ...)
    } else if (!missing(subRepoName)) {
        if(repo@subrepoName != subRepoName)
            stop("repository name on GRANRepository object and repository name passed to makeSingleGRANRepo do not match. Wrong repo object?")

    }
    repo@manifest = manifest    
    ##Don't want to accidentally build things we shouldn't
    if(length(unique(repo@manifest$subrepo))>1) {
        warning("multi-repo manifest passed to makeSingleRepo. Removing extraneous entries")
        repo@manifest = repo@manifest[repo@manifest$subrepo == repo@subrepoName,]
    }

    if(any(repo@manifest$suspended)) {
        writeGRANLog("NA", paste("Skipping suspended packages:", paste(repo@manifest$name[repo@manifest$suspended], collapse=", ")), repo = repo)
        warning(paste("Skipping suspended packages:", paste(repo@manifest$name[repo@manifest$suspended], collapse=", ")))
        
    }
    ##it is important that manifest rows <-> tarballs be a one-to-one relationship

    if(nrow(repo@manifest) != length(unique(repo@manifest$name))) {
        warning("Duplicated entries detected in manifest. Removing.")
        repo@manifest = repo@manifest[!duplicated(repo@manifest$name),]
    }

    print(paste("Building", sum(getBuilding(repo)), "packages"))
    #package, build thine self!
    repo = GRANonGRAN(repo)
    #do checkouts
    print(paste("Starting makeSrcDirs", Sys.time()))
    print(paste("Building", sum(getBuilding(repo)), "packages"))
    repo = makeSrcDirs(repo, cores = cores, scm_auth = scm_auth)
    #add reverse dependencies to build list
    repo = addRevDeps(repo)
    ##do checkouts again to grab reverse deps
    repo = makeSrcDirs(repo, cores = cores, scm_auth = scm_auth)
    #build temp repository
    print(paste("Starting buildBranchesInRepo", Sys.time()))
    print(paste("Building", sum(getBuilding(repo)), "packages"))
    ##if we have a single package specified we want to build it with or without a version bump
    repo = buildBranchesInRepo( repo = repo, temp = TRUE, cores = cores, incremental = is.null(onlyBuild))
    #test packges
    print(paste("Invoking package tests", Sys.time()))
    print(paste("Building", sum(getBuilding(repo)), "packages"))
    repo = invokePkgTests(repo, cores = cores)
    #copy successfully built tarballs to final repository
    print(paste("starting migrateToFinalRepo", Sys.time()))
    print(paste("Built", sum(getBuilding(repo)), "packages"))
    repo = migrateToFinalRepo(repo)
    
    finalizeRepo(repo)
    repo
}
