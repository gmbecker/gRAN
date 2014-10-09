buildAvailable = function(repo_manifest, dep_repos, dir, fakerepo) {
    depavail = available.packages(contrib.url(dep_repos))

    gh_avail = t(apply(repo_manifest,1, .make_avail_row, dir = dir, fakerepo = fakerepo))

    if(ncol(gh_avail) != ncol(depavail))
        stop("github generated available packages matrix does not have the same number of columns as available.packages. Please report this to the maintainer")
    rownames(gh_avail)  = repo_manifest$name
    avail = rbind(depavail, gh_avail)
    avail
}

##https://raw.githubusercontent.com/ggobi/cranvas/master/DESCRIPTION
.make_avail_row = function(manRow, dir, fakerepo) {
    destfile = file.path(dir, paste0(manRow["name"], "_DESCRIPTION"))
    if(!file.exists(destfile)) {
        stripped1 = gsub("\\.git", "", manRow["url"])
        stripped2 = gsub(".*github.com", "", stripped1)
        sdir = if(manRow["subdir"] == ".")
            "/"
        else
            paste0("/",manRow["subdir"] , "/")
        dcf_url = paste0("https://raw.githubusercontent.com",stripped2,"/",  manRow["branch"], sdir, "DESCRIPTION")
        
        
        res = download.file(dcf_url, destfile, method="wget")
        if(res)
            stop("unable to download DESCRIPTION")
    }

    dcf = read.dcf(destfile)
    fields = colnames(dcf)
    .dcfField = function(field, default = NA) {
        if(field %in% colnames(dcf)) unname(dcf[1, field]) else NA
    }
    row = c(Package = manRow["name"],
          Version = "999.9-9",
          Priority = NA,
          Depends = .dcfField("Depends"),
          Imports = .dcfField("Imports"),
          LinkingTo = .dcfField("LinkingTo"),
          Suggests = .dcfField("Suggests"),
          Enhances = NA,
          License = .dcfField("License"),
          License_is_FOSS = NA,
          License_restricts_use = NA,
          OS_type = .dcfField("OS_type"),
          Archs = NA,
          MD5sum = .dcfField("MD5sum"),
          NeedsCompilation = .dcfField("NeedsCompilation"),
          File = NA,
          Repository = fakerepo)

    as.matrix(row, nrow = 1, dimnames = list(manRow["name"], names(row)))
}
                
basepkgs = installed.packages(priority="base")[, "Package"]

##' @export
lazyGithubRepo = function(pkgs, ghMan, depRepos = getOption("repos"), avail = available.packages(contrib.url(depRepos)), dir = tempdir(), forceRefresh = FALSE, reppath = file.path(dir, "repo"), grabSuggests = FALSE, verbose = FALSE) {

    pkgsNeeded = pkgs

    repdir = file.path(reppath, "src", "contrib")
    dir.create(repdir, recursive = TRUE)
    fakerepo = paste0("file://", normalizePath(repdir))
    innerFun = function(zipUrl, pkgname, branch) {
        if(is.null(dim((avail))))
            avail = t(as.matrix(avail)) ## if we only select 1 row we get a character :(
        if(pkgname %in% avail[,"Package"] || pkgname %in% basepkgs) {
            if(verbose)
                message(sprintf("Package %s already available from repository at %s", pkg, avail[avail[,"Package"] == pkg, "Repository"]))
            pkgsNeeded <<- setdiff(pkgsNeeded, pkgname)
            return()
        }
        if(verbose)
            message(sprintf("Retrieving package %s from %s (branch %s)", pkgname, zipUrl, branch)) 

        zpfile =  file.path(dir, paste0(pkgname, "_", gsub(".*/(.*)", "\\1", zipUrl)))
        if(!file.exists(zpfile) || forceRefresh)
            success = download.file(zipUrl, zpfile, method = "wget")
        else
            success = 0
        if(success > 0)
            stop("Unable to get package zip file from git")

        uzdir = file.path(dir, paste(pkgname, branch, sep="-"))
        unzip(zpfile, exdir = dir)

        dcf = read.dcf(file.path(uzdir, "DESCRIPTION"))
        fields = colnames(dcf)
        .dcfField = function(field, default = NA) {
            if(field %in% colnames(dcf)) unname(dcf[1, field]) else NA
        }
        row = c(Package = pkgname,
            Version = .dcfField("Version"),
            Priority = NA,
            Depends = .dcfField("Depends"),
            Imports = .dcfField("Imports"),
            LinkingTo = .dcfField("LinkingTo"),
            Suggests = .dcfField("Suggests"),
            Enhances = NA,
            License = .dcfField("License"),
            License_is_FOSS = NA,
            License_restricts_use = NA,
            OS_type = .dcfField("OS_type"),
            Archs = NA,
            MD5sum = .dcfField("MD5sum"),
            NeedsCompilation = .dcfField("NeedsCompilation"),
            File = NA,
            Repository = fakerepo)

        rawdeps = c(row["Depends"],
            row["Imports"],
            row["LinkingTo"],
            if(grabSuggests) row["Suggests"] else NULL)
        rawdeps = rawdeps[!is.na(rawdeps)]
        newreqs = unlist(sapply(rawdeps, tools:::.extract_dependency_package_names))
        newreqs = unique(newreqs[!newreqs %in% c(avail[,"Package"], pkgsNeeded, basepkgs)])

        cmd = paste("cd", repdir, "; R CMD build --no-resave-data --no-build-vignettes", uzdir)
        res = tryCatch(GRANBase:::system_w_init(cmd, intern=TRUE), error = function(x) x)
        if(is(res, "error"))
            stop(paste("Unable to build package", res))
        
        ##update
        pkgsNeeded <<- setdiff(c(pkgsNeeded, newreqs), pkgname)
        
        avail <<- rbind(avail, t(as.matrix(row)))
                     #   matrix(row, nrow = 1, dimnames = list(pkgname, names(row))))

    }
    
    force(avail)
    avail = avail[!avail[,"Package"] %in% ghMan$name,]
    cnt =1 
    while(length(pkgsNeeded) && cnt < 1000){
        pkg = pkgsNeeded[1]
   #     if(!pkg %in% avail[,"Package"])  {
        if(pkg %in% ghMan$name) {
            manrow = ghMan[ghMan$name == pkg, ]
            ##https://github.com/gmbecker/ProteinVis/archive/IndelsOverlay.zip  for IndelsOverlay branch
            zipurl = paste0(gsub("\\.git", "", manrow$url), "/archive/", manrow$branch, ".zip")
            innerFun(zipurl, pkg, manrow$branch)
        } else
            stop(sprintf("Unable to locate package %s", pkg))

    #    }
        cnt = cnt + 1
    }
    
    write_PACKAGES(repdir)
    fakerepo
}


##' @export
setGeneric("Install", function(pkgs, repos, verbose = FALSE, ...) standardGeneric("Install"))

setMethod("Install", c("character", "character"), function(pkgs, repos, verbose, ...) install.packages(pkgs, repos = repos, ...))

setMethod("Install", c("character", "GithubPkgManifest"), function(pkgs, repos, verbose, ...) {

    ghrepo= lazyGithubRepo(pkgs, manifest(repos), depRepos(repos), verbose = verbose)
    avail1 = available.packages(ghrepo)
    avail2 = available.packages(contrib.url(depRepos(repos)))
    browser()
    new = !avail2[,"Package"] %in% avail1[,"Package"]
    avail = rbind(avail1, avail2[new,])
    install.packages(pkgs, available = avail, ...)
})
    
    
     
