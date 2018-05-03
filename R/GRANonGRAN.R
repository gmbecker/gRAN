# Package, build thine self, and create thine baby
#' @importFrom utils install.packages
#' @import GRANCore
#' @import switchr
#' @import methods
GRANonGRAN <- function(repo) {
    logfun(repo)("GRAN",
        paste("Creating repository specific GRAN package and",
              "installing it into the GRAN repository at",
              destination(repo)))
    
    tmpdir <- repobase(repo)
    pkgname <- paste0("GRAN", repo_name(repo))
    babyGRAN <- file.path(tmpdir, pkgname)
    if (file.exists(babyGRAN))
        unlink(babyGRAN, recursive = TRUE, force = TRUE)
    dirs <- file.path(babyGRAN, c("inst/scripts", "R"))
    sapply(dirs, dir.create, recursive = TRUE)
    GRANRepo <- repo
    fils <- list.files(system.file2("GRAN",
                                    package = "GRANBase"), recursive = TRUE)
    res <- file.copy(file.path(system.file2("GRAN",
                                            package = "GRANBase"), fils),
                     file.path(babyGRAN, fils),
                     overwrite = TRUE)
    
    if (any(!res))
        stop("Copy failed")
    saveRepo(GRANRepo, filename = file.path(babyGRAN, "inst", "myrepo.R"))
    code <- paste0("getGRAN = function(...) { install.packages('",
                   pkgname,
                   "', repos = c('",
                   repo_url(repo),
                   "', getOption('repos'))) }; getGRAN(type='source')")
    cat(code, file = file.path(babyGRAN, "inst", "scripts", "getGRAN.R"))
    cat(code, file = file.path(dest_base(repo),
                               paste0("getGRAN-", repo_name(repo), ".R")))
    DESC <- readLines(file.path(babyGRAN, "DESCRIPTION"))
    DESC[1] <- paste0("Package: ", pkgname)
    writeLines(DESC, con = file.path(babyGRAN, "DESCRIPTION"))
    cat(paste0("pkgname = '", pkgname, "'"),
        file = file.path(babyGRAN, "R", "00packagename.R"))
    
    repo <- addPkg(repo,
                   name = pkgname,
                   url = babyGRAN,
                   type = "local",
                   subdir = ".",
                   replace = TRUE)
    ## addPkg doesn't reset the results
    granInd <- which(repo_results(repo)$name == pkgname)
    repo_results(repo)[granInd, ] <- ResultsRow(name = pkgname)
    
    ##    cran_use_ok = use_cran_granbase(repo)
    cran_use_ok <- FALSE
    
    if (cran_use_ok) {
        ## This should give us GRANBase, switchr, and dependencies
        res <- tryCatch(install.packages("GRANBase",
                                         dependencies = TRUE, lib = temp_lib(repo)),
                        error = function(e) e)
        if (is(res, "error"))
            cran_use_ok <- FALSE
    }
    
    if (!cran_use_ok) {
        ## Force switchr, GRANBase, and GRANCore into the manifest and make them build
        ## Don't build them if there has been no version bump
        
        pkgs <- c("switchr",     #[1]
                  "GRANBase",
                  "GRANCore")    #[2]
        old_df <- repo_results(repo)
        mini_df <- old_df[old_df$name %in% pkgs, ]
        old_switchr_ver <- mini_df$lastbuiltversion[mini_df$name == pkgs[1]]
        old_gran_ver <- mini_df$lastbuiltversion[mini_df$name == pkgs[2]]
        old_gran_ver <- mini_df$lastbuiltversion[mini_df$name == pkgs[3]]
        
        ## Construct raw Github URLs for DESCRIPTION files of GRAN and switchr
        github_user <- "gmbecker"
        github_base_url <- "https://github.com"
        github_raw_base <- "https://raw.githubusercontent.com"
        repo_names <- c("switchr", #[1]
                        "gran", #[2]
                        "grancore")    #[3]
        repo_urls <- paste0(github_base_url, "/", github_user, "/", repo_names)
        raw_desc_urls <- paste0(github_raw_base, "/", github_user,
                                "/", repo_names, "/master/DESCRIPTION")
        switchr_conn <- url(raw_desc_urls[1])
        gran_conn <- url(raw_desc_urls[2])
        grancore_conn <- url(raw_desc_urls[3])
        
        ## Use read.dcf to extract the live versions of GRANBase and switchr
        curr_switchr_ver <- read.dcf(switchr_conn, fields = "Version")
        curr_gran_ver <- read.dcf(gran_conn, fields = "Version")
        curr_grancore_ver <- read.dcf(gran_conn, fields = "Version")
        
        ## Close the open URL connections
        close(switchr_conn)
        close(gran_conn)
        close(grancore_conn)
        
        ## Replace NAs, NULLs or character(0) with 0.0-0
        check_invalid <- function(x) {
            if (is.na(x) || is.null(x) || identical(x, character(0))) {
                return("0.0-0")
            } else {
                return(x)
            }
        }
        old_switchr_ver <- check_invalid(old_switchr_ver)
        old_gran_ver <- check_invalid(old_gran_ver)
        old_grancore_ver <- check_invalid(old_grancore_ver)
        
        ## if old_switchr_ver is not the same as curr_switchr_ver, or
        ## if old_gran_ver is not the same as curr_gran_ver,
        ## then force them into the repo and build them
        ## Otherwise, don't force them in
        force_switchr <- compareVersion(curr_switchr_ver, old_switchr_ver) == 1
        force_gran <- compareVersion(curr_gran_ver, old_gran_ver) == 1
        force_grancore <- compareVersion(curr_grancore_ver, old_grancore_ver) == 1
        if (force_switchr || force_gran || force_grancore) {
            repo <- addPkg(repo,
                           name = pkgs,
                           url = repo_urls,
                           type = "git",
                           replace = TRUE)
            
            df = repo_results(repo)
            df[df$name %in% pkgs, "building"] = TRUE
            df[df$name %in% pkgs, "lastbuiltversion"] = "0.0-0"
            
            repo_results(repo) = df
        }
    }
    repo
}
