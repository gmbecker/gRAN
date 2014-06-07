##Called by Jenkins jobs for individual packages
##
## Assumes package has already been added to manifest, and builss only the specified package (and any dependencies/reverse dependencies).

## command is of the form 'Rscript buildSinglePkg.R --repodir dir/to/subrepo  --package pkgname'

args = commandArgs(trailingOnly = TRUE)

repodir = args[1]
pkg = args[2]

##loads an object called repo, which we will use throughout
load(file.path(repodir, "repo.rda"))

if(is.null))
    repo@manifest = readManifest(repo)

if(! pkg %in% repo@manifest$name)
    stop(sprintf("The package %s does not appear to be in the GRAN manifest.", pkg))

makeSingleGRANRepo(repo = repo,
                   onlyBuild = pkg)
                   
