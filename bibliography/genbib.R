citepkgs = c("repmis", "miniCRAN", "devtools", "packrat")
lapply(citepkgs, function(x) if(!require(x)) try(install.packages(x)))

con = file("package.bib", "w")
lapply(citepkgs, function(pkg) cat(citation(package = pkg), con = con))


