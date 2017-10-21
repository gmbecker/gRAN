if (!require(parallel)) {
  mclapply2 = function(X,
                       FUN,
                       ...,
                       mc.preschedule = TRUE,
                       mc.set.seed = TRUE,
                       mc.silent = FALSE,
                       mc.cores = getOption("mc.cores", 3L),
                       mc.cleanup = TRUE,
                       mc.allow.recursive = TRUE)
    lapply(X, FUN, ...)

  mcmapply2 = function(FUN,
                       ...,
                       MoreArgs = NULL,
                       SIMPLIFY = TRUE,
                       USE.NAMES = TRUE,
                       mc.preschedule = TRUE,
                       mc.set.seed = TRUE,
                       mc.silent = FALSE,
                       mc.cores = getOption("mc.cores", 3L),
                       mc.cleanup = TRUE) {
    mapply(
      FUN,
      ...,
      MoreArgs = MoreArgs,
      SIMPLIFY = SIMPLIFY,
      USE.NAMES = USE.NAMES
    )
  }
} else {
  mclapply2 = mclapply
  mcmapply2 = mcmapply
}
