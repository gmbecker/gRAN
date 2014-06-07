getRVersion <- function() {
  paste0(R.Version()[["major"]],
         ".",
         R.Version()[["minor"]])
}
