# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  on.exit(options(op), add = TRUE)
  options(useFancyQuotes = FALSE)
}
# nocov end
