.onLoad <- function(libname, pkgname) {
  registerS3method("as.matrix", "list", as.matrix.list)
  registerS3method("as.vector", "chrongler_period", as.vector.chrongler_period)
}
