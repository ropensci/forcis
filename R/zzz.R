.onLoad <- function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
  backports::import(pkgname, "R_user_dir", force = TRUE)
} # nocov end
