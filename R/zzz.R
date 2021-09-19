.onLoad <- function(libname, pkgname) {
  # backports::import(pkgname, c("R_user_dir", "deparse1"))
  pkg <- getNamespace(pkgname)

  options("rpymat.matlab_engine" = fastqueue2())
}
