#' @importFrom glue glue
#' @importFrom IRkernel installspec
NULL

# ---- internal commons --------------------------------------------------------

get_os <- function(){
  if("windows" %in% tolower(.Platform$OS.type)){
    return("windows")
  }
  os <- tolower(R.version$os)
  if(startsWith(os, "darwin")){
    return('darwin')
  }
  if(startsWith(os, "linux")){
    return('linux')
  }
  if(startsWith(os, "solaris")){
    return('solaris')
  }
  if(startsWith(os, "win")){
    return('windows')
  }
  return('unknown')
}

is_arm <- function(){
  grepl("arch64", R.version$arch)
}

R_user_dir <- function (package, which = c("data", "config", "cache")) {
  stopifnot(is.character(package), length(package) == 1L)
  which <- match.arg(which)
  home <- normalizePath("~")
  path <- switch(which, data = {
    if (nzchar(p <- Sys.getenv("R_USER_DATA_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_DATA_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("APPDATA"), "R", "data")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Application Support", "org.R-project.R")
    else file.path(home, ".local", "share")
  }, config = {
    if (nzchar(p <- Sys.getenv("R_USER_CONFIG_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CONFIG_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("APPDATA"), "R", "config")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Preferences", "org.R-project.R")
    else file.path(home, ".config")
  }, cache = {
    if (nzchar(p <- Sys.getenv("R_USER_CACHE_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CACHE_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Caches", "org.R-project.R")
    else file.path(home, ".cache")
  })
  file.path(path, "R", package)
}


setwd2 <- function(d, quiet = FALSE){
  d <- normalizePath(d, mustWork = TRUE)

  parent_frame <- parent.frame()

  if(identical(parent_frame, .GlobalEnv)){
    warning("`setwd2` should not be called directly")
  }

  o <- getwd()

  expr <- bquote({
    message("Restore working directory -> ", .(o))
    setwd(.(o))
  })
  do.call(
    on.exit, list(expr, add = TRUE, after = TRUE),
    envir = parent_frame
  )
  if(!quiet){
    message("Setting working directory -> ", d)
  }
  setwd(d)
}

rand_string <- function (length = 50) {
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE),
        collapse = "")
}

package_installed <- function(pkg, ...) {
  isTRUE(system.file(package = pkg, ..., mustWork = FALSE) != "")
}

run_package_function <- function(
    ns, fun, ...,
    .on_failure = c("error", "warning", "ignore")) {

  .on_failure <- match.arg(.on_failure)

  tryCatch({
    withRestarts({
      f <- asNamespace(ns)[[fun]]
      if (!is.function(f)) {
        stop(sprintf("Cannot find package function %s::%s", ns, fun))
      }
      return(f(...))
    }, abort = function(...){})
  }, error = function(e) {
    if( .on_failure == "error" ) {
      stop(e)
    } else if( .on_failure == "warning" ) {
      warning(e)
    }
    invisible(e)
  })
}


rpymat_has_version <- function(min = NA) {
  if(system.file(package = "rpymat") == "") { return(FALSE) }
  if(is.na(min)) { return(TRUE) }

  rpymat_ver <- utils::packageVersion("rpymat")
  if( utils::compareVersion(as.character(rpymat_ver), min) < 0 ) {
    return(FALSE)
  }
  return(TRUE)
}


#' Whether supports customized \code{'conda'} environment
#' @returns true or false on whether multiple customized environment is
#' supported
#' @export
custom_env_support <- function() {
  rpymat_has_version("0.1.7.1")
}
