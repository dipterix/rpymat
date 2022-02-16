get_os <- function () {
  if ("windows" %in% stringr::str_to_lower(.Platform$OS.type)) {
    return("windows")
  }
  os <- stringr::str_to_lower(R.version$os)
  if (stringr::str_detect(os, "^darwin")) {
    return("darwin")
  }
  if (stringr::str_detect(os, "^linux")) {
    return("linux")
  }
  if (stringr::str_detect(os, "^solaris")) {
    return("solaris")
  }
  if (stringr::str_detect(os, "^win")) {
    return("windows")
  }
  return("unknown")
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

