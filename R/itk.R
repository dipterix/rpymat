
find_homebrew <- function(must_work = NA){
  # Sys.which("brew")
  if(getOption("rpymat.homebrew", default = "") != ""){
    path <- getOption("rpymat.homebrew", default = "")
  } else if(Sys.getenv("HOMEBREW_PREFIX") != ""){
    path <- file.path(Sys.getenv("HOMEBREW_PREFIX"), "bin", "brew")
  } else if(is_arm()) {
    path <- "/opt/homebrew/bin/brew"
  } else {
    path <- "/usr/local/bin/brew"  # TODO: check
  }
  if(!file.exists(path)){
    path <- Sys.which("brew")
  }
  path <- normalizePath(path, mustWork = FALSE)
  if(!file.exists(path)){
    msg <- "Cannot find HomeBrew. Please install homebrew via \n  rpymat::install_brew()"
    if(is.na(mustWork)){
      warning(msg)
    } else if(mustWork){
      stop(msg)
    }
  }
  path
}

find_cmake_darwin <- function(){
  # find locally installed cmake
  cmake <- Sys.which("cmake")
  if(cmake == ""){

    # find brew
    brew <- find_homebrew(must_work = TRUE)

    cmd <- sprintf('%s shellenv > /dev/null && which cmake',
                   shQuote(brew, type = "cmd"))
    cmake <- system(cmd, wait = TRUE, intern = TRUE)
    cmake <- cmake[[length(cmake)]]

    if(!length(cmake) || cmake == ""){
      cmd2 <- sprintf('%s shellenv > /dev/null && brew install cmake',
                      shQuote(brew, type = "cmd"))
      system(cmd2, wait = TRUE)
      cmake <- system(cmd, wait = TRUE, intern = TRUE)
      cmake <- cmake[[length(cmake)]]
    }

  }
  cmake
}

#' @export
brew_install <- function(package, cask = FALSE, wait = TRUE, intern = FALSE,
                         ..., dry_run = FALSE){
  brew <- find_homebrew(must_work = TRUE)
  cmd <- sprintf(
    '%s shellenv > /dev/null && brew install %s%s',
    shQuote(brew, type = "cmd"),
    ifelse(cask, "--cask ", ""),
    shQuote(package, type = "cmd")
  )
  if(dry_run){
    return(cmd)
  }
  system(cmd, wait = wait, intern = intern)
}

#' @export
find_cmake <- function(){
  cmake <- getOption("rpymat.cmake")
  if(length(cmake) == 1 && file.exists(cmake)){
    return(cmake)
  }
  # find locally installed cmake
  cmake <- Sys.which("cmake")

  if(cmake != ""){
    return(cmake)
  }

  os <- get_os()
  switch (
    os,
    "darmin" = {
      cmake <- find_cmake_darwin()
      if(!file.exists(cmake)){
        stop("Please install cmake via: \n  brew install cmake")
      }
    },
    "windows" = {
      stop("Please install rtools (>= 4.0)") # TODO: add url
    },
    "linux" = {
      stop("Please install cmake via \n  sudo apt install cmake") # TODO: add redhat, centos command
    },
    {
      stop("Please install `cmake` first")
    }
  )
  cmake
}

#' @export
ensure_ITK <- function(...){
  pass <- FALSE
  if(dir.exists(itk_cmake_path(must_work = FALSE))){
    pass <- TRUE
  }
  if(!pass){
    install_ITK(...)
  }
  return(get_itk_dir())
}

#' @export
install_ITK <- function(
  build_type = "Release",
  compile_flag = " -fPIC -O2 -Wno-c++11-long-long "
){
  cmake <- find_cmake()
  url <- "https://github.com/InsightSoftwareConsortium/ITK"

  message("Downloading ITK from ", url)

  wd <- getwd()
  tmp_dir <- tempfile(pattern = "itk_installer_")
  on.exit({
    setwd(wd)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE, after = TRUE)
  src <- file.path(tmp_dir, "ITK-src")
  bin <- file.path(tmp_dir, "ITK-bin")
  dir.create(src, showWarnings = FALSE, recursive = TRUE)
  dir.create(bin, showWarnings = FALSE, recursive = TRUE)

  git2r::clone(url, local_path = src)
  src <- normalizePath(src, mustWork = TRUE)

  setwd(bin)

  target_path <- file.path(
    tools::R_user_dir(package = "rpymat", which = "data"),
    "ITK"
  )
  dir.create(target_path, showWarnings = FALSE, recursive = TRUE)
  target_path <- normalizePath(target_path, mustWork = TRUE)


  args <- c(
    sprintf('-DCMAKE_BUILD_TYPE:STRING="%s"', build_type),
    sprintf('-DCMAKE_C_FLAGS="${CMAKE_C_FLAGS} %s -DNDEBUG  "', compile_flag),
    sprintf('-DCMAKE_CXX_FLAGS="${CMAKE_CXX_FLAGS} %s -DNDEBUG  "', compile_flag),
    sprintf('-DITK_USE_GIT_PROTOCOL:BOOL=OFF'),
    sprintf('-DBUILD_SHARED_LIBS=OFF'),
    sprintf('-DBUILD_TESTING:BOOL=OFF'),
    sprintf('-DBUILD_EXAMPLES:BOOL=OFF'),
    sprintf('-DCMAKE_INSTALL_PREFIX:PATH="%s"', target_path),
    sprintf('-DITK_LEGACY_REMOVE:BOOL=OFF'),
    sprintf('-DITK_FUTURE_LEGACY_REMOVE:BOOL=OFF'),
    sprintf('-DITK_BUILD_DEFAULT_MODULES:BOOL=OFF'),
    sprintf('-DModule_AdaptiveDenoising:BOOL=ON'),
    sprintf('-DModule_GenericLabelInterpolator:BOOL=ON'),
    sprintf('-DKWSYS_USE_MD5:BOOL=ON'),
    sprintf('-DITK_WRAPPING:BOOL=OFF'),
    sprintf('-DModule_MGHIO:BOOL=ON'),
    sprintf('-DModule_ITKDeprecated:BOOL=OFF'),
    sprintf('-DModule_ITKReview:BOOL=ON'),
    sprintf('-DModule_ITKVtkGlue:BOOL=OFF'),
    sprintf('-D ITKGroup_Core=ON'),
    sprintf('-D ITKGroup_Core=ON'),
    sprintf('-D Module_ITKReview=ON'),
    sprintf('-D ITKGroup_Filtering=ON'),
    sprintf('-D ITKGroup_IO=ON'),
    sprintf('-D ITKGroup_Numerics=ON'),
    sprintf('-D ITKGroup_Registration=ON'),
    sprintf('-D ITKGroup_Segmentation=ON'),
    shQuote(src, type = "cmd")
  )


  message("Creating Makefiles")
  system2(command = cmake, args = args, env = c(
    "CXX_STD=CXX11",
    "JTHREADS=2"
  ), wait = TRUE)

  message("Installing")

  setwd(bin)
  make <- Sys.which("make")
  system(sprintf("%s -j 2 && %s install",
                 shQuote(make, type = "cmd"),
                 shQuote(make, type = "cmd")))

}


#' @export
get_itk_dir <- function(){
  target_path <- file.path(
    tools::R_user_dir(package = "rpymat", which = "data"),
    "ITK"
  )
  target_path
}

#' @export
itk_path <- function(must_work = NA) {
  itk_dir <- get_itk_dir()
  if ( !dir.exists(itk_dir) ) {
    msg <- "ITK path does not exist. Please run the installer first.\n  install_ITK()"
    if(isTRUE(must_work)){
      stop(msg)
    } else if(is.na(must_work)){
      warning(msg)
    }
  }
  normalizePath(itk_dir, mustWork = FALSE)
}

#' @export
itk_cmake_path <- function(must_work = NA) {
  path <- itk_path(must_work)
  path <- file.path(path, "lib", "cmake")
  f <- list.files(path, pattern = "ITK-")
  if(length(f)){
    f <- f[[1]]
  }
  normalizePath(file.path(path, f), mustWork = FALSE)
}

#' @export
itk_includes <- function(must_work = NA){
  path <- file.path(itk_path(must_work), "includes")
  normalizePath(path, mustWork = FALSE)
}

#' @export
itk_libs <- function(must_work = NA){
  path <- file.path(itk_path(must_work), "lib")
  normalizePath(path, mustWork = FALSE)
}

#' @export
itk_libs <- function(must_work = NA){
  path <- file.path(itk_path(must_work), "lib")
  normalizePath(path, mustWork = FALSE)
}

#' @export
itk_compile_flags <- function(){
  str <- " -fPIC -O2  "
  if(get_os() == "windows"){
    s <- paste(c("-lws2_32", "-lgdi32", "-mwindows",
                 "-msse4.1", "-mssse3", "-Wa,-mbig-obj"), collapse = " ")
    str <- paste(str, s)
  }
  str
}

#' @export
itk_version <- function() {
  path <- itk_cmake_path(must_work = TRUE)
  fname <- basename(path)
  sub("ITK-", "", fname)
}


