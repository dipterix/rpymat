#' @name conda-env
#' @title 'Miniconda' environment
#' @description These functions/variables are used to configure
#' 'Miniconda' environment.
#' @param matlab 'Matlab' path to add to the configuration path;
#' see 'Details'
#' @param python_ver 'Python' version to use; see 'Configuration'
#' @param packages additional 'Python' or 'conda' packages to install
#' @param update whether to update 'conda'; default is false
#' @param force whether to force install the 'Miniconda' even a previous
#' version exists; default is false. Setting \code{false=TRUE} rarely
#' works. Please see 'Configuration'.
#' @param ask whether to ask for user's agreement to remove the repository.
#' This parameter should be true if your functions depend on
#' \code{remove_conda} (see 'CRAN Repository Policy'). This argument might
#' be removed and force to be interactive in the future.
#' @param fun 'Matlab' function name, character (experimental)
#' @param ... for \code{add_packages}, these are additional parameters
#' passing to \code{\link[reticulate]{conda_install}}; for
#' \code{call_matlab}, \code{...} are the parameters passing to \code{fun}
#' @param .options 'Matlab' compiler options
#' @param .debug whether to enable debug mode
#' @return None
#' @section Background & Objectives:
#' Package 'reticulate' provides sophisticated tool-sets that
#' allow us to call 'Python' functions within \code{R}. However, the
#' installation of 'Miniconda' and 'Python' can be tricky on many
#' platforms, for example, the 'M1' chip, or some other 'ARM' machines.
#' The package \code{rpymat} provides easier approach to configure on these
#' machines with totally isolated environments. Any modifications to this
#' environment will not affect your other set ups.
#'
#' Since 2014, 'Matlab' has introduced its official compiler for 'Python'.
#' The package \code{rpymat} provides a simple approach to link the
#' compiler, provided that you have proper versions of 'Matlab' installed.
#' \href{https://www.mathworks.com/content/dam/mathworks/mathworks-dot-com/support/sysreq/files/python-compatibility.pdf}{Here} is a list of
#' 'Matlab' versions with official compilers and their corresponding
#' 'Python' versions.
#'
#' @section Configuration:
#' If 'Matlan' compiler is not to be installed, In most of the cases,
#' function \code{configure_conda} with default arguments automatically
#' downloads the latest 'Miniconda' and configures the latest 'Python'.
#' If any other versions of 'Miniconda' is ought to be installed,
#' please set options \code{"reticulate.miniconda.url"} to change the
#' source location.
#'
#' On Apple's 'M1' machine, there is no official native 'Miniconda' as of
#' 2021-11-01. The default 'conda' will be the \href{https://github.com/conda-forge/miniforge/releases/tag/4.9.2-7}{'Mambaforge'} compiled by
#' the 'conda-forge' team. This version will subject to change in the near
#' future once the official support is released. Sometimes the 'Mambaforge'
#' is too advanced and not all the dependencies are properly compiled.
#' In such cases, you will receive errors like "Package 'xxx' conflicts
#' for". Try a lower version of \code{python_ver} (such as "3.9").
#'
#' If 'Matlab' is to be installed, please specify the 'Matlab' path when
#' running \code{configure_conda}. If the environment has been setup,
#' \code{configure_matlab} can link the 'Matlab' compilers without
#' removing the existing environment. For 'ARM' users, unfortunately,
#' there will be no 'Matlab' support as the compilers are written for
#' the 'Intel' chips.
#'
#' @section Initialization:
#' Once 'conda' and 'Python' environment has been installed, make sure
#' you run \code{ensure_rpymat()} before running any 'Python' code. This
#' function will make sure correct compiler is linked to your current
#' \code{R} session.
#'
#' @examples
#'
#' # The script will interactively install 'conda' to `R_user_dir`
#' \dontrun{
#'
#' # Install conda and python 3.9
#'
#' configure_conda(python_ver = '3.9')
#'
#'
#' # Add packages h5py, pandas, jupyter
#'
#' add_packages(c('h5py', 'pandas', 'jupyter'))
#'
#' # Add pip packages
#'
#' add_packages("itk", pip = TRUE)
#'
#' # Initialize the isolated environment
#'
#' ensure_rpymat()
#'
#'
#' # Remove the environment
#'
#' remove_conda()
#'
#' }
#'
NULL

#' @rdname conda-env
#' @export
CONDAENV_NAME <- "rpymat-conda-env"

VIRTUALENV_NAME <- "rpymat-virtual-env"

install_root <- function(){
  if(Sys.info()["sysname"] == "Darwin"){
    path <- path.expand("~/Library/r-rpymat")
  } else {
    root <- normalizePath(rappdirs::user_data_dir(), winslash = "/",
                          mustWork = FALSE)
    path <- file.path(root, "r-rpymat")
  }
  getOption("rpymat.install_root", path)
}

#' @rdname conda-env
#' @export
conda_path <- function(){
  file.path(install_root(), "miniconda")
}

#' @rdname conda-env
#' @export
env_path <- function(){
  return( file.path(install_root(), CONDAENV_NAME) )
}

set_conda <- function(temporary = TRUE){
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH', unset = "")
  if(old_path == ""){
    old_path <- getOption("reticulate.conda_binary", "")
  }
  if(temporary && old_path != ""){
    parent_env <- parent.frame()
    do.call(on.exit, list(bquote({
      options("reticulate.conda_binary" = .(getOption("reticulate.conda_binary", "")))
      Sys.setenv("RETICULATE_MINICONDA_PATH" = .(Sys.getenv('RETICULATE_MINICONDA_PATH', unset = "")))
    }),
    add = TRUE,
    after = FALSE), envir = parent_env)
  }
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())


  conda_path <- file.path(conda_path(), "condabin", c("conda", "conda.exe", "conda.bin"))
  conda_path <- conda_path[file.exists(conda_path)]
  if(length(conda_path)){
    options("reticulate.conda_binary" = conda_path[[1]])
  }
}

# https://www.mathworks.com/content/dam/mathworks/mathworks-dot-com/support/sysreq/files/python-compatibility.pdf
mat_pyver <- function(mat_ver){
  version_file <- system.file("matlab-python-versions.txt", package = 'rpymat')
  s <- readLines(version_file)
  s <- s[s != ""]
  s <- strsplit(s, "[ ]+")
  names <- sapply(s, '[[', 1)
  version_list <- structure(lapply(s, function(x){
    x[-1]
  }), names = names)
  re <- version_list[[mat_ver]]
  if(!length(re)){
    # read from Github
    version_file <- "https://raw.githubusercontent.com/dipterix/rpymat/main/inst/matlab-python-versions.txt"
    s <- readLines(version_file)
    s <- s[s != ""]
    s <- strsplit(s, "[ ]+")
    names <- sapply(s, '[[', 1)
    version_list <- structure(lapply(s, function(x){
      x[-1]
    }), names = names)
    re <- version_list[[mat_ver]]
  }
  re
}

#' @rdname conda-env
#' @export
configure_matlab <- function(matlab, python_ver = 'auto'){

  # TODO: must configure python first

  # matlab <- '/Applications/MATLAB_R2020b.app'
  matlab <- matlab[[1]]
  mat_engine_path <- file.path(matlab, "extern/engines/python/")
  py_path <- reticulate::conda_python(env_path())

  if(python_ver == 'auto'){
    # check matlab version

    try({
      s <- readLines(file.path(mat_engine_path, 'setup.py'))
      s <- trimws(s)
      s <- s[startsWith(s, "version")]
      if(length(s)){

        s <- s[[length(s)]]
        s <- tolower(s)
        m <- regexec("20[0-9]{2}[abcdefgh]", s)
        mat_ver <- unlist(regmatches(s, m))

        compatible_ver <- mat_pyver(mat_ver)


        if(length(compatible_ver)){
          # check installed python version
          ver <- system2(py_path, "-V", stdout = TRUE, stderr = TRUE)
          m <- regexec("([23]\\.[0-9]+)\\.[0-9]+", ver)
          ver <- regmatches(ver, m)[[1]][[2]]
          # ver <- stringr::str_match(ver, "([23]\\.[0-9]+)\\.[0-9]+")
          # ver <- ver[[2]]

          if(!ver %in% compatible_ver) {
            python_ver <- compatible_ver[[length(compatible_ver)]]
            message(sprintf("Current python version is `%s`, but matlab engine requires python version to be one of the followings: %s. Trying to install python %s. To proceed, your python version will change in the virtual environment (it is safe and your system python won't change).", ver, paste(compatible_ver, collapse = ', '), python_ver))
            if(interactive()){
              if(!isTRUE(utils::askYesNo("Continue? "))){
                stop("User abort", call. = FALSE)
              }
            }

          }
        }


      }

    })

  }

  if(python_ver != 'auto'){
    add_packages(NULL, python_ver = python_ver)
  }


  setwd2(mat_engine_path)


  build_dir <- file.path(install_root(), "matlab-engine-build")
  if(dir.exists(build_dir)){ unlink(build_dir, recursive = TRUE, force = TRUE) }
  dir.create(build_dir)
  build_dir <- normalizePath(build_dir)
  system2(py_path, c(
    "setup.py",
    "build",
    sprintf('--build-base="%s"', build_dir),
    "install"
  ), wait = TRUE)
}

auto_python_version <- function(matlab){
  matlab <- matlab[[1]]
  mat_engine_path <- file.path(matlab, "extern/engines/python/")
  s <- readLines(file.path(mat_engine_path, 'setup.py'))
  s <- trimws(s)
  s <- s[startsWith(s, "version")]

  s <- s[[length(s)]]
  s <- tolower(s)
  m <- regexec("20[0-9]{2}[abcdefgh]", s)
  mat_ver <- unlist(regmatches(s, m))
  compatible_ver <- mat_pyver(mat_ver)
  compatible_ver
}

#' @rdname conda-env
#' @export
configure_conda <- function(python_ver = "auto",
                            packages = NULL,
                            matlab = NULL,
                            update = FALSE, force = FALSE){

  error = TRUE
  set_conda(temporary = TRUE)

  # TODO: check if conda bin exists
  path <- conda_path()

  if(length(matlab)){
    python_vers <- auto_python_version(matlab)
    if( isTRUE(python_ver == 'auto') ){
      python_ver <- python_vers[[length(python_vers)]]
    } else {
      ver <- package_version(python_ver)
      if( !sprintf("%s.%s", ver$major, ver$minor) %in% python_vers ){
        stop("Requested python version is ", python_ver, ". However, this is imcompatible with your matlab installed at ", matlab[[1]], ". Please choose from the following pythons: ", paste(python_vers, collapse = ", "))
      }
    }
  }

  if(dir.exists(path) && !force){
    stop("conda path already exists. Please consider removing it by calling `rpymat::remove_conda()`")
  }
  if(force || update || !dir.exists(path)){
    miniconda_installer_url()
    tryCatch({
      reticulate::install_miniconda(path = path, update = update, force = force)
    }, error = function(e){
      print(e)
    }, warning = function(e){
      print(e)
    })
    # install_conda(path = path, update = update, force = force)
  }

  # create virtual env
  if(force || update || !CONDAENV_NAME %in% reticulate::conda_list()[['name']]){
    if( isTRUE(python_ver == "auto") ){
      reticulate::conda_create(env_path())
    } else {
      reticulate::conda_create(env_path(), python_version = python_ver)
    }
  }

  # check matlab
  if(length(matlab)){
    configure_matlab(matlab, python_ver = python_ver)
  }

  if(!length(matlab) || length(packages)) {
    add_packages(packages, python_ver)
  }
  error <- FALSE
}

#' @rdname conda-env
#' @export
remove_conda <- function(ask = TRUE){
  root <- install_root()
  if(!interactive()){
    stop("Must run in interactive mode")
  }
  if( !dir.exists(root) ){ return(invisible()) }
  if( ask ){
    message(sprintf("Removing conda at %s? \nThis operation only affects `rpymat` package and is safe.", root))
    ans <- utils::askYesNo("", default = FALSE, prompts = c("yes", "no", "cancel - default is `no`"))
    if(!isTRUE(ans)){
      if(is.na(ans)){
        message("Abort")
      }
      return(invisible())
    }
  }

  unlink(root, recursive = TRUE, force = TRUE)

  return(invisible())
}

#' @rdname conda-env
#' @export
add_packages <- function(packages = NULL, python_ver = 'auto', ...) {
  set_conda(temporary = TRUE)


  # install packages
  packages <- unique(packages)
  if(!length(packages)){ return() }
  if( isTRUE(python_ver == "auto") ){
    reticulate::conda_install(env_path(), packages = packages, ...)
  } else {
    reticulate::conda_install(env_path(), packages = packages,
                              python_version = python_ver, ...)
  }

}

#' @rdname conda-env
#' @export
ensure_rpymat <- function(){
  set_conda(temporary = FALSE)
  Sys.setenv("RETICULATE_PYTHON" = normalizePath(file.path(env_path(), 'bin', "python")))
  reticulate::use_condaenv(CONDAENV_NAME, required = TRUE)
  reticulate::py_config()
}

#' @rdname conda-env
#' @export
matlab_engine <- function(){
  set_conda(temporary = FALSE)
  reticulate::use_condaenv(CONDAENV_NAME, required = TRUE, conda = file.path(conda_path(), "bin", "conda"))

  if(reticulate::py_module_available("matlab.engine")){
    matlab <- reticulate::import('matlab.engine')
    return(invisible(matlab))
    # try({
    #   eng <- matlab$start_matlab(matlab_param)
    # })
  }
  return(invisible())
  # eng$biliear(matrix(rnorm(10), nrow = 1), matrix(rnorm(10), nrow = 1), 0.5)
  # eng$biliear(rnorm(10), rnorm(10), 0.5)

}

#' @rdname conda-env
#' @export
call_matlab <- function(fun, ..., .options = getOption("rpymat.matlab_opt", "-nodesktop -nojvm"), .debug = getOption("rpymat.debug", FALSE)){

  matlab <- matlab_engine()
  if(is.null(matlab)){
    stop("Matlab engine not configured. Please run `configure_matlab(matlab_root)` to set up matlab")
  }


  existing_engines <- getOption("rpymat.matlab_engine", NULL)
  if(is.null(existing_engines)){
    existing_engines <- fastqueue2()
    options("rpymat.matlab_engine" = existing_engines)
  }

  suc <- FALSE

  if(.debug){
    message("Existing engine: ", existing_engines$size())
  }
  if(existing_engines$size()){
    same_opt <- vapply(as.list(existing_engines), function(item){
      if(!is.environment(item)){ return(FALSE) }
      isTRUE(item$options == .options)
    }, FALSE)

    if(any(same_opt)){
      idx <- which(same_opt)[[1]]
      if(idx > 1){
        burned <- existing_engines$mremove(n = idx - 1, missing = NA)
        for(item in burned){
          if(is.environment(item)){
            existing_engines$add(item)
          }
        }
      }
      item <- existing_engines$remove()
      suc <- tryCatch({
        force(item$engine$workspace)
        TRUE
      }, error = function(e){
        # engine is invalid, quit
        item$engine$quit()
        FALSE
      })
    }
  }
  if(!suc){
    if(.debug){
      message("Creating new matlab engine with options: ", .options)
    }
    item <- new.env(parent = emptyenv())
    engine <- matlab$start_matlab(.options)
    item$engine <- engine
    item$options <- .options
    reg.finalizer(item, function(item){

      if(getOption("rpymat.debug", FALSE)){
        message("Removing a matlab instance.")
      }
      try({item$engine$quit()}, silent = TRUE)
    }, onexit = TRUE)
  } else {
    if(.debug){
      message("Using existing idle engine")
    }
  }
  on.exit({
    tryCatch({
      force(item$engine$workspace)

      if(.debug){
        message("Engine is still alive, keep it for future use")
      }
      existing_engines$add(item)
    }, error = function(e){
      if(.debug) {
        message("Engine is not alive, removing from the list.")
      }
      item$engine$quit()
    })
  })
  if(.debug) {
    message("Executing matlab call")
  }
  engine <- item$engine
  res <- engine[[fun]](...)

  return(res)
}

