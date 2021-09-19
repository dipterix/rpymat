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

#' @export
conda_path <- function(){
  file.path(install_root(), "miniconda")
}

#' @export
env_path <- function(){
  return( file.path(install_root(), CONDAENV_NAME) )
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

#' @export
configure_conda <- function(python_ver = "auto",
                            packages = NULL,
                            matlab = NULL,
                            update = TRUE, force = FALSE){

  error = TRUE
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)

    if( error ){
      warning("Error occured during the installation. Some functions might not work properly. If you want to remove this installation, run `rpymat::remove_conda()`")
    }

  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())

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

  if(force || update || !dir.exists(path)){
    reticulate::install_miniconda(path = path, update = update, force = force)
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

  env_path <- env_path()
  if(dir.exists(env_path)){
    unlink(env_path, recursive = TRUE, force = TRUE)
  }

  conda_path <- conda_path()
  if(dir.exists(conda_path)){
    unlink(conda_path, recursive = TRUE, force = TRUE)
  }

  # CRAN policy requires root to be removed if empty
  if(dir.exists(root)){
    if(!length(list.files(root, all.files = TRUE, no.. = TRUE, include.dirs = TRUE))){
      unlink(root, recursive = TRUE, force = TRUE)
    }
  }
  return(invisible())
}

#' @export
add_packages <- function(packages = NULL, python_ver = 'auto') {
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())


  # install packages
  packages <- unique(c('numpy', 'h5py', 'scipy', 'matplotlib', "ipython", packages))
  if( isTRUE(python_ver == "auto") ){
    reticulate::conda_install(env_path(), packages = packages)
  } else {
    reticulate::conda_install(env_path(), packages = packages,
                              python_version = python_ver)
  }

}

#' @export
ensure_rpymat <- function(){
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())
  reticulate::use_condaenv(CONDAENV_NAME, required = TRUE)
  reticulate::py_config()
}

#' @export
matlab_engine <- function(){
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())
  reticulate::use_condaenv(CONDAENV_NAME, required = TRUE)

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

