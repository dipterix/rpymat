#' @name jupyter
#' @title Install, register, launch 'Jupyter' notebook to the virtual environment
#' @param register_R whether to register \code{IRkernel} to the notebook
#' @param user,name,displayname,rprofile,prefix,sys_prefix,verbose see \code{\link[IRkernel]{installspec}}
#' @param root_dir,workdir default root directory of the notebook
#' @param host,port 'IP' and port of the hosting 'URL'
#' @param open_browser whether to open the browser once launched
#' @param token access token of the notebook
#' @param async whether to open the notebook in the background
#' @param dry_run whether to display the command instead of executing them;
#' used to debug the code
#' @param ... for \code{add_jupyter}, these are additional parameters passed to
#' \code{jupyter_register_R}; for \code{jupyter_launch}, these are additional
#' parameters passed to \code{jupyter_options}
#' @returns \code{jupyter_bin} returns the 'Jupyter' notebook binary path;
#' \code{jupyter_options} returns the 'Jupyter' configuration in strings;
#' \code{jupyter_server_list} returns a table of existing local 'Jupyter'
#' server hosts, ports, and tokens; \code{jupyter_check_launch} returns true
#' if a new server has been created, or false if there has been an existing
#' server at the port;
#' other functions return nothing.
#'
#' @examples
#' \dontrun{
#'
#' # Requires installation of conda
#' library(rpymat)
#'
#' # Install conda, if you have done so, skip
#' configure_conda()
#'
#'
#' # Install Jupyter notebook
#' add_jupyter(register_R = TRUE)
#'
#'
#' # Utility functions
#' jupyter_bin()
#'
#' # Please install `dipsaus` package to enable `async=TRUE` with
#' # better experience
#' jupyter_launch(async = FALSE, open_browser = TRUE)
#'
#'
#' }
#'
NULL

#' @rdname jupyter
#' @export
add_jupyter <- function(..., register_R = TRUE){
  add_packages(packages = c("notebook", "numpy", "h5py", "matplotlib", "pandas", "jupyterlab"), channel = "conda-forge", ...)
  try({
    add_packages(packages = c("jupyterlab-git", "ipywidgets", "jupyter-server-proxy"), channel = "conda-forge")
    # add_packages(c('jupyterlab_latex', 'jupyterlab_github', 'matlab_kernel'), pip = TRUE)
  })
  if(register_R && system.file("kernelspec", package = "IRkernel") != ""){
    jupyter_register_R(...)
  }
  invisible()
}

#' @rdname jupyter
#' @export
jupyter_bin <- function(){
  f <- c(
    file.path(env_path(), "bin", "jupyter"),
    file.path(env_path(), "Scripts", "jupyter.exe")
  )
  if(any(file.exists(f))){
    f <- f[file.exists(f)][[1]]
  } else {
    if(get_os() == "windows"){
      f <- f[[2]]
    } else {
      f <- f[[1]]
    }
  }
  normalizePath(f, mustWork = FALSE, winslash = "\\")
}

#' @rdname jupyter
#' @export
jupyter_register_R <- function (user = NULL, name = "ir", displayname = "R", rprofile = NULL,
          prefix = NULL, sys_prefix = NULL, verbose = getOption("verbose"))
{

  if (get_os() != "windows") {
    exit_code <- run_command(command = sprintf("%s kernelspec --version", shQuote(jupyter_bin())), stdout = FALSE, stderr = FALSE)
    if(!isTRUE(exit_code == 0)){
      warning("Please install jupyter first via `add_jupyter()`\n")
    }
  } else {
    run_command(command = sprintf("%s kernelspec --version", shQuote(jupyter_bin())))
  }
  if (is.null(user)) {
    user <- (is.null(prefix) && is.null(sys_prefix))
  }
  if (sum(user, !is.null(prefix), !is.null(sys_prefix)) > 1)  {
    stop("\"user\", \"prefix\", \"sys_prefix\" are mutually exclusive")
  }

  requireNamespace("IRkernel", quietly = TRUE)
  srcdir <- system.file("kernelspec", package = "IRkernel")
  tmp_name <- tempfile()
  dir.create(tmp_name)
  on.exit({
    if(dir.exists(tmp_name)){
      unlink(tmp_name, recursive = TRUE)
    }
  })
  file.copy(srcdir, tmp_name, recursive = TRUE)
  spec_path <- file.path(tmp_name, "kernelspec", "kernel.json")
  spec <- jsonlite::fromJSON(spec_path)
  spec$argv[[1]] <- file.path(R.home("bin"), "R")
  spec$display_name <- displayname
  if (!is.null(rprofile)) {
    spec$env <- list(R_PROFILE_USER = rprofile)
  }
  write(jsonlite::toJSON(spec, pretty = TRUE, auto_unbox = TRUE), file = spec_path)
  user_flag <- if (user) { "--user" } else { character(0) }
  prefix_flag <- if (!is.null(prefix)) { c("--prefix", prefix) } else { character(0) }
  sys_prefix_flag <- if (!is.null(sys_prefix)) { c("--sys-prefix", prefix) } else { character(0) }
  quiet_flag <- if (!verbose) { "--log-level=WARN" } else { character(0) }

  args <- c("kernelspec", "install", "--replace", "--name",
            name, user_flag, prefix_flag, sys_prefix_flag, quiet_flag,
            file.path(tmp_name, "kernelspec"))
  run_command(command = sprintf(
    "%s kernelspec install --replace --name %s %s %s %s %s %s",
    shQuote(jupyter_bin(), type = "cmd"),
    shQuote(name, type = "cmd"),
    paste(user_flag, collapse = " "),
    paste(prefix_flag, collapse = " "),
    paste(sys_prefix_flag, collapse = " "),
    paste(quiet_flag, collapse = " "),
    shQuote(normalizePath(file.path(tmp_name, "kernelspec")), type = "cmd")
  ))
  unlink(tmp_name, recursive = TRUE)
  invisible(0)
}

#' @rdname jupyter
jupyter_options <- function(root_dir, host = "127.0.0.1", port = 8888, open_browser = FALSE, token = rand_string()){
  root_dir <- normalizePath(root_dir, winslash = "\\", mustWork = TRUE)
  root_dir <- gsub('\\\\', '\\\\\\\\', root_dir)

  glue::glue(
    .sep = "\n",
    # 'c.GatewayClient.url = "http://{host}:{port+1}"',

    'c.ServerApp.ip = "{host}"',
    'c.ServerApp.allow_origin = "*"',
    'c.ServerApp.port = {port}',
    sprintf('c.ServerApp.open_browser = %s', ifelse(isTRUE(open_browser), "True", "False")),
    'c.ServerApp.base_url = "/jupyter/"',
    'c.ServerApp.token = "{token}"',
    'c.ServerApp.password = ""',
    'c.ServerApp.root_dir = "{root_dir}"',

    'c.ServerApp.tornado_settings = {{',
    '  \'headers\' : {{',
    '    \'Content-Security-Policy\' : "frame-ancestors * \'self\' "',
    '  }}',
    '}}'
  )

  # glue::glue(
  #   .sep = "\n",
  #   # 'c.GatewayClient.url = "http://{host}:{port+1}"',
  #
  #   'c.NotebookApp.ip = "{host}"',
  #   'c.NotebookApp.allow_origin = "*"',
  #   'c.NotebookApp.port = {port}',
  #   sprintf('c.NotebookApp.open_browser = %s', ifelse(isTRUE(open_browser), "True", "False")),
  #   'c.NotebookApp.base_url = "/jupyter/"',
  #   'c.NotebookApp.token = "{token}"',
  #   'c.NotebookApp.password = ""',
  #   'c.NotebookApp.notebook_dir = "{root_dir}"',
  #
  #   'c.NotebookApp.tornado_settings = {{',
  #   '  \'headers\' : {{',
  #   '    \'Content-Security-Policy\' : "frame-ancestors * \'self\' "',
  #   '  }}',
  #   '}}'
  # )
}

#' @rdname jupyter
#' @export
jupyter_launch <- function(host = "127.0.0.1", port = 8888,
                           open_browser = TRUE, workdir = getwd(),
                           async = FALSE, ..., dry_run = FALSE){
  port <- as.integer(port)
  stopifnot(is.finite(port))
  # add_jupyter()
  stopifnot(dir.exists(workdir))
  workdir <- normalizePath(workdir, mustWork = TRUE)
  conf <- jupyter_options(root_dir = workdir, host = host, port = port, open_browser = open_browser, ...)

  conf_dir <- file.path(
    R_user_dir(package = 'rpymat', which = "config"),
    'jupyter-configurations', port
  )
  dir.create(file.path(conf_dir, "custom"), showWarnings = FALSE, recursive = TRUE)
  conf_dir <- normalizePath(conf_dir)
  writeLines(conf, con = file.path(conf_dir, "jupyter_notebook_config.py"))
  writeLines(c(
    "define(['base/js/namespace'], function(Jupyter){",
    "  Jupyter._target = '_self';",
    "});"
  ), con = file.path(conf_dir, "custom", "custom.js"))
  # JUPYTER_CONFIG_DIR

  quoted_cmd <- shQuote(jupyter_bin())
  command <- c(
    sprintf("%s --paths", quoted_cmd),
    sprintf("%s lab", quoted_cmd)
  )
  env <- sprintf("JUPYTER_CONFIG_DIR=%s", shQuote(conf_dir))
  env_list <- list(`JUPYTER_CONFIG_DIR` = conf_dir)

  if(async && !dry_run){
    tf <- tempfile()

    expr <- bquote({
      ns <- asNamespace('rpymat')
      ns$run_command(.(command), workdir = .(workdir), env_list = .(env_list),
                     env = .(env), wait = TRUE, dry_run = .(dry_run))
    })

    writeLines(deparse(expr), con = tf)

    if( rstudioapi::isAvailable("1.4", child_ok = TRUE) ) {

      rstudioapi::jobRunScript(tf, name = "JupyterLab Server", workingDir = workdir, importEnv = NULL, exportEnv = "")
      try({
        rstudioapi::executeCommand("activateConsole", quiet = TRUE)
      }, silent = TRUE)

    } else {
      run_rscript(script = tf, wait = FALSE)
    }

  } else {
    re <- run_command(command, workdir = workdir, env_list = env_list,
                      env = env, wait = TRUE, dry_run = dry_run)
    if( dry_run ) {
      return(re)
    }
  }

  return(invisible())
  # run_command(sprintf("%s --config-dir", shQuote(jupyter_bin(), type = "cmd")), workdir = workdir, env = sprintf("JUPYTER_CONFIG_DIR=%s", shQuote(conf_dir, type = "cmd")), )
}


#' @rdname jupyter
#' @export
jupyter_check_launch <- function(port = 8888, host = "127.0.0.1",
                                 open_browser = TRUE, workdir = getwd(),
                                 async = 'auto', ...){
  port <- as.integer(port)
  stopifnot(is.finite(port))

  open_browser <- as.logical(open_browser)

  tryCatch({
    server_list <- jupyter_server_list()
    if(port %in% server_list$port){
      if(!isFALSE(open_browser)){
        instance <- server_list[server_list$port == port, ]
        url <- sprintf("http://%s:%s/jupyter/lab?token=%s", instance$host, instance$port, instance$token)
        utils::browseURL(url)
      }
      return(FALSE)
    }
  }, error = function(e){
    if(!identical(e$message, "No Jupyter server instance is running")){
      warning(e)
    }
  })
  if(identical(async, "auto")){
    # async <- rstudioapi::isAvailable("1.4", child_ok = TRUE)
    async <- TRUE
  } else {
    async <- as.logical(async)
  }
  jupyter_launch(port = port, open_browser = open_browser,
                 host = host, workdir = workdir, async = async,
                 ..., dry_run = FALSE)

  return(TRUE)
}


#' @rdname jupyter
#' @export
jupyter_server_list <- function(){
  quoted_cmd <- shQuote(jupyter_bin(), type = "cmd")
  command <- c(
    "echo off",
    sprintf("%s server list", quoted_cmd)
  )

  res <- run_command(command, wait = TRUE, stdout = TRUE, stderr = TRUE)

  res <- res[grepl("http[s]{0,1}://.*:[0-9]{2,5}/jupyter/", res)]
  res <- strsplit(res, "/jupyter/")
  res <- lapply(res, function(x){
    token <- ""
    if(length(x) > 1){
      token <- strsplit(x[[2]], " ")[[1]][[1]]
      if(startsWith(token, "?token=")){
        token <- sub(pattern = "^\\?token=", "", token)
      }
    }
    x <- sub("http[s]{0,1}://", "", x[[1]])
    x <- strsplit(x, "[://]")[[1]]
    c(x[1:2], token)
  })

  if(!length(res)){
    stop("No Jupyter server instance is running")
  }
  res <- do.call('rbind', res[sapply(res, length) == 3])
  res <- as.data.frame(res)
  names(res) <- c("host", "port", "token")
  res$port <- as.integer(res$port)
  res
}


#' @rdname jupyter
#' @export
jupyter_server_stop <- function(port, ...){
  quoted_cmd <- shQuote(jupyter_bin(), type = "cmd")
  command <- sprintf("%s server stop %.0f", quoted_cmd, port)
  run_command(command, wait = TRUE, ...)
}


#' @rdname jupyter
#' @export
jupyter_server_stop_all <- function(...){
  try({
    server_list <- jupyter_server_list()
    quoted_cmd <- shQuote(jupyter_bin(), type = "cmd")
    command <- sprintf("%s server stop %.0f", quoted_cmd, server_list$port)
    run_command(command, wait = TRUE, ...)
  }, silent = TRUE)
}
