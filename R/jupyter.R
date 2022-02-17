#' @name jupyter
#' @title Install, register, launch 'Jupyter' notebook to the virtual environment
#' @param register_R whether to register \code{IRkernel} to the notebook
#' @param user,name,displayname,rprofile,prefix,sys_prefix,verbose see \code{\link[IRkernel]{installspec}}
#' @param root_dir,workdir default root directory of the notebook
#' @param host,port 'IP' and port of the hosting 'URL'
#' @param open_browser whether to open the browser once launched
#' @param token access token of the notebook
#' @param async whether to open the notebook in the background
#' @param ... for \code{add_jupyter}, these are additional parameters passed to
#' \code{jupyter_register_R}; for \code{jupyter_launch}, these are additional
#' parameters passed to \code{jupyter_options}
#' @return \code{jupyter_bin} returns the 'Jupyter' notebook binary path;
#' \code{jupyter_options} returns the 'Jupyter' configuration in strings;
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
  add_packages(packages = c("jupyter", "numpy", "h5py", "matplotlib", "pandas"), ...)
  if(register_R && system.file("kernelspec", package = "IRkernel") != ""){
    jupyter_register_R(...)
  }
  invisible()
}

#' @rdname jupyter
#' @export
jupyter_bin <- function(){
  normalizePath(file.path(env_path(), "bin", "jupyter"), mustWork = FALSE)
}

#' @rdname jupyter
#' @export
jupyter_register_R <- function (user = NULL, name = "ir", displayname = "R", rprofile = NULL,
          prefix = NULL, sys_prefix = NULL, verbose = getOption("verbose"))
{
  exit_code <- run_command(command = sprintf("%s kernelspec --version", shQuote(jupyter_bin())), stdout = FALSE, stderr = FALSE)
  if (exit_code != 0) {
    stop("Please install jupyter first via `add_jupyter()`\n")
  }
  if (is.null(user)) {
    user <- (is.null(prefix) && is.null(sys_prefix))
  }
  if (sum(user, !is.null(prefix), !is.null(sys_prefix)) > 1)  {
    stop("\"user\", \"prefix\", \"sys_prefix\" are mutually exclusive")
  }

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
  glue::glue(
    .sep = "\n",
    # 'c.GatewayClient.url = "http://{host}:{port+1}"',

    'c.NotebookApp.ip = "{host}"',
    'c.NotebookApp.allow_origin = "*"',
    'c.NotebookApp.port = {port}',
    sprintf('c.NotebookApp.open_browser = %s', ifelse(isTRUE(open_browser), "True", "False")),
    'c.NotebookApp.base_url = "/jupyter/"',
    'c.NotebookApp.token = "{token}"',
    'c.NotebookApp.password = ""',
    'c.NotebookApp.notebook_dir = "{root_dir}"',

    'c.NotebookApp.tornado_settings = {{',
    '  \'headers\' : {{',
    '    \'Content-Security-Policy\' : "frame-ancestors * \'self\' "',
    '  }}',
    '}}'
  )
}

#' @rdname jupyter
#' @export
jupyter_launch <- function(workdir = getwd(), host = "127.0.0.1", port = 8888,
                           open_browser = TRUE,
                           async = TRUE, ...){
  # add_jupyter()
  stopifnot(dir.exists(workdir))
  workdir <- normalizePath(workdir, mustWork = TRUE)
  conf <- jupyter_options(root_dir = workdir, host = host, port = port, open_browser = open_browser, ...)
  conf_dir <- tempfile()
  dir.create(file.path(conf_dir, "custom"), showWarnings = FALSE, recursive = TRUE)
  conf_dir <- normalizePath(conf_dir)
  writeLines(conf, con = file.path(conf_dir, "jupyter_notebook_config.py"))
  writeLines(c(
    "define(['base/js/namespace'], function(Jupyter){",
    "  Jupyter._target = '_self';",
    "});"
  ), con = file.path(conf_dir, "custom", "custom.js"))
  # JUPYTER_CONFIG_DIR

  command <- sprintf("%s notebook", shQuote(jupyter_bin(), type = "cmd"))
  env <- sprintf("JUPYTER_CONFIG_DIR=%s", shQuote(conf_dir, type = "cmd"))



  if(async){
    if(system.file(package = 'dipsaus') != ""){
      expr <- bquote({
        ns <- asNamespace('rpymat')
        ns$run_command(.(command), workdir = .(workdir), env = .(env), wait = TRUE)
      })
      dipsaus::rs_exec(expr, rs = TRUE, wait = FALSE, quoted = TRUE, name = "Jupyter Notebook")
    } else {
      run_command(command, workdir = workdir, env = env, wait = FALSE,
                  stdout = FALSE, stderr = FALSE)
    }
  } else {
    run_command(command, workdir = workdir, env = env, wait = TRUE)
  }

  invisible()
  # run_command(sprintf("%s --config-dir", shQuote(jupyter_bin(), type = "cmd")), workdir = workdir, env = sprintf("JUPYTER_CONFIG_DIR=%s", shQuote(conf_dir, type = "cmd")), )
}

