#' @title Run 'Python' script
#' @description A wrapper of \code{\link[reticulate]{py_run_file}},
#' but with \code{rpymat} enabled
#' @param x script path
#' @param work_dir working directory of the script
#' @param local,convert passed to \code{\link[reticulate]{py_run_file}}
#' @return The values returned by \code{\link[reticulate]{py_run_file}}
#' @examples
#'
#' \dontrun{
#'
#' # Please configure conda environment first
#'
#' x <- tempfile()
#' writeLines(c(
#'   "import re",
#'   "zipcode = re.findall(r'[0-9]{5,6}', r.address)"
#' ), con = x)
#'
#' address <- '2341 Main St., 72381'
#' rpymat::run_script(x)
#'
#' py$zipcode
#'
#' }
#'
#' @export
run_script <- function(x, work_dir = NULL, local = FALSE, convert = FALSE){

  if(length(work_dir) == 1 && dir.exists(work_dir)){
    `_cwd` <- getwd()
    on.exit({setwd(`_cwd`)}, add = TRUE, after = TRUE)
    work_dir <- normalizePath(work_dir)
    setwd(work_dir)
  }

  x <- normalizePath(x, mustWork = TRUE)
  ensure_rpymat(verbose = FALSE)
  reticulate::py_run_file(file = x, local = local, convert = convert)
}

#' Enable interactive 'python' from R
#' @description Allows users to type 'python' command from R
#' console for quick code evaluation or debugging.
#' @param ... passed to \code{\link[reticulate]{repl_python}}
#' in \code{'reticulate'} package
#' @return See \code{\link[reticulate]{repl_python}}
#' @export
repl_python <- function(...) {
  ensure_rpymat(verbose = FALSE)
  reticulate::repl_python(...)
}



run_template <- function(path, .envir = parent.frame(), .verbose = FALSE, ...) {
  # path <- "./inst/py_templates/choose-dir.py"
  if(!file.exists(path)) {
    stop("`run_template`: cannot find template path")
  }

  # ensure conda
  if(!dir.exists(env_path())) {
    configure_conda()
  }

  env <- new.env(parent = .envir)
  list2env(list(...), envir = env)
  template_script <- paste(readLines(path), collapse = "\n")
  script <- glue::glue(template_script, .envir = env,
                       .open = "{{", .close = "}}", .trim = TRUE, .null = "None")
  tf <- tempfile()
  writeLines(script, con = tf, sep = "\n")
  tf <- normalizePath(tf, mustWork = TRUE)
  tfout <- paste0(tf, ".Rout")

  on.exit({
    unlink(tf)
    unlink(tfout)
  })

  # for windows
  tf2 <- gsub("\\\\", "\\\\\\\\", tf)

  code <- rpymat::run_command(sprintf("python -q %s", shQuote(tf2)), stdout = tfout, stderr = tfout, wait = TRUE)
  if( .verbose ) {
    message("Program exits with status: ", code)
  }

  re <- character()
  if(file.exists(tfout)) {
    s <- readLines(tfout)
    if( .verbose ) {
      message(paste(s, collapse = "\n"))
    }
    idx1 <- which(startsWith(s, "RPYMAT_RESULT_START"))
    idx2 <- which(startsWith(s, "RPYMAT_RESULT_END"))
    if(length(idx1)) {
      idx1 <- idx1[[1]]
      if(length(idx2)) {
        idx2 <- idx2[[1]]
        if(idx2 <= idx1) {
          idx2 <- length(s) + 1
        }
      } else {
        idx2 <- length(s) + 1
      }
      idx1 <- idx1 + 1
      idx2 <- idx2 - 1
      if(idx1 <= idx2) {
        re <- s[seq(idx1, idx2)]
      }
    }
  }

  return( structure(re, status = code) )
}

