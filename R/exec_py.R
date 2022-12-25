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

