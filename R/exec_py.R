#' @export
run_script <- function(x, work_dir = NULL, local = FALSE, convert = FALSE){

  if(length(work_dir) == 1 && dir.exists(work_dir)){
    `_cwd` <- getwd()
    on.exit({setwd(`_cwd`)}, add = TRUE, after = TRUE)
    work_dir <- normalizePath(work_dir)
    setwd(work_dir)
  }

  x <- normalizePath(x, mustWork = TRUE)
  rpymat::ensure_rpymat()
  reticulate::py_run_file(file = x, local = local, convert = convert)
}
