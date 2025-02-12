fix_indent <- function(code) {
  code_c <- strsplit(code, "\n")[[1]]
  if(length(code_c)) {
    is_blank_line <- !nzchar(trimws(code_c[[1]]))
    if( is_blank_line ) {
      code_c <- code_c[-1]
    }
    if(length(code_c)) {
      leading_wsp <- gsub("[^ ].*$", "", code_c)
      leading_wsp <- min(nchar(leading_wsp))
      if(leading_wsp > 0) {
        code_c <- substring(code_c, leading_wsp + 1)
      }
      code <- paste(code_c, collapse = "\n")
    }
  }
  code
}

#' @name run_pyscript
#' @title Run 'Python' script
#' @description A wrapper of \code{\link[reticulate]{py_run_file}},
#' but with \code{rpymat} enabled
#' @param x 'Python' script path
#' @param code 'Python' code
#' @param work_dir working directory of the script
#' @param local,convert passed to \code{\link[reticulate]{py_run_file}}
#' @param globals named list of global R variables used by 'Python' script
#' @param env_name \code{'conda'} environment name to activate, if not default.
#' It is only recommended for advanced users. For easier handling cases, use
#' \code{\link{ensure_rpymat}} to activate the environment before calling
#' 'Python'. If \code{env_name} is set other than activated, the evaluation will
#' occur in a separate session (\code{force_child_process} is always set to true
#' in such case).
#' @param force_child_process whether to force running the script in a separated
#' process; default is \code{FALSE}
#' @param ... passed to internal calls; some useful arguments include
#' \describe{
#' \item{\code{rs}}{\code{logical(1)}, whether to attempt using 'RStudio'
#' background job to run the script; default is \code{FALSE}}
#' \item{\code{args}}{logical(1), only used when \code{rs} is false,
#' passed to \code{\link{system2}}}
#' }
#' @returns The values returned by \code{\link[reticulate]{py_run_file}}
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
run_script <- function(x, work_dir = NULL, local = FALSE, convert = FALSE, globals = list()){

  if(length(work_dir) == 1 && dir.exists(work_dir)){
    `_cwd` <- getwd()
    on.exit({setwd(`_cwd`)}, add = TRUE, after = TRUE)
    work_dir <- normalizePath(work_dir)
    setwd(work_dir)
  }

  x <- normalizePath(x, mustWork = TRUE)
  ensure_rpymat(verbose = FALSE)

  py <- import_main(convert = FALSE)
  if(length(globals) > 0) {
    lapply(names(globals), function(nm) {
      if(trimws(nm) != "") {
        py[[ nm ]] <- globals[[nm]]
      } else {
        stop("rpymat::run_script(..., globals) - `globals` must be a NAMED list.")
      }
    })
  }
  reticulate::py_run_file(file = x, local = local, convert = convert)
}

#' @rdname run_pyscript
#' @export
run_pyscript <- function(x, work_dir = NULL, local = FALSE, convert = FALSE, globals = list(), env_name = NA, force_child_process = FALSE, ...) {

  rpymat::ensure_rpymat(verbose = FALSE)

  env_name <- clean_env_name(env_name)
  current_env_name <- ensure_rpymat_internals$name()

  if(length(work_dir) == 1 && dir.exists(work_dir)){
    work_dir <- normalizePath(work_dir)
  } else {
    work_dir <- getwd()
  }

  if(force_child_process || (length(current_env_name) && !identical(env_name, current_env_name))) {
    # this script needs to run in rs_exec

    tempdir(check = TRUE)
    global_tmpfile <- tempfile()
    saveRDS(list(
      x = x,
      work_dir = work_dir,
      local = local,
      convert = convert,
      globals = globals
    ), file = global_tmpfile)

    global_tmpfile <- normalizePath(global_tmpfile, mustWork = TRUE, winslash = "/")
    global_pickle <- sprintf("%s.pickle", global_tmpfile)

    on.exit({
      unlink(global_tmpfile)
      # unlink(global_pickle)
    })

    res <- rs_exec(bquote({
      global_tmpfile <- .(global_tmpfile)
      rpymat::ensure_rpymat(verbose = FALSE, env_name = .(env_name))

      .globals <- readRDS(global_tmpfile)
      with(.globals, {
        results <- rpymat::run_script(x = x, work_dir = work_dir, local = TRUE, convert = FALSE, globals = globals)
        nms <- names(results)
        nms <- nms[nms != "r" & !startsWith(nms, "_")]
        nms <- nms[vapply(nms, function(nm) {
          obj <- results[nm]
          return(!inherits(obj, "python.builtin.module"))
        }, FALSE)]
        results <- rpymat::py_dict(keys = nms, convert = FALSE, values = lapply(nms, function(nm) {
          results[nm]
        }))
        reticulate::py_save_object(results, .(global_pickle), pickle = "pickle")
      })

    }), quoted = TRUE, wait = TRUE, name = sprintf("Running python (env: %s)", env_name), focus_on_console = TRUE, ...)

    if(res == 0) {
      # Get results
      if(file.exists(global_pickle)) {
        results <- reticulate::py_load_object(filename = global_pickle, convert = convert)
      } else {
        results <- NULL
      }
      return(results)
    } else {
      stop(attr(res, "rs_exec_error"))
    }

  } else {
    run_script(x = x, work_dir = work_dir, local = local, convert = convert, globals = globals)
  }
}

#' @rdname run_pyscript
#' @export
run_pystring <- function(code, work_dir = NULL, local = FALSE, convert = FALSE, globals = list()) {

  code <- fix_indent(code)

  if(length(work_dir) == 1 && dir.exists(work_dir)){
    `_cwd` <- getwd()
    on.exit({setwd(`_cwd`)}, add = TRUE, after = TRUE)
    work_dir <- normalizePath(work_dir)
    setwd(work_dir)
  }

  ensure_rpymat(verbose = FALSE)

  py <- import_main(convert = FALSE)
  if(length(globals) > 0) {
    lapply(names(globals), function(nm) {
      if(trimws(nm) != "") {
        py[[ nm ]] <- globals[[nm]]
      } else {
        stop("rpymat::run_script(..., globals) - `globals` must be a NAMED list.")
      }
    })
  }
  return(reticulate::py_run_string(code = code, local = local, convert = convert))
}

#' Enable interactive 'python' from R
#' @description Allows users to type 'python' command from R
#' console for quick code evaluation or debugging.
#' @param ... passed to \code{\link[reticulate]{repl_python}}
#' in \code{'reticulate'} package
#' @param env_name environment name to activate, if not default. This argument
#' is ignored if any other environment is activated; see
#' \code{\link{ensure_rpymat}}.
#' @returns See \code{\link[reticulate]{repl_python}}
#' @export
repl_python <- function(..., env_name = NA) {
  ensure_rpymat(verbose = FALSE, env_name = env_name)
  reticulate::repl_python(...)
}



run_template <- function(path, .envir = parent.frame(), .verbose = FALSE, ..., .env_name = NA) {
  # path <- "./inst/py_templates/choose-dir.py"
  if(!file.exists(path)) {
    stop("`run_template`: cannot find template path")
  }

  # ensure conda
  if(!dir.exists(env_path(env_name = .env_name))) {
    configure_conda(env_name = .env_name)
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

  code <- rpymat::run_command(sprintf("python -q %s", shQuote(tf2)), stdout = tfout, stderr = tfout, wait = TRUE, env_name = .env_name)
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

