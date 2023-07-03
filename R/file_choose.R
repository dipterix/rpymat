#' @name choose-file
#' @title Choose file or directory to open via \code{'Python'}
#' @description
#' Choose a directory, one or multiple files to open, or choose a file to save.
#' @param initialfile,initialdir initial selection of file or directory
#' @param multiple whether to open multiple files
#' @param title,message dialogue title and message
#' @param verbose whether to verbose debug information
#' @param force whether to force using \code{'Python'} when native \code{R}
#' functions are available, default is false
#' @returns User-selected paths. If the users select nothing, then \code{NULL}
#' will be returned. For multiple file selection, multiple paths will
#' be returned.
#' @details
#' Base-R has \code{\link{file.choose}} function to choose files. However,
#' users cannot select multiple files nor directories. These functions fill
#' the gap by using \code{'Python'} \code{'tkinter'} package. Please make
#' sure that one-time setup function \code{\link{configure_conda}} has executed
#' before running these functions.
#'
#' The functions must run as interactive mode. If you run the functions on a
#' server, most likely you will get nothing. The functions themselves do not
#' check if you are running under interactive sessions. You must check by
#' yourself.
#'
#'
#' @examples
#'
#' if(interactive()) {
#'   choose_fileopen(multiple = TRUE)
#' }
#'
#'
#'
#' @export
choose_fileopen <- function(
    initialfile = NULL, multiple = FALSE,
    title = ifelse(multiple, "Choose Files", "Choose a File"),
    message = "", verbose = FALSE, force = FALSE) {
  if(length(initialfile) != 1 || is.na(initialfile)) {
    initialfile <- NULL
  } else {
    initialfile <- normalizePath(initialfile, mustWork = FALSE)
    initialfile <- gsub("\\\\", "/", initialfile)
  }

  multiple <- isTRUE(as.logical(multiple))
  title <- as.character(title)
  message <- as.character(message)

  if( multiple || force ) {
    re <- run_template(
      system.file(package = "rpymat", "py_templates", "choose-fileopen.py"),
      initialfile = initialfile,
      title = title,
      message = message,
      multiple = multiple,
      .verbose = verbose
    )
  } else {
    re <- tryCatch({
      file.choose(new = FALSE)
    }, error = function(e) {
      NULL
    })
  }

  if(length(re)) {
    re <- re[trimws(re) != ""]
    if(length(re)) {
      return(re)
    }
  }
  return(NULL)
}

#' @rdname choose-file
#' @export
choose_filesave <- function() {
  re <- tryCatch({
    file.choose(new = TRUE)
  }, error = function(e) {
    NULL
  })
  re
}

#' @rdname choose-file
#' @export
choose_directory <- function(
    initialdir = NULL, title = "Choose a Directory", message = "",
    verbose = FALSE) {
  if(length(initialdir) != 1 || is.na(initialdir)) {
    initialdir <- getwd()
  }
  initialdir <- normalizePath(initialdir, mustWork = FALSE)
  initialdir <- gsub("\\\\", "/", initialdir)

  title <- as.character(title)
  message <- as.character(message)

  # DIPSAUS DEBUG START
  # devtools::load_all()
  # title = "Choose a Directory"
  # message = title
  # initialdir = "."
  # path <- system.file(package = "rpymat", "py_templates", "choose-dir.py")
  # env <- new.env()
  # template_script <- paste(readLines(path), collapse = "\n")
  # script <- glue::glue(template_script, .envir = env,
  #                      .open = "{{", .close = "}}", .trim = TRUE, .null = "None")
  # script
  # verbose = TRUE

  re <- run_template(
    system.file(package = "rpymat", "py_templates", "choose-dir.py"),
    initialdir = initialdir,
    title = title,
    message = message,
    .verbose = verbose
  )
  if(length(re)) {
    re <- re[trimws(re) != ""]
    if(length(re)) {
      return(re[[1]])
    }
  }
  return(NULL)
}
