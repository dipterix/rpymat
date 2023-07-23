env_available <- function() {
  if(!dir.exists(env_path())) { return(FALSE) }
  if( !conda_is_user_defined() ) {
    if(! file.exists(conda_bin()) ) { return(FALSE) }
  }

  return(TRUE)
}

module_available <- function( module ) {
  if(!env_available()) { return(FALSE) }
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_module_available(module))
}

#' @title Read data frame from a \code{'xlsx'} file
#' @description
#' Tries to use \code{'readxl'} package or \code{'pandas'} to read data frame.
#' @param path \code{'xlsx'} file path
#' @param sheet either a character or an integer of which spread-sheet to read;
#' the number starts from \code{1}
#' @param method which method to use for reading the \code{'xlsx'} file;
#' choices are \code{'auto'} (automatically find proper method),
#' \code{'pandas'} (use \code{pandas.read_xlsx}), or \code{'readxl'} (use the
#' corresponding R package)
#' @param n_max maximum number of rows (excluding headers) to read
#' @param ... passed to 'Python' function \code{pandas.read_xlsx} or
#' \code{readxl::read_excel}, depending on \code{method}
#' @returns A \code{\link{data.frame}} table
#'
#' @examples
#'
#' \dontrun{
#'
#' rpymat::read_xlsx("Book1.xlsx", sheet = 1)
#'
#' rpymat::read_xlsx("Book1.xlsx", sheet = "sheet1")
#'
#' }
#'
#'
#' @export
read_xlsx <- function(path, sheet = NULL, method = c("auto", "pandas", "readxl"),
                      n_max = Inf, ...) {

  method <- match.arg(method)

  path <- normalizePath(path, mustWork = TRUE)

  if( method == "auto" ) {
    if( package_installed("readxl") ) {
      method <- "readxl"
    } else {
      method <- "pandas"
    }
  }

  if(!length(sheet)) {
    sheet <- 1L
  } else if(!is.character(sheet)) {
    sheet <- as.integer(sheet)
  }
  if(length(sheet) > 1) {
    stop("`sheet` must have length 1")
  }


  if( method == "readxl" ) {
    if( package_installed("rlang") ) {
      run_package_function("rlang", "check_installed", "readxl", .on_failure = "warning")
    }
    if( !package_installed("readxl") ) {
      stop("Please install package `readxl` first")
    }
    re <- run_package_function(
      "readxl",
      "read_excel",
      path = path,
      sheet = sheet,
      n_max = n_max,
      ...,
      .on_failure = "error"
    )
    return(as.data.frame(re))
  }


  # check methods
  pandas_available <- module_available("pandas")
  openpyxl_available <- module_available("openpyxl")
  pkg_add <- c("pandas", "openpyxl")[!c(pandas_available, openpyxl_available)]
  if(length(pkg_add)) {
    add_packages(packages = pkg_add)
  }

  pandas <- import("pandas", convert = FALSE, delay_load = FALSE)

  if(length(n_max) != 1 || !isTRUE(is.numeric(n_max)) ||
     is.na(n_max) || is.infinite(n_max)) {
    n_max <- NULL
  } else {
    n_max <- as.integer(n_max)
    if(n_max < 0) { n_max <- 0L }
  }

  # python starts from 0
  df <- pandas$read_excel(
    io = normalizePath(path),
    sheet_name = as.integer(sheet - 1L),
    nrows = n_max,
    ...
  )
  re <- py_to_r(df)
  return(re)
}
