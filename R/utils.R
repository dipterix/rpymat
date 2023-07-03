#' @title Get 'Python' built-in object
#' @param name object name
#' @param convert see \code{\link[reticulate]{import_builtins}}
#' @returns A python built-in object specified by \code{name}
#' @examples
#'
#' if(interactive() && dir.exists(env_path())) {
#'
#'
#' # ------ Basic case: use python `int` as an R function ---------
#' py_int <- py_builtin("int", convert = TRUE)
#'
#' # a is an R object now
#' a <- py_int(9)
#' print(a)
#' class(a)
#'
#' # ------ Use python `int` as a Python function -----------------
#' py_int2 <- py_builtin("int", convert = FALSE)
#'
#' # b in a python object
#' b <- py_int2(9)
#'
#' # There is no '[1] ' when printing
#' print(b)
#' class(b)
#'
#' # convert to R object
#' py_to_r(b)
#'
#'
#'
#' }
#'
#' @export
py_builtin <- function(name, convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  builtins <- reticulate::import_builtins(convert = convert)
  builtins[[name]]
}

#' @title Slice index in 'Python' arrays
#' @param ... passing to \code{slice} ('Python')
#' @returns Index slice instance
#'
#' @examples
#'
#'
#' if(interactive() && dir.exists(env_path())) {
#'
#'   x <- np_array(array(seq(20), c(4, 5)))
#'
#'   # equivalent to x[::2]
#'   x[py_slice(NULL, NULL, 2L)]
#'
#' }
#' @export
py_slice <- function(...) {
  ensure_rpymat(verbose = FALSE)
  reticulate::import_builtins(convert = FALSE)$slice(...)
}

#' @title List in 'Python'
#' @param ... passing to \code{list} ('Python')
#' @param convert whether to convert the results back into R; default is no
#' @returns List instance, or an R vector if converted
#'
#' @examples
#'
#'
#' if(interactive() && dir.exists(env_path())) {
#'
#'   py_list(list(1,2,3))
#'   py_list(c(1,2,3))
#'
#'   py_list(array(1:9, c(3,3)))
#'   py_list(list(list(1:3), letters[1:3]))
#'
#' }
#' @export
py_list <- function(..., convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  reticulate::import_builtins(convert = convert)$list(...)
}

to_py <- function(x, ifnotpy = NULL, ifpy = NULL) {
  ensure_rpymat(verbose = FALSE)

  x2 <- substitute(x)
  ifnotpy <- substitute(ifnotpy)
  ifpy <- substitute(ifpy)
  parent_frame <- parent.frame()
  if( !is.null(ifnotpy) && !inherits(x, "python.builtin.object") ) {
    eval(ifnotpy, parent_frame)
  } else if( !is.null(ifpy) && inherits(x, "python.builtin.object") ) {
    eval(ifpy, parent_frame)
  }
  x <- eval(x2, new.env(parent = parent_frame))
  if( !inherits(x, "python.builtin.object") ) {
    x <- r_to_py(x)
  }
  return(x)
}

is_py_inherits <- function(x, class = NULL) {
  inherits(x, c("python.builtin.object", class))
}


to_r <- function(x) {
  tryCatch({
    ensure_rpymat(verbose = FALSE)
    reticulate::py_to_r(x)
  }, error = function(e) {
    x
  })
}
