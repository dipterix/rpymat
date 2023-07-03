#' @name reticulate-reexports
#' @title Wrappers around \code{'reticulate'} package
#' @description Almost the same with \code{'reticulate'} functions, with
#' \code{rpymat} enabled by default and some minor changes
#' (see parameter \code{convert} and \code{local})
#' @param convert whether to convert \code{'Python'} objects to R; default is
#' \code{FALSE}. This is different to \code{'reticulate'}, but less error prone:
#' users must explicitly convert \code{'Python'} objects to R.
#' @param module,as,delay_load import \code{'Python'} module as alias
#' @param local whether to execute code locally so the memory sets free when the
#' function ends; default is true
#' @param object,data,x,code,keys,values,... passed to corresponding
#' \code{'reticulate'} functions as data inputs
#' @param name,silent,key,value,default other parameters passing to the
#' \code{'reticulate'} functions
#' @return \code{'Python'} built-in objects
#' @examples
#'
#' library(rpymat)
#' if(interactive() && dir.exists(env_path())) {
#'
#'   # tuple
#'   x <- tuple(1, 2, "a")
#'   print(x)
#'
#'   # convert to R object
#'   py_to_r(x)
#'
#'   # convert R object to python
#'   y <- r_to_py(list(a = 1, b = "s"))
#'
#'   # get element
#'   py_get_item(y, "a")
#'
#'   # get missing element
#'   py_get_item(y, "c", silent = TRUE)
#'
#' }
#'
NULL

#' @rdname reticulate-reexports
#' @export
import_main <- function(convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::import_main(convert = convert))
}


#' @rdname reticulate-reexports
#' @export
tuple <- function (..., convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::tuple(convert = convert, ...))
}

#' @rdname reticulate-reexports
#' @export
py_tuple <- tuple

#' @rdname reticulate-reexports
#' @export
py_help <- function (object) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_help(object = object))
}

#' @rdname reticulate-reexports
#' @export
np_array <- function (data, ...) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::np_array(data = data, ...))
}

#' @rdname reticulate-reexports
#' @export
import <- function (module, as = NULL, convert = FALSE, delay_load = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::import(module = module, as = as, convert = convert,
                            delay_load = delay_load))
}

#' @rdname reticulate-reexports
#' @export
r_to_py <- function (x, convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::r_to_py(x = x, convert = convert))
}

#' @rdname reticulate-reexports
#' @export
py_to_r <- function (x) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_to_r(x = x))
}

#' @rdname reticulate-reexports
#' @export
py_to_r_wrapper <- function (x) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_to_r_wrapper(x = x))
}

#' @rdname reticulate-reexports
#' @export
py_str <- function (object, ...) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_str(object = object, ...))
}

#' @rdname reticulate-reexports
#' @export
py_run_string <- function (code, local = TRUE, convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_run_string(code = code, local = local,
                                   convert = convert))
}

#' @rdname reticulate-reexports
#' @export
py_bool <- function (x) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_bool(x = x))
}

#' @rdname reticulate-reexports
#' @export
py_dict <- function (keys, values, convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_dict(keys = keys, values = values,
                             convert = convert))
}

#' @rdname reticulate-reexports
#' @export
py_call <- function (x, ...) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_call(x = x, ...))
}

#' @rdname reticulate-reexports
#' @export
py_del_attr <- function (x, name) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_del_attr(x = x, name = name))
}

#' @rdname reticulate-reexports
#' @export
py_del_item <- function (x, name) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_del_item(x = x, name = name))
}

#' @rdname reticulate-reexports
#' @export
py_eval <- function (code, convert = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_eval(code = code, convert = convert))
}

#' @rdname reticulate-reexports
#' @export
py_get_attr <- function (x, name, silent = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_get_attr(x = x, name = name, silent = silent))
}

#' @rdname reticulate-reexports
#' @export
py_set_attr <- function (x, name, value) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_set_attr(x = x, name = name, value = value))
}

#' @rdname reticulate-reexports
#' @export
py_get_item <- function (x, key, silent = FALSE) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_get_item(x = x, key = key, silent = silent))
}

#' @rdname reticulate-reexports
#' @export
py_set_item <- function (x, name, value) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_set_item(x = x, name = name, value = value))
}

#' @rdname reticulate-reexports
#' @export
py_len <- function (x, default = NULL) {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_len(x = x, default = default))
}

#' @rdname reticulate-reexports
#' @export
py_none <- function () {
  ensure_rpymat(verbose = FALSE)
  return(reticulate::py_none())
}



# lapply(
#   c("tuple", "py_help", "np_array", "import", "r_to_py", "py_to_r", "py_to_r_wrapper",
#     "py_str", "py_run_string", "py_bool", "py_dict", "py_call", "py_del_attr", "py_del_item",
#     "py_eval", "py_get_attr", "py_set_attr", "py_get_item", "py_set_item", "py_len", "py_none"),
#   function(name) {
#     # name <- "tuple"
#     ns <- asNamespace("reticulate")
#     fml <- formals(ns[[name]])
#     f <- function(){}
#     formals(f) <- fml
#
#     has_dots <- "..." %in% names(fml)
#     if( has_dots ) {
#       fml[["..."]] <- NULL
#     }
#
#     for(nm in names(fml)) {
#       fml[[nm]] <- str2lang(nm)
#     }
#     call <- as.call(c(list(quote(fname)), fml))
#     s <- deparse(call)
#     s <- gsub("^fname\\(", sprintf("reticulate\\:\\:%s\\(", name), s, perl = TRUE)
#     if( has_dots ) {
#       s <- gsub(")$", ", ...)", s)
#     }
#
#     body(f) <- bquote({
#       ensure_rpymat(verbose = FALSE)
#       return(.(str2lang(s)))
#     })
#     cat("#' @export\n")
#     cat(sprintf('%s <- %s', name, paste(deparse(f), collapse = "\n")), "\n\n")
#   }
# )

