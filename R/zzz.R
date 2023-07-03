
rpymat_is_setup <- function() {
  return( dir.exists(env_path()) )
}

#' @name rpymat-python-main
#' @title Get 'Python' main process environment
#' @description
#' \code{py} automatically converts 'Python' objects to R objects.
#' \code{\link{import_main}} does not convert by default; see 'Examples' for details.
#' @usage py
#' @returns The 'Python' main process as a module
#' @examples
#'
#' py_no_convert <- rpymat::import_main(convert = FALSE)
#'
#' py$a <- matrix(seq_len(16), 4)
#'
#' py_no_convert$a
#'
#' py$a
#'
#' @export
"py"
NULL

.onLoad <- function(libname, pkgname) {
  # backports::import(pkgname, c("R_user_dir", "deparse1"))
  pkg <- getNamespace(pkgname)

  options("rpymat.matlab_engine" = fastqueue2())

  main <- NULL

  makeActiveBinding("py", fun = function(){
    if (!is.null(main)) { return(main) }

    if( !rpymat_is_setup() ) {
      return( NULL )
    }

    reticulate <- asNamespace("reticulate")
    py <- tryCatch({
      if(isTRUE(reticulate$is_python_initialized())) {
        py <- import_main(convert = TRUE)
      } else {
        py <- NULL
      }
      py
    }, error = function(e) {
      tryCatch({
        return(reticulate$py)
      }, error = function(e){ NULL })
    })

    if(!is.null(py)) {
      main <<- py
    }
    main
  }, env = pkg)
}


