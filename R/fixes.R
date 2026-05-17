# Fix linking issues

#' @title Fix \verb{OpenMP} linking issue
#'
#' @param env_name environment name, see \code{\link{env_path}}.
#'
#' @details
#' On \verb{MacOS}, R framework and \verb{conda} each ship their own
#' \verb{libomp.dylib}. When both are loaded in the same process, the
#' run-time raises error because different copies of \verb{OpenMP}
#' are initialized. To solve this issue, a symbolic link to R's framework copy
#' ensures dynamical loading resolves both to the same canonical path and
#' initializes the run-time exactly once.
#'
#' @returns Whether the symbolic link is created.
#'
#' @examples
#'
#' \dontrun{
#'
#' fix_omp_conflict()
#'
#' }
#'
#' @export
fix_omp_conflict <- function(env_name = NA) {
  if (get_os() != "darwin") {
    return(invisible(FALSE))
  }
  r_omp <- file.path(R.home("lib"), "libomp.dylib")
  if (!file.exists(r_omp)) {
    return(invisible(FALSE))
  }
  if (!rpymat_is_setup()) {
    return(invisible(FALSE))
  }
  conda_omp <- file.path(env_path(env_name = env_name), "lib", "libomp.dylib")
  # Determine what conda_omp currently is:
  #   is_symlink=TRUE + file.exists=TRUE  -> valid symlink (check target)
  #   is_symlink=TRUE + file.exists=FALSE -> dangling symlink (R was upgraded
  #                                          and old version removed; re-link)
  #   is_symlink=FALSE + file.exists=TRUE -> real file (replace with symlink)
  #   is_symlink=FALSE + file.exists=FALSE -> absent; nothing to do
  link_target <- Sys.readlink(conda_omp)
  is_symlink  <- nzchar(link_target)
  if (!is_symlink && !file.exists(conda_omp)) {
    return(invisible(FALSE))
  }
  # If it is already a symlink, resolve it and compare against current R's
  # libomp (mustWork=FALSE so normalizePath does not error on dangling links).
  if (is_symlink) {
    r_real <- normalizePath(r_omp,    mustWork = FALSE)
    c_real <- normalizePath(conda_omp, mustWork = FALSE)
    if (identical(r_real, c_real)) {
      return(invisible(TRUE))   # Already points to the current R's libomp
    }
    # Stale symlink (R upgraded, target changed) – fall through to re-link
  }
  # Check write permission before attempting any file operation
  conda_lib <- dirname(conda_omp)
  if (file.access(conda_lib, mode = 2L) != 0L) {
    warning(
      "Cannot fix OpenMP conflict: no write access to '", conda_lib, "'.\n",
      "Run this function as the owner of that directory, or ask your admin ",
      "to run rpymat::fix_omp_conflict().\n",
      "As a temporary workaround you can set the environment variable ",
      "KMP_DUPLICATE_LIB_OK=TRUE, but be aware this may cause incorrect ",
      "multi-threaded results.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  # Backup the original *real* binary (once).  If conda_omp is already a
  # (stale) symlink there is nothing to back up — only a real file needs it.
  backup <- paste0(conda_omp, ".bak")
  if (!is_symlink && !file.exists(backup)) {
    if (!file.copy(conda_omp, backup)) {
      warning("Could not create backup '", backup, "'; aborting.", call. = FALSE)
      return(invisible(FALSE))
    }
  }
  # Remove old file/symlink, then create the new symlink.
  # On failure, restore from backup (only meaningful when a real file existed).
  if (!file.remove(conda_omp)) {
    warning("Could not remove '", conda_omp, "'; aborting.", call. = FALSE)
    return(invisible(FALSE))
  }
  if (!file.symlink(r_omp, conda_omp)) {
    if (!is_symlink) { file.copy(backup, conda_omp) }
    warning(
      "Could not create symlink '", conda_omp, "' -> '", r_omp, "'.\n",
      if (!is_symlink) "The original file has been restored from the backup.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}
