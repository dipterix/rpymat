
miniconda_installer_url <- function (version = "3") {
  url <- getOption("reticulate.miniconda.url")
  if (!is.null(url)) { return() }
  base <- "https://repo.anaconda.com/miniconda"
  rver <- R.version
  if( isTRUE(rver$arch == "aarch64") ){
    arch <- "armv71"
    if( startsWith(rver$os, 'darwin') ){
      options("reticulate.miniconda.url" = "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh")
    }
  }
  return()
}

# install_conda <- function(path, update = TRUE, force = FALSE){
#   ns <- asNamespace('reticulate')
#   ns$check_forbidden_install("Miniconda")
#   if (grepl(" ", path, fixed = TRUE))
#     stop("cannot install Miniconda into a path containing spaces")
#   ns$install_miniconda_preflight(path, force)
#   url <- miniconda_installer_url()
#   installer <- ns$miniconda_installer_download(url)
#   message("* Installing Miniconda -- please wait a moment ...")
#   ns$miniconda_installer_run(installer, path)
#   ok <- ns$miniconda_exists(path) && ns$miniconda_test(path)
#   if (!ok)
#     stopf("Miniconda installation failed [unknown reason]")
#   if (update)
#     ns$miniconda_update(path)
#   conda <- ns$miniconda_conda(path)
#   python <- ns$miniconda_python_package()
#   ns$messagef("* Miniconda has been successfully installed at %s.",
#            shQuote(path))
#   path
# }
