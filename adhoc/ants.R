library(rpymat)
library(reticulate)
python_ver <- "3.9"
rpymat::configure_conda(python_ver = python_ver, packages = c(
  "lapack", "openblas", "scipy", "h5py", "nibabel", "numpy", "plotly"))
rpymat::add_packages("libpng", pip = TRUE)
rpymat::add_packages("cpython", pip = TRUE)
rpymat::add_packages("antspyx", pip = TRUE, python_ver = python_ver)
# rpymat::remove_conda()
# brew install libpng
rpymat::ensure_rpymat()
ants <- import('ants')
