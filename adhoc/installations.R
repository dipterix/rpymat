# Install isolated conda
message("Install isolated conda and create a default environment: ", rpymat::CONDAENV_NAME())
rpymat::configure_conda()
rpymat::ensure_rpymat()
package_list <- list(
  default = c("numpy", "jupyter", "matplotlib", "pandas", "xarray", "h5py"),
  anaconda = c("scikit-learn")
)
lapply(names(package_list), function(channel){
  pkgs <- package_list[[channel]]
  if(channel == 'pip'){
    rpymat::add_packages(pkgs, pip = TRUE)
  } else if(channel == "default"){
    rpymat::add_packages(pkgs)
  } else {
    rpymat::add_packages(pkgs, channel = channel)
  }
})

rpymat::conda_bin()
export PATH="/Users/dipterix/Library/r-rpymat/miniconda/bin:$PATH"



dipsaus::rs_exec({
  rpymat::run_command(
    "jupyter notebook"
  )
})



