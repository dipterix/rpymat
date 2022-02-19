# Troubleshoot issues on ARM OSX

# Do not install numpy
rpymat::configure_conda()

# This will crash R
rpymat::add_packages("numpy")

rpymat::ensure_rpymat()
reticulate::repl_python()

rpymat::run_command("conda uninstall -y numpy")

# This works fine
rpymat::add_packages("numpy", pip = TRUE)

# This crash because h5py uses numpy
rpymat::add_packages("h5py")
rpymat::run_command("conda uninstall -y r-base")

rpymat::run_command("pip uninstall -y numpy")
rpymat::run_command("pip install --force-reinstall --no-deps numpy==1.12.2")

rpymat::add_packages("numpy", pip = TRUE)
rpymat::add_packages("pandas", pip = TRUE)

rpymat::run_command('conda install "libblas=*=*openblas"')

mv R_HOME/lib/libRblas.so R_HOME/lib/libRblas.so.keep
ln -s /usr/lib64/libopenblasp.so.0 R_HOME/lib/libRblas.so
_HOME/etc/ldpaths.

normalizePath("/Library/Frameworks/R.framework/Resources/lib/libRblas.dylib")
# mv "/Library/Frameworks/R.framework/Resources/lib/libRblas.dylib" "/Library/Frameworks/R.framework/Resources/lib/libRblas.dylib.origin"

# ln -s "/Users/dipterix/Library/r-rpymat/miniconda/envs/rpymat-conda-env/lib/libopenblas_vortexp-r0.3.18.dylib" "/Library/Frameworks/R.framework/Resources/lib/libRblas.dylib"

# mv "/Library/Frameworks/R.framework/Resources/lib/libRlapack.dylib" "/Library/Frameworks/R.framework/Resources/lib/libRlapack.dylib.origin"

# ln -s "/Users/dipterix/Library/r-rpymat/miniconda/envs/rpymat-conda-env/lib/libopenblas_vortexp-r0.3.18.dylib" "/Library/Frameworks/R.framework/Resources/lib/libRlapack.dylib"
