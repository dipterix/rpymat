# rpymat 0.1.8

* Supports multiple environments for incompatible packages
* Fixed windows `dll` modification issue
* Added function to accept and reject `conda` term-of-service 
* Force configuring `conda` no longer download `miniconda` of already installed

# rpymat 0.1.7

* Set longer time-out when installing `conda`
* Allow to read `xlsx` from `pandas` (if user choose to install)
* Fixed a command issue on `linux` where dot-dot cannot be used on file path to get the parent directory; using R's built-in `dirname` instead
* Fixed a `jupyter` dry-run issue
* Allowed to set base `URL` for `jupyter`
* Allow to manually override `jupyter` token with `JUPYTER_TOKEN` environment variable instead of randomly-generated tokens. This setting is mainly designed for docker.

# rpymat 0.1.6

* Fixed an installation problem introduced in version `0.1.5`
* Added utility functions for basic `Python` types
* Added global variables in `run_pyscript`

# rpymat 0.1.5

* Added `choose_directory`, `choose_fileopen`, `choose_filesave` to select files using either base `R` or `Python` (via `tcl-tk` but with better interface on `osx`)
* Ported `reticulate` functions to convert between `R` and `Python` objects
* Allowed to set `conda` to installed version via system environment `R_RPYMAT_CONDA_EXE` and `R_RPYMAT_CONDA_PREFIX`; this change will allow `conda-forge` library `r-rpymat` to use already-installed `conda`
* Speed-up `ensure_rpymat` by caching the configuration

# rpymat 0.1.4

* Muffled `conda` information when running `Python` code or scripts
* Ported `reticulate::repl_python` function
* Allowed users to list installed packages
* Allowed `jupyter` to run in background in `RStudio` jobs
* Removed optional `jupyter` add-on packages from the installation script

# rpymat 0.1.3

* Fixed command-line script failing to load `conda`
* Allowed to use the `miniconda` installed at user's home directory
* Use slash instead of backslash on `Windows` system to avoid escaping issues

# rpymat 0.1.2

* Fixed `BLAS` issues on some `Unix` systems
* Added `Windows` support
* Added support to install, configure, launch, and stop `Jupyterlab`
* Added `run_command` to invoke enhanced system command under `conda` environment

# rpymat 0.1.1

* Added a `NEWS.md` file to track changes to the package.
