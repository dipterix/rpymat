
# Isolated `Miniconda`, `Python`, and `Matlab` (experimental) Environment

<!-- badges: start -->
[![R-CMD-check](https://github.com/dipterix/rpymat/workflows/R-CMD-check/badge.svg)](https://github.com/dipterix/rpymat/actions)
<!-- badges: end -->

The goal of `rpymat` is to create a single isolated `Miniconda` and `Python` environment for reproducible pipeline scripts. The package is a shell of `reticulate` package, but provides more stable behaviors, especially on 'ARM' machines.

## Installation

You can install the released version of rpymat from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rpymat")
```

## Configure environment

Configure python after installation

```r
# change `python_ver` accordingly
rpymat::configure_conda(python_ver = 'auto')
```

Add `Python` or `conda` packages

``` r
# Add conda packages
rpymat::add_packages(c('pandas', 'numpy'))

# Add conda packages from channels
rpymat::add_packages(c('h5py'), channel = "conda-forge")

# Add pip packages
rpymat::add_packages(c('sklearn'), pip = TRUE)
```

## Use `Jupyterlab`

```r
# Install Jupyterlab, will install
# numpy, h5py, matplotlib, pandas, 
# jupyter, jupyterlab, jupyterlab-git, ipywidgets, jupyter-server-proxy
# jupyterlab_latex, jupyterlab_github, matlab_kernel
rpymat::add_jupyter()

# Launch Jupyterlab
rpymat::jupyter_launch(async = FALSE)
```

#### Advanced configurations:

```r
# Async option is only available in RStudio >= 1.4
async <- rstudioapi::isAvailable(version_needed = "1.4")


rpymat::jupyter_launch(
    async = async, workdir = "~",
    port = 18888, open_browser = TRUE,
    token = "IwontTellYouMyToken"
)
```

To query existing servers

```r
rpymat::jupyter_server_list()
#>        host  port                                              token
#> 1 127.0.0.1  8888 3hzWfGPa0EOmonaNS48jrTvpw07KiX7VKerA9ZTFJMkCOJMgfB
#> 2 127.0.0.1 18888                                IwontTellYouMyToken
```

To stop a server

```r
rpymat::jupyter_server_stop(port = 18888)
```


## Use `rpymat` with `reticulate`

```r
# Initialize the isolated environment

rpymat::ensure_rpymat()

rpymat::repl_python()
```

Then run python code interactively. 


Alternatively, you can use `rpymat::run_script(path)` to 
execute `Python` scripts, and use `reticulate::py` to obtain
the results.


## 


## Uninstall

The following command will erase the environment completely.

```r
rpymat::remove_conda()
```

