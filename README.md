
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
rpymat::configure_conda(python_ver = '3.9')
```

Add `Python` or `conda` packages

``` r
# Add packages h5py, pandas, jupyter
rpymat::add_packages(c('h5py', 'pandas', 'jupyter'))
```

## Use `rpymat` with `reticulate`

```r
# Initialize the isolated environment

rpymat::ensure_rpymat()

reticulate::repl_python()
```

Then run python code interactively. 


Alternatively, you can use `rpymat::run_script(path)` to 
execute `Python` scripts, and use `reticulate::py` to obtain
the results.



## Uninstall

The following command will erase the environment completely.

```r
rpymat::remove_conda()
```

