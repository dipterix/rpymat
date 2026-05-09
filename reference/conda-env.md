# 'Miniconda' environment

These functions/variables are used to configure 'Miniconda' environment.

## Usage

``` r
CONDAENV_NAME(env_name)

conda_path()

conda_bin()

env_path(env_name = NA)

list_pkgs(..., env_name = NA)

configure_matlab(matlab, python_ver = "auto")

configure_conda(
  python_ver = "auto",
  packages = NULL,
  matlab = NULL,
  update = FALSE,
  force = FALSE,
  standalone = FALSE,
  env_name = CONDAENV_NAME()
)

conda_tos(channel, agree = TRUE, silent_fail = FALSE)

remove_conda(ask = TRUE, env_name = NA)

add_packages(packages = NULL, python_ver = "auto", ..., env_name = NA)

ensure_rpymat(verbose = TRUE, cache = TRUE, env_name = NA)

matlab_engine()

call_matlab(
  fun,
  ...,
  .options = getOption("rpymat.matlab_opt", "-nodesktop -nojvm"),
  .debug = getOption("rpymat.debug", FALSE)
)
```

## Arguments

- env_name:

  alternative environment name to use; default is `"rpymat-conda-env"`

- ...:

  for `add_packages`, these are additional parameters passing to
  [`conda_install`](https://rstudio.github.io/reticulate/reference/conda-tools.html);
  for `call_matlab`, `...` are the parameters passing to `fun`

- matlab:

  'Matlab' path to add to the configuration path; see 'Details'

- python_ver:

  `python` version to use; see 'Configuration'

- packages:

  additional `python` or `conda` packages to install

- update:

  whether to update `conda`; default is false

- force:

  whether to force install the 'Miniconda' even a previous version
  exists; default is false. Setting `false=TRUE` rarely works. Please
  see 'Configuration'.

- standalone:

  whether to install `conda` regardless of existing `conda` environment

- channel:

  channels from which the term-of-service is to be agreed on

- agree:

  whether to agree on or reject the terms; default is true

- silent_fail:

  whether the failure to agreeing to the term should not result in
  error; default is `FALSE`, which results in error if the command
  fails.

- ask:

  whether to ask for user's agreement to remove the repository. This
  parameter should be true if your functions depend on `remove_conda`
  (see 'CRAN Repository Policy'). This argument might be removed and
  force to be interactive in the future.

- verbose:

  whether to print messages

- cache:

  whether to use cached configurations; default is true

- fun:

  'Matlab' function name, character (experimental)

- .options:

  'Matlab' compiler options

- .debug:

  whether to enable debug mode

## Value

None

## Background & Objectives

Package `reticulate` provides sophisticated tool-sets that allow us to
call `python` functions within `R`. However, the installation of
'Miniconda' and `python` can be tricky on many platforms, for example,
the 'M1' chip, or some other 'ARM' machines. The package `rpymat`
provides easier approach to configure on these machines with totally
isolated environments. Any modifications to this environment will not
affect your other set ups.

Since 2014, 'Matlab' has introduced its official compiler for `python`.
The package `rpymat` provides a simple approach to link the compiler,
provided that you have proper versions of 'Matlab' installed.
[Here](https://www.mathworks.com/support/requirements/python-compatibility.html)
is a list of 'Matlab' versions with official compilers and their
corresponding `python` versions.

## Configuration

If 'Matlab' compiler is not to be installed, In most of the cases,
function `configure_conda` with default arguments automatically
downloads the latest 'Miniconda' and configures the latest `python`. If
any other versions of 'Miniconda' is ought to be installed, please set
options `"reticulate.miniconda.url"` to change the source location.

If 'Matlab' is to be installed, please specify the 'Matlab' path when
running `configure_conda`. If the environment has been setup,
`configure_matlab` can link the 'Matlab' compilers without removing the
existing environment. For 'ARM' users, unfortunately, there will be no
'Matlab' support as the compilers are written for the 'Intel' chips.

## Initialization

Once `conda` and `python` environment has been installed, make sure you
run `ensure_rpymat()` before running any `python` code. This function
will make sure correct compiler is linked to your current `R` session.

## Examples

``` r

# The script will interactively install \code{conda} to `R_user_dir`
if (FALSE) { # \dontrun{

# Install conda and python 3.9

configure_conda(python_ver = '3.9')


# Add packages h5py, pandas, jupyter

add_packages(c('h5py', 'pandas', 'jupyter'))

# Add pip packages

add_packages("itk", pip = TRUE)

# Initialize the isolated environment

ensure_rpymat()


# Remove the environment

remove_conda()

} # }
```
