# Run 'Python' script

A wrapper of
[`py_run_file`](https://rstudio.github.io/reticulate/reference/py_run.html),
but with `rpymat` enabled

## Usage

``` r
run_script(
  x,
  work_dir = NULL,
  local = FALSE,
  convert = FALSE,
  globals = list()
)

run_pyscript(
  x,
  work_dir = NULL,
  local = FALSE,
  convert = FALSE,
  globals = list(),
  env_name = NA,
  force_child_process = FALSE,
  ...
)

run_pystring(
  code,
  work_dir = NULL,
  local = FALSE,
  convert = FALSE,
  globals = list()
)
```

## Arguments

- x:

  'Python' script path

- work_dir:

  working directory of the script

- local, convert:

  passed to
  [`py_run_file`](https://rstudio.github.io/reticulate/reference/py_run.html)

- globals:

  named list of global R variables used by 'Python' script

- env_name:

  `'conda'` environment name to activate, if not default. It is only
  recommended for advanced users. For easier handling cases, use
  [`ensure_rpymat`](http://dipterix.org/rpymat/reference/conda-env.md)
  to activate the environment before calling 'Python'. If `env_name` is
  set other than activated, the evaluation will occur in a separate
  session (`force_child_process` is always set to true in such case).

- force_child_process:

  whether to force running the script in a separated process; default is
  `FALSE`

- ...:

  passed to internal calls; some useful arguments include

  `rs`

  :   `logical(1)`, whether to attempt using 'RStudio' background job to
      run the script; default is `FALSE`

  `args`

  :   logical(1), only used when `rs` is false, passed to
      [`system2`](https://rdrr.io/r/base/system2.html)

- code:

  'Python' code

## Value

The values returned by
[`py_run_file`](https://rstudio.github.io/reticulate/reference/py_run.html)

## Examples

``` r

if (FALSE) { # \dontrun{

# Please configure conda environment first

x <- tempfile()
writeLines(c(
  "import re",
  "zipcode = re.findall(r'[0-9]{5,6}', r.address)"
), con = x)

address <- '2341 Main St., 72381'
rpymat::run_script(x)

py$zipcode

} # }
```
