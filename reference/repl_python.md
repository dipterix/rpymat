# Enable interactive 'python' from R

Allows users to type 'python' command from R console for quick code
evaluation or debugging.

## Usage

``` r
repl_python(..., env_name = NA)
```

## Arguments

- ...:

  passed to
  [`repl_python`](https://rstudio.github.io/reticulate/reference/repl_python.html)
  in `'reticulate'` package

- env_name:

  environment name to activate, if not default. This argument is ignored
  if any other environment is activated; see
  [`ensure_rpymat`](http://dipterix.org/rpymat/reference/conda-env.md).

## Value

See
[`repl_python`](https://rstudio.github.io/reticulate/reference/repl_python.html)
