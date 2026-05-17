# Fix `OpenMP` linking issue

Fix `OpenMP` linking issue

## Usage

``` r
fix_omp_conflict(env_name = NA)
```

## Arguments

- env_name:

  environment name, see
  [`env_path`](http://dipterix.org/rpymat/reference/conda-env.md).

## Value

Whether the symbolic link is created.

## Details

On `MacOS`, R framework and `conda` each ship their own `libomp.dylib`.
When both are loaded in the same process, the run-time raises error
because different copies of `OpenMP` are initialized. To solve this
issue, a symbolic link to R's framework copy ensures dynamical loading
resolves both to the same canonical path and initializes the run-time
exactly once.

## Examples

``` r

if (FALSE) { # \dontrun{

fix_omp_conflict()

} # }
```
