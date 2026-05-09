# Get 'Python' main process environment

`py` automatically converts 'Python' objects to R objects.
[`import_main`](http://dipterix.org/rpymat/reference/reticulate-reexports.md)
does not convert by default; see 'Examples' for details.

## Usage

``` r
py
```

## Format

An object of class `NULL` of length 0.

## Value

The 'Python' main process as a module

## Examples

``` r

if(interactive() && dir.exists(env_path())) {

py_no_convert <- rpymat::import_main(convert = FALSE)

py$a <- matrix(seq_len(16), 4)

py_no_convert$a

py$a

}
```
