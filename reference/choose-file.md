# Choose file or directory to open via `'Python'`

Choose a directory, one or multiple files to open, or choose a file to
save.

## Usage

``` r
choose_fileopen(
  initialfile = NULL,
  multiple = FALSE,
  title = ifelse(multiple, "Choose Files", "Choose a File"),
  message = "",
  verbose = FALSE,
  force = FALSE
)

choose_filesave()

choose_directory(
  initialdir = NULL,
  title = "Choose a Directory",
  message = "",
  verbose = FALSE
)
```

## Arguments

- initialfile, initialdir:

  initial selection of file or directory

- multiple:

  whether to open multiple files

- title, message:

  dialogue title and message

- verbose:

  whether to verbose debug information

- force:

  whether to force using `'Python'` when native `R` functions are
  available, default is false

## Value

User-selected paths. If the users select nothing, then `NULL` will be
returned. For multiple file selection, multiple paths will be returned.

## Details

Base-R has [`file.choose`](https://rdrr.io/r/base/file.choose.html)
function to choose files. However, users cannot select multiple files
nor directories. These functions fill the gap by using `'Python'`
`'tkinter'` package. Please make sure that one-time setup function
[`configure_conda`](http://dipterix.org/rpymat/reference/conda-env.md)
has executed before running these functions.

The functions must run as interactive mode. If you run the functions on
a server, most likely you will get nothing. The functions themselves do
not check if you are running under interactive sessions. You must check
by yourself.

## Examples

``` r

if(interactive()) {
  choose_fileopen(multiple = TRUE)
}


```
