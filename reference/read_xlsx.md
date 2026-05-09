# Read data frame from a `'xlsx'` file

Tries to use `'readxl'` package or `'pandas'` to read data frame.

## Usage

``` r
read_xlsx(
  path,
  sheet = NULL,
  method = c("auto", "pandas", "readxl"),
  n_max = Inf,
  ...
)
```

## Arguments

- path:

  `'xlsx'` file path

- sheet:

  either a character or an integer of which spread-sheet to read; the
  number starts from `1`

- method:

  which method to use for reading the `'xlsx'` file; choices are
  `'auto'` (automatically find proper method), `'pandas'` (use
  `pandas.read_xlsx`), or `'readxl'` (use the corresponding R package)

- n_max:

  maximum number of rows (excluding headers) to read

- ...:

  passed to 'Python' function `pandas.read_xlsx` or
  `readxl::read_excel`, depending on `method`

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) table

## Examples

``` r

if (FALSE) { # \dontrun{

rpymat::read_xlsx("Book1.xlsx", sheet = 1)

rpymat::read_xlsx("Book1.xlsx", sheet = "sheet1")

} # }

```
