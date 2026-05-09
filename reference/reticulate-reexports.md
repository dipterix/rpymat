# Wrappers around `'reticulate'` package

Almost the same with `'reticulate'` functions, with `rpymat` enabled by
default and some minor changes (see parameter `convert` and `local`)

## Usage

``` r
import_main(convert = FALSE)

tuple(..., convert = FALSE)

py_tuple(..., convert = FALSE)

py_help(object)

np_array(data, ...)

import(module, as = NULL, convert = FALSE, delay_load = FALSE)

r_to_py(x, convert = FALSE)

py_to_r(x)

py_to_r_wrapper(x)

py_str(object, ...)

py_run_string(code, local = TRUE, convert = FALSE)

py_bool(x)

py_dict(keys, values, convert = FALSE)

py_call(x, ...)

py_del_attr(x, name)

py_del_item(x, name)

py_eval(code, convert = FALSE)

py_get_attr(x, name, silent = FALSE)

py_set_attr(x, name, value)

py_get_item(x, key, silent = FALSE)

py_set_item(x, name, value)

py_len(x, default = NULL)

py_none()
```

## Arguments

- convert:

  whether to convert `'Python'` objects to R; default is `FALSE`. This
  is different to `'reticulate'`, but less error prone: users must
  explicitly convert `'Python'` objects to R.

- object, data, x, code, keys, values, ...:

  passed to corresponding `'reticulate'` functions as data inputs

- module, as, delay_load:

  import `'Python'` module as alias

- local:

  whether to execute code locally so the memory sets free when the
  function ends; default is true

- name, silent, key, value, default:

  other parameters passing to the `'reticulate'` functions

## Value

`'Python'` built-in objects

## Examples

``` r

library(rpymat)
if(interactive() && dir.exists(env_path())) {

  # tuple
  x <- tuple(1, 2, "a")
  print(x)

  # convert to R object
  py_to_r(x)

  # convert R object to python
  y <- r_to_py(list(a = 1, b = "s"))

  # get element
  py_get_item(y, "a")

  # get missing element
  py_get_item(y, "c", silent = TRUE)

}
```
