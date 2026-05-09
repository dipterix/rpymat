# Install, register, launch 'Jupyter' notebook to the virtual environment

Install, register, launch 'Jupyter' notebook to the virtual environment

## Usage

``` r
add_jupyter(..., register_R = TRUE)

jupyter_bin()

jupyter_register_R(
  user = NULL,
  name = "ir",
  displayname = "R",
  rprofile = NULL,
  prefix = NULL,
  sys_prefix = NULL,
  verbose = getOption("verbose")
)

jupyter_options(
  root_dir,
  host = "127.0.0.1",
  port = 8888,
  open_browser = FALSE,
  token = rand_string(),
  base_url = "/jupyter/"
)

jupyter_launch(
  host = "127.0.0.1",
  port = 8888,
  open_browser = TRUE,
  workdir = getwd(),
  async = FALSE,
  ...,
  dry_run = FALSE
)

jupyter_check_launch(
  port = 8888,
  host = "127.0.0.1",
  open_browser = TRUE,
  workdir = getwd(),
  async = "auto",
  ...
)

jupyter_server_list()

jupyter_server_stop(port, ...)

jupyter_server_stop_all(...)
```

## Arguments

- ...:

  for `add_jupyter`, these are additional parameters passed to
  `jupyter_register_R`; for `jupyter_launch`, these are additional
  parameters passed to `jupyter_options`

- register_R:

  whether to register `IRkernel` to the notebook

- user, name, displayname, rprofile, prefix, sys_prefix, verbose:

  see [`installspec`](https://rdrr.io/pkg/IRkernel/man/installspec.html)

- root_dir, workdir:

  default root directory of the notebook

- host, port:

  'IP' and port of the hosting 'URL'

- open_browser:

  whether to open the browser once launched

- token:

  access token of the notebook

- base_url:

  base address, default is `'/jupyter/'`

- async:

  whether to open the notebook in the background

- dry_run:

  whether to display the command instead of executing them; used to
  debug the code

## Value

`jupyter_bin` returns the 'Jupyter' notebook binary path;
`jupyter_options` returns the 'Jupyter' configuration in strings;
`jupyter_server_list` returns a table of existing local 'Jupyter' server
hosts, ports, and tokens; `jupyter_check_launch` returns true if a new
server has been created, or false if there has been an existing server
at the port; other functions return nothing.

## Examples

``` r
if (FALSE) { # \dontrun{

# Requires installation of conda
library(rpymat)

# Install conda, if you have done so, skip
configure_conda()


# Install Jupyter notebook
add_jupyter(register_R = TRUE)


# Utility functions
jupyter_bin()

# Please install `dipsaus` package to enable `async=TRUE` with
# better experience
jupyter_launch(async = FALSE, open_browser = TRUE)


} # }
```
