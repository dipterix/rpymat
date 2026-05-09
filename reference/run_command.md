# Execute command with additional environments

Enables 'conda' environment

## Usage

``` r
cmd_create(command, shell, use_glue = TRUE)

cmd_set_env(command, key, value, quote = TRUE, quote_type = "cmd")

cmd_set_workdir(command, workdir)

cmd_set_conda(command, conda_path, env_path)

cmd_build(command, .env = parent.frame(), ...)

detect_shell(suggest = NULL)

run_command(
  command,
  shell = detect_shell(),
  use_glue = FALSE,
  enable_conda = TRUE,
  stdout = "",
  stderr = "",
  stdin = "",
  input = NULL,
  env_list = list(),
  wait = TRUE,
  timeout = 0,
  ...,
  workdir = getwd(),
  dry_run = FALSE,
  print_cmd = dry_run,
  glue_env = parent.frame(),
  env_name = NA
)
```

## Arguments

- command:

  system command

- shell:

  shell type

- use_glue:

  whether to [`glue`](https://glue.tidyverse.org/reference/glue.html)
  the command; default is false

- key, value:

  environment variable key and value

- quote, quote_type:

  whether to quote the environment variables and what quote type should
  use; see [`shQuote`](https://rdrr.io/r/base/shQuote.html)

- workdir:

  the working directory

- conda_path:

  'conda' path; default is
  [`conda_path`](http://dipterix.org/rpymat/reference/conda-env.md)

- env_path:

  'conda' environment path; default is
  [`env_path`](http://dipterix.org/rpymat/reference/conda-env.md)

- suggest:

  suggested shell type; default is `'cmd'` on windows, or `'bash'` on
  others

- enable_conda:

  whether to activate 'conda'

- stdout, stderr, stdin, input, wait, timeout, ...:

  passed to [`system2`](https://rdrr.io/r/base/system2.html)

- env_list:

  a key-value pairs of environment variables

- dry_run:

  whether to dry-run the command (do not execute, simply returns the
  command), useful to debug

- print_cmd:

  whether to print the command out

- glue_env, .env:

  the environment to evaluate variables when `use_glue` is true

- env_name:

  `'conda'` environment name to activate, if not default.

## Value

All the functions return a list with class `rpymat_system_command`
except for `run_command`, which returns the exit code by
[`system2`](https://rdrr.io/r/base/system2.html).

## Examples

``` r

run_command("conda install -y numpy", dry_run = TRUE)
#> #!/usr/bin/env bash
#> cd '/home/runner/work/rpymat/rpymat/docs/reference'
#> if [ -f "/usr/share/miniconda/etc/profile.d/conda.sh" ]; then
#>   . "/usr/share/miniconda/etc/profile.d/conda.sh"
#> else
#>   export PATH="/usr/share/miniconda/bin:$PATH"
#> fi
#> 
#> conda activate "/home/runner/.local/share/r-rpymat/miniconda/envs/rpymat-conda-env"
#> cd '/home/runner/work/rpymat/rpymat/docs/reference'
#> 
#> 
#> conda install -y numpy


a <- "This is a message"
run_command('echo "{a}"', dry_run = TRUE,
            enable_conda = FALSE, use_glue = TRUE)
#> #!/usr/bin/env bash
#> cd '/home/runner/work/rpymat/rpymat/docs/reference'
#> cd '/home/runner/work/rpymat/rpymat/docs/reference'
#> 
#> 
#> echo "This is a message"


if (FALSE) { # \dontrun{

# Use `jupyter_launch()` instead. This is just a demonstration
run_command('"{jupyter_bin()}" server list', use_glue = TRUE)

} # }
```
