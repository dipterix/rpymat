#' @name run_command
#' @title Execute command with additional environments
#' @description Enables 'conda' environment
#' @param command system command
#' @param shell shell type
#' @param use_glue whether to \code{\link[glue]{glue}} the command; default is
#' false
#' @param enable_conda whether to activate 'conda'
#' @param stdout,stderr,stdin,input,wait,timeout,... passed to
#' \code{\link{system2}}
#' @param workdir the working directory
#' @param dry_run whether to dry-run the command (do not execute, simply
#' returns the command), useful to debug
#' @param print_cmd whether to print the command out
#' @param glue_env,.env the environment to evaluate variables when \code{use_glue}
#' is true
#' @param key,value environment variable key and value
#' @param quote,quote_type whether to quote the environment variables and
#' what quote type should use; see \code{\link{shQuote}}
#' @param conda_path 'conda' path; default is \code{\link{conda_path}}
#' @param env_path 'conda' environment path; default is \code{\link{env_path}}
#' @param suggest suggested shell type; default is \code{'cmd'} on windows,
#' or \code{'bash'} on others
#' @param env_list a key-value pairs of environment variables
#' @returns All the functions return a list with class
#' \code{rpymat_system_command} except for
#' \code{run_command}, which returns the exit code by \code{\link{system2}}.
#'
#' @examples
#'
#' run_command("conda install -y numpy", dry_run = TRUE)
#'
#'
#' a <- "This is a message"
#' run_command('echo "{a}"', dry_run = TRUE,
#'             enable_conda = FALSE, use_glue = TRUE)
#'
#'
#' \dontrun{
#'
#' # Use `jupyter_launch()` instead. This is just a demonstration
#' run_command('"{jupyter_bin()}" server list', use_glue = TRUE)
#'
#' }
#'
NULL

#' @export
print.rpymat_system_command <- function(x, ...){
  attrs <- attributes(x)
  cat(glue::glue(
    "{attrs$shell} command:",
    "  workdir: {attrs$workdir}",
    "  conda: {isTRUE(attrs$conda$use_conda)}",
    "  glue: {isTRUE(attrs$use_glue)}",
    .sep = "\n"
  ), "\n")
  if(length(attrs$envs)){
    cat("Environments:\n")
    for(k in names(attrs$envs)){
      if(k != ""){
        cat(sprintf("  - %s=%s\n", k, attrs$envs[[k]]))
      }
    }
  }
  cat("", x, "", sep = "\n")
  invisible(x)
}

#' @rdname run_command
#' @export
cmd_create <- function(command, shell, use_glue = TRUE){

  structure(
    paste(command, sep = "\n", collapse = "\n"),
    use_glue = as.logical(use_glue),
    class = "rpymat_system_command",
    shell = shell,
    workdir = getwd(),
    conda = list(),
    envs = list()
  )

}

#' @rdname run_command
#' @export
cmd_set_env <- function(command, key, value, quote = TRUE, quote_type = "cmd"){
  stopifnot(inherits(command, "rpymat_system_command"))
  envs <- attr(command, "envs")
  if(!is.list(envs)){
    envs <- as.list(envs)
  }
  envs[[key]] <- ifelse(quote, shQuote(value, type = quote_type), value)
  attr(command, "envs") <- envs
  command
}

#' @rdname run_command
#' @export
cmd_set_workdir <- function(command, workdir){
  if(!length(workdir) || !dir.exists(workdir)){
    workdir <- getwd()
  }
  workdir <- normalizePath(workdir)
  attr(command, "workdir") <- workdir
  command
}

#' @rdname run_command
#' @export
cmd_set_conda <- function(command, conda_path, env_path) {
  conda_path <- normalizePath(conda_path, mustWork = FALSE)
  env_path <- normalizePath(env_path, mustWork = FALSE)
  conda <- attr(command, "conda")
  conda$conda_path <- conda_path
  conda$env_path <- env_path
  conda$use_conda <- TRUE
  attr(command, "conda") <- conda
  command
}

#' @rdname run_command
#' @export
cmd_build <- function(command, .env = parent.frame(), ...) {

  attrs <- attributes(command)

  if(attrs$use_glue){
    command <- glue::glue(command, .envir = .env, ...)
  }

  shell <- attrs$shell
  if(shell %in% c("bash", "zsh", "csh", "tcsh", "sh")) {
    s_shell <- sprintf("#!/usr/bin/env %s", shell)
  } else {
    s_shell <- NULL
  }
  s_workdir <- sprintf("cd %s", shQuote(attrs$workdir))
  conda <- attrs$conda
  if(isTRUE(conda$use_conda)){
    if(shell %in% c("csh", "tcsh")){
      s_conda <- glue::glue(
        # "setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace",
        'if ( -f "{ conda$conda_path }/etc/profile.d/conda.csh" ) then',
        '  source "{ conda$conda_path }/etc/profile.d/conda.csh"',
        'else',
        '  setenv PATH "{ conda$conda_path }/bin:$PATH"',
        'endif',
        "",
        'conda activate "{ conda$env_path }"',
        .sep = "\n"
      )
    } else if(shell %in% c("bash", "zsh", "sh")){
      s_conda <- glue::glue(
        # "export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace",
        # "__conda_setup=\"$('{ conda$conda_path }/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)\"",
        # "if [ $? -eq 0 ]; then",
        # '  eval "$__conda_setup"',
        # "else",
        'if [ -f "{ conda$conda_path }/etc/profile.d/conda.sh" ]; then',
        '  . "{ conda$conda_path }/etc/profile.d/conda.sh"',
        "else",
        '  export PATH="{ conda$conda_path }/bin:$PATH"',
        "fi",
        # "fi",
        # "unset __conda_setup",
        "",
        'conda activate "{ conda$env_path }"',
        .sep = "\n"
      )
    } else {
      bin_path <- file.path(conda$conda_path, "condabin", c("conda", "conda.exe", "conda.bin", "conda.bat"))
      sel <- file.exists(bin_path)
      if(any(sel)){
        bin_path <- bin_path[sel][[1]]
      } else {
        bin_path <- bin_path[[1]]
      }
      bin_path2 <- normalizePath(bin_path, winslash = "\\", mustWork = FALSE)
      if(get_os() == "windows"){
        conda_path2 <- normalizePath(file.path(conda$conda_path, "Scripts"), winslash = "\\", mustWork = FALSE)
      } else {
        conda_path2 <- normalizePath(file.path(conda$conda_path, "bin"), winslash = "\\", mustWork = FALSE)
      }

      env_path2 <- normalizePath(conda$env_path, winslash = "\\", mustWork = FALSE)
      s_conda <- glue::glue(
        'set PATH="{ conda_path2 };%PATH%"',
        "",
        'call "{ bin_path2 }" activate "{ env_path2 }"',
        .sep = "\n"
      )
    }
  } else {
    s_conda <- NULL
  }

  envs <- attrs$envs
  if(shell %in% c("csh", "tcsh")){
    s_envs <- unlist(lapply(names(envs), function(env_key){
      env_val <- envs[[env_key]][[1]]
      sprintf("setenv %s %s", env_key, env_val)
    }))
  } else if(shell %in% c("cmd")){
    s_envs <- unlist(lapply(names(envs), function(env_key){
      env_val <- envs[[env_key]][[1]]
      sprintf("set %s=%s", env_key, env_val)
    }))
  } else {
    s_envs <- unlist(lapply(names(envs), function(env_key){
      env_val <- envs[[env_key]][[1]]
      sprintf("export %s=%s", env_key, env_val)
    }))
  }

  paste(c(
    s_shell,
    s_workdir,
    s_conda,
    s_envs,
    s_workdir,
    "\n",
    command
  ), collapse = "\n")
}



#' @rdname run_command
#' @export
detect_shell <- function(suggest = NULL){
  os <- get_os()
  if(os == 'windows'){
    re <- c("cmd", "sh")
  } else {
    re <- c("bash", "zsh", "csh", "tcsh", "sh")
  }
  if(length(suggest)){
    re0 <- re[re %in% suggest]
    if(length(re0)){
      re <- re0
    }
  }
  re[[1]]
}

cmd_run_script <- function(shell, script, wait = TRUE, ...){
  if(shell %in% c("bash", "zsh", "csh", "tcsh", "sh")){
    system2(command = Sys.which(shell), args = script, ...)
  } else if(shell %in% "cmd"){
    if(!endsWith(tolower(script), ".bat")){
      tmpfile <- tempfile(fileext = ".bat")
      if(wait) {
        on.exit({
          if(file.exists(tmpfile)){
            try({ unlink(tmpfile) })
          }
        })
      }
      s <- readLines(script)
      writeLines(s, tmpfile)
      tmpfile <- normalizePath(tmpfile, winslash = "\\")
    } else {
      tmpfile <- normalizePath(script, winslash = "\\")
    }
    system2(command = tmpfile, args = character(0L), ...)
  } else {
    stop("Shell type not recognized: ", shell)
  }

}

#' @rdname run_command
#' @export
run_command <- function(command, shell = detect_shell(),
                        use_glue = FALSE, enable_conda = TRUE,
                        stdout = "", stderr = "", stdin = "", input = NULL,
                        env_list = list(), wait = TRUE, timeout = 0, ...,
                        workdir = getwd(), dry_run = FALSE, print_cmd = dry_run,
                        glue_env = parent.frame()){

  shell <- match.arg(shell)
  command <- cmd_create(command, shell, use_glue = use_glue)
  if(enable_conda){
    conda_bin <- conda_bin()
    if(length(conda_bin)) {
      conda_path <- normalizePath(dirname(dirname(conda_bin)), winslash = "/")
    } else {
      conda_path <- conda_path()
    }
    command <- cmd_set_conda(command, conda_path, env_path())
  }
  command <- cmd_set_workdir(command, workdir)

  if(length(env_list)){
    for(key in names(env_list)){
      if(key != ""){
        value <- env_list[[key]]
      }
      command <- cmd_set_env(command = command, key = key, value = value, quote = FALSE)
    }
  }



  if( dry_run ){
    try({
      cmd <- cmd_build(command = command, .env = glue_env, ...)
      if(print_cmd){
        message(cmd)
      }
    })

    return(invisible(command))
  }

  cmd <- cmd_build(command = command, .env = glue_env, ...)
  if(print_cmd){
    message(cmd)
  }

  tmpfile <- tempfile(pattern = "rpymat_command_", fileext = ifelse(get_os() == 'windows', '.bat', ".sh"))
  if(wait){
    on.exit({
      if(file.exists(tmpfile)){
        try({ unlink(tmpfile) })
      }
    })
  }
  writeLines(cmd, con = tmpfile)

  tmpfile <- normalizePath(tmpfile)

  cmd_run_script(shell = shell, script = tmpfile,
             stdout = stdout, stderr = stderr, stdin = stdin, input = input,
             wait = wait, timeout = timeout, ...)
  # system2(command = Sys.which(shell), args = tmpfile,
  #         stdout = stdout, stderr = stderr, stdin = stdin, input = input,
  #         env = env, wait = wait, timeout = timeout, ...)

  # return(invisible(command))
}
