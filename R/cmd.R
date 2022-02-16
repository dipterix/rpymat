build_command_conda <- function(command = NULL, shell){
  conda_path <- conda_path()
  condaenv <- env_path()
  if(shell %in% c("csh", "tcsh")){
    prefix <- glue::glue(
      # "setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace",
      'if ( -f "{ conda_path }/etc/profile.d/conda.csh" ) then',
      '  source "{ conda_path }/etc/profile.d/conda.csh"',
      'else',
      '  setenv PATH "{ conda_path }/bin:$PATH"',
      'endif',
      "",
      'conda activate "{ condaenv }"',
      .sep = "\n"
    )
  } else {
    prefix <- glue::glue(
      # "export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace",
      "__conda_setup=\"$('{ conda_path }/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)\"",
      "if [ $? -eq 0 ]; then",
      '  eval "$__conda_setup"',
      "else",
      '  if [ -f "{ conda_path }/etc/profile.d/conda.sh" ]; then',
      '    . "{ conda_path }/etc/profile.d/conda.sh"',
      "  else",
      '    export PATH="{ conda_path }/bin:$PATH"',
      "  fi",
      "fi",
      "unset __conda_setup",
      "",
      'conda activate "{ condaenv }"',
      .sep = "\n"
    )
  }

  c(prefix, command)
}

build_command_workdir <- function(command = NULL, workdir = NULL, ...){
  if(!length(workdir) || !dir.exists(workdir)){
    workdir <- getwd()
  }
  workdir <- normalizePath(workdir)
  c(
    sprintf("cd %s", shQuote(workdir)),
    command
  )
}

 #' @export
run_command <- function(command, shell = c("bash", "zsh", "csh", "tcsh"),
                        use_glue = TRUE, enable_conda = TRUE,
                        stdout = "", stderr = "", stdin = "", input = NULL,
                        env = character(), wait = TRUE, timeout = 0, ...,
                        workdir = getwd(),
                        dry_run = FALSE, .env = parent.frame()){
  shell <- match.arg(shell)

  if(!check_afni()){
    stop("Cannot find AFNI. Please install AFNI first. You can run:\n  install_afni()")
  }

  command <- paste(command, sep = "\n", collapse = "\n")
  if(use_glue){
    command <- glue::glue(command, .envir = .env)
  }
  command <- build_command_workdir(command, workdir = workdir)

  # Add some code
  if(enable_conda){
    command <- build_command_conda(command, shell = shell)
  }
  command <- build_command_workdir(command, workdir = workdir)

  command <- paste(command, collapse = "\n")
  if(dry_run){
    return(command)
  }

  tmpfile <- tempfile(fileext = "rpymat_command_")
  on.exit({
    if(file.exists(tmpfile)){
      unlink(tmpfile)
    }
  })
  writeLines(command, con = tmpfile)

  tmpfile <- normalizePath(tmpfile)

  system2(command = Sys.which(shell), args = tmpfile,
          stdout = stdout, stderr = stderr, stdin = stdin, input = input,
          env = env, wait = wait, timeout = timeout, ...)
}
