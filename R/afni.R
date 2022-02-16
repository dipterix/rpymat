check_afni <- function(){
  init_afni()
  path <- Sys.which("afni")
  if(length(path)){
    path <- path[[1]]
    return(file.exists(path))
  }
  return(FALSE)
}

#' @export
run_afni <- function(command, ..., shell = c("bash", "zsh", "csh", "tcsh"), env = parent.frame()){
  shell <- match.arg(shell)

  if(!check_afni()){
    stop("Cannot find AFNI. Please install AFNI first. You can run:\n  install_afni()")
  }

  tmpfile <- tempfile(fileext = "run_afni_")
  on.exit({ unlink(tmpfile) })

  conda_path <- conda_path()
  condaenv <- rpymat::env_path()
  if(shell %in% c("csh", "tcsh")){
    prefix <- glue::glue(
      "setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace",
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
      "export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace",
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

  command <- paste(command, sep = "\n", collapse = "\n")

  writeLines(c(prefix, glue::glue(command, .envir = env)), con = tmpfile)
  tmpfile <- normalizePath(tmpfile)

  system(command = sprintf(
    "%s %s",
    shQuote(Sys.which(shell), type = "cmd"),
    shQuote(tmpfile, type = "cmd")
  ), ...)
}

install_xquartz <- function(){
  # echo mypassword | sudo -S command
  brew_install("xquartz", cask = TRUE)
}
install_tcsh <- function(){
  brew_install("tcsh")
}


init_afni <- function(){
  afni_path <- getOption("rpymat.afni.path", '~/abin')
  if(dir.exists(afni_path)){
    afni_path <- normalizePath(afni_path)
    path <- Sys.getenv("PATH")
    paths <- strsplit(path, .Platform$path.sep)[[1]]
    paths <- normalizePath(paths, mustWork = FALSE)
    if(!afni_path %in% paths){
      path <- paste(path, afni_path, sep = .Platform$path.sep)
      Sys.setenv('PATH' = path)
    }
  }

  add <- afni_path <- getOption("rpymat.afni.dylib", "/opt/X11/lib/flat_namespace")
  dyld_libpath <- Sys.getenv("DYLD_LIBRARY_PATH")
  dyld_libpaths <- strsplit(dyld_libpath, .Platform$path.sep)[[1]]
  dyld_libpaths <- normalizePath(dyld_libpaths, mustWork = FALSE)
  if(!add %in% dyld_libpaths){
    if(dyld_libpath == ""){
      dyld_libpath <- add
    } else {
      dyld_libpath <- paste(dyld_libpath, add, sep = .Platform$path.sep)
    }

    Sys.setenv('DYLD_LIBRARY_PATH' = dyld_libpath)
  }
}

install_afni_core_osx <- function(){
  path <- file.path(R_user_dir("rpymat", which = "data"), "afni")
  path <- normalizePath(path, mustWork = FALSE)

  wd <- getwd()
  on.exit({
    setwd(wd)
  }, add = TRUE, after = FALSE)


  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path <- normalizePath(path)
  csh_profile <- file.path(path, "csh_profile")
  writeLines(c(
    "if ( $?DYLD_LIBRARY_PATH ) then",
    "  setenv DYLD_LIBRARY_PATH ${DYLD_LIBRARY_PATH}:/opt/X11/lib/flat_namespace",
    "else",
    "  setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace",
    "endif"
  ), con = csh_profile)
  csh_profile <- normalizePath(csh_profile)

  setwd(path)

  if(file.exists("@update.afni.binaries")){
    system("@update.afni.binaries -d")
  } else {
    utils::download.file("https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries",
                         destfile = "@update.afni.binaries")
    cmd <- "tcsh @update.afni.binaries -package macos_10.12_local -do_extras"
    system(cmd)
  }

  cshrc <- "~/.cshrc"
  s <- NULL
  s1 <- "# >>>> rpymat afni: START (do not edit this line) >>>>"
  s2 <- "# <<<< rpymat afni: END (do not edit this line) <<<<"
  if(file.exists(cshrc)){
    s <- readLines(cshrc)
    if(length(s)){
      s <- trimws(s, which = "right", whitespace = "[ \t]")
      i1 <- which(s == s1)
      i2 <- which(s == s2)
      if(length(i1) && length(i2)){
        i1 <- i1[[1]]
        i2 <- i2[[length(i2)]]
        s <- s[-seq(i1, i2)]
      }
    }
  }
  s <- c(
    s,
    "",
    s1,
    sprintf('source %s', shQuote(csh_profile, type = "cmd")),
    s2
  )
  writeLines(s, "~/.cshrc")

  # This is deprecated
  # brew_install("cartr/qt4/pyqt")

  # Update SUMA
  init_afni()
  if(file.exists("~/abin/AFNI.afnirc")){
    file.copy("~/abin/AFNI.afnirc", "~/.afnirc", overwrite = TRUE)
    suma <- Sys.which("suma")
    system(sprintf("%s -update_env", shQuote(suma, type = "cmd")))
  }

  brew_install("netpbm")
  add_packages(c("numpy", "matplotlib"))

  # Check
  tmpfile <- tempfile(fileext = ".py", pattern = "afni_check_")
  ensure_rpymat()
  reticulate::import("subprocess")
  writeLines(c(
    "import subprocess",
    sprintf("afni_check_process = subprocess.Popen(['python', '%s', '-check_all'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)", normalizePath("~/abin/afni_system_check.py")),
    "afni_check_process = [x.decode('utf-8') for x in afni_check_process.stdout.readlines()]"
  ), tmpfile)
  results <- run_script(x = tmpfile, local = FALSE, convert = TRUE)

  unlink(tmpfile)
  message(results$afni_check_process)

}
