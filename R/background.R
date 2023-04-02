
run_rscript <- function(script, wait = TRUE, args = c("--no-save", "--no-restore"), ...){
  # use RScript
  if(!file.exists(script)){
    stop("script is missing")
  }
  script <- normalizePath(script, mustWork = TRUE, winslash = "\\")
  rscript <- R.home('bin')

  rscript <- list.files(rscript, '^rscript', full.names = TRUE, ignore.case = TRUE)
  rscript <- normalizePath(rscript, mustWork = TRUE, winslash = "\\")[[1]]

  # inject to load base packages
  sinfo <- utils::sessionInfo()
  s <- readLines(script)
  s <- c(
    paste0('library(', rev(sinfo$basePkgs), ')'),
    s
  )

  cmd <- sprintf('"%s" %s "%s"', rscript, paste(args, collapse = " "), script)
  if(get_os() == "windows"){
    system(cmd, wait = wait, show.output.on.console = FALSE, invisible = TRUE, minimized = TRUE, intern = FALSE, ...)
  } else {
    system(cmd, wait = wait, intern = FALSE, ...)
  }

  return()
}
