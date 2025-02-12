
master_session_id <- local({
  uuid <- NULL

  function() {
    if(is.null(uuid)) {
      id <- getOption("rpymat.uuid", "")
      if(!nzchar(id)) {
        id <- rand_string()
      }
      uuid <<- id
    }
    uuid
  }
})


rs_avail <- function (version_needed = "1.3", child_ok = FALSE, shiny_ok = TRUE) {
  if (!shiny_ok && system.file(package = "shiny") != "") {
    shiny <- asNamespace("shiny")
    if( !is.null(shiny$getDefaultReactiveDomain()) ) { return(FALSE) }
  }
  if (!requireNamespace("rstudioapi")) {
    return(FALSE)
  }
  rstudioapi::isAvailable(version_needed = version_needed,
                          child_ok = child_ok)
}


rs_focus_console <- function (wait = 0.5) {
  if (rs_avail(version_needed = "1.4")) {
    if (wait > 0) {
      Sys.sleep(wait)
    }
    try({
      rstudioapi::executeCommand("activateConsole", quiet = TRUE)
    }, silent = TRUE)
  }
  return(invisible())
}


rs_runjob <- function (script, name, focus_on_console = FALSE, ...) {
  rstudioapi::jobRunScript(path = script, name = name, workingDir = tempdir(),
                           importEnv = NULL, exportEnv = "")
  if (focus_on_console) {
    rs_focus_console()
  }
  return()
}

rs_runjob_alt <- function (script, name, wait = TRUE, args = c("--no-save", "--no-restore"), ignore.stdout = FALSE, ignore.stderr = FALSE, ...) {
  if (!file.exists(script)) {
    stop("script is missing")
  }
  script <- normalizePath(script, mustWork = TRUE, winslash = "\\")
  rscript <- R.home("bin")
  rscript <- list.files(rscript, "^rscript", full.names = TRUE,
                        ignore.case = TRUE)
  rscript <- normalizePath(rscript, mustWork = TRUE, winslash = "\\")[[1]]
  sinfo <- utils::sessionInfo()
  s <- readLines(script)
  s <- c(paste0("library(", rev(sinfo$basePkgs), ")"), s)
  call_args <- list(command = rscript, args = c(args, shQuote(script)),
                    wait = wait, ...)
  if (ignore.stdout) {
    call_args$stdout <- nullfile()
  }
  if (ignore.stderr) {
    call_args$stderr <- nullfile()
  }
  if (get_os() == "windows") {
    call_args$minimized <- TRUE
    call_args$invisible <- TRUE
  }
  else {
  }
  do.call(system2, call_args)
  return()
}


rs_exec_internal <- function (expr, name = "Untitled", quoted = FALSE, rs = TRUE,
                              wait = FALSE, packages = NULL, focus_on_console = FALSE,
                              ..., nested_ok = FALSE)
{
  if (!nested_ok && isTRUE(getOption("rpymat.is_child_process", FALSE))) {
    stop("rs_exec should not be nested.")
  }
  if (!quoted) {
    expr <- substitute(expr)
  }
  tdir <- tempdir(check = TRUE)
  if(!dir.exists(tdir)) {
    dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
  }
  script <- tempfile()
  state_file <- paste0(script, ".dstate")
  res_file <- paste0(script, ".res")
  writeLines("1", state_file)
  state_file <- normalizePath(state_file)
  session_id <- master_session_id()
  use_rs <- rs && rs_avail(child_ok = TRUE, shiny_ok = TRUE)
  sys_env <- Sys.getenv()


  expr_wrapped <- bquote({
    writeLines("2", .(state_file))

    local({
      ...msg... <- new.env(parent = emptyenv())
      reg.finalizer(...msg..., function(e) {
        grDevices::graphics.off()
        if (length(e$error)) {
          writeLines(c("-1", e$error), .(state_file))
        }
        else {
          writeLines("0", .(state_file))
        }
      }, onexit = TRUE)
      ...msg...$fun <- function() {
        .(expr)
      }
      tryCatch({
        options(rpymat.is_child_process = TRUE)
        options(raveio.settings_readonly = TRUE)
        options(rpymat.uuid = .(session_id))
        if (.(use_rs)) {
          options(crayon.enabled = TRUE)
          options(crayon.colors = 256)
        }
        lapply(.(packages), function(p) {
          suppressMessages({
            do.call("require", list(package = p, character.only = TRUE))
          })
        })
        local({
          sys_env <- .(sys_env)
          do.call(Sys.setenv, as.list(sys_env))
        })
        res <- ...msg...$fun()
        if (!is.null(res)) {
          saveRDS(res, file = .(res_file))
        }
        writeLines("0", .(state_file))
      }, error = function(e) {
        ...msg...$error <- e$message
        writeLines(c("-1", ...msg...$error), .(state_file))
      }, finally = {
        rm(...msg...)
        gc()
      })
    })
  })

  writeLines(utils::capture.output(print(expr_wrapped)), script, sep = "\n")
  if (use_rs) {
    rs_runjob(script, name, focus_on_console = focus_on_console, ...)
  } else {
    rs_runjob_alt(script, name, wait = wait, ...)
  }
  state <- 0
  res <- NULL
  check_f <- function() {
    if (file.exists(state_file)) {
      s <- readLines(state_file)
      s <- trimws(s)
      st <- as.integer(s[[1]])
      if (is.na(st)) {
        st <- -2
      }
      else {
        s <- s[-1]
      }
      if (st < 0) {
        unlink(script)
        unlink(state_file)
        attr(st, "rs_exec_error") <- s
        attr(st, "rs_exec_state") <- "Error"
      }
      else if (st == 0) {
        unlink(script)
        unlink(state_file)
        if (file.exists(res_file)) {
          res <<- readRDS(res_file)
        }
        unlink(res_file)
        attr(st, "rs_exec_state") <- "Success"
        attr(st, "rs_exec_result") <- res
        if (focus_on_console) {
          rs_focus_console(wait = 0)
        }
      }
      else if (st > 0) {
        attr(st, "rs_exec_state") <- "Running"
      }
      state <<- st
    }
    return(structure(state, class = "rs_exec_res"))
  }
  if (wait) {
    check_f()
    while (isTRUE(state > 0)) {
      check_f()
      Sys.sleep(0.5)
    }
    check_f <- check_f()
  }
  invisible(check_f)
}

rs_exec <- function (expr, name = "Untitled", quoted = FALSE, rs = FALSE,
                     wait = FALSE, packages = NULL, focus_on_console = FALSE,
                     ..., nested_ok = FALSE)
{
  if (!quoted) {
    expr <- substitute(expr)
  }
  check <- rs_exec_internal(expr = expr, name = name, quoted = TRUE,
                            rs = rs, wait = wait, packages = packages, focus_on_console = focus_on_console,
                            nested_ok = nested_ok, ...)
  return(check)
}


