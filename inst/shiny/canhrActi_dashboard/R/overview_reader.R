# Each raw read runs in a fresh R process that exits when it is done, so its
# memory goes back to the system. The child connects back, sends its job's
# token, writes its pid to a ready file and sends the result; end of stream with
# no result means it died. Reads are non-blocking: socketSelect() waits 0.2 s.
RAW_CHILD_BOOT <- c(
  "a <- commandArgs(TRUE)",
  "job <- readRDS(a[2])",
  "con <- socketConnection('localhost', as.integer(a[1]), blocking = FALSE, open = 'a+b', timeout = 60)",
  "writeBin(charToRaw(job$token), con)",
  "saveRDS(list(pid = Sys.getpid(), tmp = tempdir()), paste0(a[3], '.tmp'))",
  "invisible(file.rename(paste0(a[3], '.tmp'), a[3]))",
  "writeBin(serialize(do.call(job$fun, c(job$args, list(con = con))), NULL), con)",
  "close(con)"
)
RAW_CHILD_START_S <- 180   # to connect back, which takes about a second
RAW_CHILD_MAX <- 2L        # reader processes at once, across sessions

# The readers running in this R process and the sessions waiting for one
raw_child_slots <- new.env(parent = emptyenv())
raw_child_slots$n <- 0L
raw_child_slots$waiting <- list()
raw_child_slots$pending <- FALSE

# Runs in the child; an error comes back as list(ok = FALSE, ...), as before
raw_read_job <- function(datapath, cached, name, tz, prog, metrics, libpaths, con) {
  .libPaths(libpaths)
  # GGIR takes the ID and file name from the path, and an upload arrives as
  # Shiny's 0.gt3x. Only an upload's name differs from its path: it is renamed
  # in its own upload folder for the read and back after, or, if that fails,
  # linked or copied into a folder of this process's tempdir.
  path <- datapath
  nm <- basename(as.character(name)[1])
  if (!is.na(nm) && nzchar(nm) && !identical(basename(datapath), nm)) {
    p <- file.path(dirname(datapath), nm)
    if (!file.exists(p) && file.rename(datapath, p)) {
      path <- p
      on.exit(file.rename(p, datapath), add = TRUE)
    } else {
      d <- tempfile("upload")
      p <- file.path(d, nm)
      if (dir.create(d) && (suppressWarnings(file.link(datapath, p)) || file.copy(datapath, p))) path <- p
    }
  }
  tryCatch({
    # metrics is RAW_READ_METRICS, next to the cache tag that changes with it
    res <- canhrActi::read.raw.accelerometer(
      path, params = do.call(canhrActi::raw.params,
                                 c(list(desiredtz = tz), metrics)),
      progress = function(stage, i, n, msg) {
        # End of stream means the session is gone; quit() still clears tempdir()
        b <- tryCatch(readBin(con, "raw", 1), error = function(e) NULL)
        if (is.null(b) || (length(b) == 0 && !isIncomplete(con))) quit(save = "no", status = 1)
        try(cat(paste0(stage, "\t", msg, "\n"), file = prog, append = TRUE), silent = TRUE)
      })
    # A private name renamed into place, so no session sees a partial file
    tmp <- paste0(cached, ".", Sys.getpid(), ".part")
    saveRDS(res, tmp)
    if (!file.rename(tmp, cached)) { file.copy(tmp, cached, overwrite = TRUE); unlink(tmp) }
    list(ok = TRUE, cached = cached, name = name)
  }, error = function(e) list(ok = FALSE, name = name, error = conditionMessage(e)))
}

# A port in parallel's range; neither it nor the token draws from the session's RNG
raw_child_listen <- function() {
  start <- (Sys.getpid() + floor(as.numeric(Sys.time()) * 1000)) %% 1000
  for (i in 0:49) {
    port <- 11000L + as.integer((start + i) %% 1000)
    srv <- tryCatch(serverSocket(port), error = function(e) NULL, warning = function(w) NULL)
    if (!is.null(srv)) return(list(port = port, con = srv))
  }
  stop("no free local port for the reader process")
}

# The log stays locked while the child runs, which readLines() warns about
raw_child_log_lines <- function(log) {
  if (!file.exists(log)) return(character(0))
  tryCatch(suppressWarnings(readLines(log, warn = FALSE)), error = function(e) character(0))
}

raw_child_log_tail <- function(log) {
  ln <- raw_child_log_lines(log)
  ln <- trimws(ln[nzchar(trimws(ln)) & !grepl("^Execution halted", ln)])
  if (length(ln) == 0) "" else paste0(" It said: ", utils::tail(ln, 1))
}

raw_child_close <- function(child) {
  for (k in c("con", "srv")) {
    if (!is.null(child[[k]])) try(close(child[[k]]), silent = TRUE)
    child[[k]] <- NULL
  }
}

# Starts a read with its own state, holder$cur, and returns a promise of the result
raw_child_start <- function(holder, args) {
  child <- new.env(parent = emptyenv())
  holder$cur <- child
  child$slot <- TRUE
  if (isTRUE(holder$reserved)) holder$reserved <- FALSE
  else raw_child_slots$n <- raw_child_slots$n + 1L
  ok <- FALSE
  on.exit(if (!ok) {
    raw_child_close(child)
    unlink(child$job)
    raw_child_free(child)
  })
  boot <- file.path(tempdir(), "canhrActi_raw_child.R")
  if (!file.exists(boot) || !identical(readLines(boot, warn = FALSE), RAW_CHILD_BOOT)) {
    tmp <- tempfile("boot", fileext = ".R")
    writeLines(RAW_CHILD_BOOT, tmp)
    if (!file.rename(tmp, boot)) unlink(tmp)
  }
  srv <- raw_child_listen()
  child$srv <- srv$con
  child$port <- srv$port
  token <- paste(basename(tempfile(rep("", 4))), collapse = "")
  job <- tempfile("canhrActi_raw_job_", fileext = ".rds")
  child$job <- job
  fun <- raw_read_job
  environment(fun) <- globalenv()
  saveRDS(list(fun = utils::removeSource(fun), args = args, token = token), job)
  ready <- sub("[.]rds$", ".ready", job)
  log <- sub("[.]rds$", ".log", job)
  list2env(list(con = NULL, pid = NULL, tmp = NULL, part = NULL, buf = raw(0),
                token = charToRaw(token), trusted = FALSE, accepted = NULL,
                ready = ready, log = log, prog = args$prog,
                started = Sys.time(), stopped = FALSE), child)
  exe <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  st <- system2(exe, c(shQuote(boot), srv$port, shQuote(job), shQuote(ready)),
                wait = FALSE, stdout = log, stderr = log)
  if (!identical(as.integer(st), 0L)) stop("the reader process could not be started")
  ok <- TRUE

  # One step per poll: the ready file, accept, the token, then read to the end
  step <- function() {
    if (!child$trusted) {
      if (!file.exists(ready)) {
        waited <- as.numeric(difftime(Sys.time(), child$started, units = "secs"))
        if (waited > RAW_CHILD_START_S || any(grepl("^Execution halted", raw_child_log_lines(log)))) {
          stop("the reader process did not start.", raw_child_log_tail(log), call. = FALSE)
        }
        return(NULL)
      }
      if (is.null(child$pid)) {
        hello <- readRDS(ready)
        child$pid <- hello$pid
        child$tmp <- hello$tmp
        child$part <- paste0(args$cached, ".", hello$pid, ".part")
        child$hello <- Sys.time()
      }
      if (as.numeric(difftime(Sys.time(), child$hello, units = "secs")) > RAW_CHILD_START_S) {
        tools::pskill(child$pid)
        stop("the reader process did not connect.", call. = FALSE)
      }
      # The child connects and sends the token before it writes the ready file
      if (is.null(child$con)) {
        child$con <- tryCatch(socketAccept(child$srv, blocking = FALSE, open = "a+b", timeout = 1),
                              error = function(e) NULL)
        child$buf <- raw(0)
        child$accepted <- Sys.time()
        if (is.null(child$con)) return(NULL)
      }
      child$buf <- c(child$buf, readBin(child$con, "raw", 65536))
      n <- length(child$token)
      if (length(child$buf) < n && isIncomplete(child$con) &&
          as.numeric(difftime(Sys.time(), child$accepted, units = "secs")) < 10) {
        return(NULL)
      }
      if (length(child$buf) < n || !identical(child$buf[seq_len(n)], child$token)) {
        # Any other connection is closed unread
        try(close(child$con), silent = TRUE)
        child$con <- NULL
        return(NULL)
      }
      child$buf <- child$buf[-seq_len(n)]
      child$trusted <- TRUE
      close(child$srv)
      child$srv <- NULL
    }
    b <- readBin(child$con, "raw", 65536)
    child$buf <- c(child$buf, b)
    if (length(b) == 65536 || isIncomplete(child$con)) return(NULL)
    msg <- if (length(child$buf) > 0) tryCatch(unserialize(child$buf), error = function(e) NULL)
    if (is.null(msg)) {
      stop(structure(class = c("raw_child_ended", "error", "condition"), list(
        message = paste0("the reader process ended before it finished.", raw_child_log_tail(log)),
        call = NULL)))
    }
    msg
  }

  promises::promise(function(resolve, reject) {
    poll <- function() {
      if (isTRUE(child$stopped)) return()
      out <- tryCatch(step(), error = function(e) e)
      if (is.null(out)) return(later::later(poll, 0.5))
      raw_child_close(child)
      raw_child_sweep(c(job, ready, log))
      raw_child_free(child)
      if (inherits(out, "error")) {
        # A child that died leaves its temp folder; one still starting is caught later
        if (is.null(child$pid)) raw_child_reap(child) else raw_child_sweep(raw_child_files(child))
        reject(out)
      } else {
        resolve(out)
      }
    }
    later::later(poll, 0.25)
  })
}

# For a session that ended mid-read or while waiting
raw_child_stop <- function(holder) {
  raw_child_unwait(holder)
  child <- holder$cur
  if (is.null(child) || (is.null(child$srv) && is.null(child$con))) return(invisible())
  child$stopped <- TRUE
  raw_child_close(child)
  raw_child_reap(child)
  raw_child_free(child)
  invisible()
}

# A child still starting is caught when its ready file appears
raw_child_reap <- function(child, tries = RAW_CHILD_START_S / 2) {
  if (is.null(child$pid) && file.exists(child$ready)) {
    hello <- tryCatch(readRDS(child$ready), error = function(e) NULL)
    child$pid <- hello$pid
    child$tmp <- hello$tmp
  }
  if (is.null(child$pid)) {
    if (tries > 1) later::later(function() raw_child_reap(child, tries - 1), 2)
    return(invisible())
  }
  tools::pskill(child$pid)
  raw_child_sweep(c(raw_child_files(child), child$prog))
  child$pid <- NULL
  invisible()
}

raw_child_files <- function(child) {
  tmp <- child$tmp
  own <- !is.null(tmp) && grepl("^Rtmp", basename(tmp)) &&
    !identical(normalizePath(tmp, mustWork = FALSE), normalizePath(tempdir(), mustWork = FALSE))
  c(if (own) tmp, child$part, child$job, child$ready, child$log)
}

# The files of a killed process can stay locked for a moment, so retry
raw_child_sweep <- function(paths, tries = 5) {
  paths <- paths[nzchar(paths)]
  if (length(paths) == 0) return(invisible())
  unlink(paths, recursive = TRUE, force = TRUE)
  left <- paths[file.exists(paths)]
  if (length(left) > 0 && tries > 1) later::later(function() raw_child_sweep(left, tries - 1), 2)
  invisible()
}

# TRUE with a slot reserved for holder when a read may start now. Otherwise
# holder joins the line and retry() runs once a slot frees, first come first.
raw_child_slot <- function(holder, retry) {
  if (isTRUE(holder$reserved)) return(TRUE)
  s <- raw_child_slots
  mine <- vapply(s$waiting, function(w) identical(w$holder, holder), logical(1))
  if (s$n < RAW_CHILD_MAX && (length(mine) == 0 || mine[1])) {
    s$waiting <- s$waiting[!mine]
    s$n <- s$n + 1L
    holder$reserved <- TRUE
    return(TRUE)
  }
  if (!any(mine)) s$waiting[[length(s$waiting) + 1]] <- list(holder = holder, retry = retry)
  FALSE
}

raw_child_free <- function(child) {
  if (!isTRUE(child$slot)) return(invisible())
  child$slot <- FALSE
  raw_child_slots$n <- raw_child_slots$n - 1L
  raw_child_wake()
}

raw_child_unwait <- function(holder) {
  s <- raw_child_slots
  s$waiting <- Filter(function(w) !identical(w$holder, holder), s$waiting)
  if (isTRUE(holder$reserved)) {
    holder$reserved <- FALSE
    s$n <- s$n - 1L
  }
  raw_child_wake()
}

raw_child_wake <- function() {
  s <- raw_child_slots
  if (isTRUE(s$pending) || length(s$waiting) == 0 || s$n >= RAW_CHILD_MAX) return(invisible())
  s$pending <- TRUE
  later::later(function() {
    s$pending <- FALSE
    w <- s$waiting
    s$waiting <- list()
    for (x in w) try(x$retry(), silent = TRUE)
  })
  invisible()
}
