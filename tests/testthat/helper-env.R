# The environment variables the tests read. A test skips when what it needs is unset or
# missing; the three fixture and source folders default to siblings of the reference folder.
#   CANHRACTI_GGIR_REF        the GGIR reference data: recordings and stored milestones
#   CANHRACTI_GGIR_P5FIX      the part-5 fixtures, else ggir-study-p5/fixtures
#   CANHRACTI_GGIR_P34FIX     the part 3-4 fixtures, else ggir-study-p34/fixtures
#   CANHRACTI_GGIR_SRC        the R folder of the GGIR 3.3-9 source, else ggir-src/GGIR/R
#   CANHRACTI_LONG_TESTS      switch for the EE recording tests and the MOS2 whole-signal counts
#   CANHRACTI_GGIR_SLOW       switch for the live GGIR runs on MOS2 and the full .gt3x chains
#   CANHRACTI_BIG_GT3X        a large .gt3x for the optional reader, counts and part-2 checks
#   CANHRACTI_BIG_GT3X_HOURS  hours of it the counts check compares, 12 by default

# TRUE when a switch is 1, true or yes, in any case
canhr_flag <- function(name) tolower(Sys.getenv(name, unset = "")) %in% c("1", "true", "yes")

# A German LC_TIME for the calling test, or a skip when the machine has none. Some systems
# accept any locale name, so the day names must come out German as well.
local_german_time <- function(env = parent.frame()) {
  old <- Sys.getlocale("LC_TIME")
  loc <- NULL
  for (x in c("German_Germany.utf8", "German_Germany.1252", "German", "de_DE.UTF-8", "de_DE.utf8", "de_DE")) {
    set <- suppressWarnings(tryCatch(Sys.setlocale("LC_TIME", x), error = function(e) ""))
    if (nzchar(set) && identical(weekdays(as.Date("2024-01-06")), "Samstag")) {
      loc <- x
      break
    }
  }
  Sys.setlocale("LC_TIME", old)
  if (is.null(loc)) testthat::skip("no German LC_TIME locale on this machine")
  withr::local_locale(c(LC_TIME = loc), .local_envir = env)
  invisible(loc)
}
