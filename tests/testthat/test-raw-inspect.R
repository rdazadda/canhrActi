# raw.inspect and its helpers against GGIR's g.inspectfile, the failure modes (corrupt, too
# small, uppercase extensions, discovery) and the GGIRread brand test files. Reference data
# come from CANHRACTI_GGIR_REF; tests skip when a file is missing and live comparisons when
# GGIR is not installed. The EE file (370 MB, about 45 s to inspect) runs only when
# CANHRACTI_LONG_TESTS is set.

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}

skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}

skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
EE <- function() ref_file("EE_left_29.5.2017-05-30.gt3x")
EE_RDATA <- function() ref_file("timing_out", "output_timing", "meta", "basic", "meta_EE_left_29.5.2017-05-30.gt3x.RData")
TRUNC <- function() ref_file("failmodes", "din", "truncated.gt3x")
SHORT <- function() ref_file("failmodes", "din", "tooshort.gt3x")

# GGIRread's test files: the study clone next to the reference folder, else the installed copy
ggirread_tf <- function() {
  clone <- file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIRread", "inst", "testfiles")
  if (dir.exists(clone)) return(clone)
  system.file("testfiles", package = "GGIRread")
}
# GGIR's own test files (ActiGraph epoch csv, Axivity csv, ukbiobank csv)
ggir_tf <- function() {
  clone <- file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIR", "inst", "testfiles")
  if (dir.exists(clone)) return(clone)
  system.file("testfiles", package = "GGIR")
}
# the study's staged copies (weird.xyz, uppercase copies)
study_tf <- function() file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study", "tf")

load_stored_I <- function(rdata) {
  e <- new.env()
  load(rdata, envir = e)
  e$I
}

ggir_params <- list(chunksize = 1, dynrange = NULL, nonwear_range_threshold = 150,
                    rmc.noise = 13, rmc.dynamic_range = NULL)
ggir_ncb <- function(I, params = ggir_params) {
  hv <- GGIR:::g.extractheadervars(I)
  GGIR:::get_nw_clip_block_params(I$monc, I$dformc, hv$deviceSerialNumber, I$sf, params)
}
four <- function(x) x[c("clipthres", "blocksize", "sdcriter", "racriter")]

# run GGIR::g.inspectfile and collect its warnings
ggir_inspect <- function(path, ...) {
  msgs <- character()
  I <- withCallingHandlers(GGIR::g.inspectfile(path, ...), warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  attr(I, "warnings") <- msgs
  I
}

fresh_dir <- function(name) {
  d <- gsub("\\\\", "/", tempfile(paste0("rawinspect_", name)))
  dir.create(d, recursive = TRUE)
  d
}

test_that("extension token follows getbrand (one .gz peel, spelling kept)", {
  expect_identical(.raw.extension("x.gt3x"), "gt3x")
  expect_identical(.raw.extension("x.GT3X"), "GT3X")
  expect_identical(.raw.extension("x.csv.gz"), "csv")
  expect_identical(.raw.extension("x.CSV.GZ"), "CSV")
  expect_identical(.raw.extension("a.b.cwa"), "cwa")
  expect_identical(.raw.extension("noext"), "")
  expect_identical(.raw.extension(c("a.bin", "b.wav")), c("bin", "wav"))
  expect_identical(.raw.filename("C:/data/sub/MOS2E39230594.gt3x"), "MOS2E39230594.gt3x")
})

test_that("the code tables carry GGIR's integers", {
  expect_identical(.RAW_MONITOR[["ACTIGRAPH"]], 3L)
  expect_identical(.RAW_FORMAT[["GT3X"]], 6L)
  expect_identical(.RAW_MONITOR_NAMES[3], "actigraph")
  expect_identical(.RAW_FORMAT_NAMES[6], "gt3x")
  expect_identical(.RAW_EXTENSIONS, c("csv", "bin", "wav", "cwa", "gt3x"))
})

test_that("parameter plumbing accepts a list, overrides, and rejects unknown names", {
  p <- .raw.inspect.params(list(desiredtz = "UTC", dynrange = NULL))
  expect_identical(p$desiredtz, "UTC")
  expect_null(p$dynrange)
  expect_true("dynrange" %in% names(p))
  expect_identical(p$minimumFileSizeMB, 2)
  p2 <- .raw.inspect.params(list(desiredtz = "UTC"), list(desiredtz = "Europe/Helsinki", idloc = 2))
  expect_identical(p2$desiredtz, "Europe/Helsinki")
  expect_identical(p2$idloc, 2)
  expect_error(.raw.inspect.params(list(), list(nonsense = 1)), "Unknown inspection parameter")
  p3 <- .raw.inspect.params(NULL, list(desiredtz = "America/Anchorage"))
  expect_identical(p3$desiredtz, "America/Anchorage")
  expect_identical(.raw.param(list(a = NULL), "a", 5), NULL)
  expect_identical(.raw.param(list(), "a", 5), 5)
})

test_that("clipping, non-wear and block-size parameters match GGIR over the code grid", {
  skip_if_no_ggir()
  serials <- c("", "MOS2E39230594_firmware_1.9.2", "CLE2A2123456_firmware_v2.2.1",
               "NEO1F09120034", "TAS1H30182785_firmware_v1.7.2", "39434")
  for (monc in 0:7) {
    for (dformc in 1:6) {
      for (sf in c(30, 50, 85.7, 100)) {
        for (serial in serials) {
          for (dyn in list(NULL, 16)) {
            params <- ggir_params
            params$dynrange <- dyn
            if (monc == 0) { params$rmc.dynamic_range <- 4; params$rmc.noise <- 0.013 }
            info <- list(monc = as.integer(monc), dformc = as.integer(dformc), sf = sf,
                         device_serial = serial, dynrange_file = NULL)
            mine <- .raw.clip.block.params(info, params = params, pass = "getmeta")
            theirs <- GGIR:::get_nw_clip_block_params(monc, dformc, serial, sf, params)
            expect_identical(four(mine), theirs,
                             info = paste("monc", monc, "dformc", dformc, "sf", sf, serial, "dyn", format(dyn)))
            expect_identical(mine$dynrange, mine$clipthres + 0.5)
          }
        }
      }
    }
  }
  # the calibrate pass
  gt3x <- list(monc = 3L, dformc = 6L, sf = 30, device_serial = "MOS", dynrange_file = NULL)
  expect_identical(.raw.clip.block.params(gt3x, pass = "calibrate")$blocksize, 43200)
  expect_identical(.raw.clip.block.params(gt3x, pass = "calibrate", chunksize = 0.5)$blocksize, 21600)
  expect_identical(.raw.clip.block.params(gt3x, pass = "getmeta", chunksize = 0.5)$blocksize, 43200)
  veri <- list(monc = 6L, dformc = 6L, sf = 30, device_serial = "", dynrange_file = NULL)
  expect_identical(.raw.clip.block.params(veri, pass = "calibrate")$blocksize, 43200) # folded into ActiGraph
  expect_identical(.raw.clip.block.params(veri, pass = "getmeta")$racriter, 0.20)
  gene <- list(monc = 2L, dformc = 1L, sf = 86, device_serial = "012967", dynrange_file = NULL)
  expect_identical(.raw.clip.block.params(gene, pass = "calibrate")$blocksize, round((14512 * (86 / 50)) * 0.5))
  expect_identical(.raw.clip.block.params(gene, pass = "getmeta")$blocksize, round(14512 * (86 / 50)))
  cwa <- list(monc = 4L, dformc = 4L, sf = 100, device_serial = "39434", dynrange_file = NULL)
  expect_identical(.raw.clip.block.params(cwa, pass = "calibrate")$blocksize, round(12 * 3600 * 100 / 80))
  expect_identical(.raw.clip.block.params(cwa, pass = "getmeta")$blocksize, round(24 * 3600 * 100 / 80))
  mov <- list(monc = 5L, dformc = 1L, sf = 64, device_serial = "", dynrange_file = NULL)
  expect_identical(.raw.clip.block.params(mov, pass = "calibrate")$blocksize, (64 * 60 * 1440) / 2)
  expect_identical(.raw.clip.block.params(mov, pass = "getmeta")$clipthres, 15.5)
  expect_identical(.raw.clip.block.params(mov, pass = "getmeta")$dynrange_source, "movisens_assumed")
  par <- list(monc = 7L, dformc = 1L, sf = 12.5, device_serial = "", dynrange_file = 8L)
  expect_identical(.raw.clip.block.params(par, pass = "calibrate")$blocksize, 360)
  expect_identical(.raw.clip.block.params(par, pass = "getmeta")$blocksize, 720)
  expect_identical(.raw.clip.block.params(par, pass = "getmeta")$clipthres, 7.5)
  expect_identical(.raw.clip.block.params(par, pass = "getmeta")$dynrange_source, "file")
  adhoc <- list(monc = 0L, dformc = 5L, sf = 50, device_serial = "", dynrange_file = NULL)
  expect_error(.raw.clip.block.params(adhoc, pass = "getmeta", rmc.noise = NULL, rmc.dynamic_range = 8),
               "rmc.noise not specified")
  expect_identical(.raw.clip.block.params(adhoc, pass = "getmeta", rmc.noise = 0.013, rmc.dynamic_range = 8)$sdcriter, 0.013 * 1.2)
  corrupt <- list(monc = 3L, dformc = 6L, sf = NULL, device_serial = "not extracted", dynrange_file = NULL)
  expect_null(.raw.clip.block.params(corrupt, pass = "getmeta")$blocksize)
  expect_identical(.raw.clip.block.params(corrupt, pass = "getmeta")$clipthres, 7.5)
})

test_that("P1 (T1): MOS2 inspection is identical to the stored I object", {
  skip_if_no_ggir_ref()
  skip_if_no_file(MOS2())
  skip_if_no_file(MOS2_RDATA())
  info <- raw.inspect(MOS2(), desiredtz = "America/Anchorage")
  I <- load_stored_I(MOS2_RDATA())
  expect_s3_class(info, "canhrActi_raw_info")
  if (utils::packageVersion("read.gt3x") <= "1.2.0") {
    # the stored header strings were formatted by read.gt3x 1.2.0, which ignores tz; 1.3.0
    # shifts the displayed dates by the desiredtz offset
    expect_identical(info$header, I$header)
    expect_identical(.raw.ggir.I(info), I)
    expect_identical(levels(info$header$value), levels(I$header$value))
  } else {
    message("read.gt3x > 1.2.0 installed: header string compare against the stored I skipped")
    expect_identical(.raw.ggir.I(info)[-1], I[-1])
  }
  expect_identical(info$monc, 3L)
  expect_identical(info$monn, "actigraph")
  expect_identical(info$dformc, 6L)
  expect_identical(info$dformn, "gt3x")
  expect_identical(info$sf, 30)
  expect_identical(info$decn, ".")
  expect_identical(info$filename, "MOS2E39230594.gt3x")
  expect_identical(info$id, "MOS2E39230594.gt3x")
  expect_identical(info$device_serial, "MOS2E39230594_firmware_1.9.2")
  expect_identical(info$serial_prefix, "MOS")
  expect_identical(info$firmware, "1.9.2")
  expect_identical(info$header_timezone, "-04:00:00")
  expect_identical(nrow(info$header), 25L)
  expect_identical(info$dynrange, 8)
  expect_identical(info$dynrange_source, "serial_prefix")
  expect_identical(info$clipthres, 7.5)
  expect_identical(info$sdcriter, 0.013)
  expect_identical(info$racriter, 0.15)
  expect_identical(info$blocksize_calibrate, 43200)
  expect_identical(info$blocksize_getmeta, 86400)
  expect_false(info$corrupt)
  expect_false(info$too_small)
  expect_false(info$uppercase_extension)
  expect_true(info$ggir_accepts_extension)
  expect_false(info$renamed)
  expect_false(info$skipped)
  expect_identical(info$messages, character(0))
  expect_identical(info$size_bytes, 22102275)
  expect_identical(info$read_path, info$path)
  expect_identical(info$tz$desiredtz, "America/Anchorage")
  # header_list: dates from ticks, labelled GMT, version independent
  expect_identical(as.numeric(info$header_list[["Start Date"]]), 1759868880)
  expect_identical(attr(info$header_list[["Start Date"]], "tzone"), "GMT")
  expect_identical(format(info$header_list[["Start Date"]]), "2025-10-07 20:28:00")
  expect_identical(info$header_list[["Sample Rate"]], 30)
  expect_identical(info$header_list[["Serial Prefix"]], "MOS")
  expect_identical(info$header_list[["TimeZone"]], "-04:00:00")
  # a MOS serial overrides the user's dynrange
  info16 <- raw.inspect(MOS2(), desiredtz = "America/Anchorage", dynrange = 16)
  expect_identical(info16$clipthres, 7.5)
  expect_identical(info16$dynrange_source, "serial_prefix")
  # a backslash path gives the same object
  infob <- raw.inspect(gsub("/", "\\\\", MOS2()), desiredtz = "America/Anchorage")
  expect_identical(infob$path, info$path)
  expect_identical(.raw.ggir.I(infob), I)
})

test_that("P1 (T2): MOS2 matches live GGIR field by field", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_file(MOS2())
  Ig <- ggir_inspect(MOS2(), desiredtz = "America/Anchorage")
  info <- raw.inspect(MOS2(), desiredtz = "America/Anchorage")
  mine <- .raw.ggir.I(info)
  expect_identical(names(mine), names(Ig)[seq_along(names(Ig))])
  for (nm in names(Ig)) expect_identical(mine[[nm]], Ig[[nm]], info = nm)
  hv <- GGIR:::g.extractheadervars(Ig)
  expect_identical(.raw.header.vars(info), hv)
  expect_identical(info$header_vars, hv)
  expect_identical(.raw.header.vars(Ig), hv)
  for (idloc in c(1, 2, 3, 5, 6, 7)) {
    expect_identical(.raw.extract.id(hv, idloc, Ig$filename), GGIR:::extractID(hv, idloc, Ig$filename), info = idloc)
  }
  expect_warning(mine4 <- .raw.extract.id(hv, 4, Ig$filename), "Unable to extract ID")
  expect_warning(theirs4 <- GGIR:::extractID(hv, 4, Ig$filename), "Unable to extract ID")
  expect_identical(mine4, theirs4)
  expect_identical(info$id, GGIR:::extractID(hv, 1, Ig$filename))
  info6 <- raw.inspect(MOS2(), desiredtz = "America/Anchorage", idloc = 6)
  expect_identical(info6$id, "MOS2E39230594")
  ncb <- ggir_ncb(Ig)
  expect_identical(four(.raw.clip.block.params(info, pass = "getmeta")), ncb)
  expect_identical(four(.raw.clip.block.params(info, params = info$params, pass = "getmeta")), ncb)
  expect_identical(.raw.clip.block.params(info, pass = "calibrate")$blocksize, (12 * 3600) * 1)
  expect_identical(info$decn, GGIR:::g.dotorcomma(MOS2(), Ig$dformc, Ig$monc, rmc.dec = "."))
  expect_identical(.raw.decimal(MOS2(), Ig$dformc, Ig$monc, rmc.dec = "."), ".")
  info_p <- raw.inspect(MOS2(), params = raw.params(desiredtz = "America/Anchorage"))
  expect_identical(.raw.ggir.I(info_p), Ig[names(Ig)])
  info_l <- raw.inspect(MOS2(), params = list(desiredtz = "America/Anchorage"))
  expect_identical(.raw.ggir.I(info_l), Ig[names(Ig)])
})

test_that("P1: EE inspection (long running) is identical to its stored I", {
  skip_if_no_ggir_ref()
  skip_if(!canhr_flag("CANHRACTI_LONG_TESTS"), "set CANHRACTI_LONG_TESTS=1 to run the EE inspection (about 45 s)")
  skip_if_no_file(EE())
  skip_if_no_file(EE_RDATA())
  info <- raw.inspect(EE(), desiredtz = "Europe/Helsinki")
  I <- load_stored_I(EE_RDATA())
  expect_identical(.raw.ggir.I(info), I)
  expect_identical(info$sf, 100)
  expect_identical(nrow(info$header), 19L)
  expect_identical(info$serial_prefix, "TAS")
  expect_identical(info$device_serial, "TAS1F27160060_firmware_1.7.0")
  expect_identical(info$firmware, "1.7.0")
  expect_identical(info$clipthres, 7.5)
  expect_identical(info$dynrange_source, "assumed")
  expect_identical(info$blocksize_calibrate, 43200)
  expect_identical(info$blocksize_getmeta, 86400)
  expect_identical(info$header_timezone, "03:00:00")
  expect_false(info$corrupt)
  expect_false(info$too_small)
  expect_identical(format(info$header_list[["Start Date"]]), "2017-05-23 09:15:00") # the file name carries the download date
})

test_that("F1: TRUNC is corrupt (sf NULL) with both GGIR warning texts in messages", {
  skip_if_no_ggir_ref()
  skip_if_no_file(TRUNC())
  tr <- raw.inspect(TRUNC(), desiredtz = "America/Anchorage")
  expect_true(tr$corrupt)
  expect_null(tr$sf)
  expect_null(tr$header)
  expect_null(tr$header_list)
  expect_identical(tr$decn, ".")
  expect_identical(tr$monc, 3L)
  expect_identical(tr$monn, "actigraph")
  expect_identical(tr$dformc, 6L)
  expect_identical(tr$dformn, "gt3x")
  expect_identical(tr$filename, "truncated.gt3x")
  expect_identical(tr$id, "truncated.gt3x")
  expect_identical(tr$device_serial, "not extracted")
  expect_true(tr$too_small)
  expect_false(tr$skipped)
  expect_null(tr$blocksize_getmeta)
  expect_null(tr$blocksize_calibrate)
  expect_identical(tr$clipthres, 7.5)
  expect_true(any(grepl(paste0("File info could not be extracted from ", TRUNC()), tr$messages, fixed = TRUE)))
  expect_true(any(grepl("Sample frequency not recognised in truncated.gt3x", tr$messages, fixed = TRUE)))
  skip_if_no_ggir()
  Itr <- ggir_inspect(TRUNC(), desiredtz = "America/Anchorage")
  expect_identical(.raw.ggir.I(tr), Itr[names(Itr)])
  ggir_w <- unique(attr(Itr, "warnings"))
  expect_length(ggir_w, 2L)
  for (w in ggir_w) expect_true(w %in% tr$messages, info = w)
})

test_that("F2 (inspection part): SHORT is fine at inspection and flagged too_small", {
  skip_if_no_ggir_ref()
  skip_if_no_file(SHORT())
  sh <- raw.inspect(SHORT(), desiredtz = "America/Anchorage")
  expect_identical(sh$sf, 100)
  expect_false(sh$corrupt)
  expect_true(sh$too_small)
  expect_false(sh$skipped)
  expect_identical(sh$serial_prefix, "TAS")
  expect_identical(sh$device_serial, "TAS1H30182785_firmware_1.7.2")
  expect_identical(sh$clipthres, 7.5)
  expect_identical(sh$dynrange_source, "assumed")
  expect_identical(nrow(sh$header), 17L)
  expect_true(any(grepl("below GGIR's 2 MB floor", sh$messages)))
  # a TAS device honours the user's dynrange
  sh16 <- raw.inspect(SHORT(), desiredtz = "America/Anchorage", dynrange = 16)
  expect_identical(sh16$clipthres, 15.5)
  expect_identical(sh16$dynrange, 16)
  expect_identical(sh16$dynrange_source, "user")
  # the floor is a parameter
  sh_ok <- raw.inspect(SHORT(), desiredtz = "America/Anchorage", minimumFileSizeMB = 0.1)
  expect_false(sh_ok$too_small)
  expect_identical(sh_ok$messages, character(0))
  # skip_small_files reproduces GGIR's skip and reads nothing
  sk <- raw.inspect(SHORT(), desiredtz = "America/Anchorage", skip_small_files = TRUE)
  expect_true(sk$skipped)
  expect_true(sk$too_small)
  expect_false(sk$corrupt)
  expect_null(sk$sf)
  expect_null(sk$header)
  expect_null(sk$monc)
  expect_identical(sk$filename, "tooshort.gt3x")
  expect_identical(sk$messages, paste0("\nSkipping files that are too small for analysis: tooshort.gt3x",
                                       " (configurable with parameter minimumFileSizeMB)."))
  skip_if_no_ggir()
  Ish <- ggir_inspect(SHORT(), desiredtz = "America/Anchorage")
  expect_identical(.raw.ggir.I(sh), Ish[names(Ish)])
  expect_identical(four(.raw.clip.block.params(sh, pass = "getmeta")), ggir_ncb(Ish))
})

test_that("F3: an uppercase .GT3X is read through a lowercase stand-in and the source is untouched", {
  skip_if_no_ggir_ref()
  skip_if_no_file(SHORT())
  td <- fresh_dir("upper")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  up <- file.path(td, "SHORT_UPPER.GT3X")
  expect_true(file.copy(SHORT(), up, copy.date = TRUE))
  names_before <- list.files(td)
  mtime_before <- file.info(up)$mtime
  md5_before <- unname(tools::md5sum(up))

  info_up <- raw.inspect(up, desiredtz = "America/Anchorage")
  info_lo <- raw.inspect(SHORT(), desiredtz = "America/Anchorage")
  expect_true(info_up$uppercase_extension)
  expect_true(info_up$ggir_accepts_extension) # GGIR accepts "GT3X" (by renaming)
  expect_false(info_up$renamed)
  expect_identical(info_up$path, up)
  expect_identical(info_up$filename, "SHORT_UPPER.GT3X")
  expect_identical(info_up$id, "SHORT_UPPER.GT3X")
  expect_false(identical(info_up$read_path, up))
  expect_true(grepl("\\.gt3x$", info_up$read_path))
  expect_true(file.exists(info_up$read_path))
  expect_true(dirname(info_up$read_path) != dirname(up))
  expect_true(info_up$stand_in_method %in% c("link", "copy", "reused"))
  expect_true(any(grepl("GGIR renames such a file", info_up$messages)))
  same_fields <- c("monc", "monn", "dformc", "dformn", "sf", "decn", "header", "header_list",
                   "device_serial", "serial_prefix", "firmware", "header_timezone", "dynrange",
                   "dynrange_source", "clipthres", "sdcriter", "racriter", "blocksize_calibrate",
                   "blocksize_getmeta", "corrupt", "too_small")
  for (f in same_fields) expect_identical(info_up[[f]], info_lo[[f]], info = f)
  expect_identical(info_up$header_vars$deviceSerialNumber, info_lo$header_vars$deviceSerialNumber)

  # the user's file: same name, mtime and content
  expect_identical(list.files(td), names_before)
  expect_identical(file.info(up)$mtime, mtime_before)
  expect_identical(unname(tools::md5sum(up)), md5_before)
  expect_identical(unname(tools::md5sum(info_up$read_path)), md5_before)

  info_up2 <- raw.inspect(up, desiredtz = "America/Anchorage")
  expect_identical(info_up2$read_path, info_up$read_path)
  expect_identical(info_up2$stand_in_method, "reused")

  unlink(info_up$read_path)
  p_copy <- .raw.lowercase.gt3x(up, method = "copy")
  expect_identical(attr(p_copy, "method"), "copy")
  expect_identical(unname(tools::md5sum(p_copy)), md5_before)
  expect_identical(read.gt3x::parse_gt3x_info(p_copy)[["Sample Rate"]], 100)
  unlink(p_copy)

  # a mixed-case spelling GGIR would reject is read the same way
  mixed <- file.path(td, "mixed.Gt3x")
  file.copy(SHORT(), mixed)
  im <- raw.inspect(mixed, desiredtz = "America/Anchorage")
  expect_true(im$uppercase_extension)
  expect_false(im$ggir_accepts_extension)
  expect_identical(im$sf, 100)
  expect_true(any(grepl("case-sensitive extension switch", im$messages)))
  expect_true(grepl("\\.gt3x$", im$read_path))
  expect_identical(im$header, info_lo$header)

  skip_if_no_ggir()
  Ig <- ggir_inspect(SHORT(), desiredtz = "America/Anchorage")
  expect_identical(.raw.ggir.I(info_up)[-8], Ig[-8]) # everything but filename
})

test_that("F3: a hard-link stand-in has the source's bytes and leaves the source in place", {
  skip_if_no_ggir_ref()
  skip_if_no_file(SHORT())
  td <- fresh_dir("link")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  # the stand-in is linked under tempdir(), the volume td is on; exFAT and FAT refuse links
  probe <- file.path(td, "probe.txt")
  writeLines("probe", probe)
  linked <- suppressWarnings(tryCatch(file.link(probe, file.path(td, "probe_link.txt")),
                                      error = function(e) FALSE))
  skip_if_not(isTRUE(linked), "the file system under tempdir() refuses hard links")
  up <- file.path(td, "SHORT_UPPER.GT3X")
  expect_true(file.copy(SHORT(), up, copy.date = TRUE))
  md5_before <- unname(tools::md5sum(up))
  p_link <- .raw.lowercase.gt3x(up, method = "link")
  expect_identical(attr(p_link, "method"), "link")
  expect_identical(unname(tools::md5sum(p_link)), md5_before)
  unlink(p_link)
  expect_true(file.exists(up))
  expect_identical(unname(tools::md5sum(up)), md5_before)
})

test_that("F3: rename_uppercase = TRUE reproduces GGIR's rename and warning text", {
  skip_if_no_ggir_ref()
  skip_if_no_file(SHORT())
  td <- fresh_dir("rename")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  up <- file.path(td, "RENAME_ME.GT3X")
  file.copy(SHORT(), up)
  info <- raw.inspect(up, desiredtz = "America/Anchorage", rename_uppercase = TRUE)
  expect_true(info$renamed)
  expect_identical(info$read_path, file.path(td, "RENAME_ME.gt3x"))
  expect_true(file.exists(file.path(td, "RENAME_ME.gt3x")))
  expect_identical(info$filename, "RENAME_ME.GT3X") # GGIR keeps the old spelling in I$filename
  expect_identical(info$sf, 100)
  expect_true("\nWe have renamed the GT3X file to gt3x because GGIR dependency read.gt3x cannot handle uper case extension" %in% info$messages)
  skip_if_no_ggir()
  up2 <- file.path(td, "RENAME_ME2.GT3X")
  file.copy(SHORT(), up2)
  Ig <- ggir_inspect(up2, desiredtz = "America/Anchorage")
  expect_true(attr(Ig, "warnings")[1] %in% info$messages)
  expect_identical(.raw.ggir.I(info)[-8], Ig[names(Ig)][-8])
})

test_that("F3: uppercase .CWA and .BIN copies inspect like GGIR (no stand-in needed)", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  tf <- ggirread_tf()
  skip_if_no_file(file.path(tf, "ax3_testfile.cwa"))
  td <- fresh_dir("cwabin")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  copies <- c(UPPER.CWA = file.path(tf, "ax3_testfile.cwa"), GENE_UPPER.BIN = file.path(tf, "GENEActiv_testfile.bin"))
  for (nm in names(copies)) {
    dst <- file.path(td, nm)
    file.copy(copies[[nm]], dst)
    info <- raw.inspect(dst)
    Ig <- ggir_inspect(dst)
    expect_identical(.raw.ggir.I(info), Ig[names(Ig)], info = nm)
    expect_true(info$uppercase_extension)
    expect_true(info$ggir_accepts_extension)
    expect_identical(info$read_path, dst)
    expect_false(info$renamed)
  }
})

test_that("GENEActiv .bin, AX3/AX6 .cwa and Parmay .BIN test files are identical to GGIR", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  tf <- ggirread_tf()
  skip_if_no_file(file.path(tf, "GENEActiv_testfile.bin"))
  expected <- list(
    GENEActiv_testfile.bin = list(monc = 2L, dformc = 1L, sf = 86, serial = "012967", id = "notstoredinheader", cal = 12480),
    ax3_testfile.cwa = list(monc = 4L, dformc = 4L, sf = 100, serial = "39434", id = "ax3_testfile.cwa", cal = 54000),
    ax6_testfile.cwa = list(monc = 4L, dformc = 4L, sf = 100, serial = "6011834", id = "ax6_testfile.cwa", cal = 54000),
    mtx_12.5Hz_acc.BIN = list(monc = 7L, dformc = 1L, sf = 12.5, serial = "not extracted", id = "mtx_12.5Hz_acc.BIN", cal = 360),
    mtx_corrupted.bin = list(monc = 7L, dformc = 1L, sf = 100, serial = "not extracted", id = "mtx_corrupted.bin", cal = 360),
    ax3_testfile_corrupt_blocks_0_13_14_142_143_144.cwa = list(monc = 4L, dformc = 4L, sf = 100, serial = "39434",
                                                              id = "ax3_testfile_corrupt_blocks_0_13_14_142_143_144.cwa", cal = 54000)
  )
  for (fn in names(expected)) {
    p <- file.path(tf, fn)
    if (!file.exists(p)) next
    ex <- expected[[fn]]
    info <- raw.inspect(p)
    Ig <- ggir_inspect(p)
    expect_identical(.raw.ggir.I(info), Ig[names(Ig)], info = fn)
    expect_identical(info$monc, ex$monc, info = fn)
    expect_identical(info$dformc, ex$dformc, info = fn)
    expect_identical(info$sf, ex$sf, info = fn)
    expect_identical(info$device_serial, ex$serial, info = fn)
    expect_identical(info$id, ex$id, info = fn)
    expect_identical(info$header_vars, GGIR:::g.extractheadervars(Ig), info = fn)
    expect_identical(four(.raw.clip.block.params(info, pass = "getmeta")), ggir_ncb(Ig), info = fn)
    expect_identical(info$blocksize_calibrate, ex$cal, info = fn)
    expect_true(info$too_small, info = fn)
    expect_false(info$corrupt, info = fn)
    expect_identical(info$decn, suppressWarnings(GGIR:::g.dotorcomma(p, Ig$dformc, Ig$monc, rmc.dec = ".")), info = fn)
    for (w in unique(attr(Ig, "warnings"))) expect_true(w %in% info$messages, info = paste(fn, w))
  }
  gene <- raw.inspect(file.path(tf, "GENEActiv_testfile.bin"))
  expect_identical(gene$header_vars$HN, "not stored in header")
  expect_identical(gene$firmware, "Ver1.30 date 05Aug11")
  expect_identical(gene$header_timezone, "3600")
  expect_identical(as.character(gene$header["SampleRate", ]), "85.7")
  expect_identical(gene$clipthres, 7.5)
  gene2 <- raw.inspect(file.path(tf, "GENEActiv_testfile.bin"), idloc = 2)
  expect_identical(gene2$id, "GENEActiv")
  ax6 <- raw.inspect(file.path(tf, "ax6_testfile.cwa"))
  expect_identical(ax6$header["hardwareType", ][[1]], "AX6")
  expect_identical(ax6$header["accrange", ][[1]], 16L)
  expect_identical(ax6$clipthres, 7.5) # header accrange is ignored
  expect_identical(ax6$firmware, "54")
  expect_identical(nrow(ax6$header), 9L)
  expect_true(is.list(ax6$header$value))
  par <- raw.inspect(file.path(tf, "mtx_12.5Hz_acc.BIN"))
  expect_identical(par$header, "no header")
  expect_identical(par$dynrange_file, 8L)
  expect_identical(par$dynrange, 8)
  expect_identical(par$dynrange_source, "file")
  expect_identical(par$blocksize_getmeta, 720)
  cor <- raw.inspect(file.path(tf, "ax3_testfile_corrupt_blocks_0_13_14_142_143_144.cwa"))
  expect_true(any(grepl("Skipping corrupt block #0", cor$messages, fixed = TRUE)))
})

test_that("ActiGraph raw csv (.csv.gz) and Axivity csv are identical to GGIR", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  csvgz <- system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz", package = "read.gt3x")
  skip_if_no_file(csvgz)
  info <- raw.inspect(csvgz)
  Ig <- ggir_inspect(csvgz)
  expect_identical(.raw.ggir.I(info), Ig[names(Ig)])
  expect_identical(info$monc, 3L)
  expect_identical(info$dformc, 2L)
  expect_identical(info$dformn, "csv")
  expect_identical(info$sf, 100)
  expect_identical(info$device_serial, " TAS1H30182785_firmware_v1.7.2") # leading space kept
  expect_identical(info$serial_prefix, "TAS")
  expect_identical(info$firmware, "v1.7.2")
  expect_identical(nrow(info$header), 10L)
  expect_identical(rownames(info$header)[1], "First line")
  expect_identical(info$header_vars, GGIR:::g.extractheadervars(Ig))
  expect_identical(four(.raw.clip.block.params(info, pass = "getmeta")), ggir_ncb(Ig))
  expect_identical(info$blocksize_getmeta, round(14512 * (100 / 50)))
  expect_identical(info$blocksize_calibrate, round((14512 * (100 / 50)) * 0.5))
  expect_identical(info$decn, GGIR:::g.dotorcomma(csvgz, Ig$dformc, Ig$monc, rmc.dec = "."))

  gtf <- ggir_tf()
  axcsv <- file.path(gtf, "ax3_testfile_unix_timestamps.csv")
  skip_if_no_file(axcsv)
  info <- raw.inspect(axcsv)
  Ig <- ggir_inspect(axcsv)
  expect_identical(.raw.ggir.I(info), Ig[names(Ig)])
  expect_identical(info$monc, 4L)
  expect_identical(info$dformc, 2L)
  expect_identical(info$sf, 95) # floor(observed / 5) * 5
  expect_identical(info$device_serial, "not extracted")
  expect_true(is.data.frame(info$header))
  expect_identical(info$header_vars, GGIR:::g.extractheadervars(Ig))
  ax6csv <- file.path(gtf, "ax6_testfile_formatted_timestamps.csv")
  if (file.exists(ax6csv)) {
    info6 <- raw.inspect(ax6csv)
    expect_identical(.raw.ggir.I(info6), ggir_inspect(ax6csv)[names(Ig)])
  }
})

test_that("Movisens: a synthetic unisens folder is inspected like GGIR", {
  skip_if_no_ggir()
  td <- fresh_dir("movisens")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  sub <- file.path(td, "P001")
  dir.create(sub)
  writeBin(as.raw(1:100), file.path(sub, "acc.bin"))
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<unisens comment="" duration="86400" measurementId="M001" timestampStart="2020-01-01T10:00:00.000" version="2.0" xmlns="http://www.unisens.org/unisens2.0">',
    '  <customAttributes>',
    '    <customAttribute key="sensorSerialNumber" value="123456"/>',
    '    <customAttribute key="sensorType" value="Move 4"/>',
    '  </customAttributes>',
    '  <signalEntry adcResolution="12" baseline="2048" comment="" contentClass="ACC" dataType="int16" id="acc.bin" lsbValue="0.00390625" sampleRate="64" unit="g">',
    '    <binFileFormat endianess="LITTLE"/>',
    '    <channel name="acc_x"/>',
    '    <channel name="acc_y"/>',
    '    <channel name="acc_z"/>',
    '  </signalEntry>',
    '</unisens>'), file.path(sub, "unisens.xml"))
  acc <- file.path(sub, "acc.bin")
  expect_true(.raw.is.movisens(acc))
  expect_true(.raw.is.movisens(sub))
  expect_true(.raw.is.movisens(td)) # GGIR tests the folder of the first file found recursively
  info <- raw.inspect(acc)
  Ig <- ggir_inspect(acc)
  expect_identical(.raw.ggir.I(info), Ig[names(Ig)])
  expect_identical(info$monc, 5L)
  expect_identical(info$monn, "movisens")
  expect_identical(info$dformc, 1L)
  expect_identical(info$sf, 64)
  expect_identical(info$filename, "P001") # the folder name
  expect_identical(info$id, "M001")
  expect_identical(info$device_serial, "123456")
  expect_identical(info$header_vars, GGIR:::g.extractheadervars(Ig))
  expect_identical(four(.raw.clip.block.params(info, pass = "getmeta")), ggir_ncb(Ig))
  expect_identical(info$clipthres, 15.5)
  expect_identical(info$dynrange_source, "movisens_assumed")
  expect_identical(info$blocksize_getmeta, 64 * 60 * 1440)
  expect_identical(info$blocksize_calibrate, (64 * 60 * 1440) / 2)
  # discovery lists the folder by name
  d <- raw.discover(td)
  expect_identical(nrow(d), 1L)
  expect_identical(d$filename, "P001")
  expect_true(d$recognised)
  expect_true(startsWith(d$reason, "movisens acc.bin recording"))
  # a sibling folder without acc.bin gets GGIR's warning text as its reason
  dir.create(file.path(td, "P002"))
  writeLines("x", file.path(td, "P002", "notes.txt"))
  d2 <- raw.discover(td)
  expect_identical(nrow(d2), 2L)
  expect_false(d2$recognised[2])
  expect_true(grepl("do not contain the acc.bin file", d2$reason[2], fixed = TRUE))
})

test_that("F6: an ActiGraph epoch csv gives a clear error naming the file", {
  td <- fresh_dir("epoch")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  f <- file.path(td, "subject_epochs.csv")
  writeLines(c(
    "------------ Data File Created By ActiGraph wGT3XPlus ActiLife v6.10.2 Firmware v2.2.1 date format M/d/yyyy Filter Normal -----------,,,",
    "Serial Number: CLE2A2123456,,,",
    "Start Time 09:00:00,,,",
    "Start Date 8/26/2013,,,",
    "Epoch Period (hh:mm:ss) 00:00:15,,,",
    "Download Time 12:54:04,,,",
    "Download Date 9/3/2013,,,",
    "Current Memory Address: 0,,,",
    "Current Battery Voltage: 4.03     Mode = 13,,,",
    "--------------------------------------------------,,,",
    rep("0,0,0,0", 300)), f)
  err <- tryCatch(raw.inspect(f), error = function(e) e)
  expect_s3_class(err, "canhrActi_raw_inspect_error")
  expect_true(grepl("does not look like raw acceleration data", conditionMessage(err), fixed = TRUE))
  expect_true(grepl("subject_epochs.csv", conditionMessage(err), fixed = TRUE))
  expect_true(grepl("epoch (count) export", conditionMessage(err), fixed = TRUE))
  expect_identical(err$path, f)
  skip_if_no_ggir()
  expect_error(suppressWarnings(GGIR::g.inspectfile(f)), "missing value where TRUE/FALSE needed")
  gtf <- ggir_tf()
  if (file.exists(file.path(gtf, "ActiGraph13.csv"))) {
    expect_error(raw.inspect(file.path(gtf, "ActiGraph13.csv")), "does not look like raw acceleration data")
  }
})

test_that("F7: a .bin of an unknown brand stops with GGIR's text as a classed condition", {
  skip_if_no_ggir_ref()
  genea <- file.path(ggirread_tf(), "genea_testfile.bin")
  skip_if_no_file(genea)
  err <- tryCatch(raw.inspect(genea), error = function(e) e)
  expect_s3_class(err, "canhrActi_raw_inspect_error")
  expect_identical(conditionMessage(err), "\nError processing genea_testfile.bin: unrecognised .bin file")
  expect_identical(err$path, genea)
  expect_identical(.raw.bin.brand(genea), "not_recognised")
  expect_identical(.raw.bin.brand(file.path(ggirread_tf(), "GENEActiv_testfile.bin")), 2L)
  expect_identical(.raw.bin.brand(file.path(ggirread_tf(), "mtx_12.5Hz_acc.BIN")), 7L)
  skip_if_no_ggir()
  expect_error(GGIR::g.inspectfile(genea), "unrecognised .bin file", fixed = TRUE)
})

test_that("GGIR's other stops keep GGIR's texts; case-insensitive spellings are read", {
  skip_if_no_ggir_ref()
  tf <- ggirread_tf()
  wav <- file.path(tf, "ax3test.wav")
  skip_if_no_file(wav)
  expect_error(raw.inspect(wav), "Axivity .wav file format is no longer supported", fixed = TRUE)
  td <- fresh_dir("stops")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  weird <- file.path(td, "weird.xyz")
  file.copy(wav, weird)
  err <- tryCatch(raw.inspect(weird), error = function(e) e)
  expect_identical(conditionMessage(err), "\nError processing weird.xyz: unrecognised file format.\n")
  expect_error(raw.inspect(file.path(td, "missing.gt3x")), "File not found: .*missing.gt3x")
  expect_error(raw.inspect(td), "is a directory")
  gtf <- ggir_tf()
  ukb <- file.path(gtf, "ukbiobank.csv")
  if (file.exists(ukb)) {
    expect_error(raw.inspect(ukb), "GENEActiv csv reading functionality is deprecated", fixed = TRUE)
  }
  csvgz <- system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz", package = "read.gt3x")
  if (file.exists(csvgz)) {
    upper <- file.path(td, "UPPER_RAW.CSV.GZ")
    file.copy(csvgz, upper)
    info <- raw.inspect(upper)
    expect_identical(info$sf, 100)
    expect_true(info$uppercase_extension)
    expect_false(info$ggir_accepts_extension)
    expect_true(any(grepl("case-sensitive extension switch", info$messages)))
    skip_if_no_ggir()
    expect_error(GGIR::g.inspectfile(upper), "unrecognised file format", fixed = TRUE)
  }
})

test_that("F4: raw.discover reports empty, unrelated and oddly named folders with reasons", {
  td <- fresh_dir("discover")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  empty <- file.path(td, "empty")
  dir.create(empty)
  d <- raw.discover(empty)
  expect_s3_class(d, "data.frame")
  expect_identical(names(d), c("path", "filename", "size_bytes", "recognised", "reason", "too_small"))
  expect_identical(nrow(d), 0L)
  expect_true(grepl("no accelerometer files found", attr(d, "message"), fixed = TRUE))

  agd <- file.path(td, "agd_only")
  dir.create(agd)
  writeBin(as.raw(1:10), file.path(agd, "subject01.agd"))
  writeLines("hello", file.path(agd, "readme.txt"))
  d <- raw.discover(agd)
  expect_identical(nrow(d), 2L)
  expect_false(any(d$recognised))
  expect_true(all(grepl("is not one GGIR reads", d$reason, fixed = TRUE)))
  expect_true(any(grepl("extension .agd", d$reason, fixed = TRUE)))
  expect_true(grepl("no accelerometer files found", attr(d, "message"), fixed = TRUE))

  dotcsv <- file.path(td, "exports.csv_2024")
  dir.create(dotcsv)
  writeBin(as.raw(1:10), file.path(dotcsv, "subject01.agd"))
  d <- raw.discover(dotcsv)
  expect_identical(sum(d$recognised), 0L)
  expect_identical(nrow(d), 1L)

  d <- raw.discover(file.path(td, "does_not_exist.gt3x"))
  expect_identical(nrow(d), 1L)
  expect_false(d$recognised)
  expect_identical(d$reason, "file does not exist")

  # a wav file is found (GGIR lists it) but marked with GGIR's stop text
  wavdir <- file.path(td, "wav")
  dir.create(wavdir)
  writeBin(as.raw(1:10), file.path(wavdir, "x.wav"))
  writeBin(as.raw(1:10), file.path(wavdir, "y.GT3X"))
  writeBin(as.raw(1:10), file.path(wavdir, "z.csv.gz"))
  d <- raw.discover(wavdir)
  expect_identical(d$filename, c("z.csv.gz", "x.wav", "y.GT3X")) # GGIR's group order: csv, bin, wav, cwa, gt3x
  expect_identical(d$recognised, c(TRUE, FALSE, TRUE))
  expect_true(grepl("no longer supported", d$reason[2], fixed = TRUE))
  expect_true(all(d$too_small))
  expect_true(all(grepl("GGIR would skip it", d$reason[c(1, 3)], fixed = TRUE)))
  d2 <- raw.discover(wavdir, skip_small_files = TRUE)
  expect_false(any(d2$recognised))
  expect_true(all(grepl("Skipping files that are too small for analysis", d2$reason[c(1, 3)], fixed = TRUE)))
  d3 <- raw.discover(wavdir, minimumFileSizeMB = 0)
  expect_false(any(d3$too_small))
  expect_error(raw.discover(character(0)), "character vector")
})

test_that("F4: raw.discover reproduces datadir2fnames' listing on the study folder", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  tf <- study_tf()
  skip_if(!dir.exists(tf), "study tf folder not found")
  d <- raw.discover(tf)
  ggir <- GGIR:::datadir2fnames(tf, FALSE)
  in_set <- tolower(.raw.extension(d$filename)) %in% .RAW_EXTENSIONS
  expect_identical(normalizePath(d$path[in_set], winslash = "/"), normalizePath(ggir$fnamesfull, winslash = "/"))
  expect_identical(d$filename[in_set], ggir$fnames)
  expect_true(all(d$recognised[in_set & !grepl("wav$", d$filename)]))
  expect_false(any(d$recognised[!in_set]))
  # explicit paths
  d2 <- raw.discover(c(MOS2(), TRUNC(), file.path(tf, "nope.cwa")))
  expect_identical(nrow(d2), 3L)
  expect_identical(d2$recognised, c(TRUE, TRUE, FALSE))
  expect_identical(d2$too_small, c(FALSE, TRUE, FALSE))
  expect_identical(d2$size_bytes[1], 22102275)
})

test_that("decimal detection matches g.dotorcomma for every format on disk", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  tf <- ggirread_tf()
  cases <- list(
    list(path = MOS2(), dformc = 6L, monc = 3L),
    list(path = system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz", package = "read.gt3x"), dformc = 2L, monc = 3L),
    list(path = file.path(tf, "GENEActiv_testfile.bin"), dformc = 1L, monc = 2L),
    list(path = file.path(tf, "ax3_testfile.cwa"), dformc = 4L, monc = 4L),
    list(path = file.path(tf, "mtx_12.5Hz_acc.BIN"), dformc = 1L, monc = 7L)
  )
  for (cs in cases) {
    if (!file.exists(cs$path)) next
    mine <- suppressWarnings(.raw.decimal(cs$path, cs$dformc, cs$monc, rmc.dec = "."))
    theirs <- suppressWarnings(GGIR:::g.dotorcomma(cs$path, cs$dformc, cs$monc, rmc.dec = "."))
    expect_identical(mine, theirs, info = basename(cs$path))
    expect_identical(mine, ".", info = basename(cs$path))
  }
  # a comma-decimal csv is called ","
  td <- fresh_dir("comma")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  f <- file.path(td, "comma.csv")
  writeLines(c("a;b;c", rep("0,5;0,25;1,0", 200)), f)
  expect_identical(.raw.decimal(f, 2L, 4L, rmc.dec = "."), ",")
  expect_identical(.raw.decimal(f, 2L, 4L, rmc.dec = "."), suppressWarnings(GGIR:::g.dotorcomma(f, 2L, 4L, rmc.dec = ".")))
})

test_that("the print method summarises the object", {
  skip_if_no_ggir_ref()
  skip_if_no_file(SHORT())
  info <- raw.inspect(SHORT(), desiredtz = "America/Anchorage")
  out <- capture.output(print.canhrActi_raw_info(info))
  expect_true(any(grepl("actigraph (gt3x)", out, fixed = TRUE)))
  expect_true(any(grepl("100 Hz", out, fixed = TRUE)))
  expect_true(any(grepl("too_small TRUE", out, fixed = TRUE)))
  sk <- raw.inspect(SHORT(), skip_small_files = TRUE)
  out2 <- capture.output(print.canhrActi_raw_info(sk))
  expect_true(any(grepl("skipped", out2, fixed = TRUE)))
})
