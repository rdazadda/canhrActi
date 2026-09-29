# Tests for R/raw_cutpoints.R: the 48 published sets taken from GGIR's CutPoints vignette,
# the part 5 parameters one set turns into, its label and the part 1 metrics a set needs.
# The row-by-row check against the vignette reads GGIR's source in CANHRACTI_GGIR_REF
# (GGIRsrc/vignettes/CutPoints.Rmd) and skips without it.

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}
ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)

# One row per published set, as the vignette's grid tables print it: the section, the
# five cells of the row (a cell's lines joined) and the three bands
vignette_cutpoints <- function(path) {
  rows <- list()
  cur <- list()
  section <- NA_character_
  flush <- function() {
    if (length(cur) > 0) {
      cells <- vapply(seq_len(max(lengths(cur))), function(j) {
        txt <- vapply(cur, function(p) if (j <= length(p)) p[[j]] else "", "")
        trimws(gsub("\\s+", " ", paste(sub("\\\\$", "", trimws(txt)), collapse = " ")))
      }, "")
      if (length(cells) >= 5 && grepl("(Light|Moderate):", cells[5])) {
        rows[[length(rows) + 1]] <<- c(section, cells[1:5])
      }
    }
    cur <<- list()
  }
  for (ln in readLines(path, encoding = "UTF-8", warn = FALSE)) {
    if (startsWith(ln, "## Cut-points for")) section <- sub("^## Cut-points for ", "", ln)
    if (startsWith(ln, "|")) cur[[length(cur) + 1]] <- strsplit(ln, "|", fixed = TRUE)[[1]][-1]
    else flush()
  }
  flush()
  v <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(v) <- c("section", "study", "device", "age", "args", "thresholds")
  band <- function(nm) {
    m <- regmatches(v$thresholds, regexec(paste0(nm, ": ([0-9.]+|N/A)"), v$thresholds))
    suppressWarnings(as.numeric(vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, "")))
  }
  v$light <- band("Light")
  v$moderate <- band("Moderate")
  v$vigorous <- band("Vigorous")
  v$metric <- sub('.*acc\\.metric = "([A-Za-z]+)".*', "\\1", v$args)
  v
}

# the `name = value` arguments of a vignette cell as a list, sorted by name
vignette_args <- function(cell) {
  bits <- trimws(gsub("`", "", regmatches(cell, gregexpr("`[^`]+`", cell))[[1]]))
  out <- lapply(sub(".* = ", "", bits), function(x) {
    if (x %in% c("TRUE", "FALSE")) as.logical(x)
    else if (grepl('^".*"$', x)) gsub('"', "", x)
    else as.numeric(x)
  })
  names(out) <- sub(" = .*", "", bits)
  out[order(names(out), method = "radix")]
}

test_that("raw.cutpoints() holds the 48 published sets and their columns", {
  d <- raw.cutpoints()
  expect_identical(names(d), c("key", "study", "group", "age", "brand", "location", "metric",
                               "light", "moderate", "vigorous", "variant", "rescaled"))
  expect_identical(nrow(d), 48L)
  expect_identical(anyDuplicated(d$key), 0L)
  expect_identical(rownames(d), as.character(1:48))
  expect_identical(vapply(d[c("light", "moderate", "vigorous")], typeof, ""),
                   c(light = "double", moderate = "double", vigorous = "double"))
  expect_identical(typeof(d$rescaled), "logical")
  groups <- c("Preschoolers", "Children and adolescents", "Adults", "Older adults")
  expect_identical(as.vector(table(factor(d$group, levels = groups))), c(2L, 10L, 16L, 20L))
  metrics <- c("ENMO", "ENMOa", "MAD", "BFEN")
  expect_identical(as.vector(table(factor(d$metric, levels = metrics))), c(25L, 15L, 7L, 1L))
  # a band the study did not define is NA; 21 sets stop at moderate
  expect_identical(c(sum(is.na(d$light)), sum(is.na(d$moderate)), sum(is.na(d$vigorous))),
                   c(6L, 2L, 21L))
  rising <- with(d, (is.na(light) | is.na(moderate) | light < moderate) &
                   (is.na(moderate) | is.na(vigorous) | moderate < vigorous))
  expect_identical(d$key[!rising], character(0))
})

test_that("raw.cutpoints() filters by metric and group and marks what a recording can apply", {
  expect_identical(raw.cutpoints(metric = "MAD")$key,
                   c("aittasalo2015_child_mad_actigraph_hip", "aittasalo2015_child_mad_hookieam_hip",
                     "vahaypya2015_adult_mad_hookieam_hip", "buchan2023_adult_mad_activpal_rightthigh",
                     "dibben2020_older_mad_geneactiv_rightwrist", "dibben2020_older_mad_geneactiv_leftwrist",
                     "dibben2020_older_mad_geneactiv_hip"))
  both <- raw.cutpoints(metric = "ENMO", group = "Older adults")
  expect_identical(nrow(both), 12L)
  expect_identical(rownames(both), as.character(1:12))
  expect_identical(unique(c(both$metric, both$group)), c("ENMO", "Older adults"))
  expect_identical(nrow(raw.cutpoints(metric = "LFENMO")), 0L)
  av <- raw.cutpoints(available = c("ENMO", "MAD"))
  expect_identical(av$available, av$metric %in% c("ENMO", "MAD"))
  expect_identical(sum(av$available), 32L)
  expect_identical("available" %in% names(raw.cutpoints()), FALSE)
})

test_that("raw.cutpoints() carries the numbers GGIR prints for a few sets", {
  d <- raw.cutpoints()
  bands <- function(k) unlist(d[d$key == k, c("light", "moderate", "vigorous")], use.names = FALSE)
  expect_identical(bands("hildebrand2014_adult_enmo_actigraph_ndwrist"), c(44.8, 100.6, 428.8))
  expect_identical(bands("hildebrand2014_child_enmo_geneactiv_ndwrist"), c(56.3, 191.6, 695.8))
  expect_identical(bands("schaefer2014_child_bfen_geneactiv_ndwrist"), c(190, 314, 998))
  expect_identical(bands("vahaypya2015_adult_mad_hookieam_hip"), c(NA, 91, 414))
  expect_identical(bands("migueles2021_older_enmo_actigraph_ndwrist"), c(18, 60, NA))
  expect_identical(bands("bammann2021_older_enmo_actigraph_dankle"), c(NA, 342, NA))
  expect_identical(d$rescaled[d$key == "roscoe2017_pre_enmoa_geneactiv_ndwrist"], TRUE)
})

test_that("raw.cutpoints() matches GGIR's CutPoints vignette row by row", {
  skip_if_no_ggir_ref()
  path <- ref_file("GGIRsrc", "vignettes", "CutPoints.Rmd")
  skip_if_no_file(path)
  v <- vignette_cutpoints(path)
  d <- raw.cutpoints()
  expect_identical(nrow(v), nrow(d))
  expect_identical(v$metric, d$metric)
  expect_identical(v$light, d$light)
  expect_identical(v$moderate, d$moderate)
  expect_identical(v$vigorous, d$vigorous)
  expect_identical(tolower(v$device), tolower(trimws(paste(d$brand, d$location))))
  expect_identical(gsub(intToUtf8(0x2265), ">=", v$age, fixed = TRUE), d$age)
  expect_identical(sub(" .*", "", v$study), sub(" .*", "", d$study))
  years <- function(s) vapply(regmatches(s, gregexpr("[0-9]{4}", s)), paste, "", collapse = "/")
  expect_identical(years(v$study), years(d$study))
  groups <- c(preschoolers = "Preschoolers", "children/adolescents" = "Children and adolescents",
              adults = "Adults", "older adults" = "Older adults")
  expect_identical(unname(groups[v$section]), d$group)
  # a star marks a rescaled set, except among older adults, where a dagger does and one or
  # two stars name Sanders' two ROC criteria
  older <- v$section == "older adults"
  stars <- lengths(regmatches(v$study, gregexpr("*", v$study, fixed = TRUE)))
  expect_identical(ifelse(older, grepl(intToUtf8(0x2020), v$study, fixed = TRUE), stars > 0),
                   d$rescaled)
  variant <- ifelse(older & stars == 1, "Youden index",
                    ifelse(older & stars == 2, "Sensitivity over specificity", ""))
  expect_identical(variant, d$variant)
  flags <- lapply(d$key, function(k) {
    f <- raw.cutpoint(k)$metric_flags
    f[order(names(f), method = "radix")]
  })
  expect_identical(lapply(v$args, vignette_args), flags)
})

test_that("raw.cutpoint() turns a set into part 5 thresholds and part 1 flags", {
  k <- "hildebrand2014_adult_enmo_actigraph_ndwrist"
  cp <- raw.cutpoint(k)
  expect_identical(names(cp), c("threshold.lig", "threshold.mod", "threshold.vig", "acc.metric",
                                "metric_flags", "row", "filled"))
  expect_identical(c(cp$threshold.lig, cp$threshold.mod, cp$threshold.vig), c(44.8, 100.6, 428.8))
  expect_identical(cp$acc.metric, "ENMO")
  expect_identical(cp$metric_flags, list(do.enmo = TRUE, acc.metric = "ENMO"))
  d <- raw.cutpoints()
  expect_identical(cp$row, d[d$key == k, , drop = FALSE])
  expect_identical(cp$filled, character(0))
  expect_identical(raw.cutpoint("dibben2020_older_mad_geneactiv_hip")$metric_flags,
                   list(do.mad = TRUE, do.enmo = FALSE, acc.metric = "MAD"))
  expect_identical(raw.cutpoint("fraysse2020_older_enmoa_geneactiv_ndwrist")$metric_flags,
                   list(do.enmoa = TRUE, do.enmo = FALSE, acc.metric = "ENMOa"))
  expect_identical(raw.cutpoint("schaefer2014_child_bfen_geneactiv_ndwrist")$metric_flags,
                   list(do.bfen = TRUE, lb = 0.2, hb = 15, do.enmo = FALSE, acc.metric = "BFEN"))
  same <- vapply(d$key, function(k) identical(raw.cutpoint(k)$acc.metric, d$metric[d$key == k]), TRUE)
  expect_identical(d$key[!same], character(0))
})

test_that("raw.cutpoint() fills only the bands the study left out, and says which", {
  k <- "migueles2021_older_enmo_actigraph_ndwrist"
  expect_identical(raw.cutpoint(k)$threshold.vig, NA_real_)
  cp <- raw.cutpoint(k, fill = c(light = 1, vigorous = 400))
  expect_identical(c(cp$threshold.lig, cp$threshold.mod, cp$threshold.vig), c(18, 60, 400))
  expect_identical(cp$filled, "vigorous")
  cp <- raw.cutpoint("bammann2021_older_enmo_actigraph_dankle", fill = c(light = 30, vigorous = 400))
  expect_identical(c(cp$threshold.lig, cp$threshold.mod, cp$threshold.vig), c(30, 342, 400))
  expect_identical(cp$filled, c("light", "vigorous"))
  expect_error(raw.cutpoint("nope"),
               "unknown cut-point key: nope. See raw.cutpoints()$key for the 48 published sets.",
               fixed = TRUE)
})

test_that("raw.cutpoint.label() names the study, the variant, the device and the site", {
  dot <- paste0(" ", intToUtf8(0xb7), " ")
  k <- "migueles2021_older_enmo_actigraph_ndwrist"
  expect_identical(raw.cutpoint.label(k),
                   paste("Migueles 2021", "ActiGraph", "Non-dominant wrist", sep = dot))
  expect_identical(raw.cutpoint.label(k, long = TRUE),
                   paste("Migueles 2021", "ActiGraph", "Non-dominant wrist",
                         ">=70 yr (mean: 78.7 yr)", "ENMO", sep = dot))
  expect_identical(raw.cutpoint.label("sanders2019_older_enmo_geneactiv_ndwrist_youden"),
                   paste("Sanders 2019", "Youden index", "GENEActiv", "Non-dominant wrist", sep = dot))
  expect_identical(raw.cutpoint.label("esliger2011_adult_enmoa_na_leftwrist"),
                   paste("Esliger 2011", "Left wrist", sep = dot))
  expect_identical(raw.cutpoint.label("nope"), "")
  d <- raw.cutpoints()
  expect_identical(raw.cutpoint.label(d[5, ]), raw.cutpoint.label(d$key[5]))
  expect_identical(anyDuplicated(vapply(d$key, raw.cutpoint.label, "", long = TRUE)), 0L)
})

test_that("raw.cutpoint.metrics() switches on every metric the sets need and nothing off", {
  sorted <- function(x) x[order(names(x), method = "radix")]
  m <- raw.cutpoint.metrics(c("hildebrand2014_adult_enmo_actigraph_ndwrist",
                              "aittasalo2015_child_mad_actigraph_hip"))
  expect_identical(sorted(m), list(do.enmo = TRUE, do.mad = TRUE))
  # an ENMOa set asks for ENMOa; its do.enmo = FALSE does not turn ENMO off
  expect_identical(raw.cutpoint.metrics("roscoe2017_pre_enmoa_geneactiv_ndwrist"), list(do.enmoa = TRUE))
  all <- raw.cutpoint.metrics()
  expect_identical(sorted(all), list(do.bfen = TRUE, do.enmo = TRUE, do.enmoa = TRUE, do.mad = TRUE,
                                     hb = 15, lb = 0.2))
  expect_identical(raw.cutpoint.metrics("nope"), list())
  p <- do.call(raw.params, all)
  expect_identical(p[c("do.enmo", "do.enmoa", "do.mad", "do.bfen", "lb", "hb")],
                   list(do.enmo = TRUE, do.enmoa = TRUE, do.mad = TRUE, do.bfen = TRUE,
                        lb = 0.2, hb = 15))
})
