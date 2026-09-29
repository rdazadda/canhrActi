# Tests for the count cut points in R/cut_points.R: each set at its boundaries (the last
# value of one class and the first of the next), the dispatcher, the listing and the
# minute summaries built on the classes.

LV4 <- c("sedentary", "light", "moderate", "vigorous")

expect_classes <- function(got, levels, values) {
  expect_identical(class(got), c("ordered", "factor"))
  expect_identical(levels(got), levels)
  expect_identical(as.character(got), values)
}

test_that("troiano() applies the NHANES 2008 cut points", {
  expect_classes(troiano(c(0, 99, 100, 2019, 2020, 5998, 5999, 20000)), LV4,
                 c("sedentary", "sedentary", "light", "light", "moderate", "moderate",
                   "vigorous", "vigorous"))
})

test_that("matthews() puts a lifestyle class between light and moderate", {
  expect_classes(matthews(c(99, 100, 759, 760, 1951, 1952, 5724, 5725)),
                 c("sedentary", "light", "lifestyle", "moderate", "vigorous"),
                 c("sedentary", "light", "light", "lifestyle", "lifestyle", "moderate",
                   "moderate", "vigorous"))
})

test_that("santos_lozano() applies its younger and older adult cut points", {
  expect_classes(santos_lozano(c(99, 100, 3207, 3208, 8564, 8565)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
  expect_classes(santos_lozano(c(99, 100, 2750, 2751, 9358, 9359), "older"), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
  expect_error(santos_lozano(100, "middle"), "should be one of")
})

test_that("sasaki_vm3() applies Sasaki 2011 to vector magnitude counts per minute", {
  expect_classes(sasaki_vm3(c(199, 200, 2689, 2690, 6166, 6167, 9642, 9643)),
                 c(LV4, "very_vigorous"),
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous",
                   "vigorous", "very_vigorous"))
})

test_that("freedson_vm3() is Sasaki, John and Freedson's VM3 set, as ActiLife names it", {
  x <- c(199, 200, 2452, 2453, 2689, 2690, 6891, 6892, 9642, 9643)
  expect_identical(freedson_vm3(x), sasaki_vm3(x))
  # 2453, the energy equation's switch point, is no class boundary
  expect_identical(as.character(freedson_vm3(c(2452, 2453, 2689, 2690))),
                   c("light", "light", "light", "moderate"))
})

test_that("evenson() applies the Evenson 2008 child cut points", {
  expect_classes(evenson(c(100, 101, 2295, 2296, 4011, 4012)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
})

test_that("puyau() applies the Puyau 2002 child cut points", {
  expect_classes(puyau(c(799, 800, 3199, 3200, 8199, 8200)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
})

test_that("mattocks() starts moderate at 3581 and vigorous at 6130 counts per minute", {
  expect_classes(mattocks(c(100, 101, 3580, 3581, 6129, 6130)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
})

test_that("pate_preschool() is Pate 2006 in counts per minute", {
  expect_classes(pate_preschool(c(799, 800, 1679, 1680, 3367, 3368)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
  # the paper's 420 and 842 counts per 15 s
  expect_identical(as.character(pate_preschool(c(419, 420, 841, 842) * 4)),
                   c("light", "moderate", "moderate", "vigorous"))
})

test_that("butte_preschool() is Butte 2014 for the vertical axis, with no cut by age", {
  expect_classes(butte_preschool(c(239, 240, 2119, 2120, 4449, 4450)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
  expect_identical(names(formals(butte_preschool)), "counts_per_minute")
})

test_that("copeland_older() is Copeland and Esliger 2009: sedentary under 50, MVPA from 1041", {
  # the paper has one MVPA cut, so nothing is vigorous
  expect_classes(copeland_older(c(49, 50, 1040, 1041, 1801, 20000)), LV4,
                 c("sedentary", "light", "light", "moderate", "moderate", "moderate"))
})

test_that("romanzini() gives four ordered classes that never fall as counts rise", {
  x <- seq(0, 12000, by = 10)
  got <- romanzini(x)
  expect_identical(levels(got), LV4)
  expect_identical(any(diff(as.integer(got)) < 0), FALSE)
  expect_identical(as.character(got[c(1, length(x))]), c("sedentary", "vigorous"))
})

test_that("romanzini() reads Romanzini 2014 in counts per 15 s, not per minute", {
  # the paper's vertical axis cuts, 46, 607 and 818 counts per 15 s, times four
  expect_identical(as.character(romanzini(c(46, 47, 606, 607, 817, 818) * 4)),
                   c("sedentary", "light", "light", "moderate", "moderate", "vigorous"))
  expect_identical(as.character(romanzini(c(0, 181, 757, 1112, 2000, 5000))),
                   c("sedentary", "sedentary", "light", "light", "light", "vigorous"))
})

test_that("every cut point keeps NA and calls a negative count sedentary with a warning", {
  sets <- list(troiano = troiano, matthews = matthews, santos_lozano = santos_lozano,
               sasaki_vm3 = sasaki_vm3, freedson_vm3 = freedson_vm3, evenson = evenson,
               puyau = puyau, mattocks = mattocks, pate_preschool = pate_preschool,
               butte_preschool = butte_preschool, romanzini = romanzini,
               copeland_older = copeland_older)
  for (nm in names(sets)) {
    expect_warning(got <- sets[[nm]](c(-5, NA)), paste0("1 negative count values in ", nm, "\\."))
    expect_identical(as.character(got), c("sedentary", NA), info = nm)
  }
})

test_that("custom_cutpoints() opens each named class at its threshold", {
  expect_classes(custom_cutpoints(c(99, 100, 2019, 2020, 5998, 5999, NA),
                                  c(light = 100, moderate = 2020, vigorous = 5999)),
                 LV4, c("sedentary", "light", "light", "moderate", "moderate", "vigorous", NA))
  expect_classes(custom_cutpoints(c(99, 100, 2019, 2020), c(100, 2020), labels = c("low", "mid", "high")),
                 c("low", "mid", "high"), c("low", "mid", "mid", "high"))
  expect_warning(got <- custom_cutpoints(c(-1, 5), c(light = 1)), "in custom_cutpoints")
  expect_identical(as.character(got), c("sedentary", "light"))
  expect_error(custom_cutpoints(1:3, c(100, 200)), "fully named")
  expect_error(custom_cutpoints(1:3, c(a = 100, b = 200), labels = c("x", "y")),
               "length(labels) == length(thresholds) + 1", fixed = TRUE)
})

test_that("custom_cutpoints() keeps each name with its own threshold when they come unsorted", {
  got <- custom_cutpoints(c(50, 150, 2500, 7000), c(vigorous = 6000, light = 100, moderate = 2000))
  expect_classes(got, LV4, LV4)
  # labels given as such run from lowest to highest, whatever the order of the thresholds
  expect_classes(custom_cutpoints(c(50, 150, 2500, 7000), c(6000, 100, 2000),
                                  labels = c("low", "mid", "high", "top")),
                 c("low", "mid", "high", "top"), c("low", "mid", "high", "top"))
})

DIRECT <- list(
  freedson = freedson, troiano = troiano, matthews = matthews,
  santos_lozano_younger = function(x) santos_lozano(x, "younger"),
  santos_lozano_older = function(x) santos_lozano(x, "older"),
  crouter = crouter, sasaki_vm3 = sasaki_vm3, freedson_vm3 = freedson_vm3,
  evenson = evenson, puyau = puyau, mattocks = mattocks, pate_preschool = pate_preschool,
  butte_preschool = butte_preschool,
  romanzini = romanzini, copeland_older = copeland_older, canhr = CANHR.Cutpoints)

test_that("apply_cutpoints() runs the function each listed algorithm names", {
  expect_identical(sort(list_cutpoints()$algorithm, method = "radix"),
                   sort(names(DIRECT), method = "radix"))
  x <- c(0, 50, 101, 250, 800, 1100, 1700, 2000, 2300, 3000, 3300, 4100, 5800, 6200,
         7000, 8300, 9000, 9700, 10500, NA)
  same <- vapply(names(DIRECT), function(a) identical(apply_cutpoints(x, a), DIRECT[[a]](x)),
                 logical(1))
  expect_identical(names(same)[!same], character(0))
})

test_that("apply_cutpoints('auto') picks the set for the age and says which", {
  x <- c(50, 2000, 4500)
  cases <- list(list(NULL, "freedson", "No age provided"), list(4.9, "pate_preschool", "Age < 5"),
                list(5, "evenson", "Age 5-17"), list(17, "evenson", "Age 5-17"),
                list(18, "freedson", "Age 18-64"), list(64, "freedson", "Age 18-64"),
                list(65, "copeland_older", "Age >= 65"))
  for (cs in cases) {
    expect_message(got <- apply_cutpoints(x, "auto", age = cs[[1]]), cs[[3]], fixed = TRUE)
    expect_identical(got, apply_cutpoints(x, cs[[2]]))
  }
})

test_that("apply_cutpoints() refuses an unknown set and passes cv on to crouter()", {
  expect_error(apply_cutpoints(100, "nope"), "Unknown algorithm: nope")
  cv <- c(5, 15)
  got <- apply_cutpoints(c(500, 3000), "crouter", cv = cv)
  expect_identical(got, crouter(c(500, 3000), cv = cv))
  expect_identical(as.character(got), c("light", "vigorous"))
})

test_that("apply_cutpoints() gives Butte's one set whatever the age", {
  x <- c(239, 240, 2119, 2120, 4449, 4450)
  expect_identical(apply_cutpoints(x, "butte_preschool", age = 6), butte_preschool(x))
  expect_identical(apply_cutpoints(x, "butte_preschool"), butte_preschool(x))
})

test_that("apply_cutpoints() treats an epoch length the same way for every algorithm", {
  x <- seq(0, 12000, by = 25)
  algos <- list_cutpoints()$algorithm
  mode <- vapply(algos, function(a) {
    got <- apply_cutpoints(x, a, epoch_seconds = 30)
    if (identical(got, apply_cutpoints(x, a))) "as given"
    else if (identical(got, apply_cutpoints(x * 2, a))) "per minute" else "other"
  }, character(1))
  # data is already per minute, as to_cpm() gives it, so no set rescales it again
  expect_identical(unique(unname(mode)), "as given")
  # 600 counts in 30 s are 1200 counts per minute: light for Freedson
  expect_identical(as.character(apply_cutpoints(to_cpm(600, 30), "freedson", 30)), "light")
  got <- compare_cutpoints(to_cpm(c(30, 600, 1500, 3000), 30), "freedson", epoch_seconds = 30)
  expect_identical(c(got$sedentary_min, got$light_min, got$mvpa_min), c(0.5, 0.5, 1))
})

test_that("list_cutpoints() lists every set with its population, input and source", {
  all <- list_cutpoints()
  expect_identical(names(all), c("algorithm", "category", "input", "reference"))
  expect_identical(nrow(all), 16L)
  expect_identical(anyDuplicated(all$algorithm), 0L)
  expect_identical(sort(unique(all$category), method = "radix"),
                   c("adult", "children", "custom", "older_adult", "triaxial"))
  tri <- list_cutpoints("triaxial")
  expect_identical(tri$algorithm, c("sasaki_vm3", "freedson_vm3", "santos_lozano_younger",
                                    "santos_lozano_older"))
  expect_identical(tri$input, rep("VM CPM", 4))
  expect_identical(list_cutpoints("children")$algorithm,
                   c("evenson", "puyau", "mattocks", "pate_preschool", "butte_preschool",
                     "romanzini"))
  expect_identical(list_cutpoints("older_adult")$reference, "Copeland 2009")
  expect_identical(nrow(list_cutpoints("none")), 0L)
})

test_that("get_cutpoint_thresholds() marks where each listed classifier changes class", {
  bad <- character(0)
  for (a in list_cutpoints()$algorithm) {
    th <- unlist(get_cutpoint_thresholds(a), use.names = FALSE)
    th <- th[is.finite(th)]
    # each set at its defaults
    f <- function(x) apply_cutpoints(x, a)
    if (!identical(as.integer(f(th - 1)), seq_along(th)) ||
        !identical(as.integer(f(th)), seq_along(th) + 1L)) bad <- c(bad, a)
  }
  expect_identical(bad, character(0))
})

test_that("sedentary_time() and light_activity() count the wear minutes of their classes", {
  lv <- matthews(c(50, 50, 300, 800, 800, 3000))   # 2 sedentary, light, 2 lifestyle, moderate
  expect_identical(sedentary_time(lv), 2)
  expect_identical(light_activity(lv), 3)
  expect_identical(light_activity(lv, include_lifestyle = FALSE), 1)
  wear <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  expect_identical(sedentary_time(lv, wear, epoch_seconds = 30), 0.5)
  expect_identical(light_activity(lv, wear, epoch_seconds = 30), 1)
  expect_identical(sedentary_time(c("inactivity", "sedentary", "light", NA)), 2)
  expect_identical(light_activity(c("light", NA, "moderate")), 1)
})

test_that("compare_cutpoints() tabulates the minutes in each class for each algorithm", {
  x <- c(50, 500, 2000, 6000)
  got <- compare_cutpoints(x)
  expect_identical(got$algorithm, c("freedson", "troiano", "evenson"))
  expect_identical(got$sedentary_min, c(1, 1, 1))
  expect_identical(got$light_min, c(1, 2, 2))   # 2000 is moderate only for Freedson
  expect_identical(got$mvpa_min, c(2, 1, 1))
  worn <- compare_cutpoints(x, "freedson", wear_time = c(TRUE, TRUE, FALSE, TRUE))
  expect_identical(c(worn$sedentary_min, worn$light_min, worn$mvpa_min), c(1, 1, 1))
  expect_identical(compare_cutpoints(c(50, 500, 800, 2000, 6000), "matthews")$light_min, 2)
  expect_identical(compare_cutpoints(rep(10, 4), "freedson", epoch_seconds = 30)$sedentary_min, 2)
  expect_warning(got <- compare_cutpoints(x, c("freedson", "nope")), "Failed to apply nope")
  expect_identical(got$algorithm, "freedson")
})
