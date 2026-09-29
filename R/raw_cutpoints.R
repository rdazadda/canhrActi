# The published cut points, as GGIR lists them, parsed from the source of GGIR's CutPoints
# vignette (vignettes/CutPoints.Rmd), so a number here is the number GGIR prints.
#
# The metric is part of the cut point: about half the rows are defined against something
# other than ENMO, so every row carries its metric and applying a row sets acc.metric too.
# A band the study did not define is NA, not zero; part 5 needs three numbers, so the
# caller supplies the missing one. rescaled marks the rows the vignette converted from
# summed to averaged acceleration: Roscoe 2017 from g-seconds by (paper/85.7)*1000 (the
# sample rate corrected from the paper's 87.5), and Buchan 2023, Dillon 2016, Esliger
# 2011, Fraysse 2020, Phillips 2013 and Schaefer 2014 from g-minutes by
# (paper/(rate*epoch))*1000. Sanders 2019 has two ROC criteria, one row each, in variant.
# Buchan 2023 and Dillon 2016 give the same values at 30 and 100 Hz, so one row. Dibben
# 2020 has further cut points excluding aided walking and washing up, not listed.

# R code must be ASCII, so Vaha-Ypya's umlauts go in through sub(): the parser rejects a
# string over 10000 characters that holds a \u escape, and this table is longer
.RAW_CUTPOINT_TABLE <- utils::read.csv(stringsAsFactors = FALSE, strip.white = TRUE, text = sub(
  "Vaha-Ypya", "V\u00e4h\u00e4-Ypy\u00e4", fixed = TRUE, x = "
key                                              ,study               ,group                   ,age                    ,brand      ,location          ,metric,light,moderate,vigorous,variant                     ,rescaled
roscoe2017_pre_enmoa_geneactiv_ndwrist           ,Roscoe 2017         ,Preschoolers            ,4-5 yr                 ,GENEActiv  ,Non-dominant wrist,ENMOa,61.8 ,100.4,     ,                            ,TRUE
roscoe2017_pre_enmoa_geneactiv_dwrist            ,Roscoe 2017         ,Preschoolers            ,4-5 yr                 ,GENEActiv  ,Dominant wrist    ,ENMOa,94.5 ,108.5,     ,                            ,TRUE
phillips2013_child_enmoa_genea_leftwrist         ,Phillips 2013       ,Children and adolescents,8-14 yr                ,GENEA      ,Left wrist        ,ENMOa,87.5 ,250  ,750  ,                            ,TRUE
phillips2013_child_enmoa_genea_rightwrist        ,Phillips 2013       ,Children and adolescents,8-14 yr                ,GENEA      ,Right wrist       ,ENMOa,75   ,275  ,700  ,                            ,TRUE
phillips2013_child_enmoa_genea_hip               ,Phillips 2013       ,Children and adolescents,8-14 yr                ,GENEA      ,Hip               ,ENMOa,37.5 ,212.5,637.5,                            ,TRUE
schaefer2014_child_bfen_geneactiv_ndwrist        ,Schaefer 2014       ,Children and adolescents,6-11 yr                ,GENEActiv  ,Non-dominant wrist,BFEN ,190  ,314  ,998  ,                            ,TRUE
hildebrand2014_child_enmo_actigraph_ndwrist      ,Hildebrand 2014/2016,Children and adolescents,7-11 yr                ,ActiGraph  ,Non-dominant wrist,ENMO ,35.6 ,201.4,707  ,                            ,FALSE
hildebrand2014_child_enmo_geneactiv_ndwrist      ,Hildebrand 2014/2016,Children and adolescents,7-11 yr                ,GENEActiv  ,Non-dominant wrist,ENMO ,56.3 ,191.6,695.8,                            ,FALSE
hildebrand2014_child_enmo_actigraph_hip          ,Hildebrand 2014/2016,Children and adolescents,7-11 yr                ,ActiGraph  ,Hip               ,ENMO ,63.3 ,142.6,464.6,                            ,FALSE
hildebrand2014_child_enmo_geneactiv_hip          ,Hildebrand 2014/2016,Children and adolescents,7-11 yr                ,GENEActiv  ,Hip               ,ENMO ,64.1 ,152.8,514.3,                            ,FALSE
aittasalo2015_child_mad_actigraph_hip            ,Aittasalo 2015      ,Children and adolescents,13-15 yr               ,ActiGraph  ,Hip               ,MAD  ,26.9 ,332  ,558.3,                            ,FALSE
aittasalo2015_child_mad_hookieam_hip             ,Aittasalo 2015      ,Children and adolescents,13-15 yr               ,Hookie AM20,Hip               ,MAD  ,28.7 ,338  ,558.3,                            ,FALSE
esliger2011_adult_enmoa_na_leftwrist             ,Esliger 2011        ,Adults                  ,40-65 yr               ,           ,Left wrist        ,ENMOa,45   ,134  ,377  ,                            ,TRUE
esliger2011_adult_enmoa_na_rightwrist            ,Esliger 2011        ,Adults                  ,40-65 yr               ,           ,Right wrist       ,ENMOa,80   ,92   ,437  ,                            ,TRUE
esliger2011_adult_enmoa_na_waist                 ,Esliger 2011        ,Adults                  ,40-65 yr               ,           ,Waist             ,ENMOa,16   ,46   ,428  ,                            ,TRUE
hildebrand2014_adult_enmo_actigraph_ndwrist      ,Hildebrand 2014/2016,Adults                  ,21-61 yr               ,ActiGraph  ,Non-dominant wrist,ENMO ,44.8 ,100.6,428.8,                            ,FALSE
hildebrand2014_adult_enmo_geneactiv_ndwrist      ,Hildebrand 2014/2016,Adults                  ,21-61 yr               ,GENEActiv  ,Non-dominant wrist,ENMO ,45.8 ,93.2 ,418.3,                            ,FALSE
hildebrand2014_adult_enmo_actigraph_hip          ,Hildebrand 2014/2016,Adults                  ,21-61 yr               ,ActiGraph  ,Hip               ,ENMO ,47.4 ,69.1 ,258.7,                            ,FALSE
hildebrand2014_adult_enmo_geneactiv_hip          ,Hildebrand 2014/2016,Adults                  ,21-61 yr               ,GENEActiv  ,Hip               ,ENMO ,46.9 ,68.7 ,266.8,                            ,FALSE
mielke2023_adult_enmo_geneactiv_ndwrist          ,Mielke 2023         ,Adults                  ,35 (SD=11) yr          ,GENEActiv  ,Non-dominant wrist,ENMO ,36   ,92   ,283  ,                            ,FALSE
mielke2023_adult_enmo_actigraph_ndwrist          ,Mielke 2023         ,Adults                  ,35 (SD=11) yr          ,ActiGraph  ,Non-dominant wrist,ENMO ,25   ,78   ,249  ,                            ,FALSE
mielke2023_adult_enmo_geneactiv_dwrist           ,Mielke 2023         ,Adults                  ,35 (SD=11) yr          ,GENEActiv  ,Dominant wrist    ,ENMO ,30   ,85   ,270  ,                            ,FALSE
mielke2023_adult_enmo_actigraph_dwaist           ,Mielke 2023         ,Adults                  ,35 (SD=11) yr          ,ActiGraph  ,Dominant waist    ,ENMO ,40   ,65   ,190  ,                            ,FALSE
vahaypya2015_adult_mad_hookieam_hip              ,Vaha-Ypya 2015      ,Adults                  ,35 (SD=11) yr          ,Hookie AM20,Hip               ,MAD  ,     ,91   ,414  ,                            ,FALSE
dillon2016_adult_enmoa_geneactiv_ndwrist         ,Dillon 2016         ,Adults                  ,50-69 yr               ,GENEActiv  ,Non-dominant wrist,ENMOa,105.6,174.2,330  ,                            ,TRUE
dillon2016_adult_enmoa_geneactiv_dwrist          ,Dillon 2016         ,Adults                  ,50-69 yr               ,GENEActiv  ,Dominant wrist    ,ENMOa,127.8,187.6,396.4,                            ,TRUE
buchan2023_adult_enmo_activpal_rightthigh        ,Buchan 2023         ,Adults                  ,23 (SD=4) yr           ,activPAL   ,Right thigh       ,ENMO ,26.4 ,     ,     ,                            ,TRUE
buchan2023_adult_mad_activpal_rightthigh         ,Buchan 2023         ,Adults                  ,23 (SD=4) yr           ,activPAL   ,Right thigh       ,MAD  ,30.1 ,     ,     ,                            ,TRUE
sanders2019_older_enmo_geneactiv_ndwrist_youden  ,Sanders 2019        ,Older adults            ,60-86 yr               ,GENEActiv  ,Non-dominant wrist,ENMO ,20   ,32   ,     ,Youden index                ,FALSE
sanders2019_older_enmo_geneactiv_ndwrist_sensspec,Sanders 2019        ,Older adults            ,60-86 yr               ,GENEActiv  ,Non-dominant wrist,ENMO ,57   ,104  ,     ,Sensitivity over specificity,FALSE
sanders2019_older_enmo_actigraph_hip_youden      ,Sanders 2019        ,Older adults            ,60-86 yr               ,ActiGraph  ,Hip               ,ENMO ,6    ,19   ,     ,Youden index                ,FALSE
sanders2019_older_enmo_actigraph_hip_sensspec    ,Sanders 2019        ,Older adults            ,60-86 yr               ,ActiGraph  ,Hip               ,ENMO ,15   ,69   ,     ,Sensitivity over specificity,FALSE
migueles2021_older_enmo_actigraph_ndwrist        ,Migueles 2021       ,Older adults            ,>=70 yr (mean: 78.7 yr),ActiGraph  ,Non-dominant wrist,ENMO ,18   ,60   ,     ,                            ,FALSE
migueles2021_older_enmo_actigraph_dwrist         ,Migueles 2021       ,Older adults            ,>=70 yr (mean: 78.7 yr),ActiGraph  ,Dominant wrist    ,ENMO ,22   ,64   ,     ,                            ,FALSE
migueles2021_older_enmo_actigraph_hip            ,Migueles 2021       ,Older adults            ,>=70 yr (mean: 78.7 yr),ActiGraph  ,Hip               ,ENMO ,7    ,14   ,     ,                            ,FALSE
bammann2021_older_enmo_actigraph_hip             ,Bammann 2021        ,Older adults            ,62.9 (SD=3.6) yr       ,ActiGraph  ,Hip               ,ENMO ,     ,94   ,230  ,                            ,FALSE
bammann2021_older_enmo_actigraph_dwrist          ,Bammann 2021        ,Older adults            ,62.9 (SD=3.6) yr       ,ActiGraph  ,Dominant wrist    ,ENMO ,     ,122  ,234  ,                            ,FALSE
bammann2021_older_enmo_actigraph_ndwrist         ,Bammann 2021        ,Older adults            ,62.9 (SD=3.6) yr       ,ActiGraph  ,Non-dominant wrist,ENMO ,     ,100  ,245  ,                            ,FALSE
bammann2021_older_enmo_actigraph_dankle          ,Bammann 2021        ,Older adults            ,62.9 (SD=3.6) yr       ,ActiGraph  ,Dominant ankle    ,ENMO ,     ,342  ,     ,                            ,FALSE
bammann2021_older_enmo_actigraph_ndankle         ,Bammann 2021        ,Older adults            ,62.9 (SD=3.6) yr       ,ActiGraph  ,Non-dominant ankle,ENMO ,     ,331  ,     ,                            ,FALSE
fraysse2020_older_enmoa_geneactiv_ndwrist        ,Fraysse 2020        ,Older adults            ,>=70 yr (mean: 77 yr)  ,GENEActiv  ,Non-dominant wrist,ENMOa,42.5 ,98   ,     ,                            ,TRUE
fraysse2020_older_enmoa_geneactiv_dwrist         ,Fraysse 2020        ,Older adults            ,>=70 yr (mean: 77 yr)  ,GENEActiv  ,Dominant wrist    ,ENMOa,62.5 ,92.5 ,     ,                            ,TRUE
dibben2020_older_enmoa_geneactiv_rightwrist      ,Dibben 2020         ,Older adults            ,70.7 (SD=14.1) yr      ,GENEActiv  ,Right wrist       ,ENMOa,18.6 ,45.5 ,     ,                            ,FALSE
dibben2020_older_mad_geneactiv_rightwrist        ,Dibben 2020         ,Older adults            ,70.7 (SD=14.1) yr      ,GENEActiv  ,Right wrist       ,MAD  ,18.3 ,26.2 ,     ,                            ,FALSE
dibben2020_older_enmoa_geneactiv_leftwrist       ,Dibben 2020         ,Older adults            ,70.7 (SD=14.1) yr      ,GENEActiv  ,Left wrist        ,ENMOa,16.7 ,43.6 ,     ,                            ,FALSE
dibben2020_older_mad_geneactiv_leftwrist         ,Dibben 2020         ,Older adults            ,70.7 (SD=14.1) yr      ,GENEActiv  ,Left wrist        ,MAD  ,18.7 ,22.8 ,     ,                            ,FALSE
dibben2020_older_enmoa_geneactiv_hip             ,Dibben 2020         ,Older adults            ,70.7 (SD=14.1) yr      ,GENEActiv  ,Hip               ,ENMOa,7.6  ,40.6 ,     ,                            ,FALSE
dibben2020_older_mad_geneactiv_hip               ,Dibben 2020         ,Older adults            ,70.7 (SD=14.1) yr      ,GENEActiv  ,Hip               ,MAD  ,1    ,2.4  ,     ,                            ,FALSE
"))

# what each metric needs switched on in part 1, and its part 5 name; Schaefer's filter
# band is part of the BFEN definition
.RAW_CUTPOINT_FLAGS <- list(
  ENMO   = list(do.enmo = TRUE, acc.metric = "ENMO"),
  ENMOa  = list(do.enmoa = TRUE, do.enmo = FALSE, acc.metric = "ENMOa"),
  LFENMO = list(do.lfenmo = TRUE, acc.metric = "LFENMO"),
  MAD    = list(do.mad = TRUE, do.enmo = FALSE, acc.metric = "MAD"),
  BFEN   = list(do.bfen = TRUE, lb = 0.2, hb = 15, do.enmo = FALSE, acc.metric = "BFEN")
)

#' Published Cut Points for Raw Acceleration
#'
#' The cut points GGIR's CutPoints vignette lists, as a data frame, so a caller
#' can offer them by name instead of asking a reader to type three numbers.
#'
#' Each row is one published set: a study, a population, a wear location, a
#' device and an acceleration metric, with the light, moderate and vigorous
#' thresholds in milli-g. A threshold the study did not define is \code{NA}.
#'
#' @param metric Keep only rows defined against this metric, for example
#'   \code{"ENMO"}. NULL keeps every row.
#' @param group Keep only rows in this population group: Preschoolers,
#'   Children and adolescents, Adults, Older adults. NULL keeps every row.
#' @param available Character vector of the metrics a recording actually
#'   carries. When given, an \code{available} column is added saying whether
#'   the row can be applied without reading the file again.
#'
#' @return A data frame with one row per published set and the columns
#'   \code{key}, \code{study}, \code{group}, \code{age}, \code{brand},
#'   \code{location}, \code{metric}, \code{light}, \code{moderate},
#'   \code{vigorous}, \code{variant} and \code{rescaled}.
#'
#' @seealso \code{\link{raw.cutpoint}} to turn one row into parameters.
#' @examples
#' nrow(raw.cutpoints())
#' raw.cutpoints(group = "Older adults", metric = "ENMO")
#' @export
raw.cutpoints <- function(metric = NULL, group = NULL, available = NULL) {
  d <- .RAW_CUTPOINT_TABLE
  if (!is.null(metric)) d <- d[d$metric %in% metric, , drop = FALSE]
  if (!is.null(group))  d <- d[d$group %in% group, , drop = FALSE]
  if (!is.null(available)) d$available <- d$metric %in% available
  rownames(d) <- NULL
  d
}

#' Turn a Published Cut Point Into Part 5 Parameters
#'
#' Looks a row up by key and returns the arguments \code{\link{raw.timeuse}}
#' needs: the three thresholds and the metric to read them against, plus the
#' part 1 \code{do.} flags that produce that metric.
#'
#' A study that defined only two of the three bands leaves the third
#' \code{NA}, and part 5 will not run on an NA. Pass \code{fill} to supply the
#' missing band; nothing is filled in silently, because a borrowed threshold is
#' not part of the published set.
#'
#' @param key A key from \code{\link{raw.cutpoints}}.
#' @param fill Named numeric supplying bands the study did not define, for
#'   example \code{c(vigorous = 400)}. Only bands that are \code{NA} are taken
#'   from it.
#'
#' @return A list with \code{threshold.lig}, \code{threshold.mod},
#'   \code{threshold.vig}, \code{acc.metric}, the part 1 flags in
#'   \code{metric_flags}, the row itself in \code{row}, and \code{filled},
#'   naming any band that came from \code{fill} rather than from the study.
#' @examples
#' raw.cutpoint("hildebrand2014_adult_enmo_actigraph_ndwrist")
#' @export
raw.cutpoint <- function(key, fill = NULL) {
  d <- .RAW_CUTPOINT_TABLE
  i <- match(key, d$key)
  if (is.na(i)) {
    stop("unknown cut-point key: ", key,
         ". See raw.cutpoints()$key for the ", nrow(d), " published sets.",
         call. = FALSE)
  }
  r <- d[i, , drop = FALSE]
  band <- c(light = r$light, moderate = r$moderate, vigorous = r$vigorous)
  filled <- character(0)
  for (b in names(band)) {
    if (is.na(band[[b]]) && !is.null(fill) && b %in% names(fill)) {
      band[[b]] <- as.numeric(fill[[b]])
      filled <- c(filled, b)
    }
  }
  flags <- .RAW_CUTPOINT_FLAGS[[r$metric]]
  if (is.null(flags)) {
    stop("no part 1 flags known for metric ", r$metric, call. = FALSE)
  }
  list(threshold.lig = unname(band[["light"]]),
       threshold.mod = unname(band[["moderate"]]),
       threshold.vig = unname(band[["vigorous"]]),
       acc.metric = r$metric,
       metric_flags = flags,
       row = r,
       filled = filled)
}

#' One Line Naming a Published Cut Point
#'
#' The label the dashboard shows, and the same string in a caption or an
#' export, so a reader of a number can tell which study produced it.
#'
#' @param key A key from \code{\link{raw.cutpoints}}, or a row of it.
#' @param long TRUE to include the age range and the metric.
#' @return A character string.
#' @examples
#' raw.cutpoint.label("migueles2021_older_enmo_actigraph_ndwrist")
#' @export
raw.cutpoint.label <- function(key, long = FALSE) {
  r <- if (is.data.frame(key)) key else .RAW_CUTPOINT_TABLE[match(key, .RAW_CUTPOINT_TABLE$key), , drop = FALSE]
  if (nrow(r) == 0 || is.na(r$key[1])) return("")
  bits <- c(r$study[1],
            if (nzchar(r$variant[1])) r$variant[1],
            if (nzchar(r$brand[1])) r$brand[1],
            r$location[1],
            if (long) r$age[1],
            if (long) r$metric[1])
  paste(bits, collapse = " \u00b7 ")
}

#' The Metrics a Set of Cut Points Needs From Part 1
#'
#' Given the keys a caller wants to offer, the \code{do.} flags a recording
#' must have been read with for those keys to be applicable without reading
#' the file again. ENMOa and MAD cost nothing extra to read; BFEN adds a
#' filter bank and about a quarter to the read time, so the dashboard reads
#' the first three always and treats Schaefer's BFEN row as a re-read.
#'
#' @param keys Cut-point keys. NULL means every published set.
#' @return A named list of part 1 parameters, suitable for
#'   \code{\link{raw.params}}.
#' @examples
#' raw.cutpoint.metrics(c("hildebrand2014_adult_enmo_actigraph_ndwrist",
#'                        "aittasalo2015_child_mad_actigraph_hip"))
#' @export
raw.cutpoint.metrics <- function(keys = NULL) {
  d <- .RAW_CUTPOINT_TABLE
  if (!is.null(keys)) d <- d[d$key %in% keys, , drop = FALSE]
  out <- list()
  for (m in unique(d$metric)) {
    f <- .RAW_CUTPOINT_FLAGS[[m]]
    for (nm in setdiff(names(f), "acc.metric")) {
      # do.enmo = FALSE in a row only means "read this other metric instead", so keep
      # every do. that any metric switches on
      if (isTRUE(f[[nm]]) || is.numeric(f[[nm]])) out[[nm]] <- f[[nm]]
    }
  }
  out
}
