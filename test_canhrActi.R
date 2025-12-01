if ("package:canhrActi" %in% search()) detach("package:canhrActi", unload = TRUE)
library(devtools)
load_all()

data.dir <- "agd.files"
agd.files <- list.files(data.dir, pattern = "\\.agd$", full.names = TRUE)
export.dir <- "test_exports"

# Basic AGD Reading
test.file <- agd.files[1]
agd.data <- read.agd(test.file)
counts.data <- agd.counts(agd.data)
print(head(counts.data, 5))

# Single File - Full 24h
r1 <- canhrActi(test.file, wear_time_algorithm = "choi", intensity_algorithm = "freedson1998",
                calculate_mets = TRUE, calculate_fragmentation = TRUE, calculate_circadian = TRUE)
print(r1$overall_summary)
print(r1$fragmentation)
print(r1$circadian)

# Single File - Wake Only
r2 <- canhrActi(test.file, wear_time_algorithm = "choi", intensity_algorithm = "freedson1998",
                analysis_mode = "wake.only", sleep_algorithm = "cole.kripke",
                calculate_mets = TRUE, calculate_fragmentation = TRUE, calculate_circadian = TRUE)
print(r2$overall_summary)

# Batch - 24h
batch1 <- canhrActi.batch(data.dir, wear_time_algorithm = "choi", intensity_algorithm = "freedson1998",
                          calculate_fragmentation = TRUE, calculate_circadian = TRUE, export = FALSE)
print(batch1$summary)

# Batch - Wake Only
batch2 <- canhrActi.batch(data.dir, wear_time_algorithm = "choi", intensity_algorithm = "freedson1998",
                          analysis_mode = "wake.only", sleep_algorithm = "cole.kripke",
                          calculate_fragmentation = TRUE, calculate_circadian = TRUE, export = FALSE)
print(batch2$summary)

# METs Algorithms
test.counts <- data.frame(axis1 = c(500, 1000, 2000, 3000, 5000),
                          axis2 = c(400, 800, 1600, 2400, 4000),
                          axis3 = c(300, 600, 1200, 1800, 3000))
for (alg in c("freedson.vm3", "freedson.adult", "hendelman.adult", "swartz", "yngve.overground")) {
  print(paste(alg, ":", paste(round(calculate.mets(test.counts, algorithm = alg), 2), collapse = ", ")))
}

# Plots
plot_activity_profile(r1)
plot_daily_summary(r1)
plot_intensity(r1)
plot_intensity_distribution(r1)
plot_mvpa_bouts(r1)
plot_wear_time(r1)
if (!is.null(r1$circadian)) plot(r1$circadian)

# Standalone Functions

circ <- circadian.rhythm(counts = r1$epoch_data$axis1,
                         timestamps = r1$epoch_data$timestamp,
                         wear_time = r1$epoch_data$wear_time)
print(circ)

# Cut Points Comparison
sample.cpm <- c(50, 100, 500, 1000, 2000, 3000, 5000, 7000, 10000)
print(data.frame(CPM = sample.cpm, Freedson = as.character(freedson(sample.cpm)),
                 CANHR = as.character(CANHR.Cutpoints(sample.cpm))))

# Export
if (!dir.exists(export.dir)) dir.create(export.dir, recursive = TRUE)
export_canhrActi(r1, output_dir = export.dir, prefix = "test_24h")
export_canhrActi(batch1, output_dir = export.dir, prefix = "test_batch")

# Utility Functions
sr <- sample.rate(counts.data$timestamp)
filtered <- filter.time.range(counts.data, start.time = counts.data$timestamp[100],
                              end.time = counts.data$timestamp[200])
vd <- valid.days(counts.data$timestamp, wear.choi(counts.data$axis1), min.wear.hours = 10)
print(vd)

# Sleep Detection
sleep <- canhrActi.sleep(test.file, sleep_algorithm = "cole.kripke")
print(sleep)
print(sleep$summary)
print(sleep$results[[1]]$sleep_periods)
plot_sleep(sleep)
