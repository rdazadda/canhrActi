

detach("package:canhrActi", unload=TRUE)
library(canhrActi)


data.dir <- "agd.files"
agd.files <- list.files(data.dir, pattern = ".agd$", full.names = TRUE)

agd.data <- read.agd(agd.files[1])
counts.data <- agd.counts(agd.data)
print(head(counts.data, 20))

r1 <- canhrActi(agd.files[1],
                wear_time_algorithm = "choi",
                intensity_algorithm = "freedson1998")
print(r1$overall_summary)
print(r1$intensity_summary)
print(r1$daily_summary)

export_canhrActi(r1, output_dir = "r1_exports", prefix = "r1")
print(read.csv("r1_exports/r1_Summary.csv"))
print(read.csv("r1_exports/r1_DailyDetailed.csv"))
print(head(read.csv("r1_exports/r1_HourlyDetailed.csv")))

r2 <- canhrActi(agd.files[1],
                wear_time_algorithm = "troiano",
                intensity_algorithm = "freedson1998")
print(r2$overall_summary)
print(r2$intensity_summary)
print(r2$daily_summary)

export_canhrActi(r2, output_dir = "r2_exports", prefix = "r2")
print(read.csv("r2_exports/r2_Summary.csv"))
print(read.csv("r2_exports/r2_DailyDetailed.csv"))
print(head(read.csv("r2_exports/r2_HourlyDetailed.csv")))

r3 <- canhrActi(agd.files[1],
                wear_time_algorithm = "CANHR2025",
                intensity_algorithm = "CANHR")
print(r3$overall_summary)
print(r3$intensity_summary)
print(r3$daily_summary)

export_canhrActi(r3, output_dir = "r3_exports", prefix = "r3")
print(read.csv("r3_exports/r3_Summary.csv"))
print(read.csv("r3_exports/r3_DailyDetailed.csv"))
print(head(read.csv("r3_exports/r3_HourlyDetailed.csv")))

r4 <- canhrActi(agd.files[1],
                wear_time_algorithm = "choi",
                intensity_algorithm = "freedson1998",
                axis_to_analyze = "vector_magnitude")
print(r4$overall_summary)
print(r4$intensity_summary)

r5 <- canhrActi(agd.files[1],
                wear_time_algorithm = "choi",
                intensity_algorithm = "freedson1998",
                min_wear_hours = 8)
print(r5$overall_summary)
print(r5$valid_days)

r6 <- canhrActi(agd.files[1],
                wear_time_algorithm = "choi",
                intensity_algorithm = "CANHR")
print(r6$overall_summary)
print(r6$intensity_summary)

r7 <- canhrActi(agd.files[1],
                wear_time_algorithm = "troiano",
                intensity_algorithm = "CANHR")
print(r7$overall_summary)
print(r7$intensity_summary)

r8 <- canhrActi(agd.files[1],
                wear_time_algorithm = "CANHR2025",
                intensity_algorithm = "freedson1998")
print(r8$overall_summary)
print(r8$intensity_summary)

batch <- canhrActi(data.dir,
                   wear_time_algorithm = "choi",
                   intensity_algorithm = "freedson1998")
print(batch$summary)
