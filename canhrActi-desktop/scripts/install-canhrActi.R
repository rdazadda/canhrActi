# Installs canhrActi and its dashboard dependencies into the bundled
# R-Portable library. Reads inst/shiny/canhrActi_dashboard/manifest.json
# from the canhrActi GitHub repo so the desktop install matches the
# Posit deployment.

lib <- normalizePath(file.path(R.home(), "library"), mustWork = TRUE)
.libPaths(c(lib, .libPaths()))
cat("Library path:", lib, "\n")
cat("R version:   ", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("Platform:    ", R.version$platform, "\n\n")

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", lib = lib)
}

manifest_url <- "https://raw.githubusercontent.com/rdazadda/canhrActi/main/inst/shiny/canhrActi_dashboard/manifest.json"
cat("Fetching manifest from", manifest_url, "\n")
manifest <- tryCatch(
  jsonlite::fromJSON(manifest_url, simplifyVector = FALSE),
  error = function(e) {
    stop(
      "Could not fetch manifest from GitHub.\n",
      "  Error: ", conditionMessage(e), "\n",
      "  Check internet access and that the URL is reachable."
    )
  }
)

target_r <- manifest$platform
bundled_r <- paste(R.version$major, R.version$minor, sep = ".")
cat("Manifest pins R", target_r, "- bundled R is", bundled_r, "\n")
if (substr(bundled_r, 1, 3) != substr(target_r, 1, 3)) {
  warning(
    "Bundled R (", bundled_r, ") and manifest R (", target_r, ") differ.\n",
    "Package binaries may not load. Re-run with the matching R version."
  )
}

# Posit PPM URL varies by platform. Windows and macOS share one CRAN-style
# endpoint; Linux needs a distro-specific path to get pre-built binaries.
ppm_url <- if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Linux") {
  "https://packagemanager.posit.co/cran/__linux__/jammy/2025-12-30"
} else {
  "https://packagemanager.posit.co/cran/2025-12-30"
}
cat("Using package source:", ppm_url, "\n")
options(repos = c(PPM = ppm_url, CRAN = "https://cloud.r-project.org"))

if (Sys.info()[["sysname"]] == "Darwin") {
  if (file.exists("/opt/gfortran/bin/gfortran")) {
    cat("gfortran detected at /opt/gfortran/bin/gfortran\n")
  } else {
    warning("gfortran not found at /opt/gfortran; source compilation of ",
            "Fortran-using packages will fail.")
  }
}

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://r-lib.r-universe.dev", lib = lib)
}

pkgs <- manifest$packages
specs <- character(0)
skipped <- character(0)
for (name in names(pkgs)) {
  if (name == "canhrActi") next
  ver <- pkgs[[name]]$description$Version
  if (is.null(ver) || !nzchar(ver)) {
    skipped <- c(skipped, name)
    next
  }
  specs <- c(specs, sprintf("%s@%s", name, ver))
}

cat("Manifest declares", length(specs), "pinned packages to install.\n")
if (length(skipped) > 0) {
  cat("Skipped (no version):", paste(skipped, collapse = ", "), "\n")
}

pak::pkg_install(specs, ask = FALSE, upgrade = FALSE)

if ("canhrActi" %in% rownames(installed.packages(lib.loc = lib))) {
  cat("Removing cached canhrActi so the next install always reflects current source.\n")
  remove.packages("canhrActi", lib = lib)
}

# Prefer the local checkout (sibling of canhrActi-desktop). Falls back to
# GitHub only when this script runs detached from the source tree.
# upgrade = FALSE + dependencies = FALSE keeps the pinned deps installed
# above intact - critical on macOS arm64 where the PPM snapshot may not
# carry binaries for newer dep versions and source builds will fail.
local_pkg_dir <- tryCatch(
  normalizePath(file.path(getwd(), ".."), mustWork = TRUE),
  error = function(e) NA_character_
)
if (!is.na(local_pkg_dir) && file.exists(file.path(local_pkg_dir, "DESCRIPTION"))) {
  cat("\nInstalling canhrActi from LOCAL source:", local_pkg_dir, "\n")
  pak::pkg_install(paste0("local::", local_pkg_dir),
                   lib = lib, ask = FALSE, upgrade = FALSE, dependencies = FALSE)
} else {
  cat("\nInstalling canhrActi from rdazadda/canhrActi on GitHub\n")
  pak::pkg_install("github::rdazadda/canhrActi",
                   lib = lib, ask = FALSE, upgrade = FALSE, dependencies = FALSE)
}

suppressPackageStartupMessages(library(canhrActi))
cat("\ncanhrActi version:", as.character(utils::packageVersion("canhrActi")), "\n")
dashboard_dir <- system.file("shiny", "canhrActi_dashboard", package = "canhrActi")
cat("Dashboard located at:", dashboard_dir, "\n")
if (!dir.exists(dashboard_dir)) {
  stop("Dashboard directory missing - canhrActi install incomplete.")
}

cat("\nDone. Run `npm start` to launch the desktop app.\n")
