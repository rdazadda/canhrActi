# Installs canhrActi and its dashboard dependencies into the bundled
# R-Portable library, at the versions in this checkout's dashboard manifest.json
# plus the run-time packages below that the manifest does not list, with GGIR
# and GGIRread replaced by the versions in ggir_pins.

lib <- normalizePath(file.path(R.home(), "library"), mustWork = TRUE)
.libPaths(lib)
cat("Library path:", lib, "\n")
cat("R version:   ", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("Platform:    ", R.version$platform, "\n\n")

sysname <- Sys.info()[["sysname"]]

# One Posit snapshot everywhere; Linux takes the self-contained manylinux binaries.
snapshot <- "2025-12-30"
ppm_url <- if (sysname == "Linux") {
  sprintf("https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/%s", snapshot)
} else {
  sprintf("https://packagemanager.posit.co/cran/%s", snapshot)
}
cat("Using package source:", ppm_url, "\n")
options(repos = c(PPM = ppm_url, CRAN = "https://cloud.r-project.org"))
if (sysname == "Linux") {
  # PPM serves Linux binaries only to a user agent that names the R version.
  options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
}
bin_type <- if (sysname == "Linux") "source" else "binary"

# From PPM, not CRAN: CRAN's macOS binary links /Library/Frameworks/R.framework.
if (!requireNamespace("jsonlite", lib.loc = lib, quietly = TRUE)) {
  install.packages("jsonlite", lib = lib, repos = ppm_url, type = bin_type)
}

# Prefer the local checkout (sibling of canhrActi-desktop); GitHub only when detached.
local_pkg_dir <- tryCatch(
  normalizePath(file.path(getwd(), ".."), mustWork = TRUE),
  error = function(e) NA_character_
)
from_checkout <- !is.na(local_pkg_dir) && file.exists(file.path(local_pkg_dir, "DESCRIPTION"))
manifest_src <- if (from_checkout) {
  file.path(local_pkg_dir, "inst", "shiny", "canhrActi_dashboard", "manifest.json")
} else {
  "https://raw.githubusercontent.com/rdazadda/canhrActi/main/inst/shiny/canhrActi_dashboard/manifest.json"
}
cat("Reading manifest from", manifest_src, "\n")
if (from_checkout) cat("Manifest md5:", unname(tools::md5sum(manifest_src)), "\n")
manifest <- tryCatch(
  jsonlite::fromJSON(manifest_src, simplifyVector = FALSE),
  error = function(e) {
    stop(
      "Could not read the manifest.\n",
      "  Error: ", conditionMessage(e), "\n",
      "  Source: ", manifest_src
    )
  }
)
# pak may replace jsonlite, which Windows refuses while its DLL is loaded.
try(unloadNamespace("jsonlite"), silent = TRUE)

target_r <- manifest$platform
bundled_r <- paste(R.version$major, R.version$minor, sep = ".")
cat("Manifest pins R", target_r, "- bundled R is", bundled_r, "\n")
if (substr(bundled_r, 1, 3) != substr(target_r, 1, 3)) {
  warning(
    "Bundled R (", bundled_r, ") and manifest R (", target_r, ") differ.\n",
    "Package binaries may not load. Re-run with the matching R version."
  )
}

if (sysname == "Darwin") {
  if (file.exists("/opt/gfortran/bin/gfortran")) {
    cat("gfortran detected at /opt/gfortran/bin/gfortran\n")
  } else {
    warning("gfortran not found at /opt/gfortran; source compilation of ",
            "Fortran-using packages will fail.")
  }
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

# Loaded at run time but missing from the manifest: future runs the raw reads and
# .gt3x conversions; pdftools, plotly, ggrepel and patchwork back optional features.
runtime_extras <- c(
  "future@1.68.0", "globals@0.18.0", "listenv@0.10.0", "parallelly@1.46.0",
  "pdftools@3.6.0", "qpdf@1.4.1", "askpass@1.2.1", "sys@3.4.3", "curl@7.0.0",
  "plotly@4.11.0", "httr@1.4.7", "openssl@2.3.4",
  "ggrepel@0.9.6", "patchwork@1.3.2"
)
extra_names <- sub("@.*", "", runtime_extras)
specs <- c(specs, runtime_extras[!extra_names %in% names(pkgs)])
cat("Installing", length(specs), "packages in total.\n")

# The raw pages' files must match GGIR 3.3.6, which writes them; the snapshot above predates it.
ggir_pins <- c(GGIR = "3.3-6", GGIRread = "1.0.8")
ggir_snapshot <- "2026-06-01"

spec_name <- sub("@.*", "", specs)
spec_ver <- sub(".*@", "", specs)
installed_ver <- function() {
  ip <- installed.packages(lib.loc = unique(c(lib, .Library)))
  setNames(ip[, "Version"], rownames(ip))
}

# pak would build an older pin from CRAN source against this machine's libraries;
# PPM keeps manylinux binaries of older versions under Archive. pak reinstalls
# whatever it is asked for by name, so these leave its list and stay as installed.
from_archive <- character(0)
if (sysname == "Linux") {
  current <- available.packages(repos = ppm_url, filters = list())[, "Version"]
  have <- installed_ver()
  older <- which(spec_name %in% names(current) & current[spec_name] != spec_ver)
  for (i in older) {
    p <- spec_name[i]
    v <- spec_ver[i]
    # Already there from a cached bundle: keep it off pak's list too.
    if (p %in% names(have) && have[[p]] == v) {
      from_archive <- c(from_archive, p)
      next
    }
    dest <- file.path(tempdir(), sprintf("%s_%s.tar.gz", p, v))
    url <- sprintf("%s/src/contrib/Archive/%s/%s_%s.tar.gz", ppm_url, p, p, v)
    ok <- tryCatch(utils::download.file(url, dest, mode = "wb", quiet = TRUE) == 0,
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) next
    desc <- utils::untar(dest, files = file.path(p, "DESCRIPTION"), exdir = tempdir())
    if (desc != 0) next
    built <- read.dcf(file.path(tempdir(), p, "DESCRIPTION"), fields = "Built")[1, 1]
    if (is.na(built)) next
    cat("Installing the archived binary", p, v, "\n")
    install.packages(dest, repos = NULL, type = "source", lib = lib,
                     INSTALL_opts = "--no-test-load")
    from_archive <- c(from_archive, p)
  }
}

# pak lives outside the bundle. Its stable channel serves one version, so a new one stops here.
pak_version <- "0.11.1"
if (dir.exists(file.path(lib, "pak"))) {
  cat("Removing pak from the bundled library.\n")
  remove.packages("pak", lib = lib)
}
pak_lib <- file.path(tempdir(), "pak-lib")
dir.create(pak_lib, showWarnings = FALSE)
pak_repo <- sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s",
                    .Platform$pkgType, R.Version()$os, R.Version()$arch)
install.packages("pak", lib = pak_lib, repos = pak_repo, type = .Platform$pkgType)
got_pak <- tryCatch(as.character(utils::packageVersion("pak", lib.loc = pak_lib)),
                    error = function(e) "none")
if (got_pak != pak_version) {
  stop("The pak stable channel gave version ", got_pak, ", not ", pak_version,
       ". Read pak's NEWS, then update pak_version in this script.")
}
.libPaths(c(lib, pak_lib))

# The manylinux binaries need none of this machine's system libraries.
if (sysname == "Linux") Sys.setenv(PKG_SYSREQS = "false")

pak::pkg_install(specs[!spec_name %in% from_archive], lib = lib, ask = FALSE, upgrade = FALSE)

# Replaces the manifest's GGIR after the main install, so nothing else resolves differently.
# PPM has binaries of both there, except GGIR on macOS, which installs from source as before.
ggir_url <- sub(snapshot, ggir_snapshot, ppm_url, fixed = TRUE)
cat("\nInstalling", paste(names(ggir_pins), ggir_pins, collapse = ", "), "from", ggir_url, "\n")
old_repos <- options(repos = c(PPM = ggir_url, CRAN = "https://cloud.r-project.org"))
pak::pkg_install(sprintf("%s@%s", names(ggir_pins), ggir_pins), lib = lib, ask = FALSE,
                 upgrade = FALSE, dependencies = FALSE)
options(old_repos)
got_ggir <- vapply(names(ggir_pins), function(p) {
  tryCatch(utils::packageDescription(p, lib.loc = lib)$Version, error = function(e) "none")
}, "")
if (!identical(unname(got_ggir), unname(ggir_pins))) {
  stop("Wanted ", paste(names(ggir_pins), ggir_pins, collapse = ", "),
       " but the library has ", paste(names(ggir_pins), got_ggir, collapse = ", "))
}
spec_ver[match(names(ggir_pins), spec_name)] <- ggir_pins

if ("canhrActi" %in% rownames(installed.packages(lib.loc = lib))) {
  cat("Removing cached canhrActi so the next install always reflects current source.\n")
  remove.packages("canhrActi", lib = lib)
}

# upgrade = FALSE + dependencies = FALSE keeps the pinned deps installed
# above intact - critical on macOS arm64 where the PPM snapshot may not
# carry binaries for newer dep versions and source builds will fail.
if (from_checkout) {
  cat("\nInstalling canhrActi from LOCAL source:", local_pkg_dir, "\n")
  # Base R install bypasses pak's local resolver (which fails on macos-14
  # GHA runners - r-lib/pak#853). Deps were already pinned-installed above,
  # so dependencies = FALSE preserves them.
  install.packages(local_pkg_dir, repos = NULL, type = "source",
                   lib = lib, INSTALL_opts = "--no-multiarch",
                   dependencies = FALSE)
  if (!"canhrActi" %in% rownames(installed.packages(lib.loc = lib))) {
    stop("install.packages failed to install canhrActi from ", local_pkg_dir)
  }
} else {
  cat("\nInstalling canhrActi from rdazadda/canhrActi on GitHub\n")
  pak::pkg_install("github::rdazadda/canhrActi",
                   lib = lib, ask = FALSE, upgrade = FALSE, dependencies = FALSE)
}

have <- installed_ver()
missing_pkgs <- setdiff(spec_name, names(have))
if (length(missing_pkgs) > 0) {
  stop("Not installed: ", paste(missing_pkgs, collapse = ", "))
}
off_pin <- spec_name[have[spec_name] != spec_ver]
if (length(off_pin) > 0) {
  cat("Installed at a version other than the pin:",
      paste0(off_pin, " ", have[off_pin], collapse = ", "), "\n")
}

suppressPackageStartupMessages(library(canhrActi))
cat("\ncanhrActi version:", as.character(utils::packageVersion("canhrActi")), "\n")

needed_exports <- c("plot_periodogram", "plot_extended_cosinor", "plot_dfa")
missing_exports <- setdiff(needed_exports, getNamespaceExports("canhrActi"))
if (length(missing_exports) > 0) {
  stop("canhrActi missing expected exports: ", paste(missing_exports, collapse = ", "))
}

dashboard_dir <- system.file("shiny", "canhrActi_dashboard", package = "canhrActi")
cat("Dashboard located at:", dashboard_dir, "\n")
if (!dir.exists(dashboard_dir)) {
  stop("Dashboard directory missing - canhrActi install incomplete.")
}

cat("\nDone. Run `npm start` to launch the desktop app.\n")
