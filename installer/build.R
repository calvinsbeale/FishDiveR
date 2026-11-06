# Builds the standalone FishDiveR Windows app.
#
# Makes an app folder holding a copy of this R installation, the packages
# FishDiveR and its app need, FishDiveR itself (installed from this source
# folder), the launcher and the icon. If Inno Setup 6 is installed, it then
# builds the installer, FishDiveR-<version>-setup.exe, from that folder.
#
# Run from the package's source folder, with the R version to bundle:
#   Rscript installer/build.R [build folder]
# The build folder defaults to %LOCALAPPDATA%/FishDiveR-build, outside the
# source folder, as the app folder is several hundred MB.

if (.Platform$OS.type != "windows") stop("The standalone app is built on Windows.")
if (!file.exists("DESCRIPTION") || read.dcf("DESCRIPTION", "Package")[1, 1] != "FishDiveR") {
  stop("Run build.R from the FishDiveR source folder.")
}
pkg_dir <- normalizePath(".", winslash = "/")
version <- read.dcf("DESCRIPTION", "Version")[1, 1]

args <- commandArgs(trailingOnly = TRUE)
build_dir <- if (length(args) > 0) args[1] else file.path(Sys.getenv("LOCALAPPDATA"), "FishDiveR-build")
dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
build_dir <- normalizePath(build_dir, winslash = "/")
app_dir <- file.path(build_dir, "FishDiveR")
unlink(app_dir, recursive = TRUE)
dir.create(app_dir)
message("Building FishDiveR ", version, " in ", app_dir)

# 1. Copy this R installation, without the manuals, tests, C headers or R's own
# uninstaller (the app has its own)
message("Copying R ", getRversion(), " from ", R.home())
r_dir <- file.path(app_dir, "R")
dir.create(r_dir)
copied <- file.copy(list.files(R.home(), full.names = TRUE), r_dir, recursive = TRUE)
if (!all(copied)) stop("Could not copy R from ", R.home())
unlink(file.path(r_dir, c(file.path("doc", "manual"), "tests", "include")), recursive = TRUE)
unlink(list.files(r_dir, pattern = "^unins[0-9]+\\.(exe|dat|msg)$", full.names = TRUE))
lib <- file.path(r_dir, "library")

# 2. Copy the packages FishDiveR and its app need to run, at the versions this R
# uses. Packages only needed to compile others (LinkingTo) are left out.
split_field <- function(field) {
  value <- read.dcf("DESCRIPTION", field)[1, 1]
  trimws(sub("\\(.*\\)", "", strsplit(value, ",")[[1]]))
}
direct <- c(split_field("Imports"), "shiny", "bslib", "DT", "later")
installed <- utils::installed.packages()
base_pkgs <- rownames(installed)[installed[, "Priority"] %in% "base"]
needed <- unique(c(direct, unlist(tools::package_dependencies(
  direct,
  db = installed, which = c("Depends", "Imports"), recursive = TRUE
))))
needed <- setdiff(needed, c(base_pkgs, "R"))
missing_pkgs <- setdiff(needed, rownames(installed))
if (length(missing_pkgs) > 0) {
  stop("Install these packages first: ", paste(missing_pkgs, collapse = ", "))
}
message("Copying ", length(needed), " packages")
for (pkg in needed) {
  # The first library listed is the one this R loads the package from
  from <- file.path(installed[pkg, "LibPath"], pkg)
  if (normalizePath(dirname(from), winslash = "/") == normalizePath(.Library, winslash = "/")) next
  unlink(file.path(lib, pkg), recursive = TRUE)
  if (!file.copy(from, lib, recursive = TRUE)) stop("Could not copy package ", pkg)
}

# 3. Install FishDiveR from this source folder into the bundled R
message("Installing FishDiveR ", version)
status <- system2(
  file.path(r_dir, "bin", "R.exe"),
  c("CMD", "INSTALL", "--no-multiarch", paste0("--library=", shQuote(lib)), shQuote(pkg_dir))
)
if (status != 0) stop("Installing FishDiveR failed.")

# Package vignettes, tests and C/C++ headers aren't needed by the app
for (pkg_path in list.dirs(lib, recursive = FALSE)) {
  unlink(file.path(pkg_path, c("doc", "tests", "include")), recursive = TRUE)
}

# 4. The launcher and icon
dir.create(file.path(app_dir, "app"))
invisible(file.copy(file.path(pkg_dir, "installer", "launch.R"), file.path(app_dir, "app")))
invisible(file.copy(file.path(pkg_dir, "installer", "FishDiveR.ico"), app_dir))

size_mb <- sum(file.info(list.files(app_dir, recursive = TRUE, full.names = TRUE))$size) / 1e6
message("App folder: ", round(size_mb), " MB")

# 5. The installer, if Inno Setup 6 is installed
iscc <- c(
  Sys.which("ISCC"),
  file.path(Sys.getenv("ProgramFiles(x86)"), "Inno Setup 6", "ISCC.exe"),
  file.path(Sys.getenv("ProgramFiles"), "Inno Setup 6", "ISCC.exe"),
  file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Inno Setup 6", "ISCC.exe")
)
iscc <- iscc[nzchar(iscc) & file.exists(iscc)][1]
if (is.na(iscc)) {
  message(
    "Inno Setup 6 was not found, so no installer was built. The app folder can be run with:\n  ",
    file.path(r_dir, "bin", "x64", "Rscript.exe"), " --vanilla ", file.path(app_dir, "app", "launch.R")
  )
} else {
  message("Building the installer with ", iscc)
  status <- system2(iscc, c(
    paste0("/DAppVersion=", version),
    paste0("/DBuildDir=", shQuote(gsub("/", "\\\\", build_dir))),
    shQuote(file.path(pkg_dir, "installer", "FishDiveR.iss"))
  ))
  if (status != 0) stop("Building the installer failed.")
  message("Installer: ", file.path(build_dir, "output", paste0("FishDiveR-", version, "-setup.exe")))
}
