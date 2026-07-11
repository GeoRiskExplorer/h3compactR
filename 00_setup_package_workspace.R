# =============================================================================
# h3compactR PACKAGE WORKSPACE SETUP
#
# File:
#   E:/Packages/h3compactR/00_setup_package_workspace.R
#
# Purpose:
#   Convert the existing workspace folder into a standard R package and create
#   the initial development structure.
#
# Notes:
#   - Positron does not require an .Rproj file.
#   - Run this script from an R session.
#   - The package folder may already exist.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

package_name <- "h3compactR"
package_path <- "E:/Packages/h3compactR"

author_name <- "Robert Andronaco"


# -----------------------------------------------------------------------------
# 2. Console heading
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("h3compactR PACKAGE WORKSPACE SETUP\n")
cat("============================================================\n")
cat("\n")

cat("Package name:", package_name, "\n")
cat("Package path:", package_path, "\n\n")


# -----------------------------------------------------------------------------
# 3. Confirm workspace folder
# -----------------------------------------------------------------------------

if (!dir.exists(package_path)) {
  stop(
    "The package workspace does not exist:\n",
    package_path,
    call. = FALSE
  )
}

package_path <- normalizePath(
  package_path,
  winslash = "/",
  mustWork = TRUE
)

cat("Confirmed workspace:\n")
cat(package_path, "\n\n")


# -----------------------------------------------------------------------------
# 4. Install setup packages if required
# -----------------------------------------------------------------------------

setup_packages <- c(
  "usethis",
  "devtools",
  "roxygen2",
  "testthat",
  "desc"
)

missing_packages <- setup_packages[
  !vapply(
    setup_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {

  cat("Installing missing setup packages:\n")
  cat(paste0(" - ", missing_packages, collapse = "\n"))
  cat("\n\n")

  install.packages(missing_packages)
}


still_missing <- setup_packages[
  !vapply(
    setup_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(still_missing) > 0L) {
  stop(
    "The following required packages are unavailable:\n",
    paste(still_missing, collapse = ", "),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 5. Create the standard package structure
# -----------------------------------------------------------------------------

cat("============================================================\n")
cat("CREATING STANDARD R PACKAGE STRUCTURE\n")
cat("============================================================\n\n")

description_path <- file.path(package_path, "DESCRIPTION")

if (!file.exists(description_path)) {

  usethis::create_package(
    path = package_path,
    rstudio = FALSE,
    open = FALSE
  )

  cat("\nStandard package structure created.\n\n")

} else {

  cat("DESCRIPTION already exists.\n")
  cat("The folder is already recognised as an R package.\n\n")
}


# -----------------------------------------------------------------------------
# 6. Explicitly activate the package
# -----------------------------------------------------------------------------

usethis::proj_set(
  path = package_path,
  force = TRUE
)

setwd(package_path)

cat("Active package root:\n")
cat(usethis::proj_get(), "\n\n")


# -----------------------------------------------------------------------------
# 7. Verify package recognition
# -----------------------------------------------------------------------------

required_package_files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "R"
)

package_recognition_qa <- data.frame(
  item = required_package_files,
  exists = c(
    file.exists("DESCRIPTION"),
    file.exists("NAMESPACE"),
    dir.exists("R")
  )
)

cat("--- Package recognition QA ---\n")
print(package_recognition_qa, row.names = FALSE)
cat("\n")

if (!all(package_recognition_qa$exists)) {
  stop(
    "The workspace has not been correctly initialised as an R package.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 8. Configure testthat
# -----------------------------------------------------------------------------

cat("============================================================\n")
cat("CONFIGURING TESTTHAT\n")
cat("============================================================\n\n")

usethis::use_testthat(edition = 3)


# -----------------------------------------------------------------------------
# 9. Configure roxygen documentation
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CONFIGURING ROXYGEN\n")
cat("============================================================\n\n")

usethis::use_roxygen_md()


# -----------------------------------------------------------------------------
# 10. Add package documentation files
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CREATING PACKAGE DOCUMENTATION FILES\n")
cat("============================================================\n\n")

if (!file.exists("README.Rmd")) {
  usethis::use_readme_rmd()
} else {
  cat("README.Rmd already exists.\n")
}

if (!file.exists("NEWS.md")) {
  usethis::use_news_md()
} else {
  cat("NEWS.md already exists.\n")
}


# -----------------------------------------------------------------------------
# 11. Licence
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CONFIGURING LICENCE\n")
cat("============================================================\n\n")

if (!file.exists("LICENSE.md")) {

  usethis::use_apache_license(
    version = 2,
    include_future = TRUE
  )

} else {

  cat("Licence files already exist.\n")
}


# -----------------------------------------------------------------------------
# 12. Add dependencies to DESCRIPTION
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("REGISTERING PACKAGE DEPENDENCIES\n")
cat("============================================================\n\n")

imports <- c(
  "cli",
  "dplyr",
  "h3jsr",
  "rlang",
  "sf",
  "tibble",
  "vctrs"
)

suggests <- c(
  "covr",
  "ggplot2",
  "knitr",
  "mapview",
  "pkgdown",
  "rmarkdown",
  "withr"
)

for (package in imports) {
  usethis::use_package(
    package = package,
    type = "Imports"
  )
}

for (package in suggests) {
  usethis::use_package(
    package = package,
    type = "Suggests"
  )
}


# -----------------------------------------------------------------------------
# 13. Create development directories
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CREATING DEVELOPMENT DIRECTORIES\n")
cat("============================================================\n\n")

project_directories <- c(
  "dev/playpen/h3_basics",
  "dev/playpen/compaction",
  "dev/playpen/aggregation",
  "dev/playpen/cartography",
  "dev/scripts",
  "dev/fixtures",
  "dev/outputs/figures",
  "dev/outputs/maps",
  "dev/outputs/tables",
  "dev/outputs/qa",
  "analysis/plm",
  "analysis/examples",
  "analysis/outputs",
  "inst/extdata"
)

for (directory in project_directories) {

  if (!dir.exists(directory)) {

    dir.create(
      directory,
      recursive = TRUE,
      showWarnings = FALSE
    )

    cat("Created:", directory, "\n")

  } else {

    cat("Exists: ", directory, "\n")
  }

  gitkeep_path <- file.path(directory, ".gitkeep")

  if (!file.exists(gitkeep_path)) {
    file.create(gitkeep_path)
  }
}


# -----------------------------------------------------------------------------
# 14. Create initial package source files
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CREATING INITIAL R SOURCE FILES\n")
cat("============================================================\n\n")

source_files <- list(

  "R/validation.R" = c(
    "# H3 validation functions",
    "#",
    "# Functions will be added incrementally after expected behaviour",
    "# has been established in dev/playpen."
  ),

  "R/hierarchy.R" = c(
    "# H3 hierarchy functions",
    "#",
    "# Parent-child lookup and hierarchy validation."
  ),

  "R/compact.R" = c(
    "# H3 compaction functions",
    "#",
    "# Initial design is informed by h3jsr::compact().",
    "# Adapted logic must include explicit source attribution."
  ),

  "R/uncompact.R" = c(
    "# H3 uncompaction functions"
  ),

  "R/aggregation.R" = c(
    "# H3 attribute aggregation functions"
  ),

  "R/qa.R" = c(
    "# H3 compaction quality-assurance functions"
  ),

  "R/geometry.R" = c(
    "# H3 geometry functions"
  ),

  "R/plotting.R" = c(
    "# H3 compaction plotting functions"
  ),

  "R/utils.R" = c(
    "# Internal package utilities"
  )
)

for (source_file in names(source_files)) {

  if (!file.exists(source_file)) {

    writeLines(
      source_files[[source_file]],
      source_file
    )

    cat("Created:", source_file, "\n")

  } else {

    cat("Exists: ", source_file, "\n")
  }
}


# -----------------------------------------------------------------------------
# 15. Create attribution notice
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CREATING THIRD-PARTY ATTRIBUTION NOTICE\n")
cat("============================================================\n\n")

notice_text <- c(
  "# Third-party code and attribution",
  "",
  "The initial H3 compaction design in h3compactR was informed by the",
  "compact() implementation in the R package h3jsr, authored by",
  "Lauren O'Brien.",
  "",
  "h3jsr is distributed under the Apache License, Version 2.0 and provides",
  "an R interface to H3 functionality implemented through h3-js.",
  "",
  "Where h3jsr logic is copied or materially adapted, the relevant",
  "h3compactR source file must contain explicit attribution and describe",
  "the material changes made.",
  "",
  "Source project:",
  "https://github.com/obrl-soil/h3jsr"
)

writeLines(
  notice_text,
  "NOTICE.md"
)

cat("Created or updated: NOTICE.md\n")


# -----------------------------------------------------------------------------
# 16. Configure build ignores
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CONFIGURING BUILD IGNORE\n")
cat("============================================================\n\n")

rbuildignore_entries <- c(
  "^00_setup_package_workspace\\.R$",
  "^dev$",
  "^analysis$",
  "^docs$"
)

existing_rbuildignore <- if (file.exists(".Rbuildignore")) {
  readLines(".Rbuildignore", warn = FALSE)
} else {
  character()
}

writeLines(
  unique(c(existing_rbuildignore, rbuildignore_entries)),
  ".Rbuildignore"
)

cat("Updated: .Rbuildignore\n")


# -----------------------------------------------------------------------------
# 17. Document package
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("DOCUMENTING PACKAGE\n")
cat("============================================================\n\n")

devtools::document()


# -----------------------------------------------------------------------------
# 18. Final QA
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FINAL WORKSPACE QA\n")
cat("============================================================\n\n")

expected_files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "README.Rmd",
  "NEWS.md",
  "NOTICE.md",
  "tests/testthat.R",
  "R/validation.R",
  "R/compact.R"
)

expected_directories <- c(
  "R",
  "tests/testthat",
  "dev/playpen/h3_basics",
  "dev/playpen/compaction",
  "dev/playpen/aggregation",
  "dev/playpen/cartography",
  "dev/scripts",
  "dev/fixtures",
  "dev/outputs/qa",
  "analysis/plm",
  "inst/extdata"
)

file_qa <- data.frame(
  path = expected_files,
  exists = file.exists(expected_files)
)

directory_qa <- data.frame(
  path = expected_directories,
  exists = dir.exists(expected_directories)
)

cat("--- File QA ---\n")
print(file_qa, row.names = FALSE)

cat("\n--- Directory QA ---\n")
print(directory_qa, row.names = FALSE)


if (!all(file_qa$exists)) {
  stop(
    "One or more expected files are missing.",
    call. = FALSE
  )
}

if (!all(directory_qa$exists)) {
  stop(
    "One or more expected directories are missing.",
    call. = FALSE
  )
}


cat("\n")
cat("============================================================\n")
cat("h3compactR PACKAGE WORKSPACE SETUP COMPLETE\n")
cat("============================================================\n\n")

cat("Package root:\n")
cat(package_path, "\n\n")

cat("Positron workspace:\n")
cat("Open the folder directly; an .Rproj file is not required.\n\n")

cat("Next development script:\n")
cat("dev/playpen/compaction/01_h3jsr_compact_baseline.R\n\n")