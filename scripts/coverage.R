# Run from the package root: Rscript --vanilla scripts/coverage.R
# This reports coverage; it deliberately does not add a CI threshold gate.
if (!requireNamespace("covr", quietly = TRUE)) stop("Install covr to measure coverage.")
Sys.setenv(NOT_CRAN = "true", TZ = "UTC")
cov <- covr::package_coverage(type = "tests")
print(cov)
# Subsetting retains covr's source attributes and its executable-line definition.
# Use unrounded percentages for the threshold comparison.
source_files <- vapply(cov, function(x) basename(attr(x$srcref, "srcfile")$filename), character(1))
files <- sort(unique(source_files))
percent <- vapply(files, function(file) covr::percent_coverage(cov[source_files == file]), numeric(1))
report <- data.frame(file = c(paste0("R/", files), "Overall"),
                     percent = c(percent, covr::percent_coverage(cov)))
report$meets_95_percent <- report$percent >= 95
print(report, row.names = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  utils::write.csv(report, args[[1L]], row.names = FALSE)
  message("Coverage table: ", args[[1L]])
}
