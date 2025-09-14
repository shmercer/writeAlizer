#' Install optional model dependencies
#'
#' Discovers package dependencies for model fitting from the package
#' `Suggests` field, or via the option hook
#' `options(writeAlizer.require_pkgs_for_fits = function() c("pkgA", "pkgB (>= 1.2.3)"))`.
#'
#' If a hook function is provided via the option above, its return value
#' (a character vector of package tokens, optionally with version qualifiers)
#' takes precedence over reading the package `Suggests`.
#'
#' @param dry_run Logical. If `TRUE`, returns the discovered package tokens
#'   (possibly including version qualifiers) without attempting installation.
#' @return
#' - If `dry_run = TRUE`: **visible** character vector of package tokens
#'   (e.g., `c("ranger", "glmnet (>= 4.1)")`).
#' - Otherwise: (invisibly) the character vector of base package names that were
#'   required/checked for installation (e.g., `c("ranger", "glmnet")`).
#'
#' @examples
#' # See which packages would be required without installing:
#' install_model_deps(dry_run = TRUE)
#'
#' # Provide a custom hook to override discovery:
#' old <- options(writeAlizer.require_pkgs_for_fits = function() c("glmnet (>= 4.1)", "ranger"))
#' on.exit(options(old), add = TRUE)
#' install_model_deps(dry_run = TRUE)
#'
#' @export
install_model_deps <- function(dry_run = FALSE) {
  # --- helpers ---------------------------------------------------------------

  # Parse a DESCRIPTION Suggests field into tokens, preserving version qualifiers.
  parse_suggests <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x)) return(character(0))
    # Replace newlines with spaces, then split on commas
    x <- gsub("[\r\n]+", " ", x)
    tokens <- trimws(unlist(strsplit(x, "," , fixed = TRUE), use.names = FALSE))
    tokens <- tokens[nzchar(tokens)]
    unique(tokens)
  }

  # Strip any trailing version qualifier: "pkg (>= 1.2.3)" -> "pkg"
  strip_qualifier <- function(x) sub("\\s*\\(.*\\)$", "", x)

  # Check if a package is available without attaching it
  is_available <- function(pkg) {
    # requireNamespace returns TRUE/FALSE and doesn't change search path
    isTRUE(requireNamespace(pkg, quietly = TRUE))
  }

  # --- discover tokens -------------------------------------------------------

  # Option hook takes precedence if supplied
  hook <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  if (is.function(hook)) {
    pkgs_raw <- as.character(hook())
    pkgs_raw <- pkgs_raw[nzchar(pkgs_raw)]
  } else {
    # Fall back to reading Suggests from the installed writeAlizer DESCRIPTION
    suggests <- utils::packageDescription("writeAlizer", fields = "Suggests")
    pkgs_raw <- parse_suggests(suggests)
  }

  # Ensure character(0) not NA for downstream logic
  if (length(pkgs_raw) == 0L) {
    if (isTRUE(dry_run)) return(character(0))
    return(invisible(character(0)))
  }

  # --- dry run ---------------------------------------------------------------

  if (isTRUE(dry_run)) {
    # Visible return so tests/users can inspect easily
    return(pkgs_raw)
  }

  # --- install path ----------------------------------------------------------

  # Strip version qualifiers only when checking/installing
  base_names <- strip_qualifier(pkgs_raw)

  # Which are missing?
  have <- vapply(base_names, is_available, logical(1))
  missing <- base_names[!have]

  # Install missing, if any
  if (length(missing)) {
    utils::install.packages(missing)
  }

  # Return the set we checked (invisibly)
  invisible(base_names)
}
