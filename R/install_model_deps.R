#' Install model dependencies (Suggests)
#'
#' Installs packages listed in \strong{Suggests} that writeAlizer relies on.
#' This wrapper accepts a \code{model} argument for future per-model scoping,
#' but today it installs the package's Suggests set (conservative and robust).
#'
#' Use \code{dry_run = TRUE} in CI/tests to avoid installing anything and just
#' return the package names that would be installed.
#'
#' @param model Character (ignored for now; reserved for future use). Examples:
#'   "rb_mod3all", "coh_mod3all", or "all".
#' @param dry_run Logical. If TRUE, do not install; simply return the names.
#' @return Invisibly returns the character vector of package names considered.
#' @export
install_model_deps <- function(model = "all", dry_run = FALSE) {
  # If an internal helper exists in the package in the future, prefer it.
  if (exists(".wa_require_pkgs_for_fits", mode = "function")) {
    pkgs <- tryCatch(
      {
        # Assume helper can tell us the package set when not installing.
        .wa_require_pkgs_for_fits(model = model, install = !dry_run, return_pkgs = TRUE)
      },
      error = function(e) NULL
    )
    if (is.character(pkgs) && length(pkgs)) return(invisible(pkgs))
  }

  # Fallback: install everything in Suggests
  desc_path <- system.file("DESCRIPTION", package = "writeAlizer")
  if (!nzchar(desc_path)) stop("Cannot locate DESCRIPTION for writeAlizer")
  dcf <- read.dcf(desc_path)
  suggests <- if ("Suggests" %in% colnames(dcf)) dcf[1, "Suggests"] else ""
  pkgs <- unique(trimws(unlist(strsplit(suggests, ",\\s*"))))
  pkgs <- pkgs[nzchar(pkgs)]

  if (dry_run) return(invisible(pkgs))

  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) install.packages(missing)
  invisible(pkgs)
}
