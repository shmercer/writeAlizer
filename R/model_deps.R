#' Report optional model dependencies (no installation performed)
#'
#' Discovers package dependencies for model fitting from the package
#' `Suggests` field. This function **never installs** packages. It reports
#' which packages are required and which are currently missing, and prints
#' a ready-to-copy command you can run to install the missing ones manually.
#'
#' @return
#' A named list:
#' \describe{
#'   \item{required}{Character vector of discovered package tokens (may include version qualifiers), e.g. \code{c("glmnet (>= 4.1)", "ranger")}.}
#'   \item{missing}{Character vector of base package names that are not installed, e.g. \code{c("glmnet", "ranger")}.}
#' }
#'
#' The function also emits a message. If nothing is missing, it reports that all
#' required packages are installed. Otherwise, it lists the missing packages and
#' prints a copy-paste \code{install.packages()} command.
#'
#' @examples
#' md <- model_deps()
#' md$missing
#'
#' @export
model_deps <- function() {
  # helpers
  parse_suggests <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x)) return(character(0))
    x <- gsub("[\r\n]+", " ", x)
    tokens <- trimws(unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE))
    unique(tokens[nzchar(tokens)])
  }
  strip_qualifier <- function(x) sub("\\s*\\(.*\\)$", "", x)
  is_available <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

  # read Suggests from installed DESCRIPTION
  suggests <- utils::packageDescription("writeAlizer", fields = "Suggests")
  pkgs_raw <- parse_suggests(suggests)

  if (length(pkgs_raw) == 0L) {
    message("No optional model dependencies discovered.")
    return(list(required = character(0), missing = character(0)))
  }

  base_names <- strip_qualifier(pkgs_raw)
  have <- vapply(base_names, is_available, logical(1))
  missing <- base_names[!have]

  if (length(missing) == 0L) {
    message("All required packages are installed: ",
            paste(base_names, collapse = ", "))
  } else {
    cmd <- sprintf('install.packages(c("%s"))',
                   paste(missing, collapse = '", "'))
    message(
      "Missing required packages: ", paste(missing, collapse = ", "),
      "\nInstall them manually, e.g.:\n  ", cmd
    )
  }

  list(required = pkgs_raw, missing = missing)
}

#' @keywords internal
#' @noRd
install_model_deps <- function(...) {
  .Deprecated("model_deps", package = "writeAlizer",
              msg = "install_model_deps() was removed. Use model_deps().")
  model_deps()
}
