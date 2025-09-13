#' Install model dependencies (Suggests)
#'
#' Installs packages listed in **Suggests** that writeAlizer may use.
#' Use `dry_run = TRUE` to just list the packages without installing.
#'
#' Optionally, an internal/helper hook can be provided to override dependency
#' resolution (useful in tests or specialized setups):
#' * Option: set \code{options(writeAlizer.require_pkgs_for_fits = function(model, install, return_pkgs) \{ ... \})}
#' * Internal (if present): \code{.wa_require_pkgs_for_fits(model, install, return_pkgs)}
#'
#' @param model Character (reserved for future use). Currently "all" by default.
#' @param dry_run Logical. If TRUE, do not install; simply return the names.
#' @return Invisibly returns the character vector of package names considered
#'   (may include version qualifiers as declared in DESCRIPTION).
#'
#' @examples
#' # Safe for R CMD check: just list, do not install
#' pkgs <- install_model_deps(dry_run = TRUE)
#' pkgs
#'
#' # Optional helper via option (useful in tests)
#' old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
#' on.exit(options(writeAlizer.require_pkgs_for_fits = old_opt), add = TRUE)
#' options(writeAlizer.require_pkgs_for_fits = function(model, install, return_pkgs) {
#'   if (isTRUE(return_pkgs)) return(c("A_pkg_from_helper", "B_pkg_from_helper"))
#'   invisible(NULL)
#' })
#' install_model_deps(model = "all", dry_run = TRUE)
#'
#' @export
install_model_deps <- function(model = "all", dry_run = FALSE) {
  # -------- 0) Optional helper hook (used only if it yields a non-empty set) --------
  helper <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  if (is.null(helper)) {
    helper <- tryCatch(get(".wa_require_pkgs_for_fits", envir = asNamespace("writeAlizer")),
                       error = function(e) NULL)
  }
  if (is.function(helper)) {
    if (isTRUE(dry_run)) {
      pkgs_from_helper <- tryCatch(
        helper(model = model, install = FALSE, return_pkgs = TRUE),
        error = function(e) character(0)
      )
      if (length(pkgs_from_helper) > 0L) {
        return(invisible(as.character(pkgs_from_helper)))
      }
      # else fall through to Suggests
    } else {
      invisible(tryCatch(
        helper(model = model, install = TRUE, return_pkgs = FALSE),
        error = function(e) NULL
      ))
      # continue to return canonical list
    }
  }

  # -------- 1) Read Suggests from package metadata or DESCRIPTION --------
  read_suggests_dcf <- function(path) {
    if (!is.character(path) || !nzchar(path) || !file.exists(path)) return(NA_character_)
    d <- tryCatch(read.dcf(path), error = function(e) NULL)
    if (is.null(d) || nrow(d) < 1 || !"Suggests" %in% colnames(d)) return(NA_character_)
    v <- d[1, "Suggests"]; if (is.na(v) || !nzchar(v)) NA_character_ else v
  }
  read_suggests_lines <- function(path) {
    if (!is.character(path) || !nzchar(path) || !file.exists(path)) return(NA_character_)
    L <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
    if (!length(L)) return(NA_character_)
    i <- grep("^\\s*Suggests\\s*:", L)
    if (!length(i)) return(NA_character_)
    s <- sub("^\\s*Suggests\\s*:\\s*", "", L[i[1]])
    j <- i[1] + 1
    while (j <= length(L) && grepl("^\\s", L[j])) { s <- paste0(s, " ", trimws(L[j])); j <- j + 1 }
    s <- trimws(s); if (nzchar(s)) s else NA_character_
  }

  suggests <- utils::packageDescription("writeAlizer", fields = "Suggests")

  if (is.na(suggests) || !nzchar(suggests)) {
    ns_path <- tryCatch(getNamespaceInfo(asNamespace("writeAlizer"), "path"),
                        error = function(e) NULL)
    if (is.character(ns_path) && nzchar(ns_path)) {
      desc_ns <- file.path(ns_path, "DESCRIPTION")
      suggests <- read_suggests_dcf(desc_ns)
      if (is.na(suggests) || !nzchar(suggests)) suggests <- read_suggests_lines(desc_ns)
    }
  }
  if (is.na(suggests) || !nzchar(suggests)) {
    desc_cwd <- "DESCRIPTION"
    suggests <- read_suggests_dcf(desc_cwd)
    if (is.na(suggests) || !nzchar(suggests)) suggests <- read_suggests_lines(desc_cwd)
  }

  # -------- 1b) STATIC FALLBACK (guarantees non-empty in dev/test) --------
  if (is.na(suggests) || !nzchar(suggests)) {
    suggests <- paste(
      "testthat (>= 3.1.0)",
      "Cubist",
      "caretEnsemble",
      "earth",
      "gbm",
      "glmnet",
      "kernlab",
      "pls",
      "randomForest",
      "withr",
      sep = ", "
    )
  }

  # -------- 2) Parse into return tokens (keep version qualifiers) --------
  pieces <- unlist(strsplit(suggests, ","))
  pieces <- trimws(gsub("[\r\n]+", " ", pieces))
  pkgs_raw <- unique(pieces[nzchar(pieces)])

  if (isTRUE(dry_run)) {
    return(invisible(pkgs_raw))
  }

  # -------- 3) Install if missing (strip version qualifiers for checks) --------
  pkgs_clean <- sub("\\s*\\(.*\\)$", "", pkgs_raw)
  is_avail <- vapply(pkgs_clean, function(p) requireNamespace(p, quietly = TRUE), logical(1))
  missing <- pkgs_clean[!is_avail]
  if (length(missing)) utils::install.packages(missing)

  invisible(pkgs_raw)
}
