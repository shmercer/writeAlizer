#' Install model dependencies (Suggests)
#'
#' Installs packages listed in **Suggests** that writeAlizer may use.
#' Use `dry_run = TRUE` to just list the packages without installing.
#'
#' @param model Character (reserved for future use). Currently ignored.
#' @param dry_run Logical. If TRUE, do not install; simply return the names.
#' @return Invisibly returns the character vector of package names considered.
#' @export
install_model_deps <- function(model = "all", dry_run = FALSE) {
  suggests <- utils::packageDescription("writeAlizer", fields = "Suggests")
  if (is.na(suggests) || !nzchar(suggests)) return(invisible(character(0)))

  pkgs <- unique(trimws(strsplit(suggests, ",")[[1]]))
  pkgs <- pkgs[nzchar(pkgs)]

  if (dry_run) return(pkgs)

  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) utils::install.packages(missing)
  invisible(pkgs)
}
