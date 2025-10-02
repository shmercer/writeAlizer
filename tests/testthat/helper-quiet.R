# tests/testthat/helper-quiet.R
.quiet_eval <- function(expr) {
  withCallingHandlers(
    suppressWarnings(suppressMessages(force(expr))),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning")
  )
}
