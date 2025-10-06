to_file_url <- function(path) {
  stopifnot(file.exists(path))
  norm <- normalizePath(path, winslash = "/", mustWork = TRUE)
  encoded <- utils::URLencode(norm)
  if (.Platform$OS.type == "windows") {
    paste0("file:///", encoded)   # file:///C:/...
  } else {
    paste0("file://", encoded)    # file:///tmp/...
  }
}
