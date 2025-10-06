to_file_url <- function(path) {
  stopifnot(file.exists(path))
  # Normalize and encode
  norm <- normalizePath(path, winslash = "/", mustWork = TRUE)
  url_path <- utils::URLencode(norm)
  if (.Platform$OS.type == "windows") {
    paste0("file:///", url_path)  # 3 slashes then C:/...
  } else {
    paste0("file://", url_path)
  }
}
