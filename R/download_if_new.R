#' Conditionally Download a File Based on Remote Metadata
#'
#' Checks whether a remote file has changed since the last download by comparing
#' its size or last-modified timestamp. Caches files in the main package cache
#' directory (no subfolders). Automatically removes any existing directory at
#' the target file path.
#'
#' @param url A character string giving the full URL of the file to download.
#' @param destfile Character string specifying the exact local file path to save the file.
#' @param check A character string indicating the comparison method to determine
#'   whether a download is needed: `"size"` or `"modified"`.
#'
#' @return The local file path (invisibly). The file will be downloaded if a
#'   newer version is found or no cached version exists.
#' @export
download_if_new <- function(url, destfile, check = c("size", "modified")) {
  check <- match.arg(check)

  # Remove directory if it exists at the file path
  if (dir.exists(destfile)) {
    warning("A directory exists where the file should be. Removing directory: ", destfile)
    unlink(destfile, recursive = TRUE)
  }

  # Ensure parent folder exists
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  meta_path <- paste0(destfile, ".meta")

  # HEAD request
  resp <- tryCatch(httr::HEAD(url), error = function(e) NULL)
  if (is.null(resp)) {
    message("HEAD failed — downloading by default: ", basename(destfile))
    utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
    return(invisible(destfile))
  }

  # Check method: size
  if (check == "size") {
    remote_size <- suppressWarnings(as.numeric(httr::headers(resp)[["content-length"]]))
    local_size <- if (file.exists(destfile)) file.size(destfile) else NA
    if (!is.na(local_size) && !is.na(remote_size) && local_size == remote_size) {
      message("Cached file is up to date (by size): ", basename(destfile))
      return(invisible(destfile))
    }
  }

  # Check method: modified
  if (check == "modified") {
    remote_modified <- httr::headers(resp)[["last-modified"]]
    local_modified <- if (file.exists(meta_path)) readLines(meta_path, warn = FALSE) else NA
    if (!is.na(local_modified) && !is.na(remote_modified) &&
        identical(local_modified, remote_modified)) {
      message("Cached file is up to date (by Last-Modified): ", basename(destfile))
      return(invisible(destfile))
    }
  }

  # Download the file
  message("Downloading new file: ", basename(destfile))
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)

  if (check == "modified") {
    remote_modified <- httr::headers(resp)[["last-modified"]]
    if (!is.null(remote_modified)) writeLines(remote_modified, meta_path)
  }

  invisible(destfile)
}

#' Get the User Cache Directory for the Package
#'
#' Returns the path to the user-specific cache directory used by the package.
#' No subfolders are created; all files go directly in this directory.
#'
#' @return A character string giving the full path to the cache directory.
#' @export
get_cache_dir <- function() {
  dir <- rappdirs::user_cache_dir("DETRLMI")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
