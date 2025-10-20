#' Conditionally Download a File Based on Remote Metadata
#'
#' Checks whether a remote file has changed since the last download by comparing
#' its size or last-modified timestamp. Caches files in a user-specific cache
#' directory following OS conventions, optionally organized by source.
#'
#' @param url A character string giving the full URL of the file to download.
#' @param cache_dir Optional character string specifying a custom cache directory.
#'   Defaults to an OS-appropriate location via [get_cache_dir()].
#' @param check A character string indicating the comparison method to determine
#'   whether a download is needed: `"size"` or `"modified"`.
#' @param source Optional character string specifying a subfolder for the data
#'   source (e.g., `"doleta"`). Files will be stored under this subfolder in
#'   the cache directory.
#'
#' @return The local file path (invisibly). The file will be downloaded if a
#'   newer version is found or no cached version exists.
#'
#' @details
#' The function performs an HTTP `HEAD` request using [httr::HEAD()] to retrieve
#' the file’s metadata without downloading its content.
#' - When `check = "size"`, it compares the `Content-Length` header with
#'   [file.size()].
#' - When `check = "modified"`, it compares the server's `Last-Modified` header
#'   (if available) against a locally stored timestamp file with the same name
#'   plus the extension `.meta`.
#'
#' Files are cached in a platform-appropriate location determined by
#' [get_cache_dir()] and optionally organized by `source`.
#'
#' @examples
#' \dontrun{
#' # Default usage with source folder "doleta"
#' path <- download_if_new(
#'   url = "https://oui.doleta.gov/unemploy/csv/ar539.csv",
#'   check = "size",
#'   source = "doleta"
#' )
#'
#' # Inspect cache folder
#' get_cache_dir("doleta")
#' }
#'
#' @export
download_if_new <- function(url, cache_dir = NULL, check = c("size", "modified"), source = NULL) {
  check <- match.arg(check)

  # Determine cache directory
  if (is.null(cache_dir)) cache_dir <- get_cache_dir()
  if (!is.null(source)) cache_dir <- file.path(cache_dir, source)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  local_path <- file.path(cache_dir, basename(url))
  meta_path <- paste0(local_path, ".meta")

  # Perform HEAD request
  resp <- tryCatch(httr::HEAD(url), error = function(e) NULL)
  if (is.null(resp)) {
    message("HEAD request failed — downloading by default: ", basename(url))
    utils::download.file(url, destfile = local_path, mode = "wb", quiet = TRUE)
    return(invisible(local_path))
  }

  # Check method: "size"
  if (check == "size") {
    remote_size <- suppressWarnings(as.numeric(httr::headers(resp)[["content-length"]]))
    local_size <- if (file.exists(local_path)) file.size(local_path) else NA
    if (!is.na(local_size) && !is.na(remote_size) && local_size == remote_size) {
      message("Cached file is up to date (by size): ", basename(url))
      return(invisible(local_path))
    }
  }

  # Check method: "modified"
  if (check == "modified") {
    remote_modified <- httr::headers(resp)[["last-modified"]]
    local_modified <- if (file.exists(meta_path)) readLines(meta_path, warn = FALSE) else NA
    if (!is.na(local_modified) && !is.na(remote_modified) &&
        identical(local_modified, remote_modified)) {
      message("Cached file is up to date (by Last-Modified): ", basename(url))
      return(invisible(local_path))
    }
  }

  # Download the file
  message("Downloading new file: ", basename(url))
  utils::download.file(url, destfile = local_path, mode = "wb", quiet = TRUE)

  # Update metadata if using modified check
  if (check == "modified") {
    remote_modified <- httr::headers(resp)[["last-modified"]]
    if (!is.null(remote_modified))
      writeLines(remote_modified, meta_path)
  }

  invisible(local_path)
}

#' Get the User Cache Directory for the Package
#'
#' Returns the path to the user-specific cache directory used by the package.
#' Optionally, a `source` subfolder can be specified.
#'
#' @param source Optional character string specifying a subfolder name (e.g., `"doleta"`).
#' @return A character string giving the full path to the cache directory.
#'
#' @examples
#' # Default cache location
#' get_cache_dir()
#'
#' # Cache location for a source folder
#' get_cache_dir("doleta")
#'
#' @export
get_cache_dir <- function(source = NULL) {
  dir <- rappdirs::user_cache_dir("DETRLMI")
  if (!is.null(source)) dir <- file.path(dir, source)
  dir
}
