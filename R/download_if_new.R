#' Conditionally Download a File Based on Remote Metadata (BLS-aware, direct download)
#'
#' Checks whether a remote file has changed since the last download by comparing
#' its size or last-modified timestamp. Automatically determines a cache file
#' name from the URL and stores it in the package cache directory.
#' Adds special HTTP headers for BLS URLs and downloads files via `httr::GET`.
#'
#' @param url A character string giving the full URL of the file to download.
#' @param check A character string indicating the comparison method to determine
#'   whether a download is needed: `"size"` or `"modified"`.
#'
#' @return The local file path (invisibly). The file will be downloaded if a
#'   newer version is found or no cached version exists.
#'
#' @export
download_if_new <- function(url, check = c("size", "modified")) {
  check <- match.arg(check)

  # Derive cache directory and local file path
  cache_dir <- get_cache_dir()
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  filename <- basename(url)
  local_path <- file.path(cache_dir, filename)
  meta_path <- paste0(local_path, ".meta")

  # Remove directory if it exists at the file path
  if (dir.exists(local_path)) {
    warning("A directory exists where the file should be. Removing directory: ", local_path)
    unlink(local_path, recursive = TRUE)
  }

  # Set special headers for BLS URLs
  bls_headers <- NULL
  if (grepl("bls.gov", url)) {
    bls_headers <- c(
      "Accept" =
        "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      "Accept-Encoding" = "gzip, deflate, br",
      "Accept-Language" = "en-US,en;q=0.9",
      "Connection" = "keep-alive",
      "Host" = "www.bls.gov",
      "Referer" = "https://www.bls.gov/cew/classifications/areas/qcew-area-titles.htm",
      "Sec-Ch-Ua" = 'Not_A Brand";v="8", "Chromium";v="120", "Google Chrome";v="120"',
      "Sec-Ch-Ua-Mobile" = "?0",
      "Sec-Ch-Ua-Platform" = '"Windows"',
      "Sec-Fetch-Dest" = "document",
      "Sec-Fetch-Mode" = "navigate",
      "Sec-Fetch-Site" = "same-origin",
      "Sec-Fetch-User" = "?1",
      "Upgrade-Insecure-Requests" = "1",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    )
  }

  # Perform HEAD request with optional BLS headers
  resp <- tryCatch({
    if (!is.null(bls_headers)) {
      httr::HEAD(url, httr::add_headers(.headers = bls_headers))
    } else {
      httr::HEAD(url)
    }
  }, error = function(e) NULL)

  # If HEAD fails, proceed to download anyway
  if (is.null(resp)) {
    message("HEAD request failed — downloading by default: ", filename)
  } else {
    # Check method: size
    if (check == "size") {
      remote_size <- suppressWarnings(as.numeric(httr::headers(resp)[["content-length"]]))
      local_size <- if (file.exists(local_path)) file.size(local_path) else NA
      if (!is.na(local_size) && !is.na(remote_size) && local_size == remote_size) {
        message("Cached file is up to date (by size): ", filename)
        return(invisible(local_path))
      }
    }

    # Check method: modified
    if (check == "modified") {
      remote_modified <- httr::headers(resp)[["last-modified"]]
      local_modified <- if (file.exists(meta_path)) readLines(meta_path, warn = FALSE) else NA
      if (!is.na(local_modified) && !is.na(remote_modified) &&
          identical(local_modified, remote_modified)) {
        message("Cached file is up to date (by Last-Modified): ", filename)
        return(invisible(local_path))
      }
    }
  }

  # Download the file (using GET + writeBin for BLS URLs)
  message("Downloading new file: ", filename)
  if (!is.null(bls_headers)) {
    # BLS download with headers
    response <- httr::GET(url, httr::add_headers(.headers = bls_headers))
    stopifnot(httr::status_code(response) == 200)
    content_data <- httr::content(response, as = "raw")
    writeBin(content_data, local_path)
  } else {
    # Regular download for non-BLS URLs
    utils::download.file(url, destfile = local_path, mode = "wb", quiet = TRUE)
  }

  # Update metadata if using modified check
  if (check == "modified" && !is.null(resp)) {
    remote_modified <- httr::headers(resp)[["last-modified"]]
    if (!is.null(remote_modified)) writeLines(remote_modified, meta_path)
  }

  invisible(local_path)
}

#' Get the User Cache Directory for the Package
#'
#' Returns the path to the user-specific cache directory used by the package.
#' No subfolders are created; all files go directly in this directory.
#'
#' @return A character string giving the full path to the cache directory.
#'
#' @export
get_cache_dir <- function() {
  dir <- rappdirs::user_cache_dir("DETRLMI")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
