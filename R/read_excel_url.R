read_excel_url <-
function(url, ...) {
    temp <- tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp, mode = "wb")
    on.exit(unlink(temp))  # Clean up the temp file when done
    read_excel(temp, ...)
}
