###############
## Web Scraping

## Webscraper Function
download_case <- function(case_id) {
    url <- case_url(case_id)
    out <- case_path(case_id)
    if (!file.exists(out)) {
        download.file(url, out, quiet=TRUE)
        cat("Downloaded: ", case_id, "\n")
        Sys.sleep(as.integer(runif(1,4,20)))
    }
}

## Download all Cases
download_all_cases <- function() {
    case_ids <- get_case_ids()
    for (case_id in case_ids) {
        download_case(case_id)
    }
}
