# Load Packages
library(dplyr)

## Ensure working directory is correct
## If fails, set working directory to root dir of project, e.g. setwd('~/source/NASS')
expected_dirs <- c("01_abstract", "02_data_review", "data", "data/cases", "doc")
stopifnot(dir.exists(expected_dirs))

## Useful Functions
case_url  <- function(case_id) {
    paste0("http://www-nass.nhtsa.dot.gov/nass/cds/CaseForm.aspx?GetXML&caseid=", case_id, "&year=&transform=0&docInfo=0")
}
case_path <- function(case_id) {
    paste0("data/cases/", case_id, ".txt")
}
get_case_ids  <- function() {
    df <- read.table("data/nass_case_ids_filtered.txt", sep="\t", stringsAsFactors=FALSE, header=FALSE)
    return(as.character(df[, 9]))
}

## Source web-scraping code and scrape any remaining cases to data/cases
source("R/scrape.R")
download_all_cases()

