# Load Packages
library(dplyr)
library(xml2)
library(DBI)
library(RSQLite)

## Ensure working directory is correct
## If fails, set working directory to root dir of project, e.g. setwd('~/source/NASS')
expected_dirs <- c("01_abstract", "02_data_review", "data", "doc")
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

## Control variables. Use for skipping steps.
do_webscrape <- FALSE
do_parse <- FALSE

## Source web-scraping code and scrape any remaining cases to data/cases
source("R/scrape.R")
if (do_webscrape)
    download_all_cases()

## Parse XML to data frame
source("R/database.R")
source("R/parse.R")
if (do_parse)
    df <- parse_xml() # Parse the XML files

## If database not manually created above, download from Amazon S3
db_url <- 'http://gmyrland.capstone.s3.amazonaws.com/db.zip'
download.file(db_url, destfile='data/db.zip', method="auto")
unzip('data/db.zip', exdir='data')
