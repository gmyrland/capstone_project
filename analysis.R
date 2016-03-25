# Load Packages
library(dplyr)
library(xml2)
library(DBI)
library(RSQLite)
library(ggplot2)

# ML packages
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(VGAM)
library(randomForest)
library(MASS)
library(leaps)
library(glmnet)

## Ensure working directory is correct
# If fails, set working directory to root dir of project, e.g. setwd('~/source/nass')
expected_dirs <- c("01_abstract", "02_data_review", "data", "doc", "R")
stopifnot(dir.exists(expected_dirs))

## Source R files
source("R/global.R")
source("R/scrape.R")
source("R/database.R")
source("R/parse.R")
source("R/cleaning.R")
source("R/partitioning.R")

## Control variables. Use for skipping steps.
do_webscrape <- FALSE
do_parse <- FALSE
do_build_features <- FALSE

## Source web-scraping code and scrape any remaining cases to data/cases
if (do_webscrape)
    download_all_cases()

## Parse XML to data frame
if (do_parse)
    parse_xml()

## If database not manually created above, download from Amazon S3
if (!file.exists(db_path)) {
    db_url <- 'http://gmyrland.capstone.s3.amazonaws.com/db.zip'
    download.file(db_url, destfile='data/db.zip', method="auto")
    unzip('data/db.zip', exdir='data')
}

## Create custom dataset with features for machine learning
if (do_build_features) {
    df <- build_clean_dataset()
    save(df, file='data/df.Rdata')
}

## Load df
load('data/df.Rdata')
