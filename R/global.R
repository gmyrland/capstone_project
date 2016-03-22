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
