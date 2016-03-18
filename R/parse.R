##############
## XML Parsing

## Parse all case files to single rectangular data frame
parse_xml <- function() {
    case_ids <- get_case_ids()
    df <- data_frame(id = case_ids)
    for (i in 1:length(case_ids)) {
        # Progress Reporting
        if (!(i %% 1000)) print(i)

        # Fetch case file
        f <- case_path(df$id[i])
        stopifnot(file.exists(f))
        data <- read_xml(f)

        ## xml2 functions:
        # xml_attrs(data)
        # xml_name(data)
        # xml_children(data)
        # xml_siblings(xml_children(data))
        # as_list(data)

        #Case Attributes
        case_attrs <- xml_attrs(data)
        df$CaseID[i] <- case_attrs[["noNamespaceSchemaLocation"]]
        df$CaseStr[i] <- case_attrs[["NumOfVehicle"]]
        df$Schema[i] <- case_attrs[["noNamespaceSchemaLocation"]]
        df$NumOfVehicle[i] <- case_attrs[["NumOfVehicle"]]
        df$Version[i] <- case_attrs[["Version"]]
        df$Author[i] <- case_attrs[["Author"]]

        #Case Summary
        df$config[i] <- xml_text(xml_find_all(data, "//CaseSummary/Configuration"))
        #df$text[i] <- xml_text(data)
        #update_db(case_id, "config", config)
    }
    return(df)
}

#############################################
## Helper functions for extracting XML schema

## Iterate through cases to extract xml children for given xpath
parse_xml <- function(str) {
    case_ids <- get_case_ids()
    df <- data_frame(id = case_ids)
    for (i in 1:length(case_ids)) {
        if (!(i %% 1000)) print(i)
        f <- case_path(df$id[i])
        stopifnot(file.exists(f))
        data <- read_xml(f)
        subdata <- xml_find_all(data, str)
        df$count[i] <- length(subdata)
        df$children[i] <- paste(xml_name(xml_children(subdata)), collapse="|")
    }
    return(df)
}
#eg. write.csv(parse_xml("/Case/CaseForm/Crash/PSU"), "data/Case_CaseForm_Crash_PSU.csv")


## Iterate through cases to extract xml attributes for given xpath
parse_attr <- function(str) {
    case_ids <- get_case_ids()
    df <- data_frame(id = case_ids)
    for (i in 1:length(case_ids)) {
        if (!(i %% 1000)) print(i)
        f <- case_path(df$id[i])
        data <- read_xml(f)
        df$attr <- paste(unique(names(unlist(xml_attrs(xml_find_all(data, str))))), collapse="|")
    }
    return(df %>% group_by(attr) %>% summarize())
}
#eg. write.csv(parse_attr("/Case/CaseForm/Crash/PSU"), "data/attr_Case_CaseForm_Crash_PSU.csv")

## Sanity check function to ensure at least one value text value exists for given xpath
run_check <- function(str) {
    case_ids <- sample(get_case_ids())
    ret <- FALSE
    for (i in 1:length(case_ids)) {
        f <- case_path(case_ids[i])
        data <- read_xml(f)
        v <- xml_text(xml_find_all(data, str))
        if(is.character(v) && length(v) >= 1) return(TRUE)
    }
    print("NOT FOUND!!!!!")
    return(ret)

}
#eg. run_check("/Case/CaseForm/Crash2")
