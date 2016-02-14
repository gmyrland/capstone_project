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
