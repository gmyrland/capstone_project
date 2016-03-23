# Build data frame for use in modelling, including all relevant features

#build_ml_dataset <- function() {
    # Function to extract dataframe from SQLite table by name
    get_tbl <- function(name) tbl(src_sqlite(db_path), name) %>% as.data.frame(n=-1)
    
    # Join data to form rows by occupant
    df_joined <- get_tbl("Occupants") %>%
        left_join(get_tbl("Persons"), c(
            "CaseId"="CaseId",
            "Occupant_VehicleNumber"="Person_VehicleNumber",
            "Occupant_OccupantNumber"="Person_OccNumber"
        )) %>%
        left_join(get_tbl("Cases"), c("CaseId" = "CaseId")) %>%
        left_join(
            # requires aggregation to single case, use 1st event
            get_tbl("Events") %>% filter(Event_EventNumber == 1), c(
                "CaseId" = "CaseId",
                "Occupant_VehicleNumber" = "Event_VehicleNumber"
        )) %>%
        left_join(get_tbl("Vehicles"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Vehicle_VehicleNumber"
        )) %>%
        # ems: requires aggregation to single case, ignore
        left_join(get_tbl("GeneralVehicle"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "GeneralVehicle_VehicleNumber"
        )) %>%
        left_join(get_tbl("Safety"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Safety_VehicleNumber"
        )) %>%
        left_join(get_tbl("VehicleExterior"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "VehicleExterior_VehicleNumber"
        )) %>%
        left_join(get_tbl("VehicleInterior"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "VehicleInterior_VehicleNumber"
        )) %>%
        ## Filter for known outcomes only
        filter(Occupant_Injury_Treatment_Mortality %in% c('Fatal', 'Not Fatal'))

    ## Number of rows of data
    # Expected: nrow(occupants) nrow - 105296
    # Result: dim(df_joined)  nrow - 105296, ncol - 704
    # Filtered by mortality: nrow - 84,277

    ## Summarize Features
    # sapply(df, function(x) {
    #     write("\t(", file="data/out.tab",append=TRUE)
    #     write(as.numeric(sort(table(x), decreasing=TRUE))[1],file="data/out.tab",append=TRUE)
    #     write(")\t", file="data/out.tab",append=TRUE)
    #     write(paste0(gsub("\r|\n"," ",names(sort(table(x), decreasing=TRUE))),sep="\t",collapse="\t"),file="data/out.tab",append=TRUE)
    # })

    ## Populate NA's as 'Unknown'
    ## Rename some things to unkonwn
    ## Lookup Tables
    
    
    ## Build features from joined dataset
    df <- df_joined %>% transmute(
        CaseId,
        # Explanatory Variables
        age = as.double(Occupant_Occupant_Age) * ifelse(tolower(Occupant_Occupant_Age_UOM) == 'months', 1/12, 1), # years
        height = as.integer(ifelse(is.na(Occupant_Occupant_Height), Occupant_Occupant_OccHeight, Occupant_Occupant_Height)), #cms
        weight = as.integer(Occupant_Occupant_Weight), # kgs
        sex = ifelse(is.na(Occupant_Occupant_Sex), "Unknown", ifelse(Occupant_Occupant_Sex == 'Male', 'Male', 'Female')),
        role = Occupant_Occupant_Role,
        # Scaled Vars
        scaled_age = scale(age),
        scaled_height = scale(height),
        scaled_weight=scale(weight),
        # Response Variable
        fatal = as.integer(Occupant_Injury_Treatment_Mortality == 'Fatal')
    )
    #write.csv(df[sample(nrow(df),100), ], "data/data.csv", row.names=FALSE)
    #return(df)
#}


