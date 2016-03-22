# Build data frame for use in modelling, including all relevant features

build_ml_dataset <- function() {
    # Load dataset
    cases <- tbl(src_sqlite(db_path), "Cases") %>% as.data.frame
    events <- tbl(src_sqlite(db_path), "Events") %>% as.data.frame
    vehicles <- tbl(src_sqlite(db_path), "Vehicles") %>% as.data.frame
    persons <- tbl(src_sqlite(db_path), "Persons") %>% as.data.frame(n=-1)
    #ems <- tbl(src_sqlite(db_path), "EMS") %>% as.data.frame
    genvehicle <- tbl(src_sqlite(db_path), "GeneralVehicle") %>% as.data.frame
    occupants <- tbl(src_sqlite(db_path), "Occupants") %>% as.data.frame(n=-1)
    safety <- tbl(src_sqlite(db_path), "Safety") %>% as.data.frame
    vehicleext <- tbl(src_sqlite(db_path), "VehicleExterior") %>% as.data.frame
    vehicleint <- tbl(src_sqlite(db_path), "VehicleInterior") %>% as.data.frame

    # Join data to form rows based on occupant
    df_joined <- occupants %>%
        left_join(persons, c(
            "CaseId"="CaseId",
            "Occupant_VehicleNumber"="Person_VehicleNumber",
            "Occupant_OccupantNumber"="Person_OccNumber"
        )) %>%
        left_join(cases, c("CaseId" = "CaseId")) %>%
        # events: requires aggregation to single case, use 1st event
        left_join(events %>% filter(Event_EventNumber == 1) , c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Event_VehicleNumber"
        )) %>%
        left_join(vehicles, c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Vehicle_VehicleNumber"
        )) %>%
        # ems: requires aggregation to single case, ignore
        left_join(genvehicle, c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "GeneralVehicle_VehicleNumber"
        )) %>%
        left_join(safety, c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Safety_VehicleNumber"
        )) %>%
        left_join(vehicleext, c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "VehicleExterior_VehicleNumber"
        )) %>%
        left_join(vehicleint, c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "VehicleInterior_VehicleNumber"
        ))

    ## Number of rows of data
    # Expected: nrow(occupants) nrow - 105296
    # Result: dim(df_joined)  nrow - 105296, ncol - 704

    ## Summarize Features
    # sapply(df, function(x) {
    #     write("\t(", file="data/out.tab",append=TRUE)
    #     write(as.numeric(sort(table(x), decreasing=TRUE))[1],file="data/out.tab",append=TRUE)
    #     write(")\t", file="data/out.tab",append=TRUE)
    #     write(paste0(gsub("\r|\n"," ",names(sort(table(x), decreasing=TRUE))),sep="\t",collapse="\t"),file="data/out.tab",append=TRUE)
    # })

    ## Filter for known outcomes only
    df_joined <- df_joined %>% filter(Occupant_Injury_Treatment_Mortality %in% c('Fatal', 'Not Fatal'))

    ## Build features from joined dataset
    df <- df_joined %>%
        transmute(
            CaseId,
            age = Occupant_Occupant_Age,
            sex = Occupant_Occupant_Sex,

            # Response Variable
            fatal = as.integer(Occupant_Injury_Treatment_Mortality == 'Fatal')
        )

    #write.csv(df[sample(nrow(df),100), ], "data/data.csv", row.names=FALSE)
    return(df)
}


