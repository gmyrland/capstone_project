# Build data frame for use in modelling, including all relevant features

build_ml_dataset <- function() {
    # Load dataset
    cases <- tbl(src_sqlite(db_path), "Cases") %>% as.data.frame
    events <- tbl(src_sqlite(db_path), "Events") %>% as.data.frame
    vehicles <- tbl(src_sqlite(db_path), "Vehicles") %>% as.data.frame
    persons <- tbl(src_sqlite(db_path), "Persons") %>% as.data.frame(n=-1)
    ems <- tbl(src_sqlite(db_path), "EMS") %>% as.data.frame
    genvehicle <- tbl(src_sqlite(db_path), "GeneralVehicle") %>% as.data.frame
    occupants <- tbl(src_sqlite(db_path), "Occupants") %>% as.data.frame(n=-1)
    safety <- tbl(src_sqlite(db_path), "Safety") %>% as.data.frame
    vehicleext <- tbl(src_sqlite(db_path), "VehicleExterior") %>% as.data.frame
    vehicleint <- tbl(src_sqlite(db_path), "VehicleInterior") %>% as.data.frame

    df <- occupants
    return(df)
}


