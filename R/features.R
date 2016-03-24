# Build data frame for use in modelling, including all relevant features

#build_ml_dataset <- function() {
    # Join data to form rows by occupant
    df_joined <- read_db("Occupants") %>%
        left_join(read_db("Persons"), c(
            "CaseId"="CaseId",
            "Occupant_VehicleNumber"="Person_VehicleNumber",
            "Occupant_OccupantNumber"="Person_OccNumber"
        )) %>%
        left_join(read_db("Cases"), c("CaseId" = "CaseId")) %>%
        left_join(
            # requires aggregation to single case, use 1st event
            read_db("Events") %>% filter(Event_EventNumber == 1), c(
                "CaseId" = "CaseId",
                "Occupant_VehicleNumber" = "Event_VehicleNumber"
        )) %>%
        left_join(read_db("Vehicles"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Vehicle_VehicleNumber"
        )) %>%
        # ems: requires aggregation to single case, ignore
        left_join(read_db("GeneralVehicle"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "GeneralVehicle_VehicleNumber"
        )) %>%
        left_join(read_db("Safety"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "Safety_VehicleNumber"
        )) %>%
        left_join(read_db("VehicleExterior"), c(
            "CaseId" = "CaseId",
            "Occupant_VehicleNumber" = "VehicleExterior_VehicleNumber"
        )) %>%
        left_join(read_db("VehicleInterior"), c(
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

    df <- df_joined
    
    # Remove leading, repeated, and trailing, whitespace
    df[] <- lapply(df, function(x) gsub("^\\s*|(?<=\\s)\\s+|\\s*$", "", x, perl=TRUE))
    
    ## Convert Unknowns to NAs (to help with overrides from multiple fields)
    df[df == 'Unknown'] <- NA
    df[df == 'Not Reported'] <- NA
    df[df == ''] <- NA
    df[df == 'N/A'] <- NA

    ## Build features from joined dataset
    df <- df %>% transmute(
        CaseId,
        ## Explanatory Variables
        # Crash
        vehicles_involved = ifelse(is.na(CaseForm_Vehicles_NumberVehicles), Case_NumOfVehicle, CaseForm_Vehicles_NumberVehicles),
        #date = CaseForm_Crash_CrashDate, # lower the dimension
        dayofweek = CaseForm_Crash_DayOfWeek,
        is_weeked = as.integer(dayofweek %in% c('Saturday', 'Sunday')),
        crashtime = CaseForm_Crash_CrashTime, #hrs
        month = CaseForm_Crash_Month,
        # season,
        year = as.integer(CaseForm_Crash_Year),
        crash_type = CaseForm_CaseSummary_CrashType,
        crash_config = CaseForm_CaseSummary_Configuration,
        child_seat_present = CaseForm_List_CHILDSEATQTY,
        
        # Occupant
        age = as.double(Occupant_Occupant_Age) * ifelse(tolower(Occupant_Occupant_Age_UOM) == 'months', 1/12, 1), # years
        height = as.integer(ifelse(is.na(Occupant_Occupant_Height), Occupant_Occupant_OccHeight, Occupant_Occupant_Height)), #cms
        weight = as.integer(Occupant_Occupant_Weight), # kgs
        role = ifelse(is.na(Occupant_Occupant_Role), Person_Role, Occupant_Occupant_Role),
        race = Occupant_Occupant_Race, # use a mapping
        sex = ifelse(is.na(Occupant_Occupant_Sex), "Unknown", ifelse(Occupant_Occupant_Sex == 'Male', 'Male', 'Female')),
        eyewear = Occupant_Occupant_EyeWear,
        airbag_available_police = Occupant_Occupant_PoliceReportedAirBagAvail_Function,
        airbag_available = Occupant_Seat_SeatComponents_AirBagAvailable,
        airbag_deployment = Occupant_Airbag_AirbagObject_Function_SystemDeployment,
        seatbelt_availability = ifelse(is.na(Occupant_SeatBelt_ManualBelt_Belt_Availability), Occupant_SeatBelt_AutomaticBelt_AvailabilityFunction, Occupant_SeatBelt_ManualBelt_Belt_Availability),
        seatbelt_used_police = Occupant_Occupant_PoliceReportedBeltUse,
        seatbelt_used = ifelse(is.na(Occupant_SeatBelt_ManualBelt_Belt_UsedInCrash), Occupant_SeatBelt_AutomaticBelt_UsedInCrash, Occupant_SeatBelt_ManualBelt_Belt_UsedInCrash),
        seat_row = Occupant_Seat_SeatGeneral_Row,
        seat_location = Occupant_Seat_SeatGeneral_Location,
        seat_position = Person_Seat,
        entrapment = Occupant_Entrapment_Entrapment,
        posture = Occupant_Seat_SeatGeneral_Posture,
        seat_orientation = Occupant_Seat_SeatComponents_SeatOrientation,
        seat_inclination = Occupant_Seat_SeatComponents_SeatBackPriorInclination,
        
        # Vehicle
        vehicle_year = ifelse(is.na(Vehicle_Year), ifelse(is.na(VehicleExterior_Vehicle_ModelYear), GeneralVehicle_Vehicle_ModelYear, VehicleExterior_Vehicle_ModelYear), Vehicle_Year),
        make = ifelse(is.na(Vehicle_Make), ifelse(is.na(VehicleExterior_Vehicle_Make), GeneralVehicle_Vehicle_Make, VehicleExterior_Vehicle_Make), Vehicle_Make),
        mode = ifelse(is.na(Vehicle_Model), ifelse(is.na(VehicleExterior_Vehicle_Model), GeneralVehicle_Vehicle_Model, VehicleExterior_Vehicle_Model), Vehicle_Model),
        event_class = Event_Class,
        damage_area = Event_AreaOfDamage,
        contacted_area = Event_ContactedAreaOfDamage,
        contated = Event_Contacted,
        contacted_class = Event_ContactedClass,
        damage_plane = Vehicle_DamagePlane,
        body_category = GeneralVehicle_Vehicle_BodyCategory,
        body_type = ifelse(is.na(VehicleExterior_Vehicle_BodyType), GeneralVehicle_Vehicle_BodyType, VehicleExterior_Vehicle_BodyType),
        special_use = ifelse(ifelse(is.na(VehicleExterior_Vehicle_VehSpecialUse), GeneralVehicle_Vehicle_VehSpecialUse , VehicleExterior_Vehicle_VehSpecialUse) == 'No Special Use', 0, 1),
        vehicle_inspection = ifelse(is.na(VehicleExterior_Vehicle_Inspection), GeneralVehicle_Vehicle_Inspection, VehicleExterior_Vehicle_Inspection),
        curb_weight = as.double(ifelse(is.na(VehicleExterior_Vehicle_CurbWeight), ifelse(is.na(VehicleExterior_Specifications_CurbWeight), ifelse(is.na(GeneralVehicle_Vehicle_CurbWeight), GeneralVehicle_Specifications_CurbWeight, GeneralVehicle_Vehicle_CurbWeight), VehicleExterior_Specifications_CurbWeight), VehicleExterior_Vehicle_CurbWeight)), #kgs
        cargo_weight = as.double(ifelse(is.na(VehicleExterior_Vehicle_CargoWeight), GeneralVehicle_Vehicle_CargoWeight, VehicleExterior_Vehicle_CargoWeight)), #kgs
        wheelbase = as.double(ifelse(is.na(VehicleExterior_Specifications_Wheelbase), GeneralVehicle_Specifications_Wheelbase, VehicleExterior_Specifications_Wheelbase)), # cms
        overall_length = as.double(ifelse(is.na(VehicleExterior_Specifications_OverallLength), GeneralVehicle_Specifications_OverallLength, VehicleExterior_Specifications_OverallLength)), #cms
        maximum_width = as.double(ifelse(is.na(VehicleExterior_Specifications_MaximumWidth), GeneralVehicle_Specifications_MaximumWidth, VehicleExterior_Specifications_MaximumWidth)), #cms
        average_track = as.double(ifelse(is.na(VehicleExterior_Specifications_AverageTrack), GeneralVehicle_Specifications_AverageTrack, VehicleExterior_Specifications_AverageTrack)), #cms
        front_overhang = as.double(ifelse(is.na(VehicleExterior_Specifications_FrontOverhang), GeneralVehicle_Specifications_FrontOverhang, VehicleExterior_Specifications_FrontOverhang)), #cms
        rear_overhang = as.double(ifelse(is.na(VehicleExterior_Specifications_RearOverhang), GeneralVehicle_Specifications_RearOverhang, VehicleExterior_Specifications_RearOverhang)), #cms
        undeformed_end_Width = as.double(ifelse(is.na(VehicleExterior_Specifications_UndeformedEndWidth), GeneralVehicle_Specifications_UndeformedEndWidth, VehicleExterior_Specifications_UndeformedEndWidth)), #cms
        cylinders = ifelse(is.na(VehicleExterior_Specifications_EngineCylinders), GeneralVehicle_Specifications_EngineCylinders, VehicleExterior_Specifications_EngineCylinders),
        displacement = as.double(ifelse(is.na(VehicleExterior_Specifications_EngineDisplacement), GeneralVehicle_Specifications_EngineDisplacement, VehicleExterior_Specifications_EngineDisplacement)), #L
        travel_speed = as.double(GeneralVehicle_OfficialRecords_PoliceReport_TravelSpeed), # kph
        posted_speed = as.double(GeneralVehicle_OfficialRecords_PoliceReport_PostedSpeedLimit), #kph
        alcohol_present = GeneralVehicle_OfficialRecords_Driver_PARAlcoholPresent,
        alcohol_test = GeneralVehicle_OfficialRecords_Driver_AlcoholTest,
        alcohol_test_result = as.double(GeneralVehicle_OfficialRecords_Driver_TestResult),
        drugs_present = ifelse(GeneralVehicle_OfficialRecords_Driver_PAROtherDrugPresent == 'Yes other drug(s) present' | GeneralVehicle_OfficialRecords_Driver_OtherDrugTestResult == 'Drug(s) found in specimen', 1, 0),
        drive_race = ifelse(is.na(GeneralVehicle_OfficialRecords_Driver_Race), GeneralVehicle_OfficialRecords_Driver_RaceEthnicOrigin, GeneralVehicle_OfficialRecords_Driver_Race),
        travel_lanes = ifelse(is.na(GeneralVehicle_Precrash_Environment_Roadway_TravelLanes), GeneralVehicle_Precrash_Roadway_TravelLanes, GeneralVehicle_Precrash_Environment_Roadway_TravelLanes),
        roadway_alignment = ifelse(is.na(GeneralVehicle_Precrash_Environment_Roadway_Alignment), GeneralVehicle_Precrash_Roadway_Alignment, GeneralVehicle_Precrash_Environment_Roadway_Alignment),
        roadway_profile = ifelse(is.na(GeneralVehicle_Precrash_Environment_Roadway_Profile), GeneralVehicle_Precrash_Roadway_Profile, GeneralVehicle_Precrash_Environment_Roadway_Profile),
        roadway_surface = ifelse(is.na(GeneralVehicle_Precrash_Environment_Roadway_SurfaceType), GeneralVehicle_Precrash_Roadway_SurfaceType, GeneralVehicle_Precrash_Environment_Roadway_SurfaceType),
        roadway_condition = ifelse(is.na(GeneralVehicle_Precrash_Environment_Roadway_SurfaceCondition), GeneralVehicle_Precrash_Roadway_SurfaceCondition, GeneralVehicle_Precrash_Environment_Roadway_SurfaceCondition),
        light = ifelse(is.na(GeneralVehicle_Precrash_Environment_Conditions_Light), GeneralVehicle_Precrash_Conditions_Light, GeneralVehicle_Precrash_Environment_Conditions_Light),
        weather = ifelse(is.na(GeneralVehicle_Precrash_Environment_Conditions_Weather), GeneralVehicle_Precrash_Conditions_Atmosphere, GeneralVehicle_Precrash_Environment_Conditions_Weather),
        traffic_control_device = ifelse(is.na(GeneralVehicle_Precrash_Environment_TrafficControlDevices_Device), GeneralVehicle_Precrash_TrafficControlDevices_Device, GeneralVehicle_Precrash_Environment_TrafficControlDevices_Device),
        traffic_control_device_functioning = ifelse(is.na(GeneralVehicle_Precrash_Environment_TrafficControlDevices_Functioning), GeneralVehicle_Precrash_TrafficControlDevices_Functioning, GeneralVehicle_Precrash_Environment_TrafficControlDevices_Functioning),
        preevent_movement = ifelse(is.na(GeneralVehicle_Precrash_Movement_PreeventMovement), GeneralVehicle_Driver_PreeventMovement, GeneralVehicle_Precrash_Movement_PreeventMovement),
        precrash_category = ifelse(is.na(GeneralVehicle_Precrash_Movement_CriticalPrecrashCat), GeneralVehicle_Driver_CriticalPrecrashCat, GeneralVehicle_Precrash_Movement_CriticalPrecrashCat),
        precrash_event = ifelse(is.na(GeneralVehicle_Precrash_Movement_CriticalPrecrashEvent), GeneralVehicle_Driver_CriticalPrecrashEvent, GeneralVehicle_Precrash_Movement_CriticalPrecrashEvent),
        avoidance_maneuver = ifelse(is.na(GeneralVehicle_Precrash_Movement_AttemptedAvoidanceManeuver), GeneralVehicle_Driver_AttemptedAvoidanceManeuver, GeneralVehicle_Precrash_Movement_AttemptedAvoidanceManeuver),
        preimpact_stability = ifelse(is.na(GeneralVehicle_Precrash_Movement_PreimpactStability), GeneralVehicle_Driver_PreimpactStability, GeneralVehicle_Precrash_Movement_PreimpactStability),
        preimpact_location = ifelse(is.na(GeneralVehicle_Precrash_Movement_PreimpactLocation), GeneralVehicle_Driver_PreimpactLocation, GeneralVehicle_Precrash_Movement_PreimpactLocation),
        rollover = GeneralVehicle_Rollover_Data_Type,
        rollover_qtr_turns = GeneralVehicle_Rollover_Data_QuarterTurns,
        rollover_prerollover = GeneralVehicle_Rollover_PreRollover_Maneuver,
        rollover_initiation = GeneralVehicle_Rollover_Initiation_Type,
        rollover_contacted = GeneralVehicle_Rollover_Initiation_ObjectContactedClass,
        towed_unit = GeneralVehicle_Reconstruction_ReconstructionData_TowedTrailingUnit,
        transmission = ifelse(is.na(VehicleExterior_Specifications_TransmissionType), VehicleExterior_Tire_General_TransmissionType, VehicleExterior_Specifications_TransmissionType),
        drive_Wheels = ifelse(is.na(VehicleExterior_Specifications_DriveWheels), VehicleExterior_Tire_General_DriveWheels, VehicleExterior_Specifications_DriveWheels),
        fire = ifelse(!is.na(VehicleExterior_Fire_Occurrence), 1, 0),
        #do_plot(as.double(df$VehicleExterior_Tire_General_GVWR)), #kgs
        #do_plot(df$VehicleExterior_Tire_General_GAWRFront), #kgs
        #do_plot(df$VehicleExterior_Tire_General_GAWRRear), #kgs
        tire_tread_depth = as.double(VehicleExterior_Tire_General_Tires_TireObject_TreadDepth),
        compartment_integrity_loss = VehicleInterior_Integrity_PassengerCompartmentIntegrityLoss_PassCompIntegrity,
        odometer = as.double(VehicleInterior_Instrument_OdometerReading), #kms
        
        #Change to representative sample
        #df_fatal <- df_joined[df_joined$Occupant_Injury_Treatment_Mortality == 'Fatal', ]
        #df_nonfatal <- df_joined[df_joined$Occupant_Injury_Treatment_Mortality != 'Fatal', ]
        #df <- rbind(df_fatal, df_nonfatal[sample(nrow(df_nonfatal), nrow(df_fatal)), ])
        #do_plot(as.double(df$Occupant_Occupant_Weight), t="v")
        #do_plot(df$Person_Restraints)
        #do_plot(as.double(df$GeneralVehicle_OfficialRecords_PoliceReport_TravelSpeed),t="v") # for ref
        #do_plot(as.double(df$GeneralVehicle_OfficialRecords_PoliceReport_PostedSpeedLimit), t="v") #for ref
        
        # other potential features
        # n in vehicle
        
        # Response Variable
        fatal = as.integer(Occupant_Injury_Treatment_Mortality == 'Fatal')
    )
    
    sapply(df, function(x) length(unique(x)))
    
    ## Lookup Tables
    
    # Convert NA's in character vectors back to "Unknown"
    # Create logical vector of character columns for subsetting
    chr_cols <- sapply(df, is.character)
    df[chr_cols][is.na(df[chr_cols])] <- 'Unknown'
    
    # Populate NAs in numeric fields?
    num_cols <- sapply(df, is.numeric)
    applymed <- function(x) {
        m <- median(x, na.rm=TRUE)
        x <- ifelse(is.na(x), m, x)
        (x)
    }
    df[num_cols] <- lapply(df[num_cols], applymed)
    
    # Convert characters to factors
    factorize <- function(x) {
        lvls <- unique(x)
        if ('Unknown' %in% lvls) lvls <- c('Unknown', lvls[lvls != 'Unknown'])
        factor(x, levels=lvls)
    }
    df[chr_cols] <- lapply(df[chr_cols], factorize)
    
    
    #sapply(df...
    
    
    # Scale numeric attributes
    
    # change this document to data cleaning
    # maybe include weights as well
    # assessment:
    # accuracy (good quote on page 14 about imbalance)
    # precision, recall, F-Measure, G-mean
    # precision-recall curve
    
    #write.csv(df[sample(nrow(df),100), ], "data/data.csv", row.names=FALSE)
    #return(df)
#}

## Testing...
    do_plot <- function(feature, t="j") {
        df %>% transmute(
            fatal = Occupant_Injury_Treatment_Mortality=='Fatal',
            feature = feature
        ) %>%
            ggplot(aes(factor(fatal), feature)) %>%
            {ifelse(t=="v", return(. + geom_violin()), return(. + geom_jitter(alpha=0.1)))}
    }
    
    df$vehicles_involved %>% {sort(table(.), desc=TRUE)}
    df$dayofweek %>% {sort(table(.), descending=TRUE)}
    df$is_weeked %>% {sort(table(.), descending=TRUE)}
    df$crashtime %>% {sort(table(.), descending=TRUE)}
    df$month %>% {sort(table(.), descending=TRUE)}
    df$year %>% {sort(table(.), descending=TRUE)}
    df$crash_type %>% {sort(table(.), descending=TRUE)}
    df$crash_config %>% {sort(table(.), descending=TRUE)}
    df$child_seat_present %>% {sort(table(.), descending=TRUE)}
    df$age %>% {sort(table(.), descending=TRUE)}
    df$height %>% {sort(table(.), descending=TRUE)}
    df$weight %>% {sort(table(.), descending=TRUE)}
    df$role %>% {sort(table(.), descending=TRUE)}
    df$race %>% {sort(table(.), descending=TRUE)}
    df$sex %>% {sort(table(.), descending=TRUE)}
    df$eyewear %>% {sort(table(.), descending=TRUE)}
    df$airbag_available_police %>% {sort(table(.), descending=TRUE)}
    df$airbag_available %>% {sort(table(.), descending=TRUE)}
    df$airbag_deployment %>% {sort(table(.), descending=TRUE)}
    df$seatbelt_availability %>% {sort(table(.), descending=TRUE)}
    df$seatbelt_used_police %>% {sort(table(.), descending=TRUE)}
    df$seatbelt_used %>% {sort(table(.), descending=TRUE)}
    df$seat_row %>% {sort(table(.), descending=TRUE)}
    df$seat_location %>% {sort(table(.), descending=TRUE)}
    df$seat_position %>% {sort(table(.), descending=TRUE)}
    df$entrapment %>% {sort(table(.), descending=TRUE)}
    df$posture %>% {sort(table(.), descending=TRUE)}
    df$seat_orientation %>% {sort(table(.), descending=TRUE)}
    df$seat_inclination %>% {sort(table(.), descending=TRUE)}
    df$vehicle_year %>% {sort(table(.), descending=TRUE)}
    df$make %>% {sort(table(.), descending=TRUE)}
    df$mode %>% {sort(table(.), descending=TRUE)}
    df$event_class %>% {sort(table(.), descending=TRUE)}
    df$damage_area %>% {sort(table(.), descending=TRUE)}
    df$contacted_area %>% {sort(table(.), descending=TRUE)}
    df$contated %>% {sort(table(.), descending=TRUE)}
    df$contacted_class %>% {sort(table(.), descending=TRUE)}
    df$damage_plane %>% {sort(table(.), descending=TRUE)}
    df$body_category %>% {sort(table(.), descending=TRUE)}
    df$body_type %>% {sort(table(.), descending=TRUE)}
    df$special_use %>% {sort(table(.), descending=TRUE)}
    df$vehicle_inspection %>% {sort(table(.), descending=TRUE)}
    df$curb_weight %>% {sort(table(.), descending=TRUE)}
    df$cargo_weight %>% {sort(table(.), descending=TRUE)}
    df$wheelbase %>% {sort(table(.), descending=TRUE)}
    df$overall_length %>% {sort(table(.), descending=TRUE)}
    df$maximum_width %>% {sort(table(.), descending=TRUE)}
    df$average_track %>% {sort(table(.), descending=TRUE)}
    df$front_overhang %>% {sort(table(.), descending=TRUE)}
    df$rear_overhang %>% {sort(table(.), descending=TRUE)}
    df$undeformed_end_Width %>% {sort(table(.), descending=TRUE)}
    df$cylinders %>% {sort(table(.), descending=TRUE)}
    df$displacement %>% {sort(table(.), descending=TRUE)}
    df$travel_speed %>% {sort(table(.), descending=TRUE)}
    df$posted_speed %>% {sort(table(.), descending=TRUE)}
    df$alcohol_present %>% {sort(table(.), descending=TRUE)}
    df$alcohol_test %>% {sort(table(.), descending=TRUE)}
    df$alcohol_test_result %>% {sort(table(.), descending=TRUE)}
    df$drugs_present %>% {sort(table(.), descending=TRUE)}
    df$drive_race %>% {sort(table(.), descending=TRUE)}
    df$travel_lanes %>% {sort(table(.), descending=TRUE)}
    df$roadway_alignment %>% {sort(table(.), descending=TRUE)}
    df$roadway_profile %>% {sort(table(.), descending=TRUE)}
    df$roadway_surface %>% {sort(table(.), descending=TRUE)}
    df$roadway_condition %>% {sort(table(.), descending=TRUE)}
    df$light %>% {sort(table(.), descending=TRUE)}
    df$weather %>% {sort(table(.), descending=TRUE)}
    df$traffic_control_device %>% {sort(table(.), descending=TRUE)}
    df$traffic_control_device_functioning %>% {sort(table(.), descending=TRUE)}
    df$preevent_movement %>% {sort(table(.), descending=TRUE)}
    df$precrash_category %>% {sort(table(.), descending=TRUE)}
    df$precrash_event %>% {sort(table(.), descending=TRUE)}
    df$avoidance_maneuver %>% {sort(table(.), descending=TRUE)}
    df$preimpact_stability %>% {sort(table(.), descending=TRUE)}
    df$preimpact_location %>% {sort(table(.), descending=TRUE)}
    df$rollover %>% {sort(table(.), descending=TRUE)}
    df$rollover_qtr_turns %>% {sort(table(.), descending=TRUE)}
    df$rollover_prerollover %>% {sort(table(.), descending=TRUE)}
    df$rollover_initiation %>% {sort(table(.), descending=TRUE)}
    df$rollover_contacted %>% {sort(table(.), descending=TRUE)}
    df$towed_unit %>% {sort(table(.), descending=TRUE)}
    df$transmission %>% {sort(table(.), descending=TRUE)}
    df$drive_Wheels %>% {sort(table(.), descending=TRUE)}
    df$fire %>% {sort(table(.), descending=TRUE)}
    df$tire_tread_depth %>% {sort(table(.), descending=TRUE)}
    df$compartment_integrity_loss %>% {sort(table(.), descending=TRUE)}
    df$odometer %>% {sort(table(.), descending=TRUE)}
    
