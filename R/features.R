# Build data frame for use in modelling, including all relevant features

build_ml_dataset <- function() {
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

    df <- df_joined
    
    # Remove leading, repeated, and trailing, whitespace
    df[] <- lapply(df, function(x) gsub("^\\s*|(?<=\\s)\\s+|\\s*$", "", x, perl=TRUE))
    
    ## Convert Unknowns to NAs (to help with overrides from multiple fields)
    df[df == 'Unknown'] <- NA
    df[df == 'Not Reported'] <- NA
    df[df == ''] <- NA
    df[df == 'N/A'] <- NA

    ## Build features from joined dataset
    df <- df %>%
        transmute(
            CaseId,
            CaseWeight = CaseForm_Crash_Weight,
            ## Explanatory Variables
            # Crash
            vehicles_involved = ifelse(is.na(CaseForm_Vehicles_NumberVehicles), Case_NumOfVehicle, CaseForm_Vehicles_NumberVehicles),
            #date = CaseForm_Crash_CrashDate, # lower the dimension
            dayofweek = CaseForm_Crash_DayOfWeek,
            is_weeked = as.integer(dayofweek %in% c('Saturday', 'Sunday')),
            crashtime = CaseForm_Crash_CrashTime %>% {as.numeric(sub("...$","",.)) * 60 + as.numeric(sub("^...","",.))}, #hrs
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
            model = ifelse(is.na(Vehicle_Model), ifelse(is.na(VehicleExterior_Vehicle_Model), GeneralVehicle_Vehicle_Model, VehicleExterior_Vehicle_Model), Vehicle_Model),
            event_class = Event_Class,
            damage_area = Event_AreaOfDamage,
            contacted_area = Event_ContactedAreaOfDamage,
            contacted = Event_Contacted,
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
            fatal = factor(Occupant_Injury_Treatment_Mortality, levels=c('Not Fatal', 'Fatal'))
        )
    
    
    
    ## Lookup Tables
    
    # Convert NA's in character vectors back to "Unknown"
    # Create logical vector of character columns for subsetting
    chr_cols <- sapply(df, is.character)
    df[chr_cols][is.na(df[chr_cols])] <- 'Unknown'
    
    # Populate NAs in numeric fields?
    num_cols <- sapply(df, is.numeric)
    applymed <- function(x) {
        #x[x < 1000] <- NA # Several dummy NAs
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
    
    # Scale numeric attributes
    scale_nums <- function(x) {
        if (any(x > 1)) scale(x) else (x)
    }
    df[num_cols] <- lapply(df[num_cols], scale_nums)

    # change this document to data cleaning
    # assessment:
    # accuracy (good quote on page 14 about imbalance)
    # precision, recall, F-Measure, G-mean
    # precision-recall curve
    
    #write.csv(df[sample(nrow(df),100), ], "data/data.csv", row.names=FALSE)
    return(df)
}

## Testing...
    #sapply(df[chr_cols], function(x) length(unique(x)))
    #lapply(df[num_cols], hist)
    
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
    
    
    do_plot <- function(feature, t="j") {
        df %>% transmute(
            fatal = Occupant_Injury_Treatment_Mortality=='Fatal',
            feature = feature
        ) %>%
            ggplot(aes(factor(fatal), feature)) %>%
            {ifelse(t=="v", return(. + geom_violin()), return(. + geom_jitter(alpha=0.1)))}
    }
    
    df$vehicles_involved %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$dayofweek %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$is_weeked %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$crashtime %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$month %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$year %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$crash_type %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$crash_config %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$child_seat_present %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$age %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$height %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$weight %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$role %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$race %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$sex %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$eyewear %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$airbag_available_police %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$airbag_available %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$airbag_deployment %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seatbelt_availability %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seatbelt_used_police %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seatbelt_used %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seat_row %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seat_location %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seat_position %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$entrapment %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$posture %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seat_orientation %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$seat_inclination %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$vehicle_year %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$make %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$mode %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$event_class %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$damage_area %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$contacted_area %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$contated %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$contacted_class %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$damage_plane %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$body_category %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$body_type %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$special_use %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$vehicle_inspection %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$curb_weight %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$cargo_weight %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$wheelbase %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$overall_length %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$maximum_width %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$average_track %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$front_overhang %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$rear_overhang %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$undeformed_end_Width %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$cylinders %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$displacement %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$travel_speed %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$posted_speed %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$alcohol_present %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$alcohol_test %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$alcohol_test_result %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$drugs_present %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$drive_race %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$travel_lanes %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$roadway_alignment %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$roadway_profile %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$roadway_surface %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$roadway_condition %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$light %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$weather %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$traffic_control_device %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$traffic_control_device_functioning %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$preevent_movement %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$precrash_category %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$precrash_event %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$avoidance_maneuver %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$preimpact_stability %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$preimpact_location %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$rollover %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$rollover_qtr_turns %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$rollover_prerollover %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$rollover_initiation %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$rollover_contacted %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$towed_unit %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$transmission %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$drive_Wheels %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$fire %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$tire_tread_depth %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$compartment_integrity_loss %>% {print(class(.)); sort(table(.), desc=TRUE)}
    df$odometer %>% {print(class(.)); sort(table(.), desc=TRUE)}
    
