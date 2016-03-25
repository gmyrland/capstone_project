# Build data frame for use in modelling, including all relevant features

build_clean_dataset <- function() {
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

    df_raw <- df_joined
    
    # Remove leading, repeated, and trailing, whitespace
    df_raw[] <- lapply(df_raw, function(x) gsub("^\\s*|(?<=\\s)\\s+|\\s*$", "", x, perl=TRUE))
    
    ## Convert Unknowns to NAs (to help with overrides from multiple fields)
    df_raw[df_raw == 'Unknown'] <- NA
    df_raw[df_raw == 'Not Reported'] <- NA
    df_raw[df_raw == ''] <- NA
    df_raw[df_raw == 'N/A'] <- NA

    df <- df_raw
    
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
            child_seat_present = as.integer(CaseForm_List_CHILDSEATQTY == 'Yes'),
            
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
            seatbelt_used = Occupant_Occupant_PoliceReportedBeltUse,
            seat_row = Occupant_Seat_SeatGeneral_Row,
            seat_location = Occupant_Seat_SeatGeneral_Location,
            seat_position = Person_Seat,
            entrapment = Occupant_Entrapment_Entrapment,
            posture = Occupant_Seat_SeatGeneral_Posture,
            seat_orientation = Occupant_Seat_SeatComponents_SeatOrientation,
            seat_inclination = Occupant_Seat_SeatComponents_SeatBackPriorInclination,
            
            # Vehicle
            vehicle_year = as.double(ifelse(is.na(Vehicle_Year), ifelse(is.na(VehicleExterior_Vehicle_ModelYear), GeneralVehicle_Vehicle_ModelYear, VehicleExterior_Vehicle_ModelYear), Vehicle_Year)),
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
            driver_race = ifelse(is.na(GeneralVehicle_OfficialRecords_Driver_Race), GeneralVehicle_OfficialRecords_Driver_RaceEthnicOrigin, GeneralVehicle_OfficialRecords_Driver_Race),
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
            avoidance_maneuver = ifelse(is.na(GeneralVehicle_Precrash_Movement_AttemptedAvoidanceManeuver), GeneralVehicle_Driver_AttemptedAvoidanceManeuver, GeneralVehicle_Precrash_Movement_AttemptedAvoidanceManeuver),
            preimpact_stability = ifelse(is.na(GeneralVehicle_Precrash_Movement_PreimpactStability), GeneralVehicle_Driver_PreimpactStability, GeneralVehicle_Precrash_Movement_PreimpactStability),
            preimpact_location = ifelse(is.na(GeneralVehicle_Precrash_Movement_PreimpactLocation), GeneralVehicle_Driver_PreimpactLocation, GeneralVehicle_Precrash_Movement_PreimpactLocation),
            rollover = GeneralVehicle_Rollover_Data_Type,
            rollover_qtr_turns = GeneralVehicle_Rollover_Data_QuarterTurns,
            rollover_contacted = GeneralVehicle_Rollover_Initiation_ObjectContactedClass,
            towed_unit = GeneralVehicle_Reconstruction_ReconstructionData_TowedTrailingUnit,
            transmission = ifelse(is.na(VehicleExterior_Specifications_TransmissionType), VehicleExterior_Tire_General_TransmissionType, VehicleExterior_Specifications_TransmissionType),
            drive_Wheels = ifelse(is.na(VehicleExterior_Specifications_DriveWheels), VehicleExterior_Tire_General_DriveWheels, VehicleExterior_Specifications_DriveWheels),
            fire = ifelse(!is.na(VehicleExterior_Fire_Occurrence), 1, 0),
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
    
    ## Reduce and Scale Dimensions
    df$vehicles_involved[as.integer(df$vehicles_involved) >= 5] <- "More"
    df$crashtime <- df$crashtime / (24 * 60)
    df$year <- df$year - 2004
    df$age <- df$age / 100
    df$height <- log(df$height+0.001)
    df$weight <- log(df$weight+0.001)
    df$role[df$role == 'Unknown'] <- 'Passenger'
    df$race <- sub(' \\(.*\\)$', '', df$race)
    df$race[df$race == 'No driver present'] <- 'Unknown'
    df$eyewear[!df$eyewear %in% c('Unknown', 'No')] <- 'Yes'
    df$airbag_available_police[grepl('unknown|did not indicate', tolower(df$airbag_available_police))] <- 'Unknown'
    df$airbag_available_police[df$airbag_available_police == 'No air bag available'] <- 'Not deployed'
    df$airbag_deployment[grepl('unknown|did not indicate', tolower(df$airbag_deployment))] <- 'Unknown'
    df$airbag_deployment[grepl('Deployed', df$airbag_deployment)] <- 'Deployed'
    df$airbag_deployment[df$airbag_deployment == 'No air bag available'] <- 'Not deployed'
    df$seatbelt_availability <- c(
        'Lap and shoulder belt' = 'Yes', 'None available' = 'No',
        'Unknown' = 'Unknown', 'Lap Belt' = 'Yes', 'Not Equipped/Not Available' = 'No',
        'Belt available - type unknown' = 'Yes', 'Belt removed/destroyed' = 'No',
        'Automatic type unknown' = 'Yes', 'Other Belt' = 'Yes',
        'Two point automatic belts' = 'Yes', 'Lap (shoulder destroyed/removed)' = 'Damaged',
        'Shoulder Belt' = 'Yes', 'Shoulder (lap destroyed/removed)' = 'Damaged')[df$seatbelt_availability] %>% as.character
    df$seatbelt_used <- c(
        'Automatic belt' = 'Yes', 'Other type belt' = 'Yes', 'Shoulder Belt' = 'Yes', 'Unknown' = 'Unknown',
        'Lap Belt' = 'Yes', 'Child safety seat' = 'Child Seat', 'Police did not indicate belt use' = 'Unknown',
        "Police indicated 'unknown'" = 'Unknown', 'Belt used, type not specified' = 'Yes',
        'None Used' = 'No', 'Lap and shoulder belt' = 'Yes')[df$seatbelt_used] %>% as.character
    df$seat_row[df$seat_row %in% c('0')] <- 'Unknown'
    df$seat_row[df$seat_row %in% c('3','4','5')] <- 'Farther Back'
    df$seat_location[df$seat_location %in% c('0', '9', '4')] <- 'Unknown'
    df$seat_position[df$seat_position %in%  names(table(df$seat_position))[table(df$seat_position) < 20]] <- 'Unknown'
    df$entrapment[grepl('Could|restrained', df$entrapment)] <- 'Yes'
    df$entrapment[grepl('Not trapped', df$entrapment)] <- 'No'
    df$posture[grepl("Kneeling|lap of another occupant|console|Lying on or across|Bracing", df$posture)] <- 'Improper'
    df$posture[df$posture == 'Other posture'] <- 'Unknown'
    df$seat_orientation[!df$seat_orientation %in% c('Unknown', 'Forward facing seat')] <- 'Other'
    df$seat_inclination <- c(
        'Not Applicable' = 'Unknown', 'Completely Reclined' = 'Completely Reclined',
        '9' = 'Unknown', 'Not Adjustable' = 'Upright', 'Slighly Reclined' = 'Slightly Reclined',
        'Upright' = 'Upright', 'Unknown' = 'Unknown',
        'Slightly Reclined' = 'Slightly Reclined')[df$seat_inclination] %>% as.character
    df$vehicle_year <- df$vehicle_year - 2004
    df$make[df$make %in% names(table(df$make))[table(df$make) < 250]] <- 'Other'
    df$model[df$model %in% names(table(df$model))[table(df$model) < 800]] <- 'Other'
    df$event_class <- sub(" \\(.*\\)$", "", df$event_class) # could be reduced further
    df$contacted_area[grepl("Back \\(rear|Bk of unit", df$contacted_area)] <- 'Back of Truck'
    df$contacted <- sub("#.*", "", df$contacted)
    df$contacted[grepl(" pole ", df$contacted)] <- 'Pole' # could be reduced further
    df$contacted_class <- sub(" \\(.*\\)$", "", df$contacted_class) # could be reduced further, similar to event_class
    df$damage_plane <- sub(' Side', '', df$damage_plane)
    df$body_category <- sub(" \\(.*\\)$", "", df$body_category)
    df$body_type[df$body_type %in% names(table(df$body_type))[table(df$body_type) < 500]] <- 'Other'
    df$curb_weight <- log(df$curb_weight+0.001) ## scale all of these instead
    df$cargo_weight <- scale(df$cargo_weight, 11.86, 114)
    df$wheelbase <- log(df$wheelbase+0.001)
    df$overall_length <- log(df$overall_length+0.001)
    df$maximum_width <- scale(df$maximum_width, 182, 11)
    df$average_track <- log(df$average_track+0.001)
    df$front_overhang <- log(df$front_overhang+0.001)
    df$rear_overhang <- log(df$rear_overhang+0.001)
    df$undeformed_end_Width <- log(df$undeformed_end_Width+0.001)
    df$cylinders <- sub("\\+|0", "", df$cylinders)
    df$cylinders[df$cylinders == '-8887'] <- "Unknown"
    df$cylinders[df$cylinders %in% c('8', '12', '14')] <- "8 or more"
    df$travel_speed <- log(df$travel_speed + 0.001)
    df$posted_speed <- log(df$posted_speed + 0.001)
    df$alcohol_present[df$alcohol_present %in% c('No driver present', 'Not reported')] <- 'Unknown'
    df$alcohol_present <- sub(" .*", "", df$alcohol_present)
    df$alcohol_test[!grepl('[Pp]erformed|Unknown', df$alcohol_test)] <- 'No Test Performed'
    df$alcohol_test[grepl('BAC test performed', df$alcohol_test)] <- 'Test Performed'
    df$alcohol_test_result <- log(df$alcohol_test_result+0.001)
    df$driver_race <- sub(' \\(.*\\)$', '', df$driver_race)
    df$travel_lanes <- c('One' = 1, 'Two' = 2, 'Three' = 3, 'Four' = '4', 'Five'= 5,
                        'Six' = 6, 'Seven or more' = 7, 'Unknown' = 2)[df$travel_lanes] %>% as.integer
    df$roadway_alignment[df$roadway_alignment == 'Unknown'] <- 'Straight'
    df$roadway_condition[df$roadway_condition %in% names(table(df$roadway_condition))[table(df$roadway_condition) < 500]] <- 'Other'
    df$weather[df$weather %in% names(table(df$weather))[table(df$weather) < 300]] <- 'Other'
    df$traffic_control_device <- ifelse(df$traffic_control_device == 'No traffic control(s)', 'No', 'Yes')
    df$preevent_movement[df$preevent_movement %in% names(table(df$preevent_movement))[table(df$preevent_movement) < 250]] <- 'Other'
    df$precrash_category[df$precrash_category %in% names(table(df$precrash_category))[table(df$precrash_category) < 250]] <- 'Other'
    
    #df$avoidance_maneuver # split to forward/backward vs left/right?
    df$preimpact_stability[grepl("Skidding", df$preimpact_stability)] <- 'Skidding'
    df$preimpact_stability[grepl("unknown|No driver", df$preimpact_stability)] <- 'Unknown'
    df$preimpact_location[!df$preimpact_location %in% c('Stayed on roadway but left original travel lane', 'Stayed in original travel lane')] <- 'Other'
    df$rollover[grepl("No Rollover", df$rollover)] <- 'No'
    df$rollover[!df$rollover %in% c('Unknown', 'No')] <- 'Yes'
    df$rollover_qtr_turns[df$rollover_qtr_turns %in% c('18','20','13','15','14','16','11','9','12','10','8','7','6','5','End over End')] <- 'More'
    df$rollover_contacted[df$rollover_contacted %in% names(table(df$rollover_contacted))[table(df$rollover_contacted) < 500]] <- 'Unknown'
    df$towed_unit <- sub("[ ,].*", "", df$towed_unit)
    df$drive_Wheels <- gsub(".*\\(|\\)", "", df$drive_Wheels)
    df$tire_tread_depth <- log(df$tire_tread_depth+0.001)
    df$odometer <- log(df$odometer+0.001)
    
    # Convert characters to factors
    chr_cols <- sapply(df, is.character)
    factorize <- function(x) {
        lvls <- unique(x)
        if ('Unknown' %in% lvls) lvls <- c('Unknown', lvls[lvls != 'Unknown'])
        factor(x, levels=lvls)
    }
    df[chr_cols] <- lapply(df[chr_cols], factorize)
    
    # Scale numeric attributes
    # scale_nums <- function(x) {
    #     if (any(x > 10, na.rm=TRUE)) scale(x) else (x)
    # }
    # df[num_cols] <- lapply(df[num_cols], scale_nums)

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
    
    
    # do_plot <- function(feature, t="j") {
    #     df %>% transmute(
    #         fatal = Occupant_Injury_Treatment_Mortality=='Fatal',
    #         feature = feature
    #     ) %>%
    #         ggplot(aes(factor(fatal), feature)) %>%
    #         {ifelse(t=="v", return(. + geom_violin()), return(. + geom_jitter(alpha=0.1)))}
    # }
    # 
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
    df$contacted %>% {print(class(.)); sort(table(.), desc=TRUE)}
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

