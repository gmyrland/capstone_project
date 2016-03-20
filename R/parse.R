##############
## XML Parsing

## Parse all case files to single rectangular data frames and write database
parse_xml <- function() {
    # Generate vector of Case Ids to iterate through
    case_ids <- get_case_ids()
    #case_ids <- sample(case_ids, 1000) # For testing

    # Create dataframe df
    df <- data_frame(id = case_ids)

    # Wrap xml2 functions to return NA for missing elements instead of error
    xml_text <- function(x) {
        v <- xml2::xml_text(x)
        ifelse(!length(v), NA, v)
    }
    xml_attr <- function(x, attr) {
        v <- xml2::xml_attr(x, attr)
        ifelse(!length(v), NA, v)
    }

    for (i in 1:length(case_ids)) {
        # Progress Reporting on Console
        if (!(i %% 1000)) cat(i, fill=TRUE)

        # Fetch case file
        f <- case_path(df$id[i])
        stopifnot(file.exists(f))

        # Read xml data
        data <- read_xml(f)

        ## Parse xml data

        # Data for each case is saved into dataframes, one per eventual sqlite table.
        # Each of these dataframes is then stored in df. The nested dataframe df
        # contains one row per case, and each column is a vector of dataframes
        # (embedded as a single element list), one for each sqlite table. This
        # structure helps to handle the one-to-many relationships in the data.

        # Cases
        case$Case_noNamespaceSchemaLocation <- xml_find_all(data, "/Case") %>% xml_attr("noNamespaceSchemaLocation")
        case$Case_CaseID       <- xml_find_all(data, "/Case") %>% xml_attr("CaseID")
        case$Case_NumOfVehicle <- xml_find_all(data, "/Case") %>% xml_attr("NumOfVehicle")
        case$Case_CaseStr      <- xml_find_all(data, "/Case") %>% xml_attr("CaseStr")
        case$Case_Location     <- xml_find_all(data, "/Case") %>% xml_attr("Location")
        case$Case_Version      <- xml_find_all(data, "/Case") %>% xml_attr("Version")
        case$Case_Author       <- xml_find_all(data, "/Case") %>% xml_attr("Author")
        case$Case_Comment      <- xml_find_all(data, "/Case") %>% xml_attr("Comment")
        case$Case_Origin       <- xml_find_all(data, "/Case") %>% xml_attr("Origin")
        case$CaseForm_Crash_PSU <- xml_find_all(data, "/Case/CaseForm/Crash/PSU") %>% xml_text
        case$CaseForm_Crash_CaseNumber <- xml_find_all(data, "/Case/CaseForm/Crash/CaseNumber") %>% xml_text
        case$CaseForm_Crash_Stratum <- xml_find_all(data, "/Case/CaseForm/Crash/Stratum") %>% xml_text
        case$CaseForm_Crash_Weight <- xml_find_all(data, "/Case/CaseForm/Crash/Weight") %>% xml_text
        case$CaseForm_Crash_CrashDate <- xml_find_all(data, "/Case/CaseForm/Crash/CrashDate") %>% xml_text
        case$CaseForm_Crash_DayOfWeek <- xml_find_all(data, "/Case/CaseForm/Crash/DayOfWeek") %>% xml_text
        case$CaseForm_Crash_CrashTime <- xml_find_all(data, "/Case/CaseForm/Crash/CrashTime") %>% xml_text
        case$CaseForm_Crash_CrashTime_UOM <- xml_find_all(data, "/Case/CaseForm/Crash/CrashTime") %>% xml_attr("UOM")
        case$CaseForm_Crash_Status <- xml_find_all(data, "/Case/CaseForm/Crash/Status") %>% xml_text
        case$CaseForm_Crash_Month <- xml_find_all(data, "/Case/CaseForm/Crash/Month") %>% xml_text
        case$CaseForm_Crash_Year <- xml_find_all(data, "/Case/CaseForm/Crash/Year") %>% xml_text
        case$CaseForm_Crash_PSUStrat <- xml_find_all(data, "/Case/CaseForm/Crash/PSUStrat") %>% xml_text
        case$CaseForm_Crash_SASvariables_SpecifyAccident <- xml_find_all(data, "/Case/CaseForm/Crash/SASvariables/SpecifyAccident") %>% xml_text
        case$CaseForm_Crash_SASvariables_SpecifyConfig <- xml_find_all(data, "/Case/CaseForm/Crash/SASvariables/SpecifyConfig") %>% xml_text
        case$CaseForm_Crash_SASvariables_VehicleType <- xml_find_all(data, "/Case/CaseForm/Crash/SASvariables/VehicleType") %>% xml_text
        case$CaseForm_Crash_SASvariables_ConfigType <- xml_find_all(data, "/Case/CaseForm/Crash/SASvariables/ConfigType") %>% xml_text
        case$CaseForm_Crash_SASvariables_VehForms <- xml_find_all(data, "/Case/CaseForm/Crash/SASvariables/VehForms") %>% xml_text
        case$CaseForm_Crash_SASvariables_Events <- xml_find_all(data, "/Case/CaseForm/Crash/SASvariables/Events") %>% xml_text
        case$CaseForm_CaseSummary_CrashType <- xml_find_all(data, "/Case/CaseForm/CaseSummary/CrashType") %>% xml_text
        case$CaseForm_CaseSummary_Configuration <- xml_find_all(data, "/Case/CaseForm/CaseSummary/Configuration") %>% xml_text
        case$CaseForm_CaseSummary_Summary <- xml_find_all(data, "/Case/CaseForm/CaseSummary/Summary") %>% xml_text

        # Events
        xml_events <- xml_find_all(data, "/Case/CaseForm/Events/EventSum")
        events <- data_frame(CaseId = rep(df$id[i], length(xml_events)))
        for (j in seq_along(xml_events)) {
            events$Event_VehicleNumber[j] <- xml_events[j] %>% xml_attr("VehicleNumber")
            events$Event_EventNumber[j] <- xml_events[j] %>% xml_attr("EventNumber")
            events$Event_StruckVehNumEvent[j] <- xml_events[j] %>% xml_attr("StruckVehNumEvent")
            events$Event_StrikeVehEvent[j] <- xml_events[j] %>% xml_attr("StrikeVehNumEvent")
            events$Event_Class[j] <- xml_find_all(xml_events[j], "./ContactedClass") %>% xml_text
            events$Event_AreaOfDamage[j] <- xml_find_all(xml_events[j], "./AreaOfDamage") %>% xml_text
            events$Event_Contacted[j] <- xml_find_all(xml_events[j], "./Contacted") %>% xml_text
            events$Event_ContactedClass[j] <- xml_find_all(xml_events[j], "./ContactedClass") %>% xml_text
            events$Event_ContactedAreaOfDamage[j] <- xml_find_all(xml_events[j], "./ContactedAreaOfDamage") %>% xml_text
        }
        df$Events[i] <- list(events)

        # Vehicles
        xml_vehicles <- xml_find_all(data, "/Case/CaseForm/Vehicles/VehicleSum")
        vehicles <- data_frame(CaseId = rep(df$id[i], length(xml_vehicles)))
        case$CaseForm_Vehicles_NumberVehicles <- xml_find_all(data, "/Case/CaseForm/Vehicles/NumberVehicles") %>% xml_text
        for (j in seq_along(xml_vehicles)) {
            vehicles$Vehicle_VehicleNumber[j] <- xml_vehicles[j] %>% xml_attr("VehicleNumber")
            vehicles$Vehicle_Year[j] <- xml_find_all(xml_vehicles[j], "./Year") %>% xml_text
            vehicles$Vehicle_Make[j] <- xml_find_all(xml_vehicles[j], "./Make") %>% xml_text
            vehicles$Vehicle_Model[j] <- xml_find_all(xml_vehicles[j], "./Model") %>% xml_text
            vehicles$Vehicle_DamagePlane[j] <- xml_find_all(xml_vehicles[j], "./DamagePlane") %>% xml_text
            vehicles$Vehicle_Severity[j] <- xml_find_all(xml_vehicles[j], "./Severity") %>% xml_text
            vehicles$Vehicle_ComponentFailure[j] <- xml_find_all(xml_vehicles[j], "./ComponentFailure") %>% xml_text
        }
        df$Vehicles[i] <- list(vehicles)

        # Persons
        xml_persons <- xml_find_all(data, "/Case/CaseForm/Persons/Person")
        persons <- data_frame(CaseId = rep(df$id[i], length(xml_persons)))
        for (j in seq_along(xml_persons)) {
            persons$Person_VehicleNumber[j] <- xml_persons[j] %>% xml_attr("VehicleNumber")
            persons$Person_OccNumber[j] <- xml_persons[j] %>% xml_attr("OccNumber")
            persons$Person_Role[j] <- xml_find_all(xml_persons[j], "./Role") %>% xml_text
            persons$Person_Seat[j] <- xml_find_all(xml_persons[j], "./Seat") %>% xml_text
            persons$Person_Restraints[j] <- xml_find_all(xml_persons[j], "./Restraints") %>% xml_text
            persons$Person_AIScodes[j] <- xml_find_all(xml_persons[j], "./AIScodes") %>% xml_text
            persons$Person_MaxSeverity[j] <- xml_find_all(xml_persons[j], "./MaxSeverity") %>% xml_text
            persons$Person_InjurySource[j] <- xml_find_all(xml_persons[j], "./InjurySource") %>% xml_text
        }
        df$Persons[i] <- list(persons)

        # Case/CaseForm/List
        case$CaseForm_List_MAXSEVERITY <- xml_find_all(data, "/Case/CaseForm/List/MAXSEVERITY") %>% xml_text
        case$CaseForm_List_FATALQTY <- xml_find_all(data, "/Case/CaseForm/List/FATALQTY") %>% xml_text
        case$CaseForm_List_CHILDSEATQTY <- xml_find_all(data, "/Case/CaseForm/List/CHILDSEATQTY") %>% xml_text
        case$CaseForm_List_AIRBAGQTY <- xml_find_all(data, "/Case/CaseForm/List/AIRBAGQTY") %>% xml_text

        # EMS
        xml_ems <- xml_find_all(data, "/Case/EMSForm/EMSvehicle")
        ems <- data_frame(CaseId = rep(df$id[i], length(xml_ems)))
        for (j in seq_along(xml_ems)) {
            ems$EMS_EMSvehicleNumber[j] <- xml_ems[j] %>% xml_attr("EMSvehicleNumber")
            ems$EMS_EMSvehID[j] <- xml_ems[j] %>% xml_attr("EMSvehID")
            ems$EMS_AgencyType[j] <- xml_find_all(xml_ems[j], "./AgencyType") %>% xml_text
            ems$EMS_Agency[j] <- xml_find_all(xml_ems[j], "./Agency") %>% xml_text
            ems$EMS_Type[j] <- xml_find_all(xml_ems[j], "./Type") %>% xml_text
            ems$EMS_Mode[j] <- xml_find_all(xml_ems[j], "./Mode") %>% xml_text
            ems$EMS_Notified[j] <- xml_find_all(xml_ems[j], "./Notified") %>% xml_text
            ems$EMS_Arrived[j] <- xml_find_all(xml_ems[j], "./Arrived") %>% xml_text
            ems$EMS_Departed[j] <- xml_find_all(xml_ems[j], "./Departed") %>% xml_text
            ems$EMS_ArrMedical[j] <- xml_find_all(xml_ems[j], "./ArrMedical") %>% xml_text
        }
        df$EMS[i] <- list(ems)

        # GeneralVehicle
        xml_genvehicle <- xml_find_all(data, "/Case/GeneralVehicleForms/GeneralVehicleForm")
        genvehicle <- data_frame(CaseId = rep(df$id[i], length(xml_genvehicle)))
        for (j in seq_along(xml_genvehicle)) {
            genvehicle$GeneralVehicle_VehicleNumber[j] <- xml_genvehicle[j] %>% xml_attr("VehicleNumber")
            genvehicle$GeneralVehicle_VehicleID[j] <- xml_genvehicle[j] %>% xml_attr("VehicleID")
            genvehicle$GeneralVehicle_CDStype[j] <- xml_genvehicle[j] %>% xml_attr("CDStype")
            genvehicle$GeneralVehicle_Vehicle_ModelYear[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/ModelYear") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_Make[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/Make") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_Model[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/Model") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_VIN[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/VIN") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_BodyCategory[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/BodyCategory") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_BodyType[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/BodyType") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_InTransport[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/InTransport") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_TransportStatus[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/TransportStatus") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_VehSpecialUse[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/VehSpecialUse") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_Inspection[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/Inspection") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_InspectionInterval[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/InspectionInterval") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_CurbWeight[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/CurbWeight") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_CurbWeight_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/CurbWeight") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Vehicle_CurbWeightSource[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/CurbWeightSource") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_CargoWeight[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/CargoWeight") %>% xml_text
            genvehicle$GeneralVehicle_Vehicle_CargoWeight_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/CargoWeight") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Vehicle_CargoWeightSource[j] <- xml_find_all(xml_genvehicle[j], "./Vehicle/CargoWeightSource") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_Wheelbase[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/Wheelbase") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_Wheelbase_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/Wheelbase") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_OverallLength[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/OverallLength") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_OverallLength_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/OverallLength") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_MaximumWidth[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/MaximumWidth") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_MaximumWidth_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/MaximumWidth") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_CurbWeight[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/CurbWeight") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_CurbWeight_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/CurbWeight") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_AverageTrack[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/AverageTrack") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_AverageTrack_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/AverageTrack") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_FrontOverhang[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/FrontOverhang") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_FrontOverhang_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/FrontOverhang") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_RearOverhang[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/RearOverhang") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_RearOverhang_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/RearOverhang") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_UndeformedEndWidth[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/UndeformedEndWidth") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_UndeformedEndWidth_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/UndeformedEndWidth") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_EngineCylinders[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/EngineCylinders") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_EngineDisplacement[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/EngineDisplacement") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_EngineDisplacement_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/EngineDisplacement") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Specifications_ResearcherEstimatedDisposition[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/ResearcherEstimatedDisposition") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_AssessmentJustification[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/AssessmentJustification") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_CertifiedAlteredVehicle[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/CertifiedAlteredVehicle") %>% xml_text
            genvehicle$GeneralVehicle_Specifications_SuspectedModifications[j] <- xml_find_all(xml_genvehicle[j], "./Specifications/SuspectedModifications") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_PoliceReport_TowStatus[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/PoliceReport/TowStatus") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_PoliceReport_TravelSpeed[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/PoliceReport/TravelSpeed") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_PoliceReport_TravelSpeed_UOM[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/PoliceReport/TravelSpeed") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_OfficialRecords_PoliceReport_PostedSpeedLimit[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/PoliceReport/PostedSpeedLimit") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_PoliceReport_PostedSpeedLimit_UOM[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/PoliceReport/PostedSpeedLimit") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_OfficialRecords_Driver_DriverPresent[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/DriverPresent") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_PARAlcoholPresent[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/PARAlcoholPresent") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_AlcoholTest[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/AlcoholTest") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_TestResult[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/TestResult") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_TestSource[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/TestSource") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_PAROtherDrugPresent[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/PAROtherDrugPresent") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_OtherDrugTestResult[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/OtherDrugTestResult") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_ZipCode[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/ZipCode") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_Race[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/Race") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_Ethnicity[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/Ethnicity") %>% xml_text
            genvehicle$GeneralVehicle_OfficialRecords_Driver_RaceEthnicOrigin[j] <- xml_find_all(xml_genvehicle[j], "./OfficialRecords/Driver/RaceEthnicOrigin") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Trafficway_RelationToJunction[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Trafficway/RelationToJunction") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Trafficway_RelationToFlow[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Trafficway/RelationToFlow") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Roadway_TravelLanes[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Roadway/TravelLanes") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Roadway_Alignment[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Roadway/Alignment") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Roadway_Profile[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Roadway/Profile") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Roadway_SurfaceType[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Roadway/SurfaceType") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Roadway_SurfaceCondition[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Roadway/SurfaceCondition") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Conditions_Light[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Conditions/Light") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_Conditions_Weather[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/Conditions/Weather") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_TrafficControlDevices_Device[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/TrafficControlDevices/Device") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Environment_TrafficControlDevices_Functioning[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Environment/TrafficControlDevices/Functioning") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_PreeventMovement[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/PreeventMovement") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_CriticalPrecrashCat[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/CriticalPrecrashCat") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_CriticalPrecrashEvent[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/CriticalPrecrashEvent") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_AttemptedAvoidanceManeuver[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/AttemptedAvoidanceManeuver") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_PreimpactStability[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/PreimpactStability") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_PreimpactLocation[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/PreimpactLocation") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_CrashType[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/CrashType") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Movement_InattentionToDriving[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Movement/InattentionToDriving") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Distractions_DistractionElement[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Distractions/DistractionElement") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Trafficway_RelationToJunction[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Trafficway/RelationToJunction") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Trafficway_RelationToFlow[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Trafficway/RelationToFlow") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Roadway_TravelLanes[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Roadway/TravelLanes") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Roadway_Alignment[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Roadway/Alignment") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Roadway_Profile[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Roadway/Profile") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Roadway_SurfaceType[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Roadway/SurfaceType") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Roadway_SurfaceCondition[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Roadway/SurfaceCondition") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Conditions_Light[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Conditions/Light") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_Conditions_Atmosphere[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/Conditions/Atmosphere") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_TrafficControlDevices_Device[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/TrafficControlDevices/Device") %>% xml_text
            genvehicle$GeneralVehicle_Precrash_TrafficControlDevices_Functioning[j] <- xml_find_all(xml_genvehicle[j], "./Precrash/TrafficControlDevices/Functioning") %>% xml_text
            genvehicle$GeneralVehicle_Driver_PreeventMovement[j] <- xml_find_all(xml_genvehicle[j], "./Driver/PreeventMovement") %>% xml_text
            genvehicle$GeneralVehicle_Driver_CriticalPrecrashCat[j] <- xml_find_all(xml_genvehicle[j], "./Driver/CriticalPrecrashCat") %>% xml_text
            genvehicle$GeneralVehicle_Driver_CriticalPrecrashEvent[j] <- xml_find_all(xml_genvehicle[j], "./Driver/CriticalPrecrashEvent") %>% xml_text
            genvehicle$GeneralVehicle_Driver_AttemptedAvoidanceManeuver[j] <- xml_find_all(xml_genvehicle[j], "./Driver/AttemptedAvoidanceManeuver") %>% xml_text
            genvehicle$GeneralVehicle_Driver_PreimpactStability[j] <- xml_find_all(xml_genvehicle[j], "./Driver/PreimpactStability") %>% xml_text
            genvehicle$GeneralVehicle_Driver_PreimpactLocation[j] <- xml_find_all(xml_genvehicle[j], "./Driver/PreimpactLocation") %>% xml_text
            genvehicle$GeneralVehicle_Driver_CrashType[j] <- xml_find_all(xml_genvehicle[j], "./Driver/CrashType") %>% xml_text
            genvehicle$GeneralVehicle_Driver_CrashType_ConfigCatStr[j] <- xml_find_all(xml_genvehicle[j], "./Driver/CrashType") %>% xml_attr("ConfigCatStr")
            genvehicle$GeneralVehicle_Driver_InattentionToDriving[j] <- xml_find_all(xml_genvehicle[j], "./Driver/InattentionToDriving") %>% xml_text
            genvehicle$GeneralVehicle_Driver_Distraction_DistractionElement[j] <- xml_find_all(xml_genvehicle[j], "./Driver/Distraction/DistractionElement") %>% xml_text
            genvehicle$GeneralVehicle_AOPS_IsAnAOPSVehicle[j] <- xml_find_all(xml_genvehicle[j], "./AOPS/IsAnAOPSVehicle") %>% xml_text
            genvehicle$GeneralVehicle_AOPS_AirBagsDeploymentFirstSeatFrontal[j] <- xml_find_all(xml_genvehicle[j], "./AOPS/AirBagsDeploymentFirstSeatFrontal") %>% xml_text
            genvehicle$GeneralVehicle_AOPS_AigBagsDeploymentOther[j] <- xml_find_all(xml_genvehicle[j], "./AOPS/AigBagsDeploymentOther") %>% xml_text
            genvehicle$GeneralVehicle_AOPS_TypeOfOtherAirBagPresent[j] <- xml_find_all(xml_genvehicle[j], "./AOPS/TypeOfOtherAirBagPresent") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Data_Type[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Data/Type") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Data_QuarterTurns[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Data/QuarterTurns") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_PreRollover_Maneuver[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/PreRollover/Maneuver") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Initiation_Type[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Initiation/Type") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Initiation_Location[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Initiation/Location") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Initiation_ObjectContactedClass[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Initiation/ObjectContactedClass") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Initiation_ObjectContacted[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Initiation/ObjectContacted") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Specifics_InitialLocationFirstForce[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Specifics/InitialLocationFirstForce") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Specifics_InitialDirection[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Specifics/InitialDirection") %>% xml_text
            genvehicle$GeneralVehicle_Rollover_Measurement_EstimatedDistance[j] <- xml_find_all(xml_genvehicle[j], "./Rollover/Measurement/EstimatedDistance") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_HeadingAngleForHighestDeltaV[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/HeadingAngleForHighestDeltaV") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_HeadingAngleForHighestDeltaV_ImpactCategory[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/HeadingAngleForHighestDeltaV/ImpactCategory") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_HeadingAngleForHighestDeltaV_AngleThisVehicle[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/HeadingAngleForHighestDeltaV/AngleThisVehicle") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_HeadingAngleForHighestDeltaV_AngleThisVehicle_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/HeadingAngleForHighestDeltaV/AngleThisVehicle") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Reconstruction_HeadingAngleForHighestDeltaV_AngleOtherVehicle[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/HeadingAngleForHighestDeltaV/AngleOtherVehicle") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_HeadingAngleForHighestDeltaV_AngleOtherVehicle_UOM[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/HeadingAngleForHighestDeltaV/AngleOtherVehicle") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_Reconstruction_ReconstructionData_TowedTrailingUnit[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/ReconstructionData/TowedTrailingUnit") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_ReconstructionData_DocTrajectoryData[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/ReconstructionData/DocTrajectoryData") %>% xml_text
            genvehicle$GeneralVehicle_Reconstruction_ReconstructionData_PostCollisionCondOfTreePole[j] <- xml_find_all(xml_genvehicle[j], "./Reconstruction/ReconstructionData/PostCollisionCondOfTreePole") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_EventNumber[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/EventNumber") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_BasisForDeltaV[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/BasisForDeltaV") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_Total[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/Total") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_Total_UOM[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/Total") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_Longitudinal[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/Longitudinal") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_Longitudinal_UOM[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/Longitudinal") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_Lateral[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/Lateral") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_Lateral_UOM[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/Lateral") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_ConfidenceLevel[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/ConfidenceLevel") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_EnergyAbsorption[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/EnergyAbsorption") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_EnergyAbsorption_UOM[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/EnergyAbsorption") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_ImpactSpeed[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/ImpactSpeed") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_ComputerGeneratedDeltaV_ImpactSpeed_UOM[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/ComputerGeneratedDeltaV/ImpactSpeed") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_DeltaV_Event_BarrierEquivalentSpeed[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/BarrierEquivalentSpeed") %>% xml_text
            genvehicle$GeneralVehicle_DeltaV_Event_BarrierEquivalentSpeed_UOM[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/BarrierEquivalentSpeed") %>% xml_attr("UOM")
            genvehicle$GeneralVehicle_DeltaV_Event_EstimatedDeltaV[j] <- xml_find_all(xml_genvehicle[j], "./DeltaV/Event/EstimatedDeltaV") %>% xml_text
        }
        df$GeneralVehicle[i] <- list(genvehicle)

        # IMG (Not mapped)
        #/Case/IMGForm/SceneDrawings -> ...
        #/Case/IMGForm/Crashscene -> ...
        #/Case/IMGForm/Vehicle -> ...
        #/Case/IMGForm/Onscene -> ...
        #/Case/IMGForm/Exemplar -> ...
        #/Case/IMGForm/Airbag -> ...
        #/Case/IMGForm/Diagram -> ...
        #/Case/IMGForm/Crashtype -> ...
        #/Case/IMGForm/Exteriorvehicle -> ...
        #/Case/IMGForm/Injury -> ...

        # Occupant
        xml_occupant <- xml_find_all(data, "/Case/OccupantForms/OccupantForm")
        occupant <- data_frame(CaseId = rep(df$id[i], length(xml_occupant)))
        for (j in seq_along(xml_occupant)) {
            occupant$Occupant_VehicleNumber[j] <- xml_occupant[j] %>% xml_attr("VehicleNumber")
            occupant$Occupant_OccupantNumber[j] <- xml_occupant[j] %>% xml_attr("OccupantNumber")
            occupant$Occupant_OccupantID[j] <- xml_occupant[j] %>% xml_attr("OccupantID")
            occupant$Occupant_Occupant_Age[j] <- xml_find_all(xml_occupant[j], "./Occupant/Age") %>% xml_text
            occupant$Occupant_Occupant_Age_UOM[j] <- xml_find_all(xml_occupant[j], "./Occupant/Age") %>% xml_attr("UOM")
            occupant$Occupant_Occupant_OccHeight[j] <- xml_find_all(xml_occupant[j], "./Occupant/OccHeight") %>% xml_text
            occupant$Occupant_Occupant_OccHeight_UOM[j] <- xml_find_all(xml_occupant[j], "./Occupant/OccHeight") %>% xml_attr("UOM")
            occupant$Occupant_Occupant_Height[j] <- xml_find_all(xml_occupant[j], "./Occupant/Height") %>% xml_text
            occupant$Occupant_Occupant_Height_UOM[j] <- xml_find_all(xml_occupant[j], "./Occupant/Height") %>% xml_attr("UOM")
            occupant$Occupant_Occupant_Weight[j] <- xml_find_all(xml_occupant[j], "./Occupant/Weight") %>% xml_text
            occupant$Occupant_Occupant_Weight_UOM[j] <- xml_find_all(xml_occupant[j], "./Occupant/Weight") %>% xml_attr("UOM")
            occupant$Occupant_Occupant_Sex[j] <- xml_find_all(xml_occupant[j], "./Occupant/Sex") %>% xml_text
            occupant$Occupant_Occupant_FetalMortality[j] <- xml_find_all(xml_occupant[j], "./Occupant/FetalMortality") %>% xml_text
            occupant$Occupant_Occupant_Role[j] <- xml_find_all(xml_occupant[j], "./Occupant/Role") %>% xml_text
            occupant$Occupant_Occupant_Race[j] <- xml_find_all(xml_occupant[j], "./Occupant/Race") %>% xml_text
            occupant$Occupant_Occupant_Ethnicity[j] <- xml_find_all(xml_occupant[j], "./Occupant/Ethnicity") %>% xml_text
            occupant$Occupant_Occupant_EyeWear[j] <- xml_find_all(xml_occupant[j], "./Occupant/EyeWear") %>% xml_text
            occupant$Occupant_Occupant_ChildSeat[j] <- xml_find_all(xml_occupant[j], "./Occupant/ChildSeat") %>% xml_text
            occupant$Occupant_Occupant_PoliceReportedAirBagAvail_Function[j] <- xml_find_all(xml_occupant[j], "./Occupant/PoliceReportedAirBagAvail-Function") %>% xml_text
            occupant$Occupant_Occupant_PoliceReportedBeltUse[j] <- xml_find_all(xml_occupant[j], "./Occupant/PoliceReportedBeltUse") %>% xml_text
            occupant$Occupant_Occupant_SeatPosition_Row[j] <- xml_find_all(xml_occupant[j], "./Occupant/SeatPosition/Row") %>% xml_text
            occupant$Occupant_Occupant_SeatPosition_Location[j] <- xml_find_all(xml_occupant[j], "./Occupant/SeatPosition/Location") %>% xml_text
            occupant$Occupant_Ejection_EjectionObject[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject") %>% xml_text
            occupant$Occupant_Ejection_EjectionObject_EjectionNumber[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject") %>% xml_attr("EjectionNumber")
            occupant$Occupant_Ejection_EjectionObject_OccNumber[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject") %>% xml_attr("OccNumber")
            occupant$Occupant_Ejection_EjectionObject_Type[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject/Type") %>% xml_text
            occupant$Occupant_Ejection_EjectionObject_Area[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject/Area") %>% xml_text
            occupant$Occupant_Ejection_EjectionObject_Medium[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject/Medium") %>% xml_text
            occupant$Occupant_Ejection_EjectionObject_MediumStatus[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject/MediumStatus") %>% xml_text
            occupant$Occupant_Ejection_EjectionObject_Comment[j] <- xml_find_all(xml_occupant[j], "./Ejection/EjectionObject/Comment") %>% xml_text
            occupant$Occupant_Entrapment_Entrapment[j] <- xml_find_all(xml_occupant[j], "./Entrapment/Entrapment") %>% xml_text
            occupant$Occupant_Entrapment_Mobility[j] <- xml_find_all(xml_occupant[j], "./Entrapment/Mobility") %>% xml_text
            occupant$Occupant_Entrapment_EntrapmentComments[j] <- xml_find_all(xml_occupant[j], "./Entrapment/EntrapmentComments") %>% xml_text
            occupant$Occupant_Seat_SeatGeneral_Row[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatGeneral/Row") %>% xml_text
            occupant$Occupant_Seat_SeatGeneral_Location[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatGeneral/Location") %>% xml_text
            occupant$Occupant_Seat_SeatGeneral_Posture[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatGeneral/Posture") %>% xml_text
            occupant$Occupant_Seat_SeatGeneral_SeatType[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatGeneral/SeatType") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_HeadRestraintType[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/HeadRestraintType") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_HeadRestraintDamage[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/HeadRestraintDamage") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_HeadRestraintActive[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/HeadRestraintActive") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_SeatOrientation[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/SeatOrientation") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_TrackPosition[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/TrackPosition") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_SeatPerformance[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/SeatPerformance") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_IntegratedRestraints[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/IntegratedRestraints") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_LatchAnchor[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/LatchAnchor") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_LatchTether[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/LatchTether") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_SeatBackPriorInclination[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/SeatBackPriorInclination") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_SeatBackPostInclination[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/SeatBackPostInclination") %>% xml_text
            occupant$Occupant_Seat_SeatComponents_AirBagAvailable[j] <- xml_find_all(xml_occupant[j], "./Seat/SeatComponents/AirBagAvailable") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_SeatRow[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject") %>% xml_attr("SeatRow")
            occupant$Occupant_Airbag_AirbagObject_SeatLocation[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject") %>% xml_attr("SeatLocation")
            occupant$Occupant_Airbag_AirbagObject_AirbagID[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject") %>% xml_attr("AirbagID")
            occupant$Occupant_Airbag_AirbagObject_AirbagNum[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject") %>% xml_attr("AirbagNum")
            occupant$Occupant_Airbag_AirbagObject_Function_Location[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/Location") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_Status[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/Status") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_Type[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/Type") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_RedesignClass[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/RedesignClass") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_SystemDeployment[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/SystemDeployment") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_IndicationFailure[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/IndicationFailure") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_SwitchType[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/SwitchType") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Function_SwitchStatus[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Function/SwitchStatus") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Damage_FlapsOpen[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Damage/FlapsOpen") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Damage_FlapsDamaged[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Damage/FlapsDamaged") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Damage_AirBagDamaged[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Damage/AirBagDamaged") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Damage_DamageSource[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Damage/DamageSource") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Damage_NumberTethers[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Damage/NumberTethers") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Damage_NumberVentPorts[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Damage/NumberVentPorts") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_VehiclePreviousCrashes[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/VehiclePreviousCrashes") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_PriorMaintenance[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/PriorMaintenance") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_AirBagEvent[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/AirBagEvent") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_CDCDeployment[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/CDCDeployment") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_ContactOther[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/ContactOther") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_EventNumber[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV") %>% xml_attr("EventNumber")
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_PDOF[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/PDOF") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_CDC[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/CDC") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Total[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Total") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Total_UOM[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Total") %>% xml_attr("UOM")
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Longitudinal[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Longitudinal") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Longitudinal_UOM[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Longitudinal") %>% xml_attr("UOM")
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Lateral[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Lateral") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Lateral_UOM[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Lateral") %>% xml_attr("UOM")
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Energy[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Energy") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Energy_UOM[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Energy") %>% xml_attr("UOM")
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Impact[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Impact") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Barrier[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Barrier") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Estimated[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Estimated") %>% xml_text
            occupant$Occupant_Airbag_AirbagObject_Evaluation_DeltaV_Rank[j] <- xml_find_all(xml_occupant[j], "./Airbag/AirbagObject/Evaluation/DeltaV/Rank") %>% xml_text
            occupant$Occupant_ChildSeat[j] <- xml_find_all(xml_occupant[j], "./ChildSeat") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_Availability[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/Availability") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_UsedInCrash[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/UsedInCrash") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_LapBeltPosition[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/LapBeltPosition") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_ShoulderBeltPosition[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/ShoulderBeltPosition") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_Malfunction[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/Malfunction") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_AnchorageAdjustment[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/AnchorageAdjustment") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Belt_SourceOfBeltUse[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Belt/SourceOfBeltUse") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPretensioner_Pretensioner[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPretensioner/Pretensioner") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPretensioner_PretensionerDistance[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPretensioner/PretensionerDistance") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPretensioner_LatchPlate[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPretensioner/LatchPlate") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPretensioner_BeltRetractor[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPretensioner/BeltRetractor") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPretensioner_WasBeltRoutedThroughSafetyGuides[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPretensioner/WasBeltRoutedThroughSafetyGuides") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPositioning_Presence[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPositioning/Presence") %>% xml_text
            occupant$Occupant_SeatBelt_ManualBelt_Components_BeltPositioning_Use[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/ManualBelt/Components/BeltPositioning/Use") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_AvailabilityFunction[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/AvailabilityFunction") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_UsedInCrash[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/UsedInCrash") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_Motorized[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/Motorized") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_LapBeltPosition[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/LapBeltPosition") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_ShoulderBeltPosition[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/ShoulderBeltPosition") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_Malfunction[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/Malfunction") %>% xml_text
            occupant$Occupant_SeatBelt_AutomaticBelt_SourceOfBeltUse[j] <- xml_find_all(xml_occupant[j], "./SeatBelt/AutomaticBelt/SourceOfBeltUse") %>% xml_text
            occupant$Occupant_Injury_Treatment_Mortality[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/Mortality") %>% xml_text
            occupant$Occupant_Injury_Treatment_Treatment[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/Treatment") %>% xml_text
            occupant$Occupant_Injury_Treatment_InitialFacility[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/InitialFacility") %>% xml_text
            occupant$Occupant_Injury_Treatment_HospitalStay[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/HospitalStay") %>% xml_text
            occupant$Occupant_Injury_Treatment_HospitalStay_UOM[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/HospitalStay") %>% xml_attr("UOM")
            occupant$Occupant_Injury_Treatment_WorkDaysLost[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/WorkDaysLost") %>% xml_text
            occupant$Occupant_Injury_Treatment_WorkDaysLost_UOM[j] <- xml_find_all(xml_occupant[j], "./Injury/Treatment/WorkDaysLost") %>% xml_attr("UOM")
            occupant$Occupant_Injury_Medical_TimeToDeath[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/TimeToDeath") %>% xml_text
            occupant$Occupant_Injury_Medical_NumberOfInjuries[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/NumberOfInjuries") %>% xml_text
            occupant$Occupant_Injury_Medical_GCS[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/GCS") %>% xml_text
            occupant$Occupant_Injury_Medical_Blood[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/Blood") %>% xml_text
            occupant$Occupant_Injury_Medical_Abg[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/Abg") %>% xml_text
            occupant$Occupant_Injury_Medical_CauseOfDeath_Injury_Order[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/CauseOfDeath/Injury/Order") %>% xml_text
            occupant$Occupant_Injury_Medical_CauseOfDeath_Injury_Injury[j] <- xml_find_all(xml_occupant[j], "./Injury/Medical/CauseOfDeath/Injury/Injury") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_InjuryNo[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode") %>% xml_attr("InjuryNo")
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_AirBagNum[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/AirBagNum") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_SourceOfData[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/SourceOfData") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_AISCode[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/AISCode") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_Aspect_AspectObject[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/Aspect/AspectObject") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_Description[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/Description") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_InjurySource[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/InjurySource") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_ConfidenceLevel[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/ConfidenceLevel") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_DirectIndirectInjury[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/DirectIndirectInjury") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_DirectIntrusion[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/DirectIntrusion") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_IndirectIntrusion[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/IndirectIntrusion") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_AirBagRelated[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/AirBagRelated") %>% xml_text
            occupant$Occupant_InjuryCodes_Injuries_InjuryCode_Rank[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/Injuries/InjuryCode/Rank") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_DaysSince[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/DaysSince") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_Date[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/Date") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_Time[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/Time") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_Pulse[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/Pulse") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_Systolic[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/Systolic") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_Diastolic[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/Diastolic") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_RespiratoryRate[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/RespiratoryRate") %>% xml_text
            occupant$Occupant_InjuryCodes_VitalSigns_VitalSignsSample_Source[j] <- xml_find_all(xml_occupant[j], "./InjuryCodes/VitalSigns/VitalSignsSample/Source") %>% xml_text
            occupant$Occupant_Derived_MAIS[j] <- xml_find_all(xml_occupant[j], "./Derived/MAIS") %>% xml_text
            occupant$Occupant_Derived_ISS[j] <- xml_find_all(xml_occupant[j], "./Derived/ISS") %>% xml_text
        }
        df$Occupants[i] <- list(occupant)

        # Safety
        xml_safety <- xml_find_all(data, "/Case/SafetyForms/SafetyForm")
        safety <- data_frame(CaseId = rep(df$id[i], length(xml_safety)))
        for (j in seq_along(xml_safety)) {
            safety$Safety_VehicleID[j] <- xml_safety[j] %>% xml_attr("VehicleID")
            safety$Safety_VehicleNumber[j] <- xml_safety[j] %>% xml_attr("VehicleNumber")
            safety$Safety_Seat_SeatObject_Row[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/Row") %>% xml_text
            safety$Safety_Seat_SeatObject_Location[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/Location") %>% xml_text
            safety$Safety_Seat_SeatObject_HeadRestraintType[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/HeadRestraintType") %>% xml_text
            safety$Safety_Seat_SeatObject_HeadRestraintDamage[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/HeadRestraintDamage") %>% xml_text
            safety$Safety_Seat_SeatObject_HeadRestraintActive[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/HeadRestraintActive") %>% xml_text
            safety$Safety_Seat_SeatObject_SeatType[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/SeatType") %>% xml_text
            safety$Safety_Seat_SeatObject_SeatOrientation[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/SeatOrientation") %>% xml_text
            safety$Safety_Seat_SeatObject_TrackPosition[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/TrackPosition") %>% xml_text
            safety$Safety_Seat_SeatObject_SeatPerformance[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/SeatPerformance") %>% xml_text
            safety$Safety_Seat_SeatObject_IntegratedRestraints[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/IntegratedRestraints") %>% xml_text
            safety$Safety_Seat_SeatObject_LatchAnchor[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/LatchAnchor") %>% xml_text
            safety$Safety_Seat_SeatObject_LatchTether[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/LatchTether") %>% xml_text
            safety$Safety_Seat_SeatObject_SeatBackPriorInclination[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/SeatBackPriorInclination") %>% xml_text
            safety$Safety_Seat_SeatObject_SeatBackPostInclination[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/SeatBackPostInclination") %>% xml_text
            safety$Safety_Seat_SeatObject_AirbagAvailable[j] <- xml_find_all(xml_safety[j], "./Seat/SeatObject/AirbagAvailable") %>% xml_text
            safety$Safety_SeatBelt_Manual[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual") %>% xml_text
            safety$Safety_SeatBelt_Manual_SeatRow[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual") %>% xml_attr("SeatRow")
            safety$Safety_SeatBelt_Manual_SeatLocation[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual") %>% xml_attr("SeatLocation")
            safety$Safety_SeatBelt_Manual_Belt_Availability[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Belt/Availability") %>% xml_text
            safety$Safety_SeatBelt_Manual_Belt_HistoricalUse[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Belt/HistoricalUse") %>% xml_text
            safety$Safety_SeatBelt_Manual_Belt_UsedInCrash[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Belt/UsedInCrash") %>% xml_text
            safety$Safety_SeatBelt_Manual_Belt_Malfunction[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Belt/Malfunction") %>% xml_text
            safety$Safety_SeatBelt_Manual_Belt_AnchorageAdjustment[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Belt/AnchorageAdjustment") %>% xml_text
            safety$Safety_SeatBelt_Manual_Components_Pretensioner[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Components/Pretensioner") %>% xml_text
            safety$Safety_SeatBelt_Manual_Components_LatchPlate[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Components/LatchPlate") %>% xml_text
            safety$Safety_SeatBelt_Manual_Components_BeltRetractor[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Components/BeltRetractor") %>% xml_text
            safety$Safety_SeatBelt_Manual_Components_PretensionerDistance[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Components/PretensionerDistance") %>% xml_text
            safety$Safety_SeatBelt_Manual_Components_PositioningDevice[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Components/PositioningDevice") %>% xml_text
            safety$Safety_SeatBelt_Manual_Components_WasBeltRoutedThroughSafetyGuides[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Manual/Components/WasBeltRoutedThroughSafetyGuides") %>% xml_text
            safety$Safety_SeatBelt_Automatic_AutomaticObject[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Automatic/AutomaticObject") %>% xml_text
            safety$Safety_SeatBelt_Automatic_AutomaticObject_Position[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Automatic/AutomaticObject") %>% xml_attr("Position")
            safety$Safety_SeatBelt_Automatic_AutomaticObject_UsedInCrash[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Automatic/AutomaticObject/UsedInCrash") %>% xml_text
            safety$Safety_SeatBelt_Automatic_AutomaticObject_Motorized[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Automatic/AutomaticObject/Motorized") %>% xml_text
            safety$Safety_SeatBelt_Automatic_AutomaticObject_MalfunctionModes[j] <- xml_find_all(xml_safety[j], "./SeatBelt/Automatic/AutomaticObject/MalfunctionModes") %>% xml_text
            safety$Safety_AirBags_AirbagObject[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject") %>% xml_text
            safety$Safety_AirBags_AirbagObject_AirbagID[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject") %>% xml_attr("AirbagID")
            safety$Safety_AirBags_AirbagObject_AirbagNum[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject") %>% xml_attr("AirbagNum")
            safety$Safety_AirBags_AirbagObject_Function_SeatLocations_SeatLoc[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/SeatLocations/SeatLoc") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_SeatLocations_SeatLoc_seatRow[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/SeatLocations/SeatLoc") %>% xml_attr("seatRow")
            safety$Safety_AirBags_AirbagObject_Function_SeatLocations_SeatLoc_seatLocation[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/SeatLocations/SeatLoc") %>% xml_attr("seatLocation")
            safety$Safety_AirBags_AirbagObject_Function_AirbagEvent[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/AirbagEvent") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_CDCdeployment[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/CDCdeployment") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_Location[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/Location") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_Status[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/Status") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_Type[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/Type") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_RedesignClass[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/RedesignClass") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_SystemDeployment[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/SystemDeployment") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_IndicationOfFailure[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/IndicationOfFailure") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_SwitchType[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/SwitchType") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Function_SwitchStatus[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Function/SwitchStatus") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Damage_FlapsOpenAtTearPoints[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Damage/FlapsOpenAtTearPoints") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Damage_FlapsDamaged[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Damage/FlapsDamaged") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Damage_AirBagDamaged[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Damage/AirBagDamaged") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Damage_DamageSource[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Damage/DamageSource") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Damage_NumberTethers[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Damage/NumberTethers") %>% xml_text
            safety$Safety_AirBags_AirbagObject_Damage_NumberVentPorts[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/Damage/NumberVentPorts") %>% xml_text
            safety$Safety_AirBags_AirbagObject_ContactOtherOccupant[j] <- xml_find_all(xml_safety[j], "./AirBags/AirbagObject/ContactOtherOccupant") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_OccupantNumber[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/OccupantNumber") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Placement[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Placement") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_ChildPosition[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/ChildPosition") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Make[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Make") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Model[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Model") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Type[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Type") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_MftDate[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/MftDate") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_ModelNum[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/ModelNum") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_DataSource[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/DataSource") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Orientation[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Orientation") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Harness[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Harness") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_HarnessUse[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/HarnessUse") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_RetainerClip[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/RetainerClip") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_RetainerClipUse[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/RetainerClipUse") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Tether[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Tether") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_TetherUse[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/TetherUse") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_Latch[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/Latch") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_LatchUse[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/LatchUse") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_BeltRoutingUse[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/BeltRoutingUse") %>% xml_text
            safety$Safety_ChildSeat_ChildSeatOcc_LockingClipUse[j] <- xml_find_all(xml_safety[j], "./ChildSeat/ChildSeatOcc/LockingClipUse") %>% xml_text
            safety$Safety_ChildSeat_VehicleOcc_BeltRetractorType[j] <- xml_find_all(xml_safety[j], "./ChildSeat/VehicleOcc/BeltRetractorType") %>% xml_text
            safety$Safety_ChildSeat_VehicleOcc_LatchPlateType[j] <- xml_find_all(xml_safety[j], "./ChildSeat/VehicleOcc/LatchPlateType") %>% xml_text
            safety$Safety_ChildSeat_VehicleOcc_LatchAnchor[j] <- xml_find_all(xml_safety[j], "./ChildSeat/VehicleOcc/LatchAnchor") %>% xml_text
            safety$Safety_ChildSeat_VehicleOcc_LatchTether[j] <- xml_find_all(xml_safety[j], "./ChildSeat/VehicleOcc/LatchTether") %>% xml_text
        }
        df$Safety[i] <- list(safety)

        # VehicleExterior
        xml_vehicleext <- xml_find_all(data, "/Case/VehicleExteriorForms/VehicleExteriorForm")
        vehicleext <- data_frame(CaseId = rep(df$id[i], length(xml_vehicleext)))
        for (j in seq_along(xml_vehicleext)) {
            vehicleext$VehicleExterior_VehicleNumber[j] <- xml_vehicleext[j] %>% xml_attr("VehicleNumber")
            vehicleext$VehicleExterior_VehicleID[j] <- xml_vehicleext[j] %>% xml_attr("VehicleID")
            vehicleext$VehicleExterior_Vehicle_ModelYear[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/ModelYear") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_Make[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/Make") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_Model[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/Model") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_VIN[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/VIN") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_BodyType[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/BodyType") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_InTransport[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/InTransport") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_TransportStatus[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/TransportStatus") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_VehSpecialUse[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/VehSpecialUse") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_Inspection[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/Inspection") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_InspectionInterval[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/InspectionInterval") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_InspectionInterval_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/InspectionInterval") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Vehicle_CurbWeight[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/CurbWeight") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_CurbWeight_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/CurbWeight") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Vehicle_CurbWeightSource[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/CurbWeightSource") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_CargoWeight[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/CargoWeight") %>% xml_text
            vehicleext$VehicleExterior_Vehicle_CargoWeight_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/CargoWeight") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Vehicle_CargoWeightSource[j] <- xml_find_all(xml_vehicleext[j], "./Vehicle/CargoWeightSource") %>% xml_text
            vehicleext$VehicleExterior_Specifications_Wheelbase[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/Wheelbase") %>% xml_text
            vehicleext$VehicleExterior_Specifications_Wheelbase_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/Wheelbase") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_OverallLength[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/OverallLength") %>% xml_text
            vehicleext$VehicleExterior_Specifications_OverallLength_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/OverallLength") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_MaximumWidth[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/MaximumWidth") %>% xml_text
            vehicleext$VehicleExterior_Specifications_MaximumWidth_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/MaximumWidth") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_CurbWeight[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/CurbWeight") %>% xml_text
            vehicleext$VehicleExterior_Specifications_CurbWeight_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/CurbWeight") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_AverageTrack[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/AverageTrack") %>% xml_text
            vehicleext$VehicleExterior_Specifications_AverageTrack_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/AverageTrack") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_FrontOverhang[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/FrontOverhang") %>% xml_text
            vehicleext$VehicleExterior_Specifications_FrontOverhang_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/FrontOverhang") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_RearOverhang[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/RearOverhang") %>% xml_text
            vehicleext$VehicleExterior_Specifications_RearOverhang_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/RearOverhang") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_UndeformedEndWidth[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/UndeformedEndWidth") %>% xml_text
            vehicleext$VehicleExterior_Specifications_UndeformedEndWidth_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/UndeformedEndWidth") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_EngineCylinders[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/EngineCylinders") %>% xml_text
            vehicleext$VehicleExterior_Specifications_EngineCylinders_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/EngineCylinders") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_EngineDisplacement[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/EngineDisplacement") %>% xml_text
            vehicleext$VehicleExterior_Specifications_EngineDisplacement_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/EngineDisplacement") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Specifications_TransmissionType[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/TransmissionType") %>% xml_text
            vehicleext$VehicleExterior_Specifications_DriveWheels[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/DriveWheels") %>% xml_text
            vehicleext$VehicleExterior_Specifications_CertifiedAlteredVehicle[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/CertifiedAlteredVehicle") %>% xml_text
            vehicleext$VehicleExterior_Specifications_SuspectedModifications[j] <- xml_find_all(xml_vehicleext[j], "./Specifications/SuspectedModifications") %>% xml_text
            vehicleext$VehicleExterior_Fire_Origin[j] <- xml_find_all(xml_vehicleext[j], "./Fire/Origin") %>% xml_text
            vehicleext$VehicleExterior_Fire_Occurrence[j] <- xml_find_all(xml_vehicleext[j], "./Fire/Occurrence") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_FuelNumber[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement") %>% xml_attr("FuelNumber")
            vehicleext$VehicleExterior_Fuel_FuelElement_FuelType[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/FuelType") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_TankLocation[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/TankLocation") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_TankType[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/TankType") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_FillerCapLocation[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/FillerCapLocation") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_TankPreCrashCondition[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/TankPreCrashCondition") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_TankDamage[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/TankDamage") %>% xml_text
            vehicleext$VehicleExterior_Fuel_FuelElement_LeakageLocation[j] <- xml_find_all(xml_vehicleext[j], "./Fuel/FuelElement/LeakageLocation") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_eventNumber[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent") %>% xml_attr("eventNumber")
            vehicleext$VehicleExterior_CDC_CDCevent_DeformNumber[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent") %>% xml_attr("DeformNumber")
            vehicleext$VehicleExterior_CDC_CDCevent_GAD[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/GAD") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_CDCID[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/CDCID") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_EventID[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/EventID") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ObjectContactedCategory[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ObjectContactedCategory") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ObjectContacted[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ObjectContacted") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ForceDirection[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ForceDirection") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ForceDirection_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ForceDirection") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_Increment[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/Increment") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ClockDirection[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ClockDirection") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_OverUnderride[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/OverUnderride") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_HeadingAngle[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/HeadingAngle") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_HeadingAngle_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/HeadingAngle") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_DeformationLocation[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/DeformationLocation") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_LongLateral[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/LongLateral") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_VerticalLateral[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/VerticalLateral") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_Distribution[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/Distribution") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_Extent[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/Extent") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_Summary[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/Summary") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_SideImpactData_PillarInvolvement_Pillar[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/SideImpactData/PillarInvolvement/Pillar") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_SideImpactData_SillHeight[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/SideImpactData/SillHeight") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_SideImpactData_DoorSillDiff[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/SideImpactData/DoorSillDiff") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_SideImpactData_CmaxHeight[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/SideImpactData/CmaxHeight") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Total[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Total") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Total_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Total") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Longitudinal[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Longitudinal") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Longitudinal_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Longitudinal") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Lateral[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Lateral") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Lateral_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Lateral") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_EnergyAbsorption[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/EnergyAbsorption") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_EnergyAbsorption_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/EnergyAbsorption") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_ImpactSpeed[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/ImpactSpeed") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_ImpactSpeed_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/ImpactSpeed") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_BarrierEquivalentSpeed[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/BarrierEquivalentSpeed") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_BarrierEquivalentSpeed_UOM[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/BarrierEquivalentSpeed") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_EstimatedDeltaV[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/EstimatedDeltaV") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Rank[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Rank") %>% xml_text
            vehicleext$VehicleExterior_CDC_CDCevent_ComputerGeneratedDeltaV_Basis[j] <- xml_find_all(xml_vehicleext[j], "./CDC/CDCevent/ComputerGeneratedDeltaV/Basis") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_DeformNumber[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject") %>% xml_attr("DeformNumber")
            vehicleext$VehicleExterior_Crush_CrushObject_ProfileNo[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/ProfileNo") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_EventNumber[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/EventNumber") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_DirectDamageLocation[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/DirectDamageLocation") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_FieldlLocation[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/FieldlLocation") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_MaxCrushLocation[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/MaxCrushLocation") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_CDCID[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/CDCID") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_EventID[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/EventID") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_CDCNo[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/CDCNo") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_FieldL[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/FieldL") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_FieldL_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/FieldL") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_FieldLD[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/FieldLD") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_FieldLD_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/FieldLD") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_SMASHL[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/SMASHL") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_SMASHL_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/SMASHL") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_DirectD[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/DirectD") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_DirectD_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/DirectD") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_WidthCDC[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/WidthCDC") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_WidthCDC_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/WidthCDC") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Category[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Category") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_LateralMeasurements_LateralMeasurement_Sign[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/LateralMeasurements/LateralMeasurement/Sign") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_LateralMeasurements_LateralMeasurement_PlaneCategory[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/LateralMeasurements/LateralMeasurement/PlaneCategory") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_LateralMeasurements_LateralMeasurement_Max[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/LateralMeasurements/LateralMeasurement/Max") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_VerticalMeasurements_VerticalMeasurement_Sign[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/VerticalMeasurements/VerticalMeasurement/Sign") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_VerticalMeasurements_VerticalMeasurement_PlaneCategory[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/VerticalMeasurements/VerticalMeasurement/PlaneCategory") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_VerticalMeasurements_VerticalMeasurement_Max[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/VerticalMeasurements/VerticalMeasurement/Max") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_SequenceNo[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement") %>% xml_attr("SequenceNo")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_Sign[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/Sign") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_PlaneCategory[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/PlaneCategory") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_Max[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/Max") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_Max_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/Max") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C1[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C1") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C1_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C1") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C2[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C2") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C2_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C2") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C3[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C3") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C3_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C3") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C4[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C4") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C4_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C4") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C5[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C5") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C5_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C5") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C6[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C6") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_Measurements_Measurement_C6_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/Measurements/Measurement/C6") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_CMAX[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_CMAX") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_CMAX_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_CMAX") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C1[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C1") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C1_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C1") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C2[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C2") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C2_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C2") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C3[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C3") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C3_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C3") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C4[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C4") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C4_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C4") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C5[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C5") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C5_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C5") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C6[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C6") %>% xml_text
            vehicleext$VehicleExterior_Crush_CrushObject_AVG_C6_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Crush/CrushObject/AVG_C6") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_Obtained[j] <- xml_find_all(xml_vehicleext[j], "./EDR/Obtained") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRID[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile") %>% xml_attr("EDRID")
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Version[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Version") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_LampStatus[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/LampStatus") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_CDC[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/CDC") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_CDC_CDCID[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/CDC") %>% xml_attr("CDCID")
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_CDC_EventSeqNumber[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/CDC") %>% xml_attr("EventSeqNumber")
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_DeployStatus[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/DeployStatus") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_IgnitionCycleEvent[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/IgnitionCycleEvent") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Investigation[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Investigation") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Driver_Belt[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Driver/Belt") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Driver_Pretensioner[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Driver/Pretensioner") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Driver_Pretensioner_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Driver/Pretensioner") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Driver_SeatTrackForward[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Driver/SeatTrackForward") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Passenger_Belt[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Passenger/Belt") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Passenger_Seat[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Passenger/Seat") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Passenger_Pretensioner[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Passenger/Pretensioner") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Passenger_Pretensioner_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Passenger/Pretensioner") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_EDRgeneralData_Passenger_SeatTrackForward[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/EDRgeneralData/Passenger/SeatTrackForward") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_AirbagDeploy_AirbagDeployTime_Type[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/AirbagDeploy/AirbagDeployTime/Type") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_AirbagDeploy_AirbagDeployTime_Position[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/AirbagDeploy/AirbagDeployTime/Position") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_AirbagDeploy_AirbagDeployTime_Stage1[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/AirbagDeploy/AirbagDeployTime/Stage1") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_AirbagDeploy_AirbagDeployTime_Stage1_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/AirbagDeploy/AirbagDeployTime/Stage1") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_AirbagDeploy_AirbagDeployTime_Stage2[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/AirbagDeploy/AirbagDeployTime/Stage2") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_AirbagDeploy_AirbagDeployTime_Stage2_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/AirbagDeploy/AirbagDeployTime/Stage2") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_PreCrash_PreCrashData_Preseconds[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/PreCrash/PreCrashData/Preseconds") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_PreCrash_PreCrashData_Speed[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/PreCrash/PreCrashData/Speed") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_PreCrash_PreCrashData_Speed_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/PreCrash/PreCrashData/Speed") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_PreCrash_PreCrashData_EngineSpeed[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/PreCrash/PreCrashData/EngineSpeed") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_PreCrash_PreCrashData_Throttle[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/PreCrash/PreCrashData/Throttle") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_PreCrash_PreCrashData_BrakeSwitchStatus[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/PreCrash/PreCrashData/BrakeSwitchStatus") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Longitudinal_DeltaV_Time[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Longitudinal/DeltaV/Time") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Longitudinal_DeltaV_Time_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Longitudinal/DeltaV/Time") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Longitudinal_DeltaV_DeltaV[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Longitudinal/DeltaV/DeltaV") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Longitudinal_DeltaV_DeltaV_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Longitudinal/DeltaV/DeltaV") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Lateral_DeltaV_Time[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Lateral/DeltaV/Time") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Lateral_DeltaV_Time_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Lateral/DeltaV/Time") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Lateral_DeltaV_DeltaV[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Lateral/DeltaV/DeltaV") %>% xml_text
            vehicleext$VehicleExterior_EDR_EDRfile_Crash_Lateral_DeltaV_DeltaV_UOM[j] <- xml_find_all(xml_vehicleext[j], "./EDR/EDRfile/Crash/Lateral/DeltaV/DeltaV") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_TransmissionType[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/TransmissionType") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_DriveWheels[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/DriveWheels") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_GVWR[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/GVWR") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_GVWR_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/GVWR") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_GAWRFront[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/GAWRFront") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_GAWRFront_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/GAWRFront") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_GAWRRear[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/GAWRRear") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_GAWRRear_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/GAWRRear") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Front_SizeType[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Front/SizeType") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Front_Size[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Front/Size") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Front_ColdPressure[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Front/ColdPressure") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Front_ColdPressure_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Front/ColdPressure") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Rear_SizeType[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Rear/SizeType") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Rear_Size[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Rear/Size") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Rear_ColdPressure[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Rear/ColdPressure") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_RecommendedTireSize_RecommendationObject_Rear_ColdPressure_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/RecommendedTireSize/RecommendationObject/Rear/ColdPressure") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Location[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Location") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Manufacturer[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Manufacturer") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Model[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Model") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Type[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Type") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Size[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Size") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_TIN[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/*[starts-with(name(), 'TIN')]") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_TreadDepth[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/TreadDepth") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_TreadDepth_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/TreadDepth") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_MaxPressure[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/MaxPressure") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_MaxPressure_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/MaxPressure") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_MeasuredPressure[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/MeasuredPressure") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_MeasuredPressure_UOM[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/MeasuredPressure") %>% xml_attr("UOM")
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Restricted[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Restricted") %>% xml_text
            vehicleext$VehicleExterior_Tire_General_Tires_TireObject_Damage_DamageReason[j] <- xml_find_all(xml_vehicleext[j], "./Tire/General/Tires/TireObject/Damage/DamageReason") %>% xml_text
        }
        df$VehicleExterior[i] <- list(vehicleext)

        # VehicleInterior
        xml_vehicleint <- xml_find_all(data, "/Case/VehicleInteriorForms/VehicleInteriorForm")
        vehicleint <- data_frame(CaseId = rep(df$id[i], length(xml_vehicleint)))
        for (j in seq_along(xml_vehicleint)) {
            vehicleint$VehicleInterior_VehicleID[j] <- xml_vehicleint[j] %>% xml_attr("VehicleID")
            vehicleint$VehicleInterior_VehicleNumber[j] <- xml_vehicleint[j] %>% xml_attr("VehicleNumber")
            vehicleint$VehicleInterior_Integrity_PassengerCompartmentIntegrityLoss_PassCompIntegrity[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/PassengerCompartmentIntegrityLoss/PassCompIntegrity") %>% xml_text
            vehicleint$VehicleInterior_Integrity_PostCrashIntegrityLoss[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/PostCrashIntegrityLoss") %>% xml_text
            vehicleint$VehicleInterior_Integrity_IntegrityGrid[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/IntegrityGrid") %>% xml_text
            vehicleint$VehicleInterior_Integrity_IntegrityGrid_LocationCode[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/IntegrityGrid") %>% xml_attr("LocationCode")
            vehicleint$VehicleInterior_Integrity_IntegrityGrid_IntegrityLocation_Location[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/IntegrityGrid/IntegrityLocation/Location") %>% xml_text
            vehicleint$VehicleInterior_Integrity_IntegrityGrid_IntegrityLocation_Opening[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/IntegrityGrid/IntegrityLocation/Opening") %>% xml_text
            vehicleint$VehicleInterior_Integrity_IntegrityGrid_IntegrityLocation_Failure[j] <- xml_find_all(xml_vehicleint[j], "./Integrity/IntegrityGrid/IntegrityLocation/Failure") %>% xml_text
            vehicleint$VehicleInterior_Glazing_GlazingLocation[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation") %>% xml_text
            vehicleint$VehicleInterior_Glazing_GlazingLocation_LocationCode[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation") %>% xml_attr("LocationCode")
            vehicleint$VehicleInterior_Glazing_GlazingLocation_Location[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation/Location") %>% xml_text
            vehicleint$VehicleInterior_Glazing_GlazingLocation_Type[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation/Type") %>% xml_text
            vehicleint$VehicleInterior_Glazing_GlazingLocation_PreCrashStatus[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation/PreCrashStatus") %>% xml_text
            vehicleint$VehicleInterior_Glazing_GlazingLocation_ImpactDamage[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation/ImpactDamage") %>% xml_text
            vehicleint$VehicleInterior_Glazing_GlazingLocation_OccupantDamage[j] <- xml_find_all(xml_vehicleint[j], "./Glazing/GlazingLocation/OccupantDamage") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_IntrusionNumber[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject") %>% xml_attr("IntrusionNumber")
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Row[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Row") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Position[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Position") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Area[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Area") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_IntrudedComponent[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/IntrudedComponent") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Comparison[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Comparison") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Intruded[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Intruded") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Intrusion[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Intrusion") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_Magnitude[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/Magnitude") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionObject_CrushDirection[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionObject/CrushDirection") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionWidth_RowWidth_Location[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionWidth/RowWidth/Location") %>% xml_text
            vehicleint$VehicleInterior_Intrusion_IntrusionWidth_RowWidth_Width[j] <- xml_find_all(xml_vehicleint[j], "./Intrusion/IntrusionWidth/RowWidth/Width") %>% xml_text
            vehicleint$VehicleInterior_Instrument_OdometerReading[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/OdometerReading") %>% xml_text
            vehicleint$VehicleInterior_Instrument_OdometerReading_UOM[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/OdometerReading") %>% xml_attr("UOM")
            vehicleint$VehicleInterior_Instrument_OdometerSource[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/OdometerSource") %>% xml_text
            vehicleint$VehicleInterior_Instrument_DamageOccupantContact[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/DamageOccupantContact") %>% xml_text
            vehicleint$VehicleInterior_Instrument_AdjustablePedal[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/AdjustablePedal") %>% xml_text
            vehicleint$VehicleInterior_Instrument_KneeBolsterCovering[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/KneeBolsterCovering") %>% xml_text
            vehicleint$VehicleInterior_Instrument_KneeBolsterDeform[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/KneeBolsterDeform") %>% xml_text
            vehicleint$VehicleInterior_Instrument_GloveCompartmentDoor[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/GloveCompartmentDoor") %>% xml_text
            vehicleint$VehicleInterior_Instrument_AdaptiveDrivingEquipment[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/AdaptiveDrivingEquipment") %>% xml_text
            vehicleint$VehicleInterior_Instrument_AdaptiveEquipment_AdaptiveEquipmentItem[j] <- xml_find_all(xml_vehicleint[j], "./Instrument/AdaptiveEquipment/AdaptiveEquipmentItem") %>% xml_text
            vehicleint$VehicleInterior_SteeringWheel_ColumnType[j] <- xml_find_all(xml_vehicleint[j], "./SteeringWheel/ColumnType") %>% xml_text
            vehicleint$VehicleInterior_SteeringWheel_TiltAdjustment[j] <- xml_find_all(xml_vehicleint[j], "./SteeringWheel/TiltAdjustment") %>% xml_text
            vehicleint$VehicleInterior_SteeringWheel_TelescopicAdjustment[j] <- xml_find_all(xml_vehicleint[j], "./SteeringWheel/TelescopicAdjustment") %>% xml_text
            vehicleint$VehicleInterior_SteeringWheel_LocationRimSpokeDeformation[j] <- xml_find_all(xml_vehicleint[j], "./SteeringWheel/LocationRimSpokeDeformation") %>% xml_text
            vehicleint$VehicleInterior_SteeringWheel_RimSpokeDeformation[j] <- xml_find_all(xml_vehicleint[j], "./SteeringWheel/RimSpokeDeformation") %>% xml_text
            vehicleint$VehicleInterior_SteeringWheel_RimSpokeDeformation_UOM[j] <- xml_find_all(xml_vehicleint[j], "./SteeringWheel/RimSpokeDeformation") %>% xml_attr("UOM")
            vehicleint$VehicleInterior_Contact_ContactArea_Contact[j] <- xml_find_all(xml_vehicleint[j], "./Contact/ContactArea/Contact") %>% xml_text
            vehicleint$VehicleInterior_Contact_ContactArea_Component[j] <- xml_find_all(xml_vehicleint[j], "./Contact/ContactArea/Component") %>% xml_text
            vehicleint$VehicleInterior_Contact_ContactArea_Evidence[j] <- xml_find_all(xml_vehicleint[j], "./Contact/ContactArea/Evidence") %>% xml_text
            vehicleint$VehicleInterior_Contact_ContactArea_Occupant[j] <- xml_find_all(xml_vehicleint[j], "./Contact/ContactArea/Occupant") %>% xml_text
            vehicleint$VehicleInterior_Contact_ContactArea_BodyRegion[j] <- xml_find_all(xml_vehicleint[j], "./Contact/ContactArea/BodyRegion") %>% xml_text
            vehicleint$VehicleInterior_Contact_ContactArea_Confidence[j] <- xml_find_all(xml_vehicleint[j], "./Contact/ContactArea/Confidence") %>% xml_text
            vehicleint$VehicleInterior_Ejection_EjectionObject[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject") %>% xml_text
            vehicleint$VehicleInterior_Ejection_EjectionObject_EjectionNumber[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject") %>% xml_attr("EjectionNumber")
            vehicleint$VehicleInterior_Ejection_EjectionObject_OccNumber[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject") %>% xml_attr("OccNumber")
            vehicleint$VehicleInterior_Ejection_EjectionObject_Type[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject/Type") %>% xml_text
            vehicleint$VehicleInterior_Ejection_EjectionObject_Area[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject/Area") %>% xml_text
            vehicleint$VehicleInterior_Ejection_EjectionObject_Medium[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject/Medium") %>% xml_text
            vehicleint$VehicleInterior_Ejection_EjectionObject_MediumStatus[j] <- xml_find_all(xml_vehicleint[j], "./Ejection/EjectionObject/MediumStatus") %>% xml_text
            vehicleint$VehicleInterior_Entrapment[j] <- xml_find_all(xml_vehicleint[j], "./Entrapment") %>% xml_text
        }
        df$VehicleInterior[i] <- list(vehicleint)
        df$Cases[i] <- list(case)
    }

    # Write the results to a dataframe, using dplyr's bind_rows to assemble the sqlite table data
    write_db("Cases", bind_rows(df$Cases))
    write_db("Events", bind_rows(df$Events))
    write_db("Vehicles", bind_rows(df$Vehicles))
    write_db("Persons", bind_rows(df$Persons))
    write_db("EMS", bind_rows(df$EMS))
    write_db("GeneralVehicle", bind_rows(df$GeneralVehicle))
    write_db("Occupants", bind_rows(df$Occupants))
    write_db("Safety", bind_rows(df$Safety))
    write_db("VehicleExterior", bind_rows(df$VehicleExterior))
    write_db("VehicleInterior", bind_rows(df$VehicleInterior))
    return(TRUE)
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
#eg. run_check("/Case/CaseForm/Crash/PSU")
