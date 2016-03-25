##################
# Data partitions

frm <- function(names) {
    reformulate(names[!names %in% c('CaseId', 'CaseWeight', 'fatal')], 'fatal')
}

part_whole <- function(df, prop=1, n=NULL) {
    if (!is.null(n))
        prop <- (n / nrow(df))
    repl <- ifelse(prop <= 1, FALSE, TRUE)
    df[sample(nrow(df), floor(nrow(df) * prop)), ]
}

part_split <- function(df, prop.test=0.3, ...) {
    ddf <- part_whole(df, ...)
    n <- nrow(ddf)
    list(
        ddf[1:round((1-prop.test) * n), ], # train
        ddf[(round((1-prop.test) * n) + 1):n, ] # test
    )
}

part_balanced <- function(df, response, response.min, response.maj) {
    # split by response class to keep this variable stratified
    df_minority <- df[df[response] == response.min, ]
    df_majority <- df[df[response] == response.maj, ]
    # split test/train quantities from both classes
    df_minority_split <- part_split(df_minority)
    df_train_min <- df_minority_split[[1]]
    df_test_min <- df_minority_split[[2]]
    df_majority_split <- part_split(df_majority)
    df_train_maj <- df_majority_split[[1]]
    df_test_maj <- df_majority_split[[2]]
    # balance the training 
    p <- 0.5 # 0 = shrink maj to size of min, 1 = expand min to size of maj
    n <- floor(nrow(df_train_min) + p * (nrow(df_train_maj) - nrow(df_train_min)))
    df_train_min <- df_train_min[sample(nrow(df_train_min), n, replace=TRUE), ] ## amend to make sure it includes everything at least once
    df_train_maj <- df_train_maj[sample(nrow(df_train_maj), n), ]
    df_train <- rbind(df_train_min, df_train_maj)
    # no need to balance test
    df_test <- rbind(df_test_min, df_test_maj)
    # shuffle
    df_train <- df_train[sample(nrow(df_train)), ]
    df_test <- df_test[sample(nrow(df_test)), ]
    # return train and test
    list(df_train, df_test)
}
part_balanced(df, "fatal", 0, 1)

#ff <- frm(names(df)[6:])
full <- lm(ff, data=df)
null <- lm(fatal ~ 1, data=df)
stepAIC(null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)

vars <- names(df)[!names(df) %in% c('CaseId', 'CaseWeight', 'fatal')]
#for (i in 1:(length(vars)-3)) {
    ff <- frm(vars[61:82])
    #ff <- frm(vars)
    full <- lm(ff, data=df_train)
    null <- lm(fatal ~ 1, data=df_train)
    print(ff)
    print(stepAIC(null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)$call)
#}

for (var in vars) {
    print(chisq.test(df[var], df$fatal))
}

    # fatal ~ vehicles_involved + dayofweek + is_weeked + crashtime + month + year + crash_type + crash_config + child_seat_present + 
    #     age + height + weight + role + race + sex + eyewear + airbag_available_police + airbag_available + airbag_deployment + seatbelt_availability
    # lm(formula = fatal ~ crash_config + eyewear + race + age + airbag_deployment + sex + year + crash_type + crashtime + airbag_available + 
    #        weight + height + month + dayofweek + airbag_available_police + vehicles_involved + role + child_seat_present + seatbelt_availability, 
    #    data = df_train)
    # 
    # fatal ~ seatbelt_used + seat_row + seat_location + seat_position + entrapment + posture + seat_orientation + seat_inclination + 
    #     vehicle_year + make + event_class + damage_area + contacted_area + contacted + contacted_class + damage_plane + body_category + 
    #     body_type + special_use
    # lm(formula = fatal ~ posture + seatbelt_used + entrapment + event_class + damage_plane + seat_orientation + seat_inclination + contacted + 
    #        contacted_area + body_type + vehicle_year + make + damage_area + seat_position + special_use + body_category + seat_row + 
    #        seat_location, data = df_train)
    # 
    # fatal ~ curb_weight + cargo_weight + wheelbase + overall_length + maximum_width + average_track + front_overhang + rear_overhang + 
    #     undeformed_end_Width + cylinders + displacement + travel_speed + posted_speed + alcohol_present + alcohol_test + alcohol_test_result + 
    #     drugs_present + driver_race + travel_lanes + roadway_alignment
    # lm(formula = fatal ~ alcohol_test + alcohol_present + roadway_alignment + posted_speed + driver_race + travel_lanes + average_track + 
    #        rear_overhang + cylinders + maximum_width + curb_weight + displacement + alcohol_test_result + wheelbase + travel_speed + 
    #        cargo_weight + overall_length + drugs_present + front_overhang, 
    #    data = df_train)
    # 
    # fatal ~ roadway_profile + roadway_surface + roadway_condition + light + weather + traffic_control_device + traffic_control_device_functioning + 
    #     preevent_movement + precrash_category + avoidance_maneuver + preimpact_stability + preimpact_location + rollover + rollover_qtr_turns + 
    #     rollover_contacted + towed_unit + transmission + drive_Wheels + fire + tire_tread_depth + compartment_integrity_loss + odometer
    # lm(formula = fatal ~ compartment_integrity_loss + avoidance_maneuver + preimpact_location + fire + drive_Wheels + roadway_condition + 
    #        preevent_movement + precrash_category + rollover_qtr_turns + light + traffic_control_device_functioning + odometer + preimpact_stability + 
    #        tire_tread_depth + rollover + rollover_contacted + weather + roadway_surface + towed_unit + roadway_profile + transmission, 
    #    data = df_train)
    # 
