library(dplyr)
library(shiny)
library(ggplot2)
library(ROCR)

load(url('http://gmyrland.capstone.s3.amazonaws.com/df.Rdata'))

# some last minute tweaks
df$child_seat_present <- as.factor(df$child_seat_present)
df$displacement[df$displacement < 0] <- 0
df$fire <- as.factor(df$fire)
df$is_weeked <- as.factor(df$is_weeked)
df$special_use <- as.factor(df$special_use)
df$travel_lanes <- as.factor(df$travel_lanes)
df$vehicle_year[df$vehicle_year < 0] <- 0

set.seed(1234)
n <- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

ggMMplot <- function(var1, var2) {
    # Adapted from http://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
    levVar1 <- length(levels(var1))
    levVar2 <- length(levels(var2))
    jointTable <- prop.table(table(var1, var2))
    plotData <- as.data.frame(jointTable)
    plotData$marginVar1 <- prop.table(table(var1))
    plotData$var2Height <- plotData$Freq / plotData$marginVar1
    plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) + plotData$marginVar1 / 2
    ggplot(plotData, aes(var1Center, var2Height)) +
        geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black", size=0.25) +
        geom_text(aes(label = as.character(var1), x = var1Center, y = 0.5, angle=90)) +
        scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
        xlab("Predictor Proportion") + ylab("Response Proportion") + guides(fill=guide_legend(title="Fatal"))
}

ggViolinplot <- function(var1, var2) {
    ggplot(df, aes(factor(var2), var1)) + geom_violin(fill="darkgreen")
}

#terms <- names(df)[!names(df) %in% c('CaseId', 'CaseWeight', 'fatal')]
terms <- c()
lterms <- list(age = "age", airbag_available = "airbag_available", airbag_available_police = "airbag_available_police", airbag_deployment = "airbag_deployment", alcohol_present = "alcohol_present", alcohol_test = "alcohol_test", alcohol_test_result = "alcohol_test_result", average_track = "average_track", avoidance_maneuver = "avoidance_maneuver", body_category = "body_category", body_type = "body_type", cargo_weight = "cargo_weight", child_seat_present = "child_seat_present", compartment_integrity_loss = "compartment_integrity_loss", contacted = "contacted", contacted_area = "contacted_area", contacted_class = "contacted_class", crash_config = "crash_config", crash_type = "crash_type", crashtime = "crashtime", curb_weight = "curb_weight", cylinders = "cylinders", damage_area = "damage_area", damage_plane = "damage_plane", dayofweek = "dayofweek", displacement = "displacement", drive_Wheels = "drive_Wheels", driver_race = "driver_race", drugs_present = "drugs_present", entrapment = "entrapment", event_class = "event_class", eyewear = "eyewear", fire = "fire", front_overhang = "front_overhang", height = "height", is_weeked = "is_weeked", light = "light", make = "make", maximum_width = "maximum_width", model = "model", month = "month", odometer = "odometer", overall_length = "overall_length", posted_speed = "posted_speed", posture = "posture", precrash_category = "precrash_category", preevent_movement = "preevent_movement", preimpact_location = "preimpact_location", preimpact_stability = "preimpact_stability", race = "race", rear_overhang = "rear_overhang", roadway_alignment = "roadway_alignment", roadway_condition = "roadway_condition", roadway_profile = "roadway_profile", roadway_surface = "roadway_surface", role = "role", rollover = "rollover", rollover_contacted = "rollover_contacted", rollover_qtr_turns = "rollover_qtr_turns", seat_inclination = "seat_inclination", seat_location = "seat_location", seat_orientation = "seat_orientation", seat_position = "seat_position", seat_row = "seat_row", seatbelt_availability = "seatbelt_availability", seatbelt_used = "seatbelt_used", sex = "sex", special_use = "special_use", tire_tread_depth = "tire_tread_depth", towed_unit = "towed_unit", traffic_control_device = "traffic_control_device", traffic_control_device_functioning = "traffic_control_device_functioning", transmission = "transmission", travel_lanes = "travel_lanes", travel_speed = "travel_speed", undeformed_end_Width = "undeformed_end_Width", vehicle_year = "vehicle_year", vehicles_involved = "vehicles_involved", weather = "weather", weight = "weight", wheelbase = "wheelbase", year = "year")


## Shiny UI
ui <- fluidPage(
    selectInput("select", label = h3("Select Term"), choices = lterms),
    #actionButton("addterm", "Add Term"),
    hr(),
    column(12, align="center",
        ## mosiac / violin
        plotOutput("correlation", height=300),
        ## confusion matrix
        #tableOutput('conf'),
        fluidRow(
            ## roc
            column(6, plotOutput("roc", height=400, width=500), "ROC Curve"),
            ## rp
            column(6, plotOutput("rp", height=400, width=500), "Recall-Precision Curve")
        )
    )
)

## Shiny Server
server <- function(input, output, session) {
    #session$onSessionEnded(function() stopApp(returnValue=NULL))
    vals <- reactive({
        f <- reformulate(input$select, "fatal")
        var <- as.character(input$select)
        fit <- glm(f, family = binomial(link="logit"), data=train)
        probs <- predict(fit, test, type="response")
        conf <- table(test$fatal, as.integer(probs > 0.5))
        pred <- prediction(probs, test$fatal)
        list(fit=fit, var=var, probs=probs, conf=conf, pred=pred)
    })
    output$correlation <- renderPlot({
        data <- df[, vals()$var]
        print(class(data))
        if (is.double(data)) {
            ggViolinplot(data, df$fatal)
        } else {
            ggMMplot(data, df$fatal)
        }
    })
    output$roc <- renderPlot({
        plot(performance(vals()$pred, "tpr", "fpr"))
    })
    output$rp <- renderPlot({
        plot (performance(vals()$pred, "prec", "rec"))
    })
    output$conf <- renderTable({
        print(vals()$conf)
        vals()$conf
    })

}

shinyApp(ui, server)

