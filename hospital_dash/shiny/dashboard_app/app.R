library(shiny)
library(shinyTime)
library(shinyjs)
library(V8)

jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'


# arbitrary number of patients
num_er_patients <- 12
num_icu_patients <- 12
num_sdu_patients <- 15
num_ward_patients <- 24
num_dc_patients <- 6

# assumption potential_dc is 1/3 of num_ward_patients
potential_dc <- num_ward_patients/3

# arbitrary avg_dc_time
avg_dc_time <- 55

# number of total patients (sum of all patients)
# NOTE: total_patients needs to be below all dept patients (scope)
num_total_patients <- num_er_patients + num_icu_patients + num_sdu_patients + num_ward_patients + num_dc_patients

# assumptions - number of beds (mid-size hospital)
num_er_beds <- 20
num_icu_beds <- 20
num_sdu_beds <- 20
num_ward_beds <- 40

# all beds add up to total_occupied
# NOTE: total_beds needs to be below all dept beds (scope)
num_total_beds_occupied <- num_er_beds + num_icu_beds + num_sdu_beds + num_ward_beds

# assumption - ratio 2 nurse : 3 beds (society for critical care; pandemic response)
num_er_nurses <- floor(num_er_beds*(2/3))
num_icu_nurses <- floor(num_icu_beds*(2/3))
num_sdu_nurses <- floor(num_sdu_beds*(2/3))
num_ward_nurses <- floor(num_ward_beds*(2/3))

# Define UI  ----
ui <- fluidPage(
  
    titlePanel("Hospital Resource Management: COVID19"), 
    
    useShinyjs(), # include shinyjs
    extendShinyjs(text = jsCode),
    
    sidebarLayout(position = "right", 
    
      # wrap sidebarPanel in a div to reference id="Sidebar" for toggling
      div( id ="Sidebar",
      
      sidebarPanel(
        fluidRow(
          column(6, h4("Patients"), 
                 numericInput("num_total_patients", "Number of Total Patients", value = num_total_patients), 
                 numericInput("num_er_patients", "ER Patients", value = num_er_patients),
                 numericInput("num_icu_patients", "ICU Patients", value = num_icu_patients),
                 numericInput("num_sdu_patients", "SDU Patients", value = num_sdu_patients),
                 numericInput("num_ward_patients", "WARD Patients", value = num_ward_patients),
                 numericInput("num_dc_patients", "D/C", value = num_dc_patients),
                 numericInput("potential_dc", "Potential D/C", value = potential_dc),
                 numericInput("avg_dc_time", "Average D/C Time", value = avg_dc_time)
                 ),
          column(6, h4("Supplies"),
                 numericInput("num_total_beds_occupied", "Total Beds Occupied (%)", value = num_total_beds_occupied, min = 0, step = 1),
                 numericInput("num_n95", "N95 Masks", value = 30, min = 0, step = 1),
                 numericInput("num_goggles", "Goggles", value = 25),
                 numericInput("num_gloves", "Gloves", value = 65),
                 numericInput("num_ppe", "PPE", value = 55, min = 0, step = 1),
                 numericInput("num_vent", "Ventilators", value = 20),
                 )
        ),
        #useShinyjs(), # include shinyjs
        #extendShinyjs(text = jsCode),
        
        fluidRow(
          column(6, h4("Beds"),
                 numericInput("num_er_beds", "ER Beds", value = num_er_beds, min = 0, step = 1), 
                 numericInput("num_icu_beds", "ICU Beds", value = num_icu_beds, min = 0, step = 1),
                 numericInput("num_sdu_beds", "SDU Beds", value = num_sdu_beds, min = 0, step = 1),
                 numericInput("num_ward_beds", "WARD Beds", value = num_ward_beds, min = 0, step = 1),
                 ),
          column(6, h4("Nurses"),
                 numericInput("num_er_nurses", "ER Nurses", value = num_er_nurses, min = 0, step = 1), 
                 numericInput("num_icu_nurses", "ICU Nurses", value = num_icu_nurses, min = 0, step = 1),
                 numericInput("num_sdu_nurses", "SDU Nurses", value = num_sdu_nurses, min = 0, step = 1),
                 numericInput("num_ward_nurses", "WARD Nurses", value = num_ward_nurses, min = 0, step = 1),
                 )
        ),
        fluidRow(
          column(12, h4("Time"),
                 numericInput("avg_door_doc", "Average Door to Doctor", value = 60), 
                 numericInput("avg_er_floor", "Average ER to Floor", value = 30),
                 numericInput("bed_to_icu", "Bed Transfer to ICU", value = 30),
                 numericInput("bed_to_sdu", "Bed Transfer to SDU", value = 30),
                 numericInput("bed_to_ward", "Bed Transfer to WARD", value = 45),
                 numericInput("dc_order_actual", "D/C order to actual D/C", value = 60),
                 )
        ),
        fluidRow(
          column(2, textInput("message", "Mins", value = "mins")),
          column(2, textInput("message1", "Mins", value = "mins")),
          column(2, textInput("message2", "Mins", value = "mins")),
          column(2, textInput("message3", "Mins", value = "mins")),
          column(2, textInput("message4", "Mins", value = "mins")),
          column(2, textInput("message5", "Mins", value = "mins")),
                 
        )
      )), #--sidebarPanel() --div(id = "Sidebar")
    
    
    mainPanel(actionButton("toggleSidebar", "Hide sidebar"),
      #"Main Panel",
      fluidRow(
        column(4, #h3("Column1"),
               
               fluidRow(
                 column(12, h2("Patients"),
                        fixedRow(
                            column(4, "TOTAL", 
                                        "PATIENTS",verbatimTextOutput("num_total_patients"), tags$head(tags$style(HTML("#num_total_patients {font-size: 36px}")))), 
                            column(4, "DEPARTMENT", 
                                             "ER" ,verbatimTextOutput("num_er_patients"), tags$head(tags$style(HTML("#num_er_patients {background-color: black}", "#num_er_patients {color: white}", "#num_er_patients {font-size: 24px}"))),
                                             "ICU",verbatimTextOutput("num_icu_patients"), tags$head(tags$style(HTML("#num_icu_patients {background-color: #505050}", "#num_icu_patients {color: white}", "#num_icu_patients {font-size: 24px}"))),
                                             "SDU",verbatimTextOutput("num_sdu_patients"), tags$head(tags$style(HTML("#num_sdu_patients {background-color: #808080}", "#num_sdu_patients {color: white}", "#num_sdu_patients {font-size: 24px}"))),
                                             "WARD",verbatimTextOutput("num_ward_patients"), tags$head(tags$style(HTML("#num_ward_patients {background-color: #D3D3D3}", "#num_ward_patients {color: white}", "#num_ward_patients {font-size: 24px}"))),
                                             "D/C",verbatimTextOutput("num_dc_patients"), tags$head(tags$style(HTML("#num_dc_patients {font-size: 24px}")))),
                            column(4, "STATUS",
                                            "POTENTIAL D/C", verbatimTextOutput("potential_dc"), tags$head(tags$style(HTML("#potential_dc {font-size: 36px}"))),
                                            "AVG D/C TIME", verbatimTextOutput("avg_dc_time"), tags$head(tags$style(HTML("#avg_dc_time {background-color: gray}", "#avg_dc_time {color: white}", "#avg_dc_time {font-size: 36px}")))
                                   )
                                )
                       )
                 ),
               
               fluidRow(column(12, h2("Beds Occupied (%)"), verbatimTextOutput("num_total_beds_occupied"), 
                               tags$head(tags$style(HTML("#num_total_beds_occupied {background-color: grey}", "#num_total_beds_occupied {color: white}", "#num_total_beds_occupied {font-size: 36px}"))))),
               
               fluidRow(
                 column(12, h2("Supplies"),
                        fixedRow(column(4, "N95 MASK", verbatimTextOutput("num_n95"), tags$head(tags$style(HTML("#num_n95 {background-color: grey}", "#num_n95 {color: white}", "#num_n95 {font-size: 30px}"))), 
                                            "PPE"     ,  verbatimTextOutput("num_ppe"), tags$head(tags$style(HTML("#num_ppe {background-color: grey}", "#num_ppe {color: white}", "#num_ppe {font-size: 30px}")))), 
                                 column(4, "GOGGLES", verbatimTextOutput("num_goggles"), tags$head(tags$style(HTML("#num_goggles {background-color: grey}", "#num_goggles {color: white}", "#num_goggles {font-size: 30px}"))),
                                          "VENTILATORS", verbatimTextOutput("num_vent"), tags$head(tags$style(HTML("#num_vent {background-color: grey}", "#num_vent {color: white}", "#num_vent {font-size: 30px}")))), 
                                 column(4, "GLOVES", verbatimTextOutput("num_gloves"), tags$head(tags$style(HTML("#num_gloves {background-color: grey}", "#num_gloves {color: white}", "#num_gloves {font-size: 30px}"))))
                                 )
                        )
                 )
               
               ),
        column(4, #h3("Column2"),
               fluidRow(column(12, h2("Beds"),
                              fixedRow(column(6, "ER", verbatimTextOutput("num_er_beds"), tags$head(tags$style(HTML("#num_er_beds {background-color: grey}", "#num_er_beds {color: white}", "#num_er_beds {font-size: 36px}"))),
                                                  "SDU", verbatimTextOutput("num_sdu_beds"), tags$head(tags$style(HTML("#num_sdu_beds {background-color: grey}", "#num_sdu_beds {color: white}", "#num_sdu_beds {font-size: 36px}")))
                                              ), 
                                       column(6, "ICU", verbatimTextOutput("num_icu_beds"), tags$head(tags$style(HTML("#num_icu_beds {background-color: grey}", "#num_icu_beds {color: white}", "#num_icu_beds {font-size: 36px}"))),
                                                  "WARD", verbatimTextOutput("num_ward_beds"), tags$head(tags$style(HTML("#num_ward_beds {background-color: grey}", "#num_ward_beds {color: white}", "#num_ward_beds {font-size: 36px}")))
                                              )
                                       )
                               )),
               fluidRow(column(12, h2("Nurses"),
                               fixedRow(column(6, "ER", verbatimTextOutput("num_er_nurses"), tags$head(tags$style(HTML("#num_er_nurses {background-color: grey}", "#num_er_nurses {color: white}", "#num_er_nurses {font-size: 36px}"))),
                                                  "SDU", verbatimTextOutput("num_sdu_nurses"), tags$head(tags$style(HTML("#num_sdu_nurses {background-color: grey}", "#num_sdu_nurses {color: white}", "#num_sdu_nurses {font-size: 36px}")))
                                               ), 
                                        column(6, "ICU", verbatimTextOutput("num_icu_nurses"), tags$head(tags$style(HTML("#num_icu_nurses {background-color: grey}", "#num_icu_nurses {color: white}", "#num_icu_nurses {font-size: 36px}"))),
                                                  "WARD", verbatimTextOutput("num_ward_nurses"), tags$head(tags$style(HTML("#num_ward_nurses {background-color: grey}", "#num_ward_nurses {color: white}", "#num_ward_nurses {font-size: 36px}"))),
                                               )
                                        )
                               )),
               ),
        
        column(4, h2("Time"),#h3("Column3"), 
               
               fluidRow(column(8, "AVG DOOR to DOCTOR", verbatimTextOutput("avg_door_doc"), tags$head(tags$style(HTML("#avg_door_doc {background-color: gray}", "#avg_door_doc {color: white}", "#avg_door_doc {font-size: 28px}")))), 
                        column(4, "TIME", verbatimTextOutput("message"), tags$head(tags$style(HTML("#message {background-color: white}", "#message {color: black}", "#message {font-size: 28px}"))) )),
               
               fluidRow(column(8, "AVG ER to FLOOR", verbatimTextOutput("avg_er_floor"), tags$head(tags$style(HTML("#avg_er_floor {background-color: gray}", "#avg_er_floor {color: white}", "#avg_er_floor {font-size: 28px}")))), 
                        column(4, "TIME", verbatimTextOutput("message1"), tags$head(tags$style(HTML("#message1 {background-color: white}", "#message1 {color: black}", "#message1 {font-size: 28px}"))) )),
               
               fluidRow(column(8, "BED TRANSFER to ICU", verbatimTextOutput("bed_to_icu"), tags$head(tags$style(HTML("#bed_to_icu {background-color: gray}", "#bed_to_icu {color: white}", "#bed_to_icu {font-size: 28px}")))), 
                        column(4, "TIME", verbatimTextOutput("message2"), tags$head(tags$style(HTML("#message2 {background-color: white}", "#message2 {color: black}", "#message2 {font-size: 28px}"))) )),
               
               fluidRow(column(8, "BED TRANSFER to SDU", verbatimTextOutput("bed_to_sdu"), tags$head(tags$style(HTML("#bed_to_sdu {background-color: gray}", "#bed_to_sdu {color: white}", "#bed_to_sdu {font-size: 28px}")))), 
                        column(4, "TIME", verbatimTextOutput("message3"), tags$head(tags$style(HTML("#message3 {background-color: white}", "#message3 {color: black}", "#message3 {font-size: 28px}"))) )),
               
               fluidRow(column(8, "BED TRANSFER to WARD", verbatimTextOutput("bed_to_ward"), tags$head(tags$style(HTML("#bed_to_ward {background-color: gray}", "#bed_to_ward {color: white}", "#bed_to_ward {font-size: 28px}")))), 
                        column(4, "TIME", verbatimTextOutput("message4"), tags$head(tags$style(HTML("#message4 {background-color: white}", "#message4 {color: black}", "#message4 {font-size: 28px}"))) )),
               
               fluidRow(column(8, "D/C ORDER to ACTUAL D/C", verbatimTextOutput("dc_order_actual"), tags$head(tags$style(HTML("#dc_order_actual {background-color: gray}", "#dc_order_actual {color: white}", "#dc_order_actual {font-size: 28px}")))), 
                        column(4, "TIME", verbatimTextOutput("message5"), tags$head(tags$style(HTML("#message5 {background-color: white}", "#message5 {color: black}", "#message5 {font-size: 28px}"))) )),
               
               #fluidRow(column(12, "AVG DOOR to DOCTOR", verbatimTextOutput("avg_door_doc"), tags$head(tags$style(HTML("#avg_door_doc {background-color: orange}", "#avg_door_doc {color: white}", "#avg_door_doc {font-size: 28px}"))))),
               #fluidRow(column(12, "AVG ER to FLOOR", verbatimTextOutput("avg_er_floor"), tags$head(tags$style(HTML("#avg_er_floor {background-color: orange}", "#avg_er_floor {color: white}", "#avg_er_floor {font-size: 28px}"))))),
               #fluidRow(column(12, "BED TRANSFER to ICU", verbatimTextOutput("bed_to_icu"), tags$head(tags$style(HTML("#bed_to_icu {background-color: green}", "#bed_to_icu {color: white}", "#bed_to_icu {font-size: 28px}"))))),
               #fluidRow(column(12, "BED TRANSFER to SDU", verbatimTextOutput("bed_to_sdu"), tags$head(tags$style(HTML("#bed_to_sdu {background-color: green}", "#bed_to_sdu {color: white}", "#bed_to_sdu {font-size: 28px}"))))),
               #fluidRow(column(12, "BED TRANSFER to WARD", verbatimTextOutput("bed_to_ward"), tags$head(tags$style(HTML("#bed_to_ward {background-color: red}", "#bed_to_ward {color: white}", "#bed_to_ward {font-size: 28px}"))))),
               #fluidRow(column(12, "D/C ORDER to ACTUAL D/C", verbatimTextOutput("dc_order_actual"), tags$head(tags$style(HTML("#dc_order_actual {background-color: orange}", "#dc_order_actual {color: white}", "#dc_order_actual {font-size: 28px}"))))),
               ),
        
        
      )
      #plotOutput("distPlot")
    ) # -- mainPanel()
    
 
  
  ) # -- sidebarLayout()
) # -- fluidPage()

# Define server logic  ----
server <- function(input, output, session) {

  ####### toggling sidebarPanel to hide ########
  
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  
  #### ---------------------------------- ####
  
  output$num_total_patients <- renderText({ input$num_total_patients })
  output$num_er_patients <- renderText({ input$num_er_patients })
  output$num_icu_patients <- renderText({ input$num_icu_patients })
  output$num_sdu_patients <- renderText({ input$num_sdu_patients })
  output$num_ward_patients <- renderText({ input$num_ward_patients })
  output$num_dc_patients <- renderText({ input$num_dc_patients })
  output$potential_dc <- renderText({ input$potential_dc })
  output$avg_dc_time <- renderText({ input$avg_dc_time })
  
  output$num_total_beds_occupied <- renderText({ input$num_total_beds_occupied })
  
  output$num_n95 <- renderText({ input$num_n95})
  output$num_ppe <- renderText({ input$num_ppe})
  output$num_goggles <- renderText({ input$num_goggles })
  output$num_vent <- renderText({ input$num_vent })
  output$num_gloves <- renderText({ input$num_gloves })
  
  output$num_er_beds <- renderText({ input$num_er_beds })
  output$num_sdu_beds <- renderText({ input$num_sdu_beds })
  output$num_icu_beds <- renderText({ input$num_icu_beds })
  output$num_ward_beds <- renderText({ input$num_ward_beds })
  
  output$num_er_nurses <- renderText({ input$num_er_nurses })
  output$num_sdu_nurses <- renderText({ input$num_sdu_nurses })
  output$num_icu_nurses <- renderText({ input$num_icu_nurses })
  output$num_ward_nurses <- renderText({ input$num_ward_nurses })
  
  output$avg_door_doc <- renderText({ input$avg_door_doc })
  output$avg_er_floor <- renderText({ input$avg_er_floor })
  output$bed_to_icu <- renderText({ input$bed_to_icu })
  output$bed_to_sdu <- renderText({ input$bed_to_sdu })
  output$bed_to_ward <- renderText({ input$bed_to_ward })
  output$dc_order_actual <- renderText({ input$dc_order_actual })
  
  ####### Add Time - Mins #######
  
  output$message <- renderText({ input$message })
  output$message1 <- renderText({ input$message1 })
  output$message2 <- renderText({ input$message2 })
  output$message3 <- renderText({ input$message3 })
  output$message4 <- renderText({ input$message4 })
  output$message5 <- renderText({ input$message5 })
  
  ####### conditional rendering: AVG D/C TIME ########
  
  observeEvent(input$avg_dc_time, {
    x <- input$avg_dc_time
    if (x > 70){
      js$backgroundCol("avg_dc_time", "red")
    } else if (x <= 70 && x > 60) {
      js$backgroundCol("avg_dc_time", "orange")
    } else {
      js$backgroundCol("avg_dc_time", "green")
    }
  })
  
  ####### conditional rendering: BEDS OCCUPIED ########
  
  observeEvent(input$num_total_beds_occupied, {
    x <- input$num_total_beds_occupied
    if (x < 75){
      js$backgroundCol("num_total_beds_occupied", "red")
    } else if (x >= 75 && x < 100) {
      js$backgroundCol("num_total_beds_occupied", "orange")
    } else {
      js$backgroundCol("num_total_beds_occupied", "green")
    }
  })
  
  ####### conditional rendering: SUPPLIES ########
  
  observeEvent(input$num_n95, {
    x <- input$num_n95
    if (x < 30){
      js$backgroundCol("num_n95", "red")
    } else if (x >= 30 && x < 40) {
      js$backgroundCol("num_n95", "orange")
    } else {
      js$backgroundCol("num_n95", "green")
    }
  })
  
  observeEvent(input$num_ppe, {
    x <- input$num_ppe
    if (x < 55){
      js$backgroundCol("num_ppe", "red")
    } else if (x >= 55 && x < 65) {
      js$backgroundCol("num_ppe", "orange")
    } else {
      js$backgroundCol("num_ppe", "green")
    }
  })
  
  observeEvent(input$num_goggles, {
    x <- input$num_goggles
    if (x < 25){
      js$backgroundCol("num_goggles", "red")
    } else if (x >= 25 && x < 35) {
      js$backgroundCol("num_goggles", "orange")
    } else {
      js$backgroundCol("num_goggles", "green")
    }
  })
  
  observeEvent(input$num_vent, {
    x <- input$num_vent
    if (x < 20){
      js$backgroundCol("num_vent", "red")
    } else if (x >= 20 && x < 30) {
      js$backgroundCol("num_vent", "orange")
    } else {
      js$backgroundCol("num_vent", "green")
    }
  })
  
  observeEvent(input$num_gloves, {
    x <- input$num_gloves
    if (x < 65){
      js$backgroundCol("num_gloves", "red")
    } else if (x >= 65 && x < 75) {
      js$backgroundCol("num_gloves", "orange")
    } else {
      js$backgroundCol("num_gloves", "green")
    }
  })
  
  
  ####### conditional rendering: BEDS ########
  
  observeEvent(input$num_er_beds, {
    x <- input$num_er_beds
    if (x < 15){
      js$backgroundCol("num_er_beds", "red")
    } else if (x >= 15 && x < 20) {
      js$backgroundCol("num_er_beds", "orange")
    } else {
      js$backgroundCol("num_er_beds", "green")
    }
  })
  
  observeEvent(input$num_sdu_beds, {
    x <- input$num_sdu_beds
    if (x < 15){
      js$backgroundCol("num_sdu_beds", "red")
    } else if (x >= 15 && x < 20) {
      js$backgroundCol("num_sdu_beds", "orange")
    } else {
      js$backgroundCol("num_sdu_beds", "green")
    }
  })
  
  observeEvent(input$num_icu_beds, {
    x <- input$num_icu_beds
    if (x < 15){
      js$backgroundCol("num_icu_beds", "red")
    } else if (x >= 15 && x < 20) {
      js$backgroundCol("num_icu_beds", "orange")
    } else {
      js$backgroundCol("num_icu_beds", "green")
    }
  })
  
  observeEvent(input$num_ward_beds, {
    x <- input$num_ward_beds
    if (x < 30){
      js$backgroundCol("num_ward_beds", "red")
    } else if (x >= 30 && x < 40) {
      js$backgroundCol("num_ward_beds", "orange")
    } else {
      js$backgroundCol("num_ward_beds", "green")
    }
  })
  
  ######## conditional rendering: Nurses ########
  
  observeEvent(input$num_er_nurses, {
    x <- input$num_er_nurses
    if (x <= num_er_patients/2){
      js$backgroundCol("num_er_nurses", "red")
    } else if (x > (num_er_patients/2) && x < num_er_nurses) {
      js$backgroundCol("num_er_nurses", "orange")
    } else {
      js$backgroundCol("num_er_nurses", "green")
    }
  })
  
  observeEvent(input$num_icu_nurses, {
    x <- input$num_icu_nurses
    if (x <= num_icu_patients/2){
      js$backgroundCol("num_icu_nurses", "red")
    } else if (x > (num_icu_patients/2) && x < num_icu_nurses) {
      js$backgroundCol("num_icu_nurses", "orange")
    } else {
      js$backgroundCol("num_icu_nurses", "green")
    }
  })
  
  observeEvent(input$num_sdu_nurses, {
    x <- input$num_sdu_nurses
    if (x <= num_sdu_patients/3){
      js$backgroundCol("num_sdu_nurses", "red")
    } else if (x > (num_sdu_patients/3) && x < num_sdu_nurses) {
      js$backgroundCol("num_sdu_nurses", "orange")
    } else {
      js$backgroundCol("num_sdu_nurses", "green")
    }
  })
  
  observeEvent(input$num_ward_nurses, {
    x <- input$num_ward_nurses
    if (x <= num_ward_patients/4){
      js$backgroundCol("num_ward_nurses", "red")
    } else if (x > (num_ward_patients/4) && x < num_ward_nurses) {
      js$backgroundCol("num_ward_nurses", "orange")
    } else {
      js$backgroundCol("num_ward_nurses", "green")
    }
  })
  
  ####### conditional rendering: TIME ########
  
  observeEvent(input$avg_door_doc, {
    x <- input$avg_door_doc
    if (x >= 90){
      js$backgroundCol("avg_door_doc", "red")
    } else if (x < 90 && x > 60) {
      js$backgroundCol("avg_door_doc", "orange")
    } else {
      js$backgroundCol("avg_door_doc", "green")
    }
  })
  
  observeEvent(input$avg_er_floor, {
    x <- input$avg_er_floor
    if (x >= 60){
      js$backgroundCol("avg_er_floor", "red")
    } else if (x < 60 && x > 30) {
      js$backgroundCol("avg_er_floor", "orange")
    } else {
      js$backgroundCol("avg_er_floor", "green")
    }
  })
  
  observeEvent(input$bed_to_icu, {
    x <- input$bed_to_icu
    if (x >= 60){
      js$backgroundCol("bed_to_icu", "red")
    } else if (x < 60 && x > 30) {
      js$backgroundCol("bed_to_icu", "orange")
    } else {
      js$backgroundCol("bed_to_icu", "green")
    }
  })
  
  observeEvent(input$bed_to_sdu, {
    x <- input$bed_to_sdu
    if (x >= 60){
      js$backgroundCol("bed_to_sdu", "red")
    } else if (x < 60 && x > 30) {
      js$backgroundCol("bed_to_sdu", "orange")
    } else {
      js$backgroundCol("bed_to_sdu", "green")
    }
  })
  
  observeEvent(input$bed_to_ward, {
    x <- input$bed_to_ward
    if (x >= 75){
      js$backgroundCol("bed_to_ward", "red")
    } else if (x < 75 && x > 45) {
      js$backgroundCol("bed_to_ward", "orange")
    } else {
      js$backgroundCol("bed_to_ward", "green")
    }
  })
  
  observeEvent(input$dc_order_actual, {
    x <- input$dc_order_actual
    if (x >= 90){
      js$backgroundCol("dc_order_actual", "red")
    } else if (x < 90 && x > 60) {
      js$backgroundCol("dc_order_actual", "orange")
    } else {
      js$backgroundCol("dc_order_actual", "green")
    }
  })
  
  
  
}

# Run the app ----

shinyApp(ui = ui, server = server)