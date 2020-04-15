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


# Define UI  ----
ui <- fluidPage(
    
  
    titlePanel("Hospital Resource Management: COVID19"),
    
    sidebarLayout(position = "right",
      sidebarPanel(
        
        
        fluidRow(
          column(6, h1("Patients"), 
                 numericInput("num_total_patients", "Number of Total Patients", value = 36), 
                 numericInput("num_er_patients", "ER Patients", value = 2),
                 numericInput("num_icu_patients", "ICU Patients", value = 4),
                 numericInput("num_sdu_patients", "SDU Patients", value = 20),
                 numericInput("num_ward_patients", "WARD Patients", value = 10),
                 numericInput("num_dc_patients", "D/C", value = 0),
                 ),
          column(6, h1("Supplies"),
                 numericInput("num_total_beds_occupied", "Total Beds Occupied (%)", value = 74),
                 numericInput("num_n95", "N95 Masks", value = 30),
                 numericInput("num_goggles", "Goggles", value = 25),
                 numericInput("num_gloves", "Gloves", value = 65),
                 numericInput("num_ppe", "PPE", value = 55),
                 numericInput("num_vent", "Ventilators", value = 20),
                 )
        ),
        useShinyjs(), # include shinyjs
        extendShinyjs(text = jsCode),
        
        fluidRow(
          column(6, h1("Beds"),
                 numericInput("num_er_beds", "ER Beds", value = 46, min = 0, step = 1), 
                 numericInput("num_icu_beds", "ICU Beds", value = 50),
                 numericInput("num_sdu_beds", "SDU Beds", value = 63),
                 numericInput("num_ward_beds", "WARD Beds", value = 97),
                 ),
          column(6, h1("Nurses"),
                 numericInput("num_er_nurses", "ER Nurses", value = 68), 
                 numericInput("num_icu_nurses", "ICU Nurses", value = 32),
                 numericInput("num_sdu_nurses", "SDU Nurses", value = 18),
                 numericInput("num_ward_nurses", "WARD Nurses", value = 23),
                 )
        ),
        fluidRow(
          column(12, h1("Time"),
                 textInput("avg_door_doc", "Average Door to Doctor", value = "83 mins"),
                 textInput("avg_er_floor", "Average ER to Floor", value = "45 mins"),
                 textInput("bed_to_icu", "Bed Transfer to ICU", value = "0 hr 29 mins"),
                 textInput("bed_to_sdu", "Bed Transfer to SDU", value = "0 hr 35 mins"),
                 textInput("bed_to_ward", "Bed Transfer to WARD", value = "2 hr 05 mins"),
                 textInput("dc_order_actual", "D/C order to actual D/C", value = "1 hr 01 mins"),
                 )
        )
      ),
    
    
    mainPanel(
      "Main Panel",
      fluidRow(
        column(4, h4("Column1"),
               
               fluidRow(
                 column(12, "Patient Section",
                        fixedRow(
                            column(4, "Total", verbatimTextOutput("num_total_patients"), tags$head(tags$style(HTML("#num_total_patients {font-size: 36px}")))), 
                            column(4, "Dept", verbatimTextOutput("num_er_patients"), tags$head(tags$style(HTML("#num_er_patients {background-color: black}", "#num_er_patients {color: white}"))),
                                              verbatimTextOutput("num_icu_patients"), tags$head(tags$style(HTML("#num_icu_patients {background-color: #505050}", "#num_icu_patients {color: white}"))),
                                              verbatimTextOutput("num_sdu_patients"), tags$head(tags$style(HTML("#num_sdu_patients {background-color: #808080}", "#num_sdu_patients {color: white}"))),
                                              verbatimTextOutput("num_ward_patients"), tags$head(tags$style(HTML("#num_ward_patients {background-color: #D3D3D3}", "#num_ward_patients {color: white}"))),
                                              verbatimTextOutput("num_dc_patients")),
                            column(4, "Status")
                                )
                       )
                 ),
               
               fluidRow(column(12, "Total Beds Occupied", verbatimTextOutput("num_total_beds_occupied"), 
                               tags$head(tags$style(HTML("#num_total_beds_occupied {background-color: orange}", "#num_total_beds_occupied {color: black}", "#num_total_beds_occupied {font-size: 36px}"))))),
               
               fluidRow(
                 column(12, "Supplies",
                        fixedRow(column(4, "Supplies1", verbatimTextOutput("num_n95"), tags$head(tags$style(HTML("#num_n95 {background-color: green}", "#num_n95 {color: white}"))), 
                                                        verbatimTextOutput("num_ppe"), tags$head(tags$style(HTML("#num_ppe {background-color: green}", "#num_ppe {color: white}")))), 
                                 column(4, "Supplies2", verbatimTextOutput("num_goggles"), tags$head(tags$style(HTML("#num_goggles {background-color: red}", "#num_goggles {color: white}"))),
                                                        verbatimTextOutput("num_vent"), tags$head(tags$style(HTML("#num_vent {background-color: green}", "#num_vent {color: white}")))), 
                                 column(4, "Supplies3", verbatimTextOutput("num_gloves"), tags$head(tags$style(HTML("#num_gloves {background-color: orange}", "#num_gloves {color: white}"))))
                                 )
                        )
                 )
               
               ),
        column(4, h4("Column2"),
               fluidRow(column(12, "Beds Section",
                              fixedRow(column(6, "Bed1", verbatimTextOutput("num_er_beds"), tags$head(tags$style(HTML("#num_er_beds {background-color: white}", "#num_er_beds {color: black}", "#num_er_beds {font-size: 36px}"))),
                                                         verbatimTextOutput("num_sdu_beds"), tags$head(tags$style(HTML("#num_sdu_beds {background-color: red}", "#num_sdu_beds {color: white}", "#num_sdu_beds {font-size: 36px}")))
                                              ), 
                                       column(6, "Bed2", verbatimTextOutput("num_icu_beds"), tags$head(tags$style(HTML("#num_icu_beds {background-color: green}", "#num_icu_beds {color: white}", "#num_icu_beds {font-size: 36px}"))),
                                                          verbatimTextOutput("num_ward_beds"), tags$head(tags$style(HTML("#num_ward_beds {background-color: green}", "#num_ward_beds {color: white}", "#num_ward_beds {font-size: 36px}")))
                                              )
                                       )
                               )),
               fluidRow(column(12, "Nurses Section",
                               fixedRow(column(6, "Nurse1", verbatimTextOutput("num_er_nurses"), tags$head(tags$style(HTML("#num_er_nurses {background-color: green}", "#num_er_nurses {color: white}", "#num_er_nurses {font-size: 36px}"))),
                                                            verbatimTextOutput("num_sdu_nurses"), tags$head(tags$style(HTML("#num_sdu_nurses {background-color: red}", "#num_sdu_nurses {color: white}", "#num_sdu_nurses {font-size: 36px}")))
                                               ), 
                                        column(6, "Nurse2", verbatimTextOutput("num_icu_nurses"), tags$head(tags$style(HTML("#num_icu_nurses {background-color: orange}", "#num_icu_nurses {color: white}", "#num_icu_nurses {font-size: 36px}"))),
                                                            verbatimTextOutput("num_ward_nurses"), tags$head(tags$style(HTML("#num_ward_nurses {background-color: red}", "#num_ward_nurses {color: white}", "#num_ward_nurses {font-size: 36px}"))),
                                               )
                                        )
                               )),
               ),
        column(4, h4("Column3"),
               fluidRow(column(12, "Avg Door to Doctor", verbatimTextOutput("avg_door_doc"), tags$head(tags$style(HTML("#avg_door_doc {background-color: orange}", "#avg_door_doc {color: white}", "#avg_door_doc {font-size: 28px}"))))),
               fluidRow(column(12, "Avg ER to Floor", verbatimTextOutput("avg_er_floor"), tags$head(tags$style(HTML("#avg_er_floor {background-color: orange}", "#avg_er_floor {color: white}", "#avg_er_floor {font-size: 28px}"))))),
               fluidRow(column(12, "Beds Transfer to ICU", verbatimTextOutput("bed_to_icu"), tags$head(tags$style(HTML("#bed_to_icu {background-color: green}", "#bed_to_icu {color: white}", "#bed_to_icu {font-size: 28px}"))))),
               fluidRow(column(12, "Beds Transfer to SDU", verbatimTextOutput("bed_to_sdu"), tags$head(tags$style(HTML("#bed_to_sdu {background-color: green}", "#bed_to_sdu {color: white}", "#bed_to_sdu {font-size: 28px}"))))),
               fluidRow(column(12, "Beds Transfer to WARD", verbatimTextOutput("bed_to_ward"), tags$head(tags$style(HTML("#bed_to_ward {background-color: red}", "#bed_to_ward {color: white}", "#bed_to_ward {font-size: 28px}"))))),
               fluidRow(column(12, "Beds Time D/C Order to Actual D/C", verbatimTextOutput("dc_order_actual"), tags$head(tags$style(HTML("#dc_order_actual {background-color: orange}", "#dc_order_actual {color: white}", "#dc_order_actual {font-size: 28px}"))))),
               )
      )
      #plotOutput("distPlot")
      
    )
  )
)

# Define server logic  ----
server <- function(input, output, session) {

  output$num_total_patients <- renderText({ input$num_total_patients })
  output$num_er_patients <- renderText({ input$num_er_patients })
  output$num_icu_patients <- renderText({ input$num_icu_patients })
  output$num_sdu_patients <- renderText({ input$num_sdu_patients })
  output$num_ward_patients <- renderText({ input$num_ward_patients })
  output$num_dc_patients <- renderText({ input$num_dc_patients })
  
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
  
  observeEvent(input$num_er_beds, {
    x <- input$num_er_beds
    if (x < 46){
      js$backgroundCol("num_er_beds", "red")
    } else if (x >= 46 && x < 70) {
      js$backgroundCol("num_er_beds", "orange")
    } else {
      js$backgroundCol("num_er_beds", "green")
    }
  })
  
}

# Run the app ----

shinyApp(ui = ui, server = server)