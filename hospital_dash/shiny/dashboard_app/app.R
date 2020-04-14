library(shiny)

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
        fluidRow(
          column(6, h1("Beds"),
                 numericInput("num_er_beds", "ER Beds", value = 46), 
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
          column(12, h1("Time"))
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
                              fixedRow(column(6, "Bed1", verbatimTextOutput("num_er_beds"), tags$head(tags$style(HTML("#num_er_beds {background-color: orange}", "#num_er_beds {color: white}", "#num_er_beds {font-size: 36px}"))),
                                                         verbatimTextOutput("num_sdu_beds"), tags$head(tags$style(HTML("#num_sdu_beds {background-color: red}", "#num_sdu_beds {color: white}", "#num_sdu_beds {font-size: 36px}")))
                                              ), 
                                       column(6, "Bed2", verbatimTextOutput("num_icu_beds"), tags$head(tags$style(HTML("#num_icu_beds {background-color: green}", "#num_icu_beds {color: white}", "#num_icu_beds {font-size: 36px}"))),
                                                          verbatimTextOutput("num_ward_beds"), tags$head(tags$style(HTML("#num_ward_beds {background-color: green}", "#num_ward_beds {color: white}", "#num_ward_beds {font-size: 36px}")))
                                              )
                                       )
                               )),
               fluidRow(column(12, "Nurses Section",
                               fixedRow(column(6, "Nurse1"), column(6, "Nurse2"))
                               )),
               ),
        column(4, h4("Column3"),
               fluidRow(column(12, "Avg Door to")),
               fluidRow(column(12, "Beds Transfer to ICU")),
               fluidRow(column(12, "Beds Transfer to SDU")),
               fluidRow(column(12, "Beds Transfer to WARD")),
               fluidRow(column(12, "Beds Time D/C Order to Actual D/C")),
               )
      )
      #plotOutput("distPlot")
      
    )
  )
)

# Define server logic  ----
server <- function(input, output) {

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

}

# Run the app ----

shinyApp(ui = ui, server = server)