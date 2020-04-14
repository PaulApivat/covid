library(shiny)

# Define UI  ----
ui <- fluidPage(
  
    titlePanel("Hospital Resource Management: COVID19"),
    
    sidebarLayout(position = "right",
      sidebarPanel(
        
        fluidRow(
          column(6, h1("Patients"), 
                 numericInput("num", "Number of Total Patients", value = 36), 
                 numericInput("num", "ER Patients", value = 2),
                 numericInput("num", "ICU Patients", value = 4),
                 numericInput("num", "SDU Patients", value = 20),
                 numericInput("num", "WARD Patients", value = 10),
                 numericInput("num", "D/C", value = 0),
                 ),
          column(6, h1("Supplies"),
                 numericInput("percent", "Total Beds Occupied (%)", value = 74),
                 numericInput("num", "N95 Masks", value = 30),
                 numericInput("num", "Goggles", value = 25),
                 numericInput("num", "Gloves", value = 65),
                 numericInput("num", "PPE", value = 55),
                 numericInput("num", "Ventilators", value = 20),
                 )
        ),
        
      ),
    
    
    mainPanel(
      "Main Panel",
      fluidRow(
        column(4, h4("Column1")),
        column(4, h4("Column2")),
        column(4, h4("Column3"))
      )
      #plotOutput("distPlot")
      
    )
  )
)

# Define server logic  ----
server <- function(input, output) {


}

# Run the app ----

shinyApp(ui = ui, server = server)