######-------- Notes on creating a Shiny App------ ########

## Bare minimum of a Shiny App

library(shiny)


# Define UI  ----
ui <- fluidPage(
    titlePanel("Title Panel"),
    
    # sdiebarLayout always takes two arguments (cannot have more than one 'mainPanel()')
    # can change position of sidebarPanel (left is default)
    sidebarLayout(position = "right",
      sidebarPanel("Sidebar Panel",
        # can have multiple fluidRow()
        fluidRow(column(), column())
        ),
      mainPanel("Main Panel")
    )
  
)

# Define server logic  ----
server <- function(input, output) {


}

# Run the app ----

shinyApp(ui = ui, server = server)


##### ----------- Structure--------####
# structure, layout, basic colors,
# shinyTime input
install.packages("shinyTime")
library(shinyTime)
# then, conditional colors
library(shinyjs)
library(V8)

