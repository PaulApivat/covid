library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  navbarPage("",
             sidebarLayout(position = "right",
                      div( id ="Sidebar",sidebarPanel(
                      )),


                      mainPanel(actionButton("toggleSidebar", "Toggle sidebar")
                      )
             )
  )
)

server <-function(input, output, session) {
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
}

shinyApp(ui, server)