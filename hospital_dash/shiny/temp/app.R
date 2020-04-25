library(shiny)
library(shinyjs)

if (interactive()){
  shinyApp(
    
    ui <- fluidPage(
      
      actionButton("show", "Show modal dialog"),
      
      useShinyjs(),
      sidebarLayout(position = "right",
                    div( id ="Sidebar",sidebarPanel(
                    )),
                    
                    
                    mainPanel(actionButton("toggleSidebar", "Toggle sidebar")
                    )
      )
    ), # ui
    
    server <-function(input, output, session) {
      observeEvent(input$toggleSidebar, {
        shinyjs::toggle(id = "Sidebar")
      })
      
      observeEvent(input$show, {
        url <- a("Explainer Post", href="http://getwyze.com/")
        showModal(modalDialog(
          title = "Important Message",
          tagList("This is an important message!", url)
        ))
      })
      
    }
    
  ) # shinyApp
} # if (interactive())





shinyApp(ui, server)