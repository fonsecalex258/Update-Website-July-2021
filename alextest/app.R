library(shiny)
ui <- fluidPage(
    htmlOutput("mytext"), 
    textOutput ("selected_var")
    
)

server <- function(input, output, session) {
    output$mytext <- renderUI({
        mylist <- c("a", "b", "c", "d")
        HTML(paste(mylist, sep = "", collapse = '<br/>'))
    })
    
    
    
    output$selected_var <- renderText({ 
        
        
        #d = ncol((forest_case))
        
        
        text("You have selected", 3, " hello")
    })
}

shinyApp(ui, server)