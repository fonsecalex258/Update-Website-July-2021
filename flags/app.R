library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(checkboxInput("chk", label = "Hide studies: ", value = T)),
  #dashboardSidebar(actionButton("chk", "CAGADA: ")),
  dashboardBody(
    box(width = 8, DT::dataTableOutput("test"))
  )
)

server <- function(input, output, session) {
  df <- reactive({
    #if (input$chk) NULL else data.frame(A = 1:20, B = 2*1:20)
    if (input$chk) NULL else data.frame(cafo3)
  })
  
  output$test <- DT::renderDataTable({
    datatable(df())
  })
}

shinyApp(ui = ui, server = server)