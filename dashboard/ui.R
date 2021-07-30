dashboardPage(
    skin = "purple",
    dashboardHeader(
        title = tags$span(class = "mytitle", "Outcomes used in papers"), 
        titleWidth = 630
    ),
    
    dashboardSidebar(
        sidebarMenu(
            
            menuItem("About the Studies", tabName = "eda", icon = icon("chart-bar"))
            
        )
        
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "eda",
                    fluidRow(
                        box(width = 12,
                            p("The literture about human health impacts of living near production animals is quite limited. After conducting an exhaustive search, our team identified 16 studies consisting of 10 study populations to include in the analysis.
                    Those 16 studies were conducted in only three countries. The health outcomes were lower and upper respiratory tracts, MRSA, other infectious disease, neurological, 
                    psychological, dermatological, otologic, ocular, gastrointestinal, stress and mood, and other non-infectious health outcomes."),
                            radioGroupButtons(
                                inputId = "eda_btn", justified = TRUE, label = "",
                                choices = c(`<i class='fa fa-globe'></i> Geographical Distribution` = "sp", 
                                            `<i class='fa fa-calendar-alt'></i> Timeline` = "ts", 
                                            `<i class='fa fa-poll'></i> Health Outcome` = "coef")
                            ),
                            uiOutput("eda_text"),
                            uiOutput("eda_plot")
                        )
                    )
            )
        )
    )
    
            