library(shiny)


shinyUI(fluidPage(
    
    titlePanel(h3("Heart Disease Health Indicators")),

    main_page <- tabPanel(
        title = "",
        titlePanel(""),
        sidebarLayout(
            sidebarPanel(
                title = "Inputs",
                fileInput("csv_input","Import CSV file",accept=".csv"),
                hr(style="border-color: purple;"),
                selectInput("var_1","Boxplot",choices="Select"),
                selectInput("var_2","Grouped By",choices="Select"),
                actionButton("run_button","Submit",icon=icon("play")),
                hr(style="border-color: purple;"),
                br(),
                selectInput("var_0","Barplot",choices="Select"),
                actionButton("run_button2","Submit",icon=icon("play"))
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        title = "Grouped Data Analysis",
                plotOutput("plot_1", width = "100%")

                    ),
                    tabPanel(
                        title = "Barplots",
                        plotOutput("plot_2", width = "100%")
                        
                    )
                )
                # conditionalPanel("input$run_button",plotOutput("plot_1", width = "100%")),
                # conditionalPanel("input$run_button2",plotOutput("plot_2", width = "100%")),
                

            )
        )
    ),
    
    
    
    
    about_page <- tabPanel(
        title = "About",
        titlePanel("Data analysis in R"),
        img(src="logo.png"),
        br(),
        "Created with R Shiny",
        br(),
        "Maria Anastasia Rouni",
        br(),
        "2021 November"
    )
    
)
)
