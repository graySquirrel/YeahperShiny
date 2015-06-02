library(shiny)
# Define UI for Yeahper explore application
shinyUI(fluidPage(
    # Application title
    div(class="outer",
        titlePanel("Yeahper Data Explorer!"),
        
        tags$head(
            # Include our custom CSS
            includeCSS("styles.css")
        ),
    # Sidebar with a slider input for the number of bins
#     sidebarLayout(
#         sidebarPanel(
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                          width = 450, height = "auto",
#             selectInput("select", label = h3("Select a Keyword"), 
#                         choices = list("Choice 1" = 1, "Choice 2" = 2,
#                                        "Choice 3" = 3), selected = 1),
            textInput("searchTerms", label = h3("Keyword search")),
#             dateRangeInput("dates", label = h3("Date range")),
            actionButton("recalculate", label = "Calculate"),
            #tags$br(),
            plotOutput("authorPlot", height = 200),
            plotOutput("feedPlot", height = 200),
            plotOutput("countryPlot", height = 200)
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("rankedArticles")
        )
    )
))