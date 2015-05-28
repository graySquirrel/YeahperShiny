library(shiny)
# Define UI for Yeahper explore application
shinyUI(fluidPage(
    # Application title
    titlePanel("Yeahper Data Explorer!"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
#             selectInput("select", label = h3("Select a Keyword"), 
#                         choices = list("Choice 1" = 1, "Choice 2" = 2,
#                                        "Choice 3" = 3), selected = 1),
            textInput("searchTerms", label = h3("Keyword search")),
#             dateRangeInput("dates", label = h3("Date range")),
            actionButton("recalculate", label = "Calculate")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("rankedArticles")
        )
    )
    
))