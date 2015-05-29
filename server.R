library(shiny)
library(rJava)
library(RMongo)
library(tm)
source("tfidfDotProduct.R")
source("mongoQuery.R")
source("helper.R")

####################################################################
# check out observe and isolate for ref
# http://shiny.rstudio.com/reference/shiny/latest/observeEvent.html
# btw, it is deprecated, so caveat emptor
# using eventReactive so table only updated when recalculate is 
#    clicked, not automatically.
shinyServer(
    function(input, output) {
        v <- reactiveValues(data = NULL)
        
        #dtret <- eventReactive
        observeEvent(input$recalculate, function() {
            #print("doing it")
            v$data <- doit(input) # defined in helper.R
        })
#         observeEvent(input$searchTerms, function() {
#             print("doing it too")
#             v$data <- doit(input) # defined in helper.R
#         })
        output$rankedArticles <- renderDataTable({
            if (is.null(v$data)) return()
            v$data
        }, #dtret(),
        options = list(pageLength = 5, 
                       lengthMenu = list(c(5, 10, 15, 20), 
                                         c('5', '10', '15', '20')),
                       reactive = FALSE)
        )}
)


