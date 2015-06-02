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
        output$rankedArticles <- renderDataTable(
            {
                if (is.null(v$data)) return()
                v$data
            }, #dtret(),
            options = list(pageLength = 5, 
                           lengthMenu = list(c(5, 10, 15, 20), 
                                             c('5', '10', '15', '20')),
                           reactive = FALSE)
        )
        output$authorPlot <- renderPlot({
            if (is.null(v$data)) return()
            a_histo <- table(data.frame(v$data$authors))
            a_pareto <- a_histo[order(a_histo,decreasing=TRUE)]
            lim <- 10
            par(mai=c(.5,2.5,.1,.1))
            barplot(a_pareto[2:lim+1],horiz=TRUE,las=1,main = "top authors")
            #print(head(v$data))
            #c(1,2,3)
        })
        output$feedPlot <- renderPlot({
            if (is.null(v$data)) return()
            o <- v$data$feed
            o <- table(data.frame(o))
            o_pareto <- o[order(o,decreasing=TRUE)]
            par(mai=c(.5,2.5,.1,.1))
            barplot(o_pareto,horiz=TRUE,las=1,main = "top feeds")
        })
        output$countryPlot <- renderPlot({
            if (is.null(v$data)) return()
            o <- v$data$country
            o <- table(data.frame(o))
            o_pareto <- o[order(o,decreasing=TRUE)]
            lim <- 10
            par(mai=c(.5,2.5,.1,.1))
            barplot(o_pareto[2:lim+1],horiz=TRUE,las=1,main = "top countries outside US")
        })
    }
)


