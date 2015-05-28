library(shiny)
library(rJava)
library(RMongo)
library(tm)
source("tfidfDotProduct.R")

readCache <- TRUE
if (readCache) {
    query <- read.table(file="query.csv",header = TRUE, sep = ",",
                        colClasses = "character")
} else {
    mg1 <- mongoDbConnect("MongoLab-gt",
                          host="ds045077.mongolab.com",
                          port=45077)
    dbAuthenticate(mg1, "bbacon", "Password01")
    
    print(dbShowCollections(mg1))
}

asdf <- function(input) {
    thequery <- input$searchTerms
    print("Dfoo")
    print(thequery)
}

# check out observe and isolate for ref
# http://shiny.rstudio.com/reference/shiny/latest/observeEvent.html
# btw, it is deprecated, so caveat emptor
# using eventReactive so table only updated when recalculate is clicked, not automatically.
shinyServer(
    function(input, output) {
        dfoo <- eventReactive(input$recalculate, function() {
            asdf(input)
        })
        dtret <- eventReactive(input$recalculate, function() {
            thequery <- input$searchTerms
            #print(thequery)
            # form the query
            if (!readCache) {
                # we want to filter query by date range and boolean or of search terms
                yesterday <- format(Sys.time() - 86400,
                                    tz="Europe/London",format="%Y-%m-%dt%H:%M:%Sz")
                query <- dbGetQuery(mg1, 'articles', 
                                    paste("{'PublishDate': {'$gt':'", yesterday, "'}}",sep=""))
            }
            # compile keywords
            keywords <- query$Keywords
            # parse the keywords to make keyword vectors for each article
            keywords <- strsplit(keywords, "\"")
            keywords <- lapply(keywords,function(x) x[x != " , " & x != "[ " & x != "]"])
            keywords.list <- lapply(keywords,function(x) paste(x,sep="",collapse=" "))
            # remove empty keyword entries
            #keywords.list <- keywords.list[keywords.list != ""]
            #headings.list <- query$Heading[keywords.list != ""]
            #keywords.list <- paste(keywords.list,headings.list)
            keywords.list <- paste(keywords.list,query$Heading)
            #print(length(keywords.list))
            # convert the keyword list to a tfidf matrix
            tfidf.matrix <- create.tfidf.from.doclist(keywords.list)
            # how big is it?
            #print(paste("tfidf matrix size: ",dim(tfidf.matrix)))
            # print a sample
            #tfidf.matrix[90:100, 6:20]
            
            # Now map the query text onto the document tfidf matrix to get doc scores
            authors <- query$Authors
            authors <- strsplit(authors, "\"")
            authors <- lapply(authors,function(x) x[which(x == "FullName")+2])
            # i'm sure there is a regex for the following
            authors <- lapply(authors, function(x) sub("the associated press","",x))
            authors <- lapply(authors, function(x) sub("associated press","",x))
            # the as.character inserts NA where there is NULL strings
            authors <- unlist(as.character(authors))
            authors <- gsub("character\\(0\\)","unknown",authors)
            authors <- gsub("^$","unknown", authors)
            authors <- gsub("c\\(","", authors)
            authors <- gsub("\\)","", authors)
            authors <- gsub("\"","", authors)
            
            doc.scores <- create.doc.scores.from.tfidf.and.query(tfidf.matrix, thequery)
            results.df <- data.frame(score = t(doc.scores)/max(doc.scores), 
                                     query$Heading,
                                     query$PublishDate,
                                     query$OriginatingFeed,
                                     authors,
                                     text = unlist(keywords.list))
            colnames(results.df) <- c("score","heading","date","feed","authors","keywords")
            results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
            return(results.df)
        })

        output$rankedArticles <- renderDataTable(
            dfoo(),#dtret(),
            options = list(pageLength = 5, 
                           lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
                           reactive = FALSE)
        )}
)


