# helper.R
# high level functions that are used in server.R

####################################################################
# what i want to do is to get the search info, compiled from
#   Date Range, search terms 
# and create a mongo query that will do a date bound boolean OR search
# and return queryResults
# mongoResults <- queryMongo(mg1, startDate, endDate, searchTerms)
#
# then we will constuct the tfidf, and create a ranking score for similarity
# doc.scores <- create.doc.scores(mongoResults)
#
# Then we need to post process - sort, plot, map, etc.
####################################################################
doit <- function(input) {
    searchTerms <- input$searchTerms
    startDate <- NULL # tbd
    endDate <- NULL # tbd
    #mg1 is global from mongoquery.R
    # this will be the raw query results
    mongoResults <- queryMongo(mg1, startDate, endDate, searchTerms)
    # clean up keywords - specific to the db schema
    keywords.list <- keywordCleanup(mongoResults$Keywords)
    # add Heading to keywords list for more completeness
    keywords.list <- paste(keywords.list,mongoResults$Heading)
    # clean up authors - specific to the db schema
    authors <- authorCleanup(mongoResults$Authors)
    
    tfidf.matrix <- create.tfidf.from.doclist(keywords.list)
    doc.scores <- create.doc.scores.from.tfidf.and.query(tfidf.matrix, searchTerms)
    results.df <- data.frame(score = t(doc.scores)/max(doc.scores), 
                             mongoResults$Heading,
                             mongoResults$PublishDate,
                             mongoResults$OriginatingFeed,
                             authors,
                             text = unlist(keywords.list))
    colnames(results.df) <- c("score","heading","date","feed","authors","keywords")
    results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
    return(results.df)
}