# mongoquery.R will do utilities for special parsing of results from Mongo
#
# these are globals for the entire app... not session or anything.
#  Notice that it is executed inline, not a function.
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
####################################################################
queryMongo <- function(mg1, startDate, endDate, searchTerms) {
    #print(searchTerms)
    if (!readCache) {
        # we want to filter query by date range and boolean or of search terms
        yesterday <- format(Sys.time() - 86400,
                            tz="Europe/London",format="%Y-%m-%dt%H:%M:%Sz")
        query <- dbGetQuery(mg1, 'articles', 
                            paste("{'PublishDate': {'$gt':'", yesterday, "'}}",sep=""))
    }
    else query
}
####################################################################
keywordCleanup <- function(keywords) {
    # parse the keywords to make keyword vectors for each article
    keywords <- strsplit(keywords, "\"")
    keywords <- lapply(keywords,function(x) x[x != " , " & x != "[ " & x != "]"])
    keywords <- lapply(keywords,function(x) paste(x,sep="",collapse=" "))

    return (keywords)
}
####################################################################
authorCleanup <- function(authors) {
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
    return(authors)
}
####################################################################