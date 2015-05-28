library(NLP)
library(tm)
# tfidfDotProduct.R
# implements
# create.tfidf.from.doclist(doclist) returns tfidf matrix. Each row is a term
#    doclist is a list of character strings
# create.tfidf.from.query(doc.tfidf, query) returns a tfidf vector for the query
#    doc.tfidf is output from create.tfidf.from.doclist
#    query is a character string
# create.doc.scores.from.tfidf.and.query(doc.tfidf, query) returns a doc.score matrix
#    doc.tfidf is output from create.tfidf.from.doclist
#    query is a character string
#    This uses create.tfidf.from.query, then also does the dot product.

# Internal functions
dd <- F
## Compute tfidf weights from a term frequency vector and a document
# frequency scalar
get.tf.idf.weights <- function(tf.vec, df, numDocs) {
    weight = rep(0, length(tf.vec))
    weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(numDocs/df)
    #    weight[tf.vec > 0] = tf.vec[tf.vec > 0] * log2(N.keywords/df)
    #    weight[tf.vec > 0] = tf.vec[tf.vec > 0]
    #    weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0]))
    return(weight)
}

## Compute weights vector for a term
### input is a vector of tf across all documents in corpus
get.weights.per.term.vec <- function(tfidf.row) {
    NumberOfDocuments <- length(tfidf.row)
    term.df <- sum(tfidf.row > 0)
    tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df, NumberOfDocuments)
    return(tf.idf.vec)
}

#### create.tfidf.from.doclist
create.tfidf.from.doclist <- function(doclist) {
    my.docs <- VectorSource(doclist)
    my.docs$Names <- names(doclist)
    NumberOfDocuments <- length(doclist)
    # create corpus
    my.corpus <- Corpus(my.docs)
    # clean up corpus
    my.corpus <- tm_map(my.corpus, content_transformer(removePunctuation))
    #my.corpus <- tm_map(my.corpus, stemDocument) # Snowball not available for R 3.1.2
    my.corpus <- tm_map(my.corpus, content_transformer(removeNumbers))
    my.corpus <- tm_map(my.corpus, content_transformer(tolower))
    my.corpus <- tm_map(my.corpus, content_transformer(stripWhitespace))
    #Create the Term-Document matrix, which is a 2D representation of the 
    #  term frequency for each document.  Terms in rows, docs in cols
    my.corpus <- tm_map(my.corpus, content_transformer(PlainTextDocument))
    term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
    #Convert the sparse matrix representation to a normal matrix to work from.
    term.doc.matrix <- as.matrix(term.doc.matrix.stm)
    # Calculate tfidf weights
    tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
    colnames(tfidf.matrix) <- colnames(term.doc.matrix)
    # Dont forget to scale document vectors by sum of square to normalize
    tfidf.matrix <- scale(tfidf.matrix, center = FALSE, 
                          scale = sqrt(colSums(tfidf.matrix^2)))
    return(tfidf.matrix)
}

#### create.tfidf.from.query
create.tfidf.from.query <- function(doc.tfidf, query) {
    if(dd){print(class(query));print (query)}
    my.query <- unlist(strsplit(query," "))  # split query into vector of terms
    # don't forget to normalize query text same way as doc corpus was.
    my.query <- removePunctuation(my.query)
    my.query <- removeNumbers(my.query)
    my.query <- tolower(my.query)
    my.query <- stripWhitespace(my.query)

#     if(dd){print(class(my.query));print (query);print(my.query)}
#     my.query <- VectorSource(my.query)
#     my.query <- Corpus(my.query)
#     my.query <- tm_map(my.query, content_transformer(removePunctuation))
#     #my.corpus <- tm_map(my.corpus, stemDocument) # Snowball not available for R 3.1.2
#     my.query <- tm_map(my.query, content_transformer(removeNumbers))
#     my.query <- tm_map(my.query, content_transformer(tolower))
#     my.query <- tm_map(my.query, content_transformer(stripWhitespace))
    
    term.names <- row.names(doc.tfidf) # get term names from tfidf matrix
    numDocs <- dim(doc.tfidf)[2]
    numTerms <- length(term.names)
    if(dd){print(numDocs);print(numTerms);print(my.query)}
    weights = rep(0, numTerms)
    for (i in my.query) {
        term.index <- which(term.names == i)
        if(dd){print("term index");print(term.index);print(i)}
        if (length(term.index) > 0) {       # length will be 0 if no match
            term <- doc.tfidf[term.index,]
            term.df <- sum(term > 0)
            weights[term.index] <- get.tf.idf.weights(c(1), term.df, numDocs)
        }
    }
    weights <- scale(weights, center = FALSE, scale = sqrt(sum(weights^2)))
    return(weights)
}

#### create.doc.scores.from.tfidf.and.query
create.doc.scores.from.tfidf.and.query <- function(doc.tfidf, query) {
    # convert the query to a tfidf matrix
    query.vector <- create.tfidf.from.query(doc.tfidf, query)
    # get the similarity vector from the dot product
    doc.scores <- t(query.vector) %*% doc.tfidf
    return(doc.scores)
}