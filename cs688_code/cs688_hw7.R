
library(RISmed)
#install.packages("arulesSequences")
library("arulesSequences")
library(tm)
library(quanteda.textmodels) # for dfm
library(quanteda)

### query 500 article information :

# since some article titles could be NA, I first get about 700 articles, then subset 500 articles with non-NA title
res <- EUtilsSummary(query = "alzheimer", type="esearch", db="pubmed", datetype='pdat', mindate=2018, maxdate=2020, retmax=700)
fetch <- EUtilsGet(res) # get the query results
pubmed_title <- ArticleTitle(fetch) # extract Title
pubmed_year <- YearPubmed(fetch) # extract publish year
pubmed_month <- month.abb[MonthPubmed(fetch)] # extract publish month

# select 500 non-NA title
ind <- which(!is.na(pubmed_title))[1:500]
article_title <- pubmed_title[ind]
article_year <- pubmed_year[ind]
article_month <- pubmed_month[ind]

# using dfm() to create document term matrix
article_title1 <- removePunctuation(article_title, preserve_intra_word_dashes = T) # remove punctuation
article_title2 <- gsub("[0-9]", " ", article_title1) # remove numbers
corp <- corpus(article_title2)
DTM <- dfm(corp, tolower = T, remove = stopwords("english"), verbose = T) # document term matrix
DTM_matrix <- as(DTM, "matrix")

# create items for each document
keywords <- colnames(DTM)
keywords_items <- sapply(1:nrow(DTM), function(x) paste(keywords[DTM_matrix[x,] > 0], collapse = ','))
write.table(keywords_items, "keywords_items.csv", quote = FALSE, col.names = FALSE, row.names = FALSE)

# *** read the keywords backs ***
keywords_items <- read.table("keywords_items.csv") 
pubmed_tr <- read.transactions("keywords_items.csv", sep = ',', quote = '')

### Association Rule Mining Algorithm: 

# apriori
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, target = "rules"))
summary(apriori_rules)
apriori_result <- as(apriori_rules, "data.frame")
apriori_result <- apriori_result[order(apriori_result$support, decreasing = TRUE),]
head(apriori_result)

# eclat
eclat_rules <- eclat(pubmed_tr, parameter = list(supp = 0.02))
summary(eclat_rules)
eclat_result <- as(eclat_rules, "data.frame")
eclat_result <- eclat_result[order(eclat_result$support, decreasing = TRUE),]
head(eclat_result)

### Sequence Mining Algorithm: SPADE
pubmed_tr2 <- pubmed_tr
pubmed_tr2@itemsetInfo <- data.frame(sequenceID = rep(1:5, each = 100), eventID = rep(1:100, 5))

cspade_rules <- cspade(pubmed_tr2, parameter = list(support = 0.8, maxsize = 1, maxlen = 1), control = list(verbose = TRUE))
summary(cspade_rules)
cspade_result <- as(cspade_rules, "data.frame")
cspade_result <- cspade_result[order(cspade_result$support, decreasing = TRUE),]
head(cspade_result)
