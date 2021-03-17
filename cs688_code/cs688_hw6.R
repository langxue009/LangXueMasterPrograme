library(ndjson)
library(tm)
library(tidyverse)
library(tidytext)
#install.packages("quanteda")
#install.packages("quanteda.textmodels")
library(quanteda)
library(quanteda.textmodels)
#install.packages("topicmodels")
library(topicmodels) 

######### part 2
# get all gz_file names
gz_files <- list.files(pattern = "*.json")

# extract tagline
tagline_list <- lapply(gz_files, function(x){
  indiegogo_json <- stream_in(x)
  return(indiegogo_json$data.tagline)
})

# extract article title
title_list <- lapply(gz_files, function(x){
  indiegogo_json <- stream_in(x)
  return(indiegogo_json$data.title)
})

# merge tagline and title from the 5 JSON files
all_tagline <- do.call("c", tagline_list)
all_title <- do.call("c", title_list)

# convert title to bag of words
indiegogo = tibble(tagline = all_tagline, title = all_title)
title_word = indiegogo %>% select(title) %>% unnest_tokens(word, 'title')

######### part 3

title_corpus = corpus(all_title)
title_dfm = dfm(title_corpus)

# LSA 
title_lsa.model <- textmodel_lsa(title_dfm[1:100,], nd = 7)
#title_lsa.model <- textmodel_lsa(title_dfm, nd = 7) #*****

# LDA 
title_dfm2topicmodels <- convert(title_dfm, to = "topicmodels")
title_lda.model <- LDA(title_dfm2topicmodels, 7)

######### part 4

# LSA

title_lsa.similarity <- as.data.frame(t(title_lsa.model$docs)) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

par(mar = c(0, 4, 4, 2))
plot(title_lsa.similarity,
     main = "LSA topic similarity by features",
     xlab = "",
     sub = "")

# LDA

title_lda.similarity <- as.data.frame(title_lda.model@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

par(mar = c(0, 4, 4, 2))
plot(title_lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

######### Reference
# https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/advancing-text-mining/
