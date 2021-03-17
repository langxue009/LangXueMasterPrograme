
library(rtweet)
library(tidytext)
#install.packages("syuzhet")
library(syuzhet)
#install.packages('udpipe')
library(udpipe)

# search for tweets
tweets <- search_tweets("fitness+wearables", n = 90)
contents <- tolower(unique(tweets$text))
has_fitness <- grepl("fitness", contents)
has_wearables <- grepl("wearables", contents)
contents <- contents[has_fitness & has_wearables]
length(contents)

# obtain sentiment score
scores_nrc <- get_sentiment(contents, method = 'nrc')
scores_afinn <- get_sentiment(contents, method = 'afinn')
scores_bing <- get_sentiment(contents, method = 'bing')

# theme analysis 
model <- udpipe_download_model(language = "english-ewt")
ud_english <- udpipe_load_model(model$file_model)
contents_ud <- udpipe_annotate(ud_english, x = contents, tagger = "default", parser = "none")
contents_anno <- as.data.frame(contents_ud)
keywords <- keywords_rake(contents_anno, term = "lemma", group = "doc_id",
                          relevant = contents_anno$xpos %in% c("NN"), n_min = 0, ngram_max = 100)
head(keywords)

# output
tweet_keywords = ""
sentiment_df = data.frame(tweet = contents, sentiments_scores = scores_afinn, tweet_keywords)
write.csv(sentiment_df, "sentiment_df.csv")
