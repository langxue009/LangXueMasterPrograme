library(ggplot2)
library(tidyr)
library(gridExtra)

#install.packages("RISmed")
library(RISmed)
#help(package="RISmed")

#install.packages("ggalt")
library(ggalt)

# sentiment packages
#install.packages("syuzhet")
library(syuzhet)

library("dtw")
library('stats')
library(tm)
library(quanteda.textmodels)
library(quanteda)
library("arulesSequences")

####################### Question 1/2

terms <- c("obesity", "cancer", "covid-19", "wearable", "mental health", "influenza")
years <- 2011:2020

# create function to extract the frequency based on year and term
terms_freq = function(year, term){
  res <- EUtilsSummary(term, type="esearch", db="pubmed", datetype='pdat', mindate=year, maxdate=year, retmax=1000)
  return(QueryCount(res))
}

# query from the E-utility servers
freq_counts <- lapply(terms, function(x) sapply(years, function(y) terms_freq(y, x)))
names(freq_counts) <- terms
freq_counts <- do.call("rbind", freq_counts)
colnames(freq_counts) <- 2011:2020

# plot area chart
freq_counts
time <- rep(2011:2020, 6)
frequency <- as.numeric(t(freq_counts))
term_group <- rep(terms, each = 10)
freq_dataframe <- data.frame(time, frequency, term_group)

ggplot(freq_dataframe, aes(x=time, y=frequency, fill=term_group)) + geom_area() + 
  ggtitle("Area Chart of the frequencies for terms Influenza, Obesity, Cancer, Covid-19, Wearable, Mental Health")

# plot Dumbbell-chart

freq_df <- data.frame(terms, freq_counts[,c("2011", "2020")])
ggplot(freq_df, aes(y=terms, x=X2011, xend=X2020)) + 
  geom_dumbbell(size = 1, colour_x = "red", colour_xend = "blue", dot_guide=TRUE, dot_guide_size=0.25) + 
  xlab("frequency") + 
  ggtitle("Dumbbell-chart of the keywords change from 2011 to 2020")

####################### Question 3

### understanding the sentiment methods 

char_v = c("abandoned", "abandon", "aabandon", "abhors")
get_sentiment(char_v, method = "nrc", path_to_tagger = NULL,
              cl = NULL, language = "english", lexicon = NULL)
get_sentiment(char_v, method = "afinn", path_to_tagger = NULL,
              cl = NULL, language = "english", lexicon = NULL)

# briefly check the words in each method 
library(tidytext)
get_sentiments("bing") # bing defined sentiments for 6,786 words
get_sentiments("afinn") # bing defined sentiments for 2,477 words (score from -5 to 5)
get_sentiments("nrc") # bing defined sentiments (negative or positive only) for 5,636 words

table(get_sentiments("afinn")$value)

nrc_words = get_sentiments("nrc")
nrc_words_pos_neg = subset(nrc_words, nrc_words$sentiment %in% c("negative", "positive"))
table(nrc_words_pos_neg$sentiment)

table(get_sentiments("nrc")$value)

length(unique(get_sentiments("nrc")$word)) # nrc has 6468 words
sum(table(nrc_words_pos_neg$sentiment)) # nrc only has 5636 words indicated as positive or negative


# create function to extract the setiment score based on year and term
sentiment = function(year, term){
  res <- EUtilsSummary(term, type="esearch", db="pubmed", datetype='pdat', mindate=year, maxdate=year, retmax=1000)
  fetch <- EUtilsGet(res)
  contents <- ArticleTitle(fetch)
  
  # obtain sentiment score
  scores_nrc <- get_sentiment(contents, method = 'nrc')
  scores_afinn <- get_sentiment(contents, method = 'afinn')
  scores_bing <- get_sentiment(contents, method = 'bing')
  return(data.frame(nrc = mean(scores_nrc), 
                    afinn = mean(scores_afinn), 
                    bing = mean(scores_bing)))
}


terms <- c("obesity", "cancer", "covid-19", "wearable", "mental health", "influenza")
years <- 2011:2020

# calculate the sentiments for each keyword across 10 years
sentiment_list = lapply(terms, function(x) lapply(years, function(y) sentiment(y, x)))
names(sentiment_list) = terms
setiment_list_df = lapply(sentiment_list, function(x) do.call("rbind", x))

# draw trajectory
setiment_list_df$obesity

toLongFormat = function(x){
  out = data.frame(type = rep(c("nrc", "afinn", "bing"), each = 10), 
                   time = rep(2011:2020, 3), 
                   score = do.call("c", x))
  out$type = factor(out$type)
  return(out)
}

trajectory_list = lapply(1:6, function(k) ggplot(toLongFormat(setiment_list_df[[k]])) +
  aes(x = time, y = score, group = type, col = type) +
  geom_line() + 
  ggtitle(paste0("Sentiment Trajectory for term ", terms[k])))

grid.arrange(trajectory_list[[1]], 
             trajectory_list[[2]], 
             trajectory_list[[3]], 
             trajectory_list[[4]], 
             trajectory_list[[5]], 
             trajectory_list[[6]], 
             nrow = 2, ncol = 3)

####################### Question 4: Clustering and Association Rule Mining

get_DTM = function(year, term) {
  res <- EUtilsSummary(
    term,
    type = "esearch",
    db = "pubmed",
    datetype = 'pdat',
    mindate = year,
    maxdate = year,
    retmax = 1000
  )
  fetch <- EUtilsGet(res) # get the query results
  pubmed_title <- ArticleTitle(fetch)
  pubmed_title <- removePunctuation(pubmed_title, preserve_intra_word_dashes = T) # remove punctuation
  pubmed_title <- gsub("[0-9]", " ", pubmed_title)
  corp <- corpus(pubmed_title)
  DTM <-
    dfm(
      corp,
      tolower = T,
      remove = stopwords("english"),
      verbose = T
    ) # document term matrix
  DTM_matrix <-
    as(DTM, "matrix") # row denotes paper, column denotes the unique word in title
  return(DTM_matrix)
}

# get DTM from title for each term
all_DTM = lapply(terms[1], function(x) get_DTM(2020, x))

# get distance matrix for each DTM
all_DTM_dist = lapply(all_DTM, dist)

# plot dengrogram
hc = hclust(all_DTM_dist[[1]], method = "complete")
plot(hc, main = "Cluster Dengrogram for paper with title obesity", xlab = "")
hc = hclust(all_DTM_dist[[2]], method = "complete")
plot(hc, main = "Cluster Dengrogram for paper with title cancer", xlab = "cancer")
hc = hclust(all_DTM_dist[[3]], method = "complete")
plot(hc, main = "Cluster Dengrogram for paper with title covid-19", xlab = "")
hc = hclust(all_DTM_dist[[4]], method = "complete")
plot(hc, main = "Cluster Dengrogram for paper with title wear", xlab = "")
hc = hclust(all_DTM_dist[[5]], method = "complete")
plot(hc, main = "Cluster Dengrogram for paper with title mental health", xlab = "")
hc = hclust(all_DTM_dist[[6]], method = "complete")
plot(hc, main = "Cluster Dengrogram for paper with title influenza", xlab = "")

# # "ward.D", "ward.D2"
# hc = hclust(a, method = "ward.D2")
# plot(hc, main = "Cluster Dengrogram for paper with title obesity", xlab = "", hang = -1)
# library(Matrix)
# library(skmeans)
# library(cluster)
# DTM_matrix2 = DTM_matrix[apply(DTM_matrix, 1, sum) != 0,]
# clust_sk <- skmeans(DTM_matrix2, 2, method='pclust', control=list(verbose=TRUE))
# 
# summary(silhouette(clust_sk))
# a = dist(DTM_matrix2)
# plot(silhouette(clust_sk))

### Association rule mining

# term: obesity
pubmed_tr <- as(all_DTM[[1]], "transactions")
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, conf = 0.8, target = "rules"))
inspect(sort(apriori_rules, by = "lift"))
#      lhs             rhs        support    confidence coverage   lift     count
# [1]  {tissue}     => {adipose}  0.02309237 0.8846154  0.02610442 31.46703 23   
# [2]  {adipose}    => {tissue}   0.02309237 0.8214286  0.02811245 31.46703 23   
# [3]  {index,body} => {mass}     0.02911647 1.0000000  0.02911647 26.91892 29   
# [4]  {controlled} => {trial}    0.02008032 0.8695652  0.02309237 26.24506 20   
# [5]  {randomized} => {trial}    0.02008032 0.8695652  0.02309237 26.24506 20   
# [6]  {body,mass}  => {index}    0.02911647 1.0000000  0.02911647 25.53846 29   
# [7]  {physical}   => {activity} 0.02610442 0.8125000  0.03212851 24.52273 26   
# [8]  {mass}       => {index}    0.03112450 0.8378378  0.03714859 21.39709 31   
# [9]  {index,mass} => {body}     0.02911647 0.9354839  0.03112450 17.25448 29   
# [10] {systematic} => {review}   0.02911647 0.9354839  0.03112450 15.79224 29   
# [11] {loss}       => {weight}   0.02108434 0.8400000  0.02510040 12.67636 21   

# term: cancer
pubmed_tr <- as(all_DTM[[2]], "transactions")
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, conf = 0.8, target = "rules"))
inspect(sort(apriori_rules, by = "lift"))
#     lhs                 rhs         support    confidence coverage   lift      count
# [1] {systematic}     => {review}    0.02730030 0.9642857  0.02831143 19.462828 27   
# [2] {hepatocellular} => {carcinoma} 0.02123357 0.9130435  0.02325581 12.900000 21   
# [3] {prostate}       => {cancer}    0.03842265 0.9743590  0.03943377  2.458268 38   
# [4] {breast}         => {cancer}    0.05864510 0.8923077  0.06572295  2.251256 58   
# [5] {colorectal}     => {cancer}    0.03640040 0.8571429  0.04246714  2.162536 36   
# [6] {lung}           => {cancer}    0.04145602 0.8541667  0.04853387  2.155028 41   
# [7] {cervical}       => {cancer}    0.02730030 0.8181818  0.03336704  2.064239 27 

# term: covid-19
pubmed_tr <- as(all_DTM[[3]], "transactions")
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, conf = 0.8, target = "rules"))
inspect(sort(apriori_rules, by = "lift"))
#      lhs                  rhs      support confidence coverage lift      count
# [1]  {systematic}      => {review} 0.026   1.0000000  0.026    18.867925 26   
# [2]  {mental}          => {health} 0.023   0.9200000  0.025    13.939394 23   
# [3]  {covid-,mental}   => {health} 0.021   0.9130435  0.023    13.833992 21   
# [4]  {pandemic,impact} => {covid-} 0.020   0.9523810  0.021     1.404692 20   
# [5]  {mental}          => {covid-} 0.023   0.9200000  0.025     1.356932 23   
# [6]  {mental,health}   => {covid-} 0.021   0.9130435  0.023     1.346672 21   
# [7]  {among}           => {covid-} 0.036   0.8571429  0.042     1.264223 36   
# [8]  {impact}          => {covid-} 0.036   0.8372093  0.043     1.234822 36   
# [9]  {risk}            => {covid-} 0.020   0.8333333  0.024     1.229105 20   
# [10] {experience}      => {covid-} 0.020   0.8333333  0.024     1.229105 20   
# [11] {case}            => {covid-} 0.020   0.8333333  0.024     1.229105 20   
# [12] {pandemic,health} => {covid-} 0.020   0.8333333  0.024     1.229105 20   
# [13] {patients}        => {covid-} 0.091   0.8125000  0.112     1.198378 91   
# [14] {healthcare}      => {covid-} 0.021   0.8076923  0.026     1.191287 21   
# [15] {treatment}       => {covid-} 0.021   0.8076923  0.026     1.191287 21 

# term: wearable
pubmed_tr <- as(all_DTM[[4]], "transactions")
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, conf = 0.8, target = "rules"))
inspect(sort(apriori_rules, by = "lift"))
#     lhs             rhs        support confidence coverage lift     count
# [1] {parkinsons} => {disease}  0.034   0.9444444  0.036    20.53140 34   
# [2] {machine}    => {learning} 0.022   0.9166667  0.024    19.50355 22   

# term: mental health
pubmed_tr <- as(all_DTM[[5]], "transactions")
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, conf = 0.8, target = "rules"))
inspect(sort(apriori_rules, by = "lift"))
#      lhs                         rhs      support    confidence coverage   lift      count
# [1]  {quality}                => {life}   0.02902903 0.9062500  0.03203203 20.118750 29   
# [2]  {systematic}             => {review} 0.03703704 1.0000000  0.03703704 12.645570 37   
# [3]  {health,pandemic}        => {covid-} 0.02802803 0.9333333  0.03003003  5.938854 28   
# [4]  {mental,health,pandemic} => {covid-} 0.02602603 0.9285714  0.02802803  5.908553 26   
# [5]  {mental,pandemic}        => {covid-} 0.02902903 0.9062500  0.03203203  5.766521 29   
# [6]  {pandemic}               => {covid-} 0.06106106 0.8840580  0.06906907  5.625312 61   
# [7]  {cross-sectional}        => {study}  0.02102102 0.8400000  0.02502503  5.212174 21   
# [8]  {health,pandemic}        => {mental} 0.02802803 0.9333333  0.03003003  3.852893 28   
# [9]  {health,covid-,pandemic} => {mental} 0.02602603 0.9285714  0.02802803  3.833235 26   
# [10] {mental,among}           => {health} 0.02702703 0.9310345  0.02902903  3.509824 27   
# [11] {health,covid-}          => {mental} 0.04804805 0.8421053  0.05705706  3.476294 48   
# [12] {mental,covid-,pandemic} => {health} 0.02602603 0.8965517  0.02902903  3.379831 26   
# [13] {mental,pandemic}        => {health} 0.02802803 0.8750000  0.03203203  3.298585 28   
# [14] {care,mental}            => {health} 0.02602603 0.8666667  0.03003003  3.267170 26   
# [15] {mental,covid-}          => {health} 0.04804805 0.8571429  0.05605606  3.231267 48   
# [16] {study,mental}           => {health} 0.03003003 0.8333333  0.03603604  3.141509 30  

# term: influenza
pubmed_tr <- as(all_DTM[[6]], "transactions")
apriori_rules <- apriori(pubmed_tr, parameter = list(supp = 0.02, conf = 0.8, target = "rules"))
inspect(sort(apriori_rules, by = "lift"))
#      lhs                    rhs         support    confidence coverage   lift      count
# [1]  {systematic}        => {review}    0.02502503 0.8928571  0.02802803 18.977964  25  
# [2]  {n,avian}           => {h}         0.02302302 0.8846154  0.02602603 13.595858  23  
# [3]  {influenza,n,avian} => {h}         0.02302302 0.8846154  0.02602603 13.595858  23  
# [4]  {h,virus}           => {n}         0.02602603 0.9629630  0.02702703 11.186047  26  
# [5]  {influenza,h,virus} => {n}         0.02602603 0.9629630  0.02702703 11.186047  26  
# [6]  {ah}                => {n}         0.02202202 0.9565217  0.02302302 11.111223  22  
# [7]  {influenza,ah}      => {n}         0.02102102 0.9545455  0.02202202 11.088266  21  
# [8]  {h}                 => {n}         0.05605606 0.8615385  0.06506507 10.007871  56  
# [9]  {influenza,h}       => {n}         0.05005005 0.8474576  0.05905906  9.844304  50  
# [10] {h,avian}           => {influenza} 0.02902903 1.0000000  0.02902903  2.055556  29  
# [11] {n,avian}           => {influenza} 0.02602603 1.0000000  0.02602603  2.055556  26  
# [12] {virus,avian}       => {influenza} 0.02302302 1.0000000  0.02302302  2.055556  23  
# [13] {h,virus}           => {influenza} 0.02702703 1.0000000  0.02702703  2.055556  27  
# [14] {h,n,avian}         => {influenza} 0.02302302 1.0000000  0.02302302  2.055556  23  
# [15] {h,n,virus}         => {influenza} 0.02602603 1.0000000  0.02602603  2.055556  26  
# [16] {avian}             => {influenza} 0.05205205 0.9811321  0.05305305  2.016771  52  
# [17] {n,virus}           => {influenza} 0.03303303 0.9705882  0.03403403  1.995098  33  
# [18] {ah}                => {influenza} 0.02202202 0.9565217  0.02302302  1.966184  22  
# [19] {n,ah}              => {influenza} 0.02102102 0.9545455  0.02202202  1.962121  21  
# [20] {hemagglutinin}     => {influenza} 0.02002002 0.9523810  0.02102102  1.957672  20  
# [21] {h}                 => {influenza} 0.05905906 0.9076923  0.06506507  1.865812  59  
# [22] {h,n}               => {influenza} 0.05005005 0.8928571  0.05605606  1.835317  50  
# [23] {n}                 => {influenza} 0.07607608 0.8837209  0.08608609  1.816537  76  
# [24] {virus}             => {influenza} 0.13913914 0.8742138  0.15915916  1.796995 139  
# [25] {infection,virus}   => {influenza} 0.02902903 0.8529412  0.03403403  1.753268  29  

################### theme analysis 
library(udpipe)
library(lattice)

# create function to extract all titles based on year and term
get_all_title = function(year, term){
  res <- EUtilsSummary(term, type="esearch", db="pubmed", datetype='pdat', mindate=year, maxdate=year, retmax=1000)
  fetch <- EUtilsGet(res)
  contents <- ArticleTitle(fetch)
  return(contents)
}
terms <- c("obesity", "cancer", "covid-19", "wearable", "mental health", "influenza")
years <- 2011:2020

# get all titles for each keyword across 10 years
all_title_list = lapply(terms, function(x) get_all_title(2020, x))
names(all_title_list) = terms

# download the model
model <- udpipe_download_model(language = "english-ewt")
ud_english <- udpipe_load_model(model$file_model)

# plot function
plot_most_freq_word = function(term) {
  contents = all_title_list[[term]]
  contents_ud <-
    udpipe_annotate(ud_english,
                    x = contents,
                    tagger = "default",
                    parser = "none")
  contents_anno <- as.data.frame(contents_ud)
  stats <- subset(contents_anno, upos %in% "NOUN")
  stats <- txt_freq(x = stats$lemma)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  barchart(
    key ~ freq,
    data = head(stats, 30),
    col = "cadetblue",
    main = paste("Most occurring nouns for the term", term),
    xlab = "Freq"
  )
}

plot_most_freq_word("obesity")
plot_most_freq_word("cancer")
plot_most_freq_word("covid-19")
plot_most_freq_word("wearable")
plot_most_freq_word("mental health")
plot_most_freq_word("influenza")

