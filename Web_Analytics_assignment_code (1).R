#Setting the right working directory:
setwd("C:/Users/Deepthi/Desktop/assignment")
getwd()

library(rtweet)
library(quanteda)
library(tm)

#GIVES ORIGINAL TWEETS AS RTS IS SET TO FALSE
CAATweets = search_tweets("#CAA", n=1500, lang="en", include_rts = F)

#EDA
dim(CAATweets)
names(CAATweets)
head(CAATweets)

#FREQUENCY OF SCREEN NAMES
table(CAATweets$screen_name)

#TO GET TOP 10 SCREEN NAMES/USER WHO TWEETED ON THIS PARTICULAR TOPIC
screen_name_frequency= sort(table(CAATweets$screen_name),decreasing = TRUE)
head(screen_name_frequency,10)


#PLOTTING TWEETS ON MAP
library(maps)
library(ggplot2)
library(ggthemes)

saveRDS(CAATweets, "CAATweets.rds")
CAATweets = readRDS("CAATweets.rds")

# Append latitude and longitude variables using all available geolocation information
CAATweets_coord = lat_lng(CAATweets)
View(CAATweets_coord)

CAATweets_Map = na.omit(CAATweets_coord[,c("lat", "lng")])
View(CAATweets_Map)

# Call the world map
world_basemap = ggplot() +
  borders("world",fill = "gray95")+theme_map()
# Plot the lat and lng points on the world map
world_basemap +
  geom_point(data = CAATweets_Map, aes(x = lng, y = lat),
             colour = 'blue', alpha = .5, size=2)

#time series based frequency of tweets, by hours
CAATweets_ts = ts_data(CAATweets, by ='hours')
head(CAATweets_ts,10)

#timeseries plot
ts_plot(CAATweets, by ='hours')



#####DATA CLEANING

#Extract tweet text and save it in a data frame
CAATweets_text <- CAATweets$text
CAATweets_text[1:3]


#Remove URL'S from text
library(qdapRegex)
CAATweets_text_url <- rm_twitter_url(CAATweets_text)
CAATweets_text_url[1:3]

#Remove spl characters,punctuations & numbers
CAATweets_text_chars <- gsub("[^A-Za-z]", " ", CAATweets_text_url)
CAATweets_text_chars[1:3]

#CONVERT TO CORPUS
#CORPUS IS GENERALLY IN THE FORM OF LIST
CAATweets_corpus <- Corpus(VectorSource(CAATweets_text_chars))
CAATweets_corpus[[3]]$content


#CONVERT TEXT CORPUS TO LOWERCASE
CAATweets_corpus_lwr <- tm_map(CAATweets_corpus, tolower) 
CAATweets_corpus_lwr[[3]]$content

# Common stop words in English
stopwords("english")
# Remove stop words from corpus
CAATweets_corpus_stpwd <- tm_map(CAATweets_corpus_lwr, removeWords, stopwords("english")) 
CAATweets_corpus_stpwd[[3]]$content

# Remove additional spaces
CAATweets_corpus_final <- tm_map(CAATweets_corpus_stpwd, stripWhitespace) 
CAATweets_corpus_final[[3]]$content


#Stemming a document
CAATweets_corpus_stem = tm_map(CAATweets_corpus_final,stemDocument, language="english")
CAATweets_corpus_stem[[3]]$content


#####Conduct text mining on the data

#extract term frequency
#from here we can remove few words based on domain knowledge or requirement
library(qdap)
term_count  <-  freq_terms(CAATweets_corpus_stem, 50)
term_count


# Create a vector of custom stop words
custom_stop <- c("u","t","s")

# Remove custom stop words
CAATweets_corpus_redefined <- tm_map(CAATweets_corpus_stem,removeWords, custom_stop)


# Term count after refining corpus
term_count1  <-  freq_terms(CAATweets_corpus_redefined, 50)
term_count1

#Save clean corpus to rds file
saveRDS(CAATweets_corpus_redefined, "CAATweets_corpus_redefined.rds")


# Create a bar plot of frequent terms
library(ggplot2)

# Create a bar plot
ggplot(term_count1, aes(x = reorder(WORD,  -FREQ),  y = FREQ)) +
  geom_bar(stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a colorful word cloud
library(RColorBrewer)
library(wordcloud)
wordcloud(CAATweets_corpus_redefined, max.words = 100, 
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)

#Create a document term matrix and term document matrix

tweet_dtm <- DocumentTermMatrix(CAATweets_corpus_redefined)
tweet_tdm <- TermDocumentMatrix(CAATweets_corpus_redefined)
inspect(tweet_tdm)
inspect(tweet_dtm)

#Word association plot1

tweet_tdm2 <- removeSparseTerms(tweet_tdm, sparse = 0.98)
inspect(tweet_tdm2)
hc <- hclust(d = dist(tweet_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)


#Word association plots2 - specific to a word
#WORD CORRELATION
#use any words, preferably use high freq words to check correlation | .5 means 50 or 50 plus percentage of correlation must be associated with the given word
#i have taken word from high frequency words detected
```{r}
# Create associations
associations <- findAssocs(tweet_tdm, "caa", 0.5)
# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3)


#####sentiment analysis
library(syuzhet)

# Perform sentiment analysis
CAATweets_text_chars <- iconv(CAATweets_text_chars, "UTF-8", "ASCII", sub="")
View(CAATweets_text_chars)
sa.value <- get_nrc_sentiment(CAATweets_text_chars)

# View the sentiment scores
sa.value[1:5,]


# Calculate sum of sentiment scores
score <- colSums(sa.value[,])

# Convert to data frame
score_df <- data.frame(score)

# View the data frame
score_df


# Convert row names into 'sentiment' column
# Combine with sentiment scores
sa.score <- cbind(sentiment = row.names(score_df), 
                  score_df, row.names=NULL)

# View data frame with sentiment scores
print(sa.score)

# Plot the sentiment scores
ggplot(data = sa.score, aes(x = sentiment, y = score, 
                            fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####TOPIC MODELLING

#Find the sum of words in each Document
rowTotals <- apply(tweet_dtm , 1, sum) 
tweet_dtm_new   <- tweet_dtm[rowTotals> 0, ]


#CREATE A TOPIC MODEL
library(topicmodels)
lda_5 = LDA(tweet_dtm_new, k=5)
top_10terms = terms(lda_5,10)
top_10terms
