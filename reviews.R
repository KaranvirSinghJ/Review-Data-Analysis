library(tidyverse)
library(tm)
library(quanteda)
library(syuzhet)
library(topicmodels)
library(wordcloud)
library(readxl)
library(ggplot2)

# Define additional stopwords
additional_stopwords <- c("apple", "watch", "'m", "'ve", "'s")

# Load the data
reviews <- as.data.frame(reviews_data2)


# Preprocessing for sentiment analysis and word cloud generation
corpus <- Corpus(VectorSource(reviews$review_text))
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c(stopwords("english"), additional_stopwords)) %>%
  tm_map(stripWhitespace)

# Ensure no empty documents proceed to DTM creation
corpus_clean <- corpus_clean[sapply(corpus_clean, function(doc) nchar(as.character(doc)) > 0)]

# Correct way to convert cleaned corpus to a text vector for the word cloud and sentiment analysis
text_vector <- sapply(corpus_clean, function(doc) as.character(doc))# Generate Wordcloud for EDA
set.seed(123) # For reproducibility
wordcloud(words = text_vector, max.words = 100, random.order = FALSE, scale = c(3, 0.5))

# Prepare DTM and remove sparse terms
dtm <- DocumentTermMatrix(corpus_clean)
dtm <- removeSparseTerms(dtm, 0.995) 
# Remove terms that appear in less than 0.5% of the documents
# Filter out documents with no terms after preprocessing
valid_docs_index <- rowSums(as.matrix(dtm)) > 0
dtm_filtered <- dtm[valid_docs_index, ]

# Sentiment Analysis with AFINN and Bing
sentiments_afinn <- sapply(text_vector, get_sentiment, method = "afinn")
sentiments_bing <- sapply(text_vector, get_sentiment, method = "bing")

# Create a data frame for sentiment comparison
sentiment_comparison <- data.frame(AFINN = sentiments_afinn, Bing = sentiments_bing)
sentiment_comparison <- na.omit(sentiment_comparison) # Remove NA for clear plotting

# Display a summary of sentiment values
print("Summary of AFINN Sentiment Scores:")
print(summary(sentiments_afinn))
print("Summary of Bing Sentiment Scores:")
print(summary(sentiments_bing))

# Apply LDA for Topic Modeling on the filtered DTM
lda_model <- LDA(dtm_filtered, k = 4, control = list(seed = 123))

# Explore topics
topics <- terms(lda_model, 6)
print("Topics from LDA Model:")
print(topics)

# Beta values: probabilities of terms given topics
beta <- lda_model@beta
print("Beta values (Probabilities of Terms Given Topics):")
print(beta)

# Gamma values: probabilities of topics given documents
gamma <- lda_model@gamma
print("Gamma values (Probabilities of Topics Given Documents):")
print(head(gamma))














library(tm)
library(topicmodels)
library(tidytext)
library(textstem)
library(ldatuning)
library(lexicon)
library(janitor)
library(tidyr)
library(tidyverse)
library(tidyselect)
library(tidytext)
library(caret)
library(RColorBrewer)
library(wordcloud)

mydata<-as.data.frame(reviews_data2)

mydf<-mydata$review_text

#create a corpus
mydf_corpus<-VCorpus(VectorSource(mydf))
mydf_corpus<-tm_map(mydf_corpus,stripWhitespace)
mydf_corpus<-tm_map(mydf_corpus,content_transformer(removePunctuation))
mydf_corpus<-tm_map(mydf_corpus,content_transformer(tolower))
mydf_corpus<-tm_map(mydf_corpus,removeWords,stopwords("en"))

mydf_corpus<-tm_map(mydf_corpus,stemDocument)


#convert into document term matrix

dtm<-DocumentTermMatrix(mydf_corpus)
doc_zero<-rowSums(as.matrix(dtm))
dtm<-dtm[doc_zero>0,]
inspect(dtm)

mydf_lda<-LDA(dtm,k=2,control=list(seed=2024)) 

#Exploring
df_topic<-tidy(mydf_lda,matrix="gamma")
df_topterms<-df_topic %>% group_by(topic) %>% slice_max(gamma,n=10)

ggplot(df_topterms) + geom_col(aes(reorder(term,+beta), term, fill=factor(topic)))
ggplot(df_topterms) + geom_col(aes(reorder(term,+beta), term, fill=factor(topic)))+facet_wrap(~topic)

df_gamma<-tidy(mydf_lda,matrix="gamma")
df_top_docs<-df_gamma %>% group_by(topic) %>% slice_max(gamma, n=10)

ggplot(df_topterms) + geom_col(aes(reorder(term,+gamma), term, fill=factor(topic)))
ggplot(df_topterms) + geom_col(aes(reorder(term,+gamma), term, fill=factor(topic)))+facet_wrap(~topic)

mydata$review_text[2898]
mydata$review_text[896]
mydata$review_text[653]



#optimal number of clusters
cluster_no<-FindTopicsNumber(dtm,topics = seq(2,3), 
                             metrics=c("Griffiths2004","CaoJuan2009","Arun2010","Deveaud2014"),method = "Gibbs",
                             control=list(seed=2024),mc.cores = 2L,verbose = TRUE)

#install textdata, wordcloud
#sentiment analysis
mydf_sent<-data.frame(doc=1:200,text=mydata$review_text[1:200])
textdf<-mydf_sent %>% unnest_tokens(word,text)
df_words<-textdf %>% count(doc,word,sort=TRUE)
data("stop_words", "apple", "watch", "series")
df_words_clean<-df_words %>% anti_join(stop_words)
df_words_clean %>% filter(n>=6) %>% ggplot(aes(n,word))+geom_col()

# sentiment analysis
get_sentiments(lexicon = "afinn")
sent_afinn<-df_words_clean %>% inner_join(get_sentiments("afinn")) %>% group_by(doc) %>%
  summarise(sentiment=sum(value))

summary(sent_afinn$sentiment)
sent_afinn %>% filter(sentiment==25)
mydata$review_text[[3025]]

get_sentiments("bing")
sent_bing<-df_words_clean %>% inner_join(get_sentiments("bing")) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_word=positive-negative) %>%
  group_by(doc) %>%
  summarise(sentiment=sum(sentiment_word))

summary(sent_bing)
sent_bing %>% filter(sentiment==29)



get_sentiments("nrc")

nrc_joy<-get_sentiments("nrc") %>% filter(sentiment=="joy")
view(nrc_joy)

sent_nrc<-df_words_clean %>% inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)


subset<-mydata[1:20,]
subset$vader_score<-vader_df(subset$review_text)
mydata$vader_score<-vader_df(mydata$review_text)


wordcloud(words=df_words_clean$word, freq=df_words_clean$n, min.freq=5, max.words=20,
          colors=brewer.pal(8, "Dark2"))

