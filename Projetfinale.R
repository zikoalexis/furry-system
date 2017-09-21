library(reshape2)
library(tidyverse)
library(tidyquant)
library(tidytext)
library(graph)
library(Rgraphviz)
library(tm)
library(igraph)
library(RColorBrewer)
library(httr)
library(twitteR)
library(topicmodels)
library(data.table)
library(wordcloud)
library(ggplot2)
library(SnowballC)
consumer_key <- 'jLsAAVr79OGee7xJFqGEEyK4f'
consumer_secret <- 'kkXDD1H6ksTnywXX6FvcDPkNUbkj6v1BE2VxCjv9vcjqg3EvgU'
access_token <- '869866832982925314-5jujeQ6QjDeY6yZDnLuTG9iip2oovT9'
access_secret <- 'aO97YEaVlRGRK24SmvF4qaacDl7GrL2DdE3N6vMATT99a'
setup_twitter_oauth(consumer_key,consumer_secret,access_token ,access_secret)

twitter_search =searchTwitter("morocco", n=200, lang="en")

tweets.df <- twListToDF(tweets)

# Accéder au texte seulement ne pas inclure les videos et les photos ..
text1 <- sapply(twitter_search, function(x) x$getText())
# SUpprimer les retweets
text1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text1)
# Supprimer les identifications d'autres personnes 
text1 = gsub("@\\w+", "", text1)
# Supprimer les ponctuations
text1 = gsub("[[:punct:]]", "", text1)
# SUpprimer les nombre
text1 = gsub("[[:digit:]]", "", text1)
# Supprimer les liens 
text1 = gsub("http\\w+", "", text1)
# spprimer les espaces vides
text1 = gsub("[ \t]{2,}", "", text1)
text1 = gsub("^\\s+|\\s+$", "", text1)

tryTolower = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
text1 = sapply(text1, tryTolower)

#transformation en corpus 
text1_corpus <- Corpus(VectorSource(text1))
#transformation en termdocument matrix en supprimant les stopwords en anglais
#et des mots qui se répetent souvent comme rabat
tdm =TermDocumentMatrix(
  text1_corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = c("http","rabat", stopwords("english")),
    removeNumbers = TRUE,
    tolower = TRUE)
)
m = as.matrix(tdm)

# Compter les mots d'une facon decroisante 
word_freqs = sort(rowSums(m), decreasing = TRUE)
# creation d'une dataframe avec les  mots et leurs fréquences
dm = data.frame(word = names(word_freqs), freq = word_freqs)
# création du wordcloud
wordcloud(dm$word, dm$freq, min.freq=2, random.order = FALSE, 
          colors = brewer.pal(11, "Spectral"))


#term association a khay dyali
# les mots frequents dans la frequence est au moins a 15
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))

# Trouver les associations le parametre 0.2 est modifiable 
findAssocs(tdm, "morocco", 0.2)
p <- plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)
print(p)
#Fréquence des mots  
term.freq <- rowSums(as.matrix(tdm))
#les mots fréquents plus que 20 ce parametre est modifiable aussi selon
#ce qu'on veut 
term.freq <- subset(term.freq, term.freq >=20)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + 
  xlab("Terms") + ylab("nbre de fois ") +coord_flip()


#clusterr a satt !!
# supprimer les sparseterms sparse ne doit pas depasser 1
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# clustering 
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6)

#transformation de termdocument matrix a document term matrix 
dtm <- as.DocumentTermMatrix(tdm)


lda <- LDA(dtm, k = 8) # 8 sujets
(term <- terms(lda,6)) # 4 mots qui revelent le sujet 

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
topic <- topics(lda, 1)
tweets.df <- twListToDF(twitter_search)
topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")


source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")
twitterMap("MoroccoWNews", fileName="followersMap.pdf", nMax=1500)

load("my_friends.RData")
load("my_followers.RData")

followers_df %>%
  count(lang) %>%
  droplevels() %>%
  ggplot(aes(x = reorder(lang, desc(n)), y = n)) +
  geom_bar(stat = "identity", color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "langue utilisée",
       y = "nbre de followers ")



followers_df %>%
  mutate(date = as.Date(created, format = "%Y-%m-%d"),
         today = as.Date("2017-06-07", format = "%Y-%m-%d"),
         days = as.numeric(today - date),
         statusesCount_pDay = statusesCount / days) %>%
  select(screenName, followersCount, statusesCount_pDay) %>%
  arrange(desc(followersCount)) %>%
  top_n(10)

data(stop_words)

#supprimer les stopwords et les liens utiliser la racine des mots 
tidy_descr <- followers_df %>%
  unnest_tokens(word, description) %>%
  mutate(word_stem = wordStem(word)) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl("\\.|http", word))

tidy_descr %>%
  count(word_stem, sort = TRUE) %>%
  filter(n > 70) %>%
  ggplot(aes(x = reorder(word_stem, n), y = n)) +
  geom_col(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
  coord_flip() +
  theme_tq() +
  labs(x = "",
       y = "Nbre de fois ")

#wordcloud des mots les plus fréquents dans notre
tidy_descr %>%
  count(word_stem) %>%
  mutate(word_stem = removeNumbers(word_stem)) %>%
  with(wordcloud(word_stem, n, max.words = 100, colors = palette_light()))


tidy_descr_ngrams <- followers_df %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  filter(!grepl("\\.|http", bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- tidy_descr_ngrams %>%
  count(word1, word2, sort = TRUE)
bigram_counts %>%
  filter(n > 10) %>%
  ggplot(aes(x = reorder(word1, -n), y = reorder(word2, -n), fill = n)) +
  geom_tile(alpha = 0.8, color = "white") +
  scale_fill_gradientn(colours = c(palette_light()[[1]], palette_light()[[2]])) +
  coord_flip() +
  theme_tq() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Premier mot",
       y = "deuxieme mot")


bigrams_separated <- followers_df %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  filter(!grepl("\\.|http", bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "not" | word1 == "no") %>%
  filter(!word2 %in% stop_words$word)

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = palette_light()) +
  labs(x = "",
       y = "Score de sentiments * nbre d'occurrences",
       title = "Mots précédés par  \"not\"") +
  coord_flip() +
  theme_tq()

tidy_descr_sentiment <- tidy_descr %>%
  left_join(select(bigrams_separated, word1, word2), by = c("word" = "word2")) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  rename(nrc = sentiment.x, bing = sentiment.y) %>%
  mutate(nrc = ifelse(!is.na(word1), NA, nrc),
         bing = ifelse(!is.na(word1) & bing == "positive", "negative", 
                       ifelse(!is.na(word1) & bing == "negative", "positive", bing)))
tidy_descr_sentiment %>%
  filter(nrc != "positive") %>%
  filter(nrc != "negative") %>%
  gather(x, y, nrc, bing) %>%
  count(x, y, sort = TRUE) %>%
  filter(n > 10) %>%
  ggplot(aes(x = reorder(y, n), y = n)) +
  facet_wrap(~ x, scales = "free") +
  geom_col(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
  coord_flip() +
  theme_tq() +
  labs(x = "",
       y = "sentiment des followers en NRC et en bing ")


tidy_descr_sentiment %>%
  count(word, bing, sort = TRUE) %>%
  acast(word ~ bing, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = palette_light()[1:2],
                   max.words = 100)



dtm_words_count <- tidy_descr %>%
  mutate(word_stem = removeNumbers(word_stem)) %>%
  count(screenName, word_stem, sort = TRUE) %>%
  ungroup() %>%
  filter(word_stem != "") %>%
  cast_dtm(screenName, word_stem, n)



dtm_lda <- LDA(dtm_words_count, k = 5, control = list(seed = 1234))

topics_beta <- tidy(dtm_lda, matrix = "beta")

p1 <- topics_beta %>%
  filter(grepl("[a-z]+", term)) %>% 
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, color = factor(topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE, alpha = 0.5) +
  scale_color_manual(values = palette_light()) +
  scale_fill_manual(values = palette_light()) +
  facet_wrap(~ topic, ncol = 5) +
  coord_flip() +
  theme_tq() +
  labs(x = "",
       y = "beta (~ occurrence dans les sujets 1-5)",
       title = "Les 7 mots qui refletent le plus le sujet ")


user_topic <- tidy(dtm_lda, matrix = "gamma") %>%
  arrange(desc(gamma)) %>%
  group_by(document) %>%
  top_n(1, gamma)

p2 <- user_topic %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  ggplot(aes(x = reorder(document, -gamma), y = gamma, color = factor(topic))) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  geom_point(show.legend = FALSE, size = 2, alpha = 0.8) +
  scale_color_manual(values = palette_light()) +
  scale_fill_manual(values = palette_light()) +
  theme_tq() +
  coord_flip() +
  labs(x = "",
       y = "gamma\n(~ Les personnes qui parlent du sujet dans leur description)")

grid.arrange(p1, p2, ncol = 1, heights = c(0.7, 0.3))
