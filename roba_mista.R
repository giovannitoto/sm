# ---------------------------------------------------------------------------- #

# Osservo gli n-grammi e decido quali considerare come un'unica entita'
library(dplyr)
library(stringr)
library(tidytext)

rm(list=ls())
load("data/01_tweets.RData")
tweets <- tweets[!(tweets$username=="LegaSalvini"),]
tweets <- tweets[tweets$reply_to==0,]
tweets <- tweets[tweets$language=="it",]
tweets$tweet <- tolower(tweets$tweet)
tweets$tweet <- str_squish(tweets$tweet)
tweets$tweet <- str_remove(tweets$tweet, "aggiornamento \\d+")
tweets$tweet <- gsub("\\b[0-9\\W]+\\b", "", tweets$tweet)
stop_words <- read.table("data/stopwords-it.txt", encoding="UTF-8",
                         quote="\"", comment.char="")$V1
stop_words <- paste(stop_words, collapse = '\\b|\\b')
stop_words <- paste0('\\b', stop_words, '\\b')
tweets$tweet <- stringr::str_replace_all(tweets$tweet, stop_words, "")

res1 <- tweets %>% unnest_ngrams(word,tweet,n=1) %>% group_by(word) %>%
  summarise(tot=n()) %>% arrange(desc(tot))
res2 <- tweets %>% unnest_ngrams(word,tweet,n=2) %>% group_by(word) %>%
  summarise(tot=n()) %>% arrange(desc(tot))
res3 <- tweets %>% unnest_ngrams(word,tweet,n=3) %>% group_by(word) %>%
  summarise(tot=n()) %>% arrange(desc(tot))


# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidytext)
# Togliamo gli stem che compaiono in una percentuale troppo bassa di tweet;
# in particolare, rimuoviamo gli stem che compaiono in meno del 0.1%=0.001 dei tweet.
rm(list=ls())
load("data/02_tweets_stem.RData")
tw <- tw[!(tw$username=="LegaSalvini"),]
tw <- tw[tw$reply_to==0,]

# conto quante volte appare ogni parola in ogni tweet
parole_usate <- tw %>% select(tweet=tweet) %>% mutate(id = 1:n()) %>%
  unnest_tokens(word, tweet) %>% group_by(id, word) %>%
  summarise(tot=n())
# conto in quanti tweet differenti le parole compaiono
parole_usate <- parole_usate %>% group_by(word) %>%
  summarise(tot=n(), freq=n()/nrow(tw))
parole_usate %>% select(word, tot, freq) %>% arrange(tot) %>% print(n=50)
parole_usate %>% select(word, tot, freq) %>% arrange(desc(tot)) %>% print(n=50)

# numero di parole rare (a bassa frequenza)
sum(parole_usate$tot<10)

nrow(parole_usate[parole_poco_usate$freq<0.0001,])

# ---------------------------------------------------------------------------- #

library(rtweet)      # ts_plot

tt <- tweets %>% select(tweet, date=date, partito=partito) %>% mutate(id = 1:n())
tt$date <-  as_datetime(tt$date)

p1 <- tt %>% group_by(partito) %>% filter(date > "2021-04-10") %>% 
  ts_plot("days") + 
  geom_point() +
  scale_x_datetime(date_labels="%Y-%m-%d %H:%M:%S") +
  theme(legend.position="right")

p2 <- tt %>% group_by(partito) %>% filter(date > "2020-04-10") %>%
  ts_plot("months") + 
  geom_point() +
  theme_light() +
  scale_x_datetime(date_breaks="2 month", date_labels="%Y-%m",
                   minor_breaks="4 days") +
  theme(legend.position="right")

require(gridExtra)
grid.arrange(p1, p2, nrow=2, ncol=1, heights=c(5,5))

# ---------------------------------------------------------------------------- #

# Ottengo lista di hashtag utilizzati

# con lapply (*) si considera anche il numero di hashtag all'interno del
# tweet (#salvini come unico hashtag e #salvini con anche altri hashtag sono
# considerati diversi)

load("Tweets.RData")

hashtag_to_fix <- unique(tweets$hashtags)
hashtag_list <- c()
for (h in hashtag_to_fix) {
  if (h=="[]") { next }
  h_tmp <- gsub("\\[|\\]|'", "", h)
  if ( grepl(",", h_tmp) ) {
    h_tmp <- strsplit(h_tmp, ",")
    # (*)
    #h_tmp <- lapply(h_tmp, function(x) paste(x,length(h_tmp[[1]]),sep="_"))
  }
  h_tmp <- lapply(h_tmp, trimws)[[1]]
  hashtag_list <- c(hashtag_list, h_tmp)
}
sort(table(hashtag_list), decreasing=T)[1:40]


sum(grepl("coronavirus|COVID-19|pandemia", tweets$tweet))


# ---------------------------------------------------------------------------- #

short_tweet <- tweets[nchar(tweets$tweet)<100,"tweet"]
#View(short_tweet)

# distribuzione della lunghezza dei tweet
hist(nchar(tweets$tweet), nclass=50, col=3)
abline(v=summary(nchar(tweets$tweet))[-4], col=1, lwd=2, lty=2)
abline(v=summary(nchar(tweets$tweet))[4], col=1, lwd=2)

# ----------------------------------------------------------------------------- #

library(forcats)
library(ggplot2)
library(dplyr)
nomi <- unique(tweets$username)
ntweets <- sapply(nomi, function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T)[1:10])
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

ntweets <- sapply(unique(tweets$partito), function(x) length(tweets$tweet[tweets$partito == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# ----------------------------------------------------------------------------- #

# Numero di tweet per ogni politico per ogni partito

## Lega
ntweets <- sapply(unique(tweets$username[tweets$partito == "LSP"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## Fradelli d'Italia
ntweets <- sapply(unique(tweets$username[tweets$partito == "FdI"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## Forza Italia
ntweets <- sapply(unique(tweets$username[tweets$partito == "FI"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## Italia Viva
ntweets <- sapply(unique(tweets$username[tweets$partito == "IV"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## Partito Democratico
ntweets <- sapply(unique(tweets$username[tweets$partito == "PD"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## Movimento 5 Stelle
ntweets <- sapply(unique(tweets$username[tweets$partito == "M5S"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## Giuseppe Conte
ntweets <- sapply(unique(tweets$username[tweets$partito == "MVP"]), function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()