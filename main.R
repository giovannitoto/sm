# ---------------------------------------------------------------------------- #

library(tidytext)
require(dplyr)
library(TextWiller)
library(magrittr)    # per rimuovere piu' parole insieme
library(rtweet)      # ts_plot
library(lubridate)   # as_datetime

# ---------------------------------------------------------------------------- #

rm(list=ls())
setwd("C:/Users/Giovanni/Desktop/SM_progetto")
# Importo il dataset ed elimino le varibili non importanti
source("preparazione_dataset.R")

# ---------------------------------------------------------------------------- #

# Applico filtri al dataset
tt <- tweets[tweets$username=="forza_italia",]
tt <- tt[tt$language=="it",]

# Tengo solo testo del tweet, username, partito e creo indice di riga
tt <- tt %>% select(tweet, username=username, partito=partito) %>% mutate(id = 1:n())

# Normalizzo i tweet facendo cose
tt$tweet <- normalizzaTesti(tt$tweet,
                  tolower = TRUE,
                  normalizzahtml = TRUE,
                  normalizzacaratteri = TRUE,
                  normalizzaemote = TRUE,
                  normalizzaEmoticons = TRUE,
                  normalizzapunteggiatura = TRUE,
                  normalizzaslang = TRUE,
                  fixed = TRUE,
                  perl = TRUE,
                  preprocessingEncoding = TRUE,
                  encoding = "UTF-8",
                  sub = "",
                  contaStringhe = c("\\?", "\\!", "@", "#", "(\200|euro)", "(\\$|dollar)",
                                    "SUPPRESSEDTEXT"),
                  suppressInvalidTexts = TRUE,
                  verbatim = TRUE,
                  remove = TRUE,
                  removeUnderscore = FALSE)
tt$tweet <- str_remove(tt$tweet, "wwwurlwww")


# Rimuovo stopwords
stop_words <- read.table("stopwords-it.txt", encoding="UTF-8",
                         quote="\"", comment.char="")$V1
stopwords_regex <- paste(stop_words, collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
tt$tweet <- stringr::str_replace_all(tt$tweet, stopwords_regex, "")


tt %>% unnest_tokens(word,tweet) %>% 
  group_by(word) %>% summarise(tot=n()) %>% 
  arrange(desc(tot)) %>% print(n=80)


# ---------------------------------------------------------------------------- #

# Identifico i bigrammi con maggior frequenza

ttU = tt %>% unnest_ngrams(word, tweet, n=2)
#parole che vogliamo eliminare
tt$tweet = str_remove(tt$tweet, "non | oggi |più")
tt$tweet = str_remove(tt$tweet, "aggiornamento \\d+")
#aggiornamento1, 2, ..d+ sta per numeri
tt$tweet = gsub("\\b[0-9\\W]+\\b", "", tt$tweet)#gsub 
#rimuovo parole unicamente composti da cifre
tt$tweet = wordStem(tt$tweet, language = "it")
ttU = tt %>% unnest_ngrams(word, tweet, n=2) #bigrammi

tt %>% unnest_ngrams(word,tweet,n=2) %>% group_by(word) %>% #raggruppo per parola
  summarise(tot=n()) %>% 
  arrange(desc(tot)) %>% print(n=20)


# ---------------------------------------------------------------------------- #

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