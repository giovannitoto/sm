# ---------------------------------------------------------------------------- #

# Pulizia dei tweet; i tweet non in italiano sono scartati.

# ---------------------------------------------------------------------------- #

rm(list=ls())
load("data/01_tweets.RData")
tw <- tweets[tweets$language=="it",]
rm(tweets)

# ---------------------------------------------------------------------------- #

library(TextWiller)
library(dplyr)
library(stringr)

# ---------------------------------------------------------------------------- #

# Utilizziamo la funzione normalizzaTesti che rende confrontabili i tweet
# normalizzando url, caratteri speciali, emoticons, punteggiatura, ...
# Il ciclo serve solo a capire a che punto e' il procedimento.
for(i in unique(tw$partito)){
  cat(i, "\n", sep="")
  for(j in unique(tw$username[tw$partito == i])){
    cat(j, ", ", sep="")
    tt <- tw[tw$username == j,]
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
    tw$tweet[tw$username == j] <- tt$tweet
  }
  cat("\n")
}
rm(list=c("i", "j", "tt", "vocabolarioEmoteSorted","itastopwords"))

# ---------------------------------------------------------------------------- #

# Utilizziamo le stopwords contenute in "stopword-it.txt" per rimuovere le
# parole non utili per l'analisi del testo.
stop_words <- read.table("data/stopwords-it.txt", encoding="UTF-8",
                         quote="\"", comment.char="")$V1
stopwords_regex <- paste(stop_words, collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
# Anche qui il ciclo serve solo a capire a che punto e' il procedimento.
for(i in unique(tw$partito)){
  cat(i, "\n")
  for(j in unique(tw$username[tw$partito == i])){
    cat(j, ", ", sep="")
    tw$tweet[tw$username == j] <- stringr::str_replace_all(tw$tweet[tw$username == j], stopwords_regex, "")
  }
  cat("\n")
}
rm(list=c("i", "j", "stop_words", "stopwords_regex"))

# ---------------------------------------------------------------------------- #

# Togliamo delle sequenze particolari di caratteri:
#  - 'aggiornamento #', dove # e' un numero (regex: \d+)
#  - parole unicamente composte da cifre
tw$tweet <- str_remove(tw$tweet, "aggiornamento \\d+")
tw$tweet <- gsub("\\b[0-9\\W]+\\b", "", tw$tweet)

# ---------------------------------------------------------------------------- #

save(tw, file="data/02_tweets.RData")

# ---------------------------------------------------------------------------- #