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
library(quanteda)
library(quanteda.textstats)

# ---------------------------------------------------------------------------- #

# Importo la lista delle stopwords, che verra' utilizzata all'interno del ciclo
stop_words <- read.table("data/stopwords-it.txt", encoding="UTF-8",
                         quote="\"", comment.char="")$V1
stop_words <- paste(stop_words, collapse = '\\b|\\b')
stop_words <- paste0('\\b', stop_words, '\\b')
# Importo la lista delle parole da considerare come singole entita'
words_to_merge <- read.delim("data/words_to_merge.txt", header=F)$V1
words_to_merge <- sapply(words_to_merge, function(x) paste("\\b",x,"\\b",sep=""))
words_to_merge <- words_to_merge[order(nchar(words_to_merge),decreasing=T)]

# Il ciclo serve solo a capire a che punto e' il procedimento.
for(i in unique(tw$partito)){
  cat(i, "\n", sep="")
  for(j in unique(tw$username[tw$partito==i])){
    cat(j, ", ", sep="")
    tt <- tw[tw$username == j,]
    # Tengo solo testo del tweet, username, partito e creo indice di riga
    tt <- tt %>% select(tweet, username=username, partito=partito) %>% mutate(id = 1:n())
    # Utilizzo la funzione normalizzaTesti che rende confrontabili i tweet
    # normalizzando url, caratteri speciali, emoticons, punteggiatura, ...
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
                                contaStringhe = c("\\?", "\\!", "@", "#",
                                                  "(\200|euro)", "(\\$|dollar)",
                                                  "SUPPRESSEDTEXT"),
                                suppressInvalidTexts = TRUE,
                                verbatim = TRUE,
                                remove = FALSE,
                                removeUnderscore = FALSE)
    # Rimuovo url, che sono codificati col la stringa "wwwurlwww" da normalizzaTesti
    tt$tweet <- str_remove(tt$tweet, "wwwurlwww")
    # Considero come alcuni gruppi di parole come singole entita'; a livello
    # pratico, unisco le parole sostituendo gli spazi con "_"
    for (w in words_to_merge) {
      tt$tweet <- str_replace_all(tt$tweet, w, str_replace_all(w," ","_"))
    }
    # Utilizzo le stopwords contenute in "stopword-it.txt" per rimuovere le
    # parole non utili per l'analisi del testo.
    tt$tweet <- stringr::str_replace_all(tt$tweet, stop_words, "")
    # Tolgo delle sequenze particolari di caratteri:
    #  - 'aggiornamento #', dove # e' un numero (regex: \d+)
    #  - parole unicamente composte da cifre
    tt$tweet <- str_remove(tt$tweet, "aggiornamento \\d+")
    tt$tweet <- gsub("\\b[0-9\\W]+\\b", "", tt$tweet)
    # Sostituisco spazi multipli con un singolo spazio
    tt$tweet <- str_squish(tt$tweet)
    # stemming
    tt$tweet <- sapply(str_split(tt$tweet," "),
                       function(x) paste(char_wordstem(x,language="ita"),collapse=" "))
    # Infine, sostituisco i tweet originali con i nuovi tweet normalizzati
    tw$tweet[tw$username==j] <- tt$tweet
  }
  cat("\n")
}

# ---------------------------------------------------------------------------- #

rm(list=c("i", "j", "w", "tt", "vocabolarioEmoteSorted", "stop_words", "words_to_merge"))

# ---------------------------------------------------------------------------- #

save(tw, file="data/02_tweets_stem.RData")

# ---------------------------------------------------------------------------- #