# ---------------------------------------------------------------------------- #

# Pulizia dei tweet con caratteri anomali.

# ---------------------------------------------------------------------------- #

rm(list=ls())
load("data/02_tweets_stem.RData")
# Considero solo i tweet originali, non le risposte ad altri tweet
tw <- tw[tw$reply_to==0,]

# per rendere identicabili i tweet, aggiungo la colonna id
tw$id <- 1:nrow(tw)
# metto la colonna in posizione 1
tw <- tw[, c(19,2:18)]

# ---------------------------------------------------------------------------- #

library(dplyr)
library(stringr)

# ---------------------------------------------------------------------------- #

REGEX8 <- "u[0-9]{4}[0-9A-Fa-f]{4}"
REGEX4 <- "u[0-9]{4}"

REGEX <- REGEX4

sum(str_detect(tw$tweet,REGEX))
tw_to_check <- which(str_detect(tw$tweet,REGEX))
# Ottengo lista degli unicode ancora presenti nei tweet puliti
unicode_to_remove <- tw$tweet[tw_to_check] %>% str_match_all(REGEX) %>% unlist %>% unique %>% sort

for (w in unicode_to_remove) {
  tw$tweet2 <- str_remove(tw$tweet, w)
  cat(w, " ")
}

for (i in tw_to_check) {
  cat("Autore:", tw$username[i], "-", tw$partito[i], "\n")
  print(tw$tweet[i])
  print(tw$tweet2[i])
  cat(tw$tweet_original[i], "\n\n")
}

# ---------------------------------------------------------------------------- #