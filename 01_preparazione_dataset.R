# ---------------------------------------------------------------------------- #

# Rimozione delle colonne non necessarie.

# ---------------------------------------------------------------------------- #

rm(list=ls())
tw <- read.csv("data/00_tweets.csv", sep="\t", encoding="UTF-8")
tw <- tw[tw$language=="it",]

# ---------------------------------------------------------------------------- #

# rimuovo lingua poiche' considero solo quelli italiani (it)
tw$language <- NULL
# rimuovo id del tweet
tw$X.U.FEFF.id <- NULL
# rimuovo id della conversazione (99409 modalita')
tw$conversation_id <- NULL
# created_at non so cosa voglia dire (immagino sia una conversione numerica
# della data di creazione)
tw$created_at <- NULL
# rimuovo timezone poiche' sempre la stessa, ovvero 200
tw$timezone <- NULL
# rimuovo luogo poiche' disponibile per una sola osservazione
tw$place <- NULL
# user_id, user_id_str, username e name sono in corrispondenza univoca: mantengo
# solo l'username
tw$user_id <- NULL
tw$user_id_str <- NULL
tw$name <- NULL
# creo variabile che mi dice se il tweet contiene un link esterno oppure no
tw$urls[tw$urls=="[]"] <- 0
tw$urls[tw$urls!=0] <- 1
# rimuovo link diretto del post
tw$link <- NULL
# non so cosa sia ma ha solo valori nulli
tw$cashtags <- NULL
# photos e' la lista degli url delle foto del tweet; presenza/assenza di url
# e' contenuta nella var 'video'
tw$photos <- NULL
tw$thumbnail <- NULL
# non ci sono retweet, ergo variabili legate sono inutili
tw$retweet <- NULL
tw$retweet_id <- NULL
tw$retweet_date <- NULL
tw$user_rt <- NULL
tw$user_rt_id <- NULL
# variabili relative a traduzione sono vuote
tw$trans_dest <- NULL
tw$trans_src <- NULL
tw$translate <- NULL
# altre variabili nulle
tw$near <- NULL
tw$geo <- NULL
tw$source <- NULL
tw$search <- NULL
# creo variabile presenza/assenza di una citazione (quote) nel tweet
tw$quote_url[tw$quote_url==""] <- 0
tw$quote_url[tw$quote_url!=0] <- 1
# creo variabile risposta/non risposta ad un altro tweet
tw$reply_to[tw$reply_to=="[]"] <- 0
tw$reply_to[tw$reply_to!=0] <- 1

# converto la variabile 'hashtags' in testo per renderla analizzabile
tw$hashtags <- gsub("\\[|\\]|'|,", "", tw$hashtags)
# calcolo il numero di hashtag presente in ogni tweet
htags_count <- strsplit(tw$hashtags, " ")
htags_count <- lapply(htags_count, trimws)
htags_count <- lapply(htags_count, length)
htags_count <- unlist(htags_count)
tw$hashtags_count <- htags_count
# conto il numero di caratteri
tw$tweet_nchar <- nchar(tw$tweet)

# ---------------------------------------------------------------------------- #

save(tw, file="data/01_tweets.RData")

# ---------------------------------------------------------------------------- #