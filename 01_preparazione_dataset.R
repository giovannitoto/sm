# ---------------------------------------------------------------------------- #

# Rimozione delle colonne non necessarie.

# ---------------------------------------------------------------------------- #

rm(list=ls())
tweets <- read.csv("data/00_tweets.csv", sep="\t", encoding="UTF-8")

# ---------------------------------------------------------------------------- #

# rimuovo id del tweet
tweets$X.U.FEFF.id <- NULL
# rimuovo id della conversazione (99409 modalita')
tweets$conversation_id <- NULL
# created_at non so cosa voglia dire (immagino sia una conversione numerica
# della data di creazione)
tweets$created_at <- NULL
# rimuovo timezone poiche' sempre la stessa, ovvero 200
tweets$timezone <- NULL
# rimuovo luogo poiche' disponibile per una sola osservazione
tweets$place <- NULL
# user_id, user_id_str, username e name sono in corrispondenza univoca: mantengo
# solo l'username
tweets$user_id <- NULL
tweets$user_id_str <- NULL
tweets$name <- NULL
# creo variabile che mi dice se il tweet contiene un link esterno oppure no
tweets$urls[tweets$urls=="[]"] <- 0
tweets$urls[tweets$urls!=0] <- 1
# rimuovo link diretto del post
tweets$link <- NULL
# non so cosa sia ma ha solo valori nulli
tweets$cashtags <- NULL
# photos e' la lista degli url delle foto del tweet; presenza/assenza di url
# e' contenuta nella var 'video'
tweets$photos <- NULL
tweets$thumbnail <- NULL
# non ci sono retweet, ergo variabili legate sono inutili
tweets$retweet <- NULL
tweets$retweet_id <- NULL
tweets$retweet_date <- NULL
tweets$user_rt <- NULL
tweets$user_rt_id <- NULL
# variabili relative a traduzione sono vuote
tweets$trans_dest <- NULL
tweets$trans_src <- NULL
tweets$translate <- NULL
# altre variabili nulle
tweets$near <- NULL
tweets$geo <- NULL
tweets$source <- NULL
tweets$search <- NULL
# creo variabile presenza/assenza di una citazione (quote) nel tweet
tweets$quote_url[tweets$quote_url==""] <- 0
tweets$quote_url[tweets$quote_url!=0] <- 1
# creo variabile risposta/non risposta ad un altro tweet
tweets$reply_to[tweets$reply_to=="[]"] <- 0
tweets$reply_to[tweets$reply_to!=0] <- 1

# ---------------------------------------------------------------------------- #

save(tweets, file="data/01_tweets.RData")

# ---------------------------------------------------------------------------- #