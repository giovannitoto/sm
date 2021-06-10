# ---------------------------------------------------------------------------- #

# Analisi esplorative varie:
#  - rimozione stem troppo rari e/o comuni

# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidyverse)

# ---------------------------------------------------------------------------- #

rm(list=ls())
load("data/02_tweets_stem.RData")
load("C:/Users/noido/Documenti/GitHub/sm/data/02_tweets_stem.RData")
# ---------------------------------------------------------------------------- #

for (part in unique(tw$partito)) {
  tmp_tw <- tw[tw$partito==part,]
  cat(part, "\n")
  cat("Numero tweet:", nrow(tmp_tw), "\n")
  cat("Numero tweet risposta:", nrow(tmp_tw[tmp_tw$reply_to==1,]), "\n")
  cat("% tweet risposta:", mean(tmp_tw$reply_to==1), "\n")
  # contenuti non testuali (link esterni, img, video)
  cat("\n")
  cat("% tweet con immagini e/o video:", mean(tmp_tw$video==1), "\n")
  cat("% tweet con link esterni:", mean(tmp_tw$urls==1), "\n")
  # like, reply, retweet totali
  cat("\n")
  cat("Numero like totali:", sum(tw$nlikes), "\n")
  cat("Numero di risposte tot. ai tweet:", sum(tmp_tw$nreplies), "\n")
  cat("Numero di retweet totali:", sum(tmp_tw$nretweets), "\n")
  # like, reply, retweet medi per tweet (per confrontare partiti)
  cat("\n")
  cat("Numero di like medi a un tweet:", mean(tmp_tw$nlikes), "\n")
  cat("Num. di risposte medie a un tweet:", mean(tmp_tw$nreplies), "\n")
  cat("Num. di retweet medi per un tweet:", mean(tmp_tw$nretweets), "\n")
  # like, reply, retweet (mediana)
  cat("\n")
  cat("Num. di like mediani per un tweet:", median(tmp_tw$nlikes), "\n")
  cat("Num. di risposte mediane per un tweet:", median(tmp_tw$nreplies), "\n")
  cat("Num. di retweet mediani per un tweet:", median(tmp_tw$nretweets), "\n")
  # like, reply, retweet (massimo)
  cat("\n")
  cat("Num. di like massimo per un tweet:", max(tmp_tw$nlikes), "\n")
  cat("Num. di risposte massimo per un tweet:", max(tmp_tw$nreplies), "\n")
  cat("Num. di retweet massimo per un tweet:", max(tmp_tw$nretweets), "\n")
  # hashtags
  cat("\n")
  cat("Numero di hashtag usati:", sum(tmp_tw$hashtags_count), "\n")
  cat("Numero di hashtag medi per tweet:", mean(tmp_tw$hashtags_count), "\n")
  cat("Numero di hashtag mediano per tweet:", median(tmp_tw$hashtags_count), "\n")
  cat("\n------------------------------------------------\n\n")
}

# ---------------------------------------------------------------------------- #

# Considero solo i tweet originali, non le risposte ad altri tweet
tw <- tw[tw$reply_to==0,]

# ---------------------------------------------------------------------------- #

# Tweet in giornata
tw$hour[tw$hour==0] <- 24
tw$hour[tw$hour==1] <- 25
tw$hour[tw$hour==2] <- 26

par(mfrow=c(1,2))
# grafico frequenze assolute
matplot(table(tw$hour, tw$partito), type="b", bty="l", lwd=3 , pch=17,
        xlab="Ore" , ylab="Frequenza", main="Tweet per ogni ora del giorno")
# grafico frequenza relative
matplot(table(tw$hour,tw$partito) %>% prop.table, type="b", bty="l", lwd=3, pch=17,
        xlab="Ore" , ylab="Frequenza relativa", main="Tweet per ogni ora del giorno")
# legenda
legend("topright", legend = c("FdI", "FI", "IV", "LSP", "M5S", "PD"),  col=1:6, 
       pch=17, bty="n", pt.cex=2, cex=1, text.col="black", horiz=F, inset=c(0.01,0.1))

# ---------------------------------------------------------------------------- #