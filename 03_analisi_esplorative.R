# ---------------------------------------------------------------------------- #

# Analisi esplorative varie:
#  - rimozione stem troppo rari e/o comuni

# ---------------------------------------------------------------------------- #

library(dplyr)
library(tm)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)

# ---------------------------------------------------------------------------- #

rm(list=ls())
load("data/02_tweets_stem.RData")

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

#Rete di unigrammi
require(Matrix)
tt =  tw %>% select(tweet=tweet, partito=partito) %>% mutate(id = 1:n())
tt = tt %>% unnest_tokens(word,tweet) %>%
  group_by(id,word) %>% mutate(freq = n())
X = tt %>% cast_sparse(row=id, column=word, value=freq)
dim(X)  # 41417 46942
M=t(X) %*% X
#matrix parole x parole= sorta di distanza che ci misura quante volte due parole distinte compaiono nello stesso documento 
dim(M)

#Rete di unigrammi
library(igraph)
g = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                add.rownames=TRUE)
g = simplify(g, remove.loops=T)
# tengo solo la rete completamente connessa piu' grande
g = delete_vertices(g, which(components(g)$membership != 1)) 

#g = delete_vertices(g, which(degree(g) < 2))

components(g)

#Cluster####

#LOUVAIN####
gr = cluster_louvain(graph = g)
#rappresentazione grafica
library(tidygraph) 
library(ggraph)
gr_id = membership(gr)
V(g)$color = as.numeric(factor(gr_id))
large = which(degree(g)>quantile(degree(g), .75))
V(g)$label = NA
V(g)$label[large] = V(g)$name[large]
df_gg = as_tbl_graph(g)
pl = ggraph(df_gg, layout="fr") +
  geom_edge_link(show.legend = FALSE, aes(alpha=weight)) +
  geom_node_point(aes(color = factor(color)),show.legend = F) +
  geom_node_label(aes(label=label,colour = factor(color)),show.legend=F) +
  scale_size(guide="none") +
  theme_graph()
# salvo in file
ggsave(pl, file="net.pdf", device=cairo_pdf, width=20, height=10)


#altri metodi#####
#ENUMERAZIONE (troppo oneroso comp.)##
gr_optim <- cluster_optimal(graph = g)#per numerazione
#FAST GREEDY: ottimizza modularity
gr_greedy <- cluster_fast_greedy(graph = g)# via di mezzo tra i due 
#WALKTRAP: basato su RW
gr_walk <- walktrap.community(graph=g)

# ---------------------------------------------------------------------------- #

#tweet in giornata
tw$hour[tw$hour==0] <- 24
tw$hour[tw$hour==1] <- 25
tw$hour[tw$hour==2] <- 26

tweetsPD <- tw[tw$partito=="PD",]
tweetsFDI <- tw[tw$partito=="FdI",]
tweetsFI <- tw[tw$partito=="FI",]
tweetsIV <- tw[tw$partito=="IV",]
tweetsLSP <- tw[tw$partito=="LSP",]
tweetsM5S <- tw[tw$partito=="M5S",]

par(mfrow=c(1,2))

plot(table(tweetsIV$hour), type="b", bty="l" ,
     xlab="hours" , ylab="Frequenza" ,
     col=1 , lwd=3 , pch=17, main="Italia Viva", xlim=c(6,26), ylim=c(0,1800))
lines(table(tweetsFDI$hour), type="b" , bty="l" ,
      xlab="hours" , ylab="Frequenza" , col=2,
      lwd=3 , pch=17, main="Fratelli d'Italia")
lines(table(tweetsPD$hour), type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=3 , lwd=3 , pch=17, main="Partito Democratico")
lines(table(tweetsFI$hour), type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=4 , lwd=3 , pch=17, main="Forza Italia")
lines(table(tweetsLSP$hour), type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=5 , lwd=3 , pch=17, main="Lega")
lines(table(tweetsM5S$hour), type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=6, lwd=3 , pch=17, main="Movimento 5 stelle")


legend("topright", 
       legend = c("IV", "FDI", "PD", "FI", "LSP", "MS5"), 
       col = 1:6, 
       pch = 17, 
       bty = "n", 
       pt.cex = 2, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.1))

plot(table(tweetsIV$hour) %>% prop.table, type="b", bty="l" ,
     xlab="hours" , ylab="Frequenza Relativa" ,
     col=1 , lwd=3 , pch=17, main="Italia Viva", xlim=c(6,26))
lines(table(tweetsFDI$hour) %>% prop.table, type="b" , bty="l" ,
         xlab="hours" , ylab="Frequenza Relativa" , col=2,
         lwd=3 , pch=17, main="Fratelli d'Italia")
lines(table(tweetsPD$hour) %>% prop.table, type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=3 , lwd=3 , pch=17, main="Partito Democratico")
lines(table(tweetsFI$hour) %>% prop.table, type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=4 , lwd=3 , pch=17, main="Forza Italia")
lines(table(tweetsLSP$hour) %>% prop.table, type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=5 , lwd=3 , pch=17, main="Lega")
lines(table(tweetsM5S$hour) %>% prop.table, type="b" , bty="l" , xlab="hours" , ylab="Frequenza" ,
      col=6, lwd=3 , pch=17, main="Movimento 5 stelle")


# ---------------------------------------------------------------------------- #

tw[tw$hour==9 & tw$partito=="IV","username"]
length(tw[tw$username=="ItaliaViva","username"])


tw$tw






# ---------------------------------------------------------------------------- #



