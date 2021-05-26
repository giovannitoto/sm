# ---------------------------------------------------------------------------- #

# Analisi esplorative varie:
# - Rete su tutti gli stem
# - Clustering attraverso Louvain
# - Interpretazione dei gruppi guardando le parole più importanti
#       - Importanza per degree e per betweenness

# ---------------------------------------------------------------------------- #

library(dplyr)
library(tm)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(tidygraph) 
library(ggraph)

# ---------------------------------------------------------------------------- #

rm(list=ls())
setwd("C:/Users/Emanuele/Documenti/GitHub/sm")
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
tt =  tw %>% select(tweet=tweet_stem, partito=partito) %>% mutate(id = 1:n())
tt = tt %>% unnest_tokens(word,tweet) %>%
  group_by(id,word) %>% mutate(freq = n())
X = tt %>% cast_sparse(row=id, column=word, value=freq, )
nomi <- colnames(X)
del = which(colSums(X)<=1)
X = X[,-del]
colnames(X) <- nomi[-del]

dim(X)  # 34641 13311
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



#Cluster####

#LOUVAIN####
gr = cluster_louvain(graph = g)
table(gr$membership)

#   1    2    3    4    5    6    7 
#1917  196 4129 3014  804 2937  314 

#------------------------------------------#
# Gruppo 1 n: 1917 - Dirette su facebok
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 1],] %>% cast_sparse(row=id, column=word, value=freq)
X <- X[, colSums(X)>quantile(colSums(X), probs = 0.90)] # mostro il 10% delle parole
dim(X)  # 23421   192
M=t(X) %*% X
g1 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g1 = simplify(g1, remove.loops=T)

names(sort(degree(g1), decreasing = T)[1:30])
sort(betweenness(g1), decreasing = T)[1:20]

#------------------------------------------#
# Gruppo 2 n: 196 - Tante emoticon, roba estera: politica, american's cup, USA, Brexit, ...
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 2],] %>% cast_sparse(row=id, column=word, value=freq)
dim(X)  #  983 196
M=t(X) %*% X
g2 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g2 = simplify(g2, remove.loops=T)
names(V(g2))
sort(degree(g2), decreasing = T)[1:50]
sort(betweenness(g2), decreasing = T)[1:20]



#------------------------------------------#
# Gruppo 3 n: 4129 - Violenza sulle donne
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 3],] %>% cast_sparse(row=id, column=word, value=freq)
X <- X[, colSums(X)>quantile(colSums(X), probs = 0.90)] # mostro il 10% delle parole
dim(X)  # 27376   413
M=t(X) %*% X
g3 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g3 = simplify(g3, remove.loops=T)

sort(degree(g3), decreasing = T)[1:30]
sort(betweenness(g3), decreasing = T)[1:20]

#------------------------------------------#
# Gruppo 4 n: 3014 - Manovre economico - politiche per l'emergenza
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 4],] %>% cast_sparse(row=id, column=word, value=freq)
X <- X[, colSums(X)>quantile(colSums(X), probs = 0.90)] # mostro il 10% delle parole
dim(X)  # 29924   302
M=t(X) %*% X
g4 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g4 = simplify(g4, remove.loops=T)

sort(degree(g4), decreasing = T)[1:30]
sort(betweenness(g4), decreasing = T)[1:20]

#------------------------------------------#
# Gruppo 5 n: 804 - Tweet su Roma 
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 5],] %>% cast_sparse(row=id, column=word, value=freq)
dim(X)  #  11032   804
M=t(X) %*% X
g5 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g5 = simplify(g5, remove.loops=T)

sort(degree(g5), decreasing = T)[1:30]
sort(betweenness(g5), decreasing = T)[1:20]

#------------------------------------------#
# Gruppo 6 n: 2937 - Politica interna
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 6],] %>% cast_sparse(row=id, column=word, value=freq)
X <- X[, colSums(X)>quantile(colSums(X), probs = 0.90)] # mostro il 10% delle parole
dim(X)  # 27613   291
M=t(X) %*% X
g6 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g6 = simplify(g6, remove.loops=T)

sort(degree(g6), decreasing = T)[1:30]
sort(betweenness(g6), decreasing = T)[1:20]

#------------------------------------------#
# Gruppo 7 n: 314 - Covid/salute/ospedali/vaccini
#------------------------------------------#

X = tt[tt$word %in% gr$names[gr$membership == 7],] %>% cast_sparse(row=id, column=word, value=freq)
dim(X)  # 27613   291
M=t(X) %*% X
g7 = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                 add.rownames=TRUE)
g7 = simplify(g7, remove.loops=T)

sort(degree(g7), decreasing = T)[1:30]
sort(betweenness(g7), decreasing = T)[1:20]



# ---------------------------------------------------------------------------- #
# Facciamo l'ACL sulle parole 'più importanti' di ogni gruppo

ris = list()

for(i in 1:length(unique(gr$membership))){
  cat(i, "\n")
  X = tt[tt$word %in% gr$names[gr$membership == i],] %>% cast_sparse(row=id, column=word, value=freq)
  M=t(X) %*% X
  g = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                   add.rownames=TRUE)
  g = simplify(g, remove.loops=T)
  ris[[i]] = names(sort(degree(g), decreasing = T)[1:(dim(X)[2]*0.1)])
}
sum(sapply(ris, length)) # abbiamo 1327 stem 
ris <- c(ris[[1]], ris[[2]], ris[[3]], ris[[4]], ris[[5]], ris[[6]], ris[[7]])
length(ris)


dataM <- X[, colnames(X) %in% ris]
dim(dataM) # 34641  1327
sum(rowSums(dataM) == 0) # 251
dataM <- dataM[rowSums(dataM) > 0,]
dim(dataM)



# ---------------------------------------------------------------------------- #

#tweet in giornata
tw$hour[tw$hour==0] <- 24
tw$hour[tw$hour==1] <- 25
tw$hour[tw$hour==2] <- 26

par(mfrow=c(1,2))

matplot(table(tw$hour, tw$partito), type="b", bty="l" ,
        xlab="hours" , ylab="Frequenza", lwd=3 , pch=17, main="Tweet per ogni ora del giorno")
matplot(prop.table(table(tw$hour, tw$partito)), type="b", bty="l",
        xlab="hours" , ylab="Frequenza", lwd=3 , pch=17, main="Tweet per ogni ora del giorno")

legend("topright", 
       legend = c("FdI", "FI", "IV", "LSP", "M5S", "PD"), 
       col = 1:6, 
       pch = 17, 
       bty = "n", 
       pt.cex = 2, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.1))


# ---------------------------------------------------------------------------- #

# L'idea è quella di fare una LDA con 7 gruppi e poi verificare se i gruppi trovati
# corrispondono ai gruppi trovati con la rete delle parole.

# ---------------------------------------------------------------------------- #



