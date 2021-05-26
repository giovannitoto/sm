# ---------------------------------------------------------------------------- #

# Analisi esplorative varie:
# - Rete su tutti gli stem
# - Clustering attraverso Louvain
# - Interpretazione dei gruppi guardando le parole più importanti
#       - Importanza per degree e per betweenness

# - Analisi delle corrispondenze con le parole più importanti

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
library(FactoMineR)
library(factoextra)

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


dataM <- as.matrix(X[, colnames(X) %in% ris])
dim(dataM) # 34641  1327
sum(rowSums(dataM) == 0) # 251
dataM <- dataM[rowSums(dataM) > 0,]
dim(dataM)

class(dataM)

res.ca <- CA(dataM, graph = T)
print(res.ca)
# Eigenvalue
eig.val <- get_eigenvalue(res.ca)
eig.val
#se eigenvalue = 1 situazione problematica...
#eigenvalues sono la projected intertia
#somma tutta interzia (somma eigenvalue) ??? phi square
inerzia <- sum(eig.val[,1])
size <-sum(nobel)
chi <- inerzia*size

fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 60))
#inerzia di una nube di punti ??? somma di inerzia di ogni punto
#inerzia di un punto ??? la massa per la distanza al quadrato dal centro di gravit??? (distanza chi quadrato)
#duality: ragionare su inerzia di righe o di colonne ??? lo stesso..giocano un ruolo simmetrico. Non come PCA!
#troviamo assi ortogonali che massimizzano la bont??? della proiezione (projected inertia), data da massa per distanza a quadrato della proiezione dall'origine

fviz_ca_biplot(res.ca)



# ---------------------------------------------------------------------------- #

# L'idea è quella di fare una LDA con 7 gruppi e poi verificare se i gruppi trovati
# corrispondono ai gruppi trovati con la rete delle parole.

# ---------------------------------------------------------------------------- #



