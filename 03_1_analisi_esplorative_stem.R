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
#setwd("C:/Users/Emanuele/Documenti/GitHub/sm")
#setwd("C:/Users/noido/Documenti/GitHub/sm")
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



#Cluster####

#LOUVAIN####
gr = cluster_louvain(graph = g)
table(gr$membership)

#   1    2    3    4    5    6    7 
#1917  196 4129 3014  804 2937  314 

#------------------------------------------#
# Gruppo 1 n: 1917 - Dirette su facebook
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

ris = c()

for(i in 1:length(unique(gr$membership))){
  cat(i, "\n")
  Xi = tt[tt$word %in% gr$names[gr$membership == i],] %>% cast_sparse(row=id, column=word, value=freq)
  M=t(Xi) %*% Xi
  g = graph_from_adjacency_matrix(M, weighted=TRUE, mode="undirected",
                                   add.rownames=TRUE)
  g = simplify(g, remove.loops=T)
  cat(dim(Xi)[2], "x 0.1=", dim(Xi)[2]*0.1, "\n" )
  ris = c(ris, names(sort(degree(g), decreasing = T)[1:(dim(Xi)[2]*0.1)]))
}
length(ris)
sum(grepl("emote_", ris)) # ci sono 29 emoticon
ris[grepl("emote_", ris)]
#------------------------------------------------------------------------------#
# CA per partito #
#------------------------------------------------------------------------------#
create_X_partito <- function(tweet){
  tt =  tweet %>% select(tweet=tweet_stem, partito=partito, utente=username) %>% mutate(id = 1:n())
  tt = tt %>% unnest_tokens(word,tweet) %>% group_by(id,word) %>% mutate(freq=n())
  tt = tt %>% group_by(partito,word) %>% mutate(freq=sum(freq))
  X = tt %>% cast_sparse(row=partito, column=word, value=freq)
  cat("Dimensioni X:", dim(X), "\n")
  return(X)
}

X_partito <- create_X_partito(tw)
sum(colSums(X_partito) == 1) #   9342 hapax, compaiono una volta in tutto il corpus
sum(colSums(X_partito > 1) == 1) # 5083 stem che compaiono solo in un partito
max(X_partito) # 2582 è la frequenza più alta

hist(colSums(X_partito), nclass = 100, xlim = c(0,500), freq = F)
lines(density(colSums(X_partito)), col = 2, lwd = 2)
#------------------------------------------------------------------------------#
# Selezioniamo solo gli stem che sono in ris, ovvero il 10% delle parole più usate in ogni gruppo

dataM <- as.matrix(X_partito[, colnames(X_partito) %in% ris])
colnames(dataM)[colSums(dataM) == 1] # 0 nessun hapax
sum(colSums(dataM > 1) == 1) # 24 stem che compaiono solo in un partito
colnames(dataM)[colSums(dataM > 1) == 1]

hist(colSums(dataM), nclass = 100, xlim = c(0, 1000), freq = F)

dim(dataM) #  6 partiti x 1327 stem
sum(rowSums(dataM) == 0) # 0, nessun partito risulta non rappresentato

res.ca <- CA(dataM, graph = F) # mette i putni riga e punti colonna ma fa cagare perchè abbiamo 1327 parole
#faccio il plot solo dei punti riga perche' mi interessano i partiti 
fviz_ca_row(res.ca, repel = TRUE)


# se consideriamo la prima dimensione:
#   - FI, PD, IV molto vicine, allo stesso livello praticamente
#   - M5S e LSP agli estremi
#   - FdI intermedio tra LSP e il gruppetto di 3
# Se consideriamo la seconda dimensione:
#   - gruppetto di tre da soli all'estremo negativo
#   - LSP e M5S allo stesso livello positivo 



#plot CA per cos2: indice di qualita'
#cos2 elevati sono in rosso
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
#valore max di cos2 somma a 1
#coerente con i contributi dei partiti sulle prime due dimensioni
#somma del coseno al quadrato di quella riga delle prime due dim -> infatti PD e FI sono rossi

# grafico Cos2 righe per dim 1 e 2
# dim1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# M5S e Lega quelli con i maggiori contributi per la prima dimensione

# dim2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
# Lega e IV le due maggiori 

# Eigenvalue
eig.val <- get_eigenvalue(res.ca)
#        eigenvalue variance.percent cumulative.variance.percent
#Dim.1 0.16769498         36.61530                    36.61530
#Dim.2 0.12477814         27.24464                    63.85993
#Dim.3 0.06311873         13.78164                    77.64157
#Dim.4 0.05321652         11.61954                    89.26111
#Dim.5 0.04918320         10.73889                   100.00000

#ha trovato 5 dimensioni (non 7 come i gruppi trovati nella rete)
eig.val#il primo grafico mi spiega il 63% della variabilità
#forse Ã¨ da disegnare anche il plot per 2^ e 3^ dimensione-> arrivo a 76% var tot
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))

#per estrarre valori che mi possono servire
row<- get_ca_row(res.ca)
row
# Contributi dei partiti sulle dimensioni
row$contrib
#            Dim 1     Dim 2       Dim 3      Dim 4      Dim 5
#M5S 7.177108e+01 15.764313  0.01119796  0.2708699  0.1206916
#LSP 2.364157e+01 39.497808 11.49244030  0.5270024  0.3600231
#PD  2.958495e-01 13.097252  2.17562481 54.4609424 17.4103904
#FI  3.855467e-04  8.052813  7.47655966  2.6529516 63.7422701
#FdI 4.280838e+00  0.106368 60.47164502  4.8893886 16.5719276
#IV  1.027118e-02 23.481446 18.37253225 37.1988450  1.7946973

#primo fattore PD, FdI, IV e M5S, secondo FI, FdI


#vedo gli stem piu significativi: grafico Cos2 colonne per dim 1 e 2
col<- get_ca_col(res.ca)
col$contrib[1:50,] #prime 50 parole importanti
#% di contributo non alto ma sto considerando  1328 stem quindi ok
# dim1: prime 20 parole
fviz_contrib(res.ca, choice = "col", axes = 1, top = 20)
# la prima potrebbe essere: faccia con le stelle o faccia sorpresa

# dim2: prime 20 parole 
fviz_contrib(res.ca, choice = "col", axes = 2, top = 20)


# ---------------------------------------------------------------------------- #
# Clustering delle parole sulle coordinate colonna
# ---------------------------------------------------------------------------- #
# Dai grafici delle prime due dimensioni sembrano esserci tre gruppi
# Verifichiamo attraverso un clustering

# HCPC
hc <- HCPC(res.ca, nb.clust=-1)
fviz_dend(hc, show_labels = FALSE)
# Individuals facor map
fviz_cluster(hc, geom = "point", main = "Factor map")
View(hc$data.clust)
hc$desc.var$category
View(hc$data.clust)
hc$desc.var
hc$desc.axes
hc$desc.ind

# Metodi gerarchici

seed.dist <- dist(res.ca$row$coord, method="euclidean") # method="manhattan"
hcc <- hclust(seed.dist, method="complete")
plot(hcc)
# m5S per i fatti suoi e poi altri due gruppi
rect.hclust(hcc, k=3, border="red", )

# Metodo agglomerativo AGNES

library(cluster)
ag <- agnes(res.ca$row$coord, metric="euclidean", method="average", trace.lev=1)
ag
plot(ag, which.plots=2)
#uguale

# Metodo divisivo DIANA

library(cluster)
da <- diana(res.ca$row$coord, metric="euclidean", trace.lev=1)
da
plot(da, which.plots=2)

# ---------------------------------------------------------------------------- #

# Sono tutti coerenti e uguali nei risultati

# quindi conclusioni: su tutte e 5 le dimensioni si conferma quanto visto con 2 dimensioni:
#     - M5S da solo isolato
#     - LSP, FdI insieme
#     - PD, FI, IV insieme con gli ultimi due maggiormente legati

# come interpretiamo questi tre gruppi? 
# stiamo raggruppando usando le parole che utilizzano, quindi si raggruppano in base a come si esprimono
#     - LSP e FdI sono partiti monoesponente: Salvini da una parte e Crosetto dall'altra.
#     - M5S invece punta sul movimento nell'insieme e non sul singolo.

# Attenzioni:
# LSP e FdI non hanno la pagina del partito, mentre M5S ha solo pagina del partito.
# Potrebbe essere un motivo della separazione così netta tra quei partiti.



# ---------------------------------------------------------------------------- #
# LDA su tutte le parole
# ---------------------------------------------------------------------------- #

#LDA per partito
rm(list = ls())
library(topicmodels)
load("data/02_tweets_stem.RData")
tt =  tw %>% select(tweet=tweet_stem, partito=partito, username = username) %>% mutate(id = 1:n())
tt = tt %>% unnest_tokens(word,tweet) %>% group_by(id,word) %>% mutate(freq = n())
X = tt %>% cast_sparse(row=id, column=word, value=freq)
nomi <- colnames(X)
del = which(colSums(X)<=1)
summary(colSums(X))
X = X[,-del]
colnames(X) <- nomi[-del]
del <- rowSums(X) == 0
X = X[-del,]

#per ogni partito faccio la LDA

#a questo punto metto 5 o 7 gruppi? con 7 non si capiscono i topic
q_lda=LDA(as.matrix(X), k=7, method="Gibbs", control=list(seed=123,
                                                   burnin=500,
                                                   iter=1000))
beta_topics =tidy(q_lda, matrix="beta")
beta_topics

termini=beta_topics %>% group_by(topic) %>% top_n(10,beta) %>% 
  arrange(topic)
termini
term_pl=termini %>% ungroup %>% mutate(term=reorder(term, beta))
term_pl

#plot
library(ggplot2)
plot_base=ggplot(term_pl, aes(term, beta, fill=factor(topic)))+
  geom_col()+facet_wrap(~topic, scales = "free", nrow=1)
plot_base+coord_flip()
gamma_doc=tidy(q_lda, matrix="gamma")
gamma_doc %>% arrange(document)
ggplot(gamma_doc)+geom_density(aes(gamma, fill=factor(topic)))+
  facet_wrap(~topic, ncol=3)

doc_high=gamma_doc %>% group_by(topic) %>% top_n(3, gamma)
doc_high

