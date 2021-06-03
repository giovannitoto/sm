# ---------------------------------------------------------------------------- #

# Analisi:
# - Network of Words sugli stem (rete di unigrammi)
# - Clustering attraverso Louvain, applicato alla Network of Words
# - Interpretazione delle comunita' guardando le parole più importanti
# - Selezione del 10% di parole piu' importanti per comunita' (maggior degree)
# - Analisi delle corrispondenze lessicali (LCA) per partito con solo le parole più importanti
# - Clustering applicato alle coordinate colonna della LCA

# ---------------------------------------------------------------------------- #

library(dplyr)
library(tm)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(tidygraph) 
library(ggraph)
library(Matrix)
library(igraph)
library(stringr)
library(FactoMineR)
library(factoextra)
library(cluster)  # AGNES, DIANA

# ---------------------------------------------------------------------------- #

rm(list=ls())
load("data/02_tweets_stem.RData")
# Considero solo i tweet originali, non le risposte ad altri tweet
tw <- tw[tw$reply_to==0,]

# ---------------------------------------------------------------------------- #

### Network of Words

# Preparo dataframe per la rete
tt <-  tw %>% select(tweet=tweet_stem, partito=partito) %>% mutate(id = 1:n())
tt <- tt %>% unnest_tokens(word, tweet) %>% group_by(id,word) %>% mutate(freq = n())
X <- tt %>% cast_sparse(row=id, column=word, value=freq)
nomi <- colnames(X)
# Rimuovo gli hapax
del <- which(colSums(X)<=1)
X <- X[,-del]
colnames(X) <- nomi[-del]
dim(X)  # 34641 x 13311

# Costruisco matrice parole x parole, ogni suo elemento e' una sorta di distanza
# che misura quante volte due parole distinte compaiono nello stesso documento 
M <- t(X) %*% X
dim(M)  # 13311 x 13311

# Costruisco la rete di unigrammi
g <- graph_from_adjacency_matrix(M, weighted=T, mode="undirected", add.rownames=T)
# Rimuovo i self loops poiche' non d'interesse; in teoria non serve
g <- simplify(g, remove.loops=T)
# Tengo solo la rete completamente connessa piu' grande; in realta', si ha gia'
# una rete completamente connessa, quindi e' un comando balzabile
table(components(g)$membership)
g <- delete_vertices(g, which(components(g)$membership != 1)) 
# g ha 13311 nodi/stem e 1687165 archi/co-occorrenze

# ---------------------------------------------------------------------------- #

### Clustering: Metodo di Louvain

gr <- cluster_louvain(graph=g)
table(gr$membership)
#   1    2    3    4    5    6    7 
#1917  196 4129 3014  804 2937  314 

# Ho identificato 7 gruppi: costruisco una nuova sotto-rete per ogi comunita' e 
# identifico il 10% delle parole piu' importanti
ris = c()
top50_words_by_community <- c()
for(i in 1:length(unique(gr$membership))){
  cat(i, "- ")
  Xi = tt[tt$word %in% gr$names[gr$membership==i],] %>% cast_sparse(row=id, column=word, value=freq)
  M <- t(Xi) %*% Xi
  g <- graph_from_adjacency_matrix(M, weighted=T, mode="undirected",add.rownames=T)
  g <- simplify(g, remove.loops=T)
  cat(dim(Xi)[2], "x 0.1=", dim(Xi)[2]*0.1, "\n")
  top50_words_by_community <- cbind(top50_words_by_community,
                                    paste(str_pad(sort(degree(g),decreasing=T),4,pad=" "),
                                          names(sort(degree(g),decreasing=T)), sep=" - ")[1:50])
  ris <- c(ris, names(sort(degree(g), decreasing = T)[1:(dim(Xi)[2]*0.1)]))
}
# In totale, gli stem importanti sono 1327 di cui 29 emote
length(ris)                # 1327 stem
sum(grepl("emote_", ris))  #   29 emote

# Nella variabile 'top50_words_by_community' ho salvato i 50 nodi/stem con maggior
# degree per comunita'; li utilizzo per associare dei topic:
#  - Gruppo 1 n: 1917 - Dirette su facebook
#  - Gruppo 2 n:  196 - Tante emoticon, roba estera: politica, american's cup, USA, Brexit, ...
#  - Gruppo 3 n: 4129 - Violenza sulle donne
#  - Gruppo 4 n: 3014 - Manovre economico - politiche per l'emergenza
#  - Gruppo 5 n:  804 - Tweet su Roma 
#  - Gruppo 6 n: 2937 - Politica interna
#  - Gruppo 7 n:  314 - Covid/salute/ospedali/vaccini

# ---------------------------------------------------------------------------- #

### Analisi delle Corrispondenze Lessicali per partito

# Costruisco una nuova matrice che considera tutti i tweet di un partito come un
# unico documento; cosi' facendo avro' una matrice 6 x 22653
create_X_partito <- function(tweet) {
  # Funzione per considerare tutti i tweet di un partito come un'unico testo e 
  # costruire la corrispondente matrice doc x stem
  tt =  tweet %>% select(tweet=tweet_stem, partito=partito, utente=username) %>% mutate(id = 1:n())
  tt = tt %>% unnest_tokens(word,tweet) %>% group_by(id,word) %>% mutate(freq=n())
  tt = tt %>% group_by(partito,word) %>% mutate(freq=sum(freq))
  X = tt %>% cast_sparse(row=partito, column=word, value=freq)
  cat("Dimensioni X:", dim(X), "\n")
  return(X)
}

X_partito <- create_X_partito(tw)
sum(colSums(X_partito) == 1)     # 9342 hapax (compaiono una volta in tutto il corpus)
sum(colSums(X_partito > 1) == 1) # 5083 stem che compaiono solo in un partito
max(X_partito)                   # 2582 è la frequenza più alta

# Distribuzione delle frequenze assolute degli stem nel corpus (DA SISTEMARE)
hist(colSums(X_partito), nclass = 100, xlim = c(0,500), freq = F)
lines(density(colSums(X_partito)), col = 2, lwd = 2)

# La matrice 'X_partito' contiene tutti gli stem: elimino tutti quelli che non
# sono presenti in 'ris', ovvero non sono tra il 10% delle parole più usate in
# ogni gruppo
dataM <- as.matrix(X_partito[, colnames(X_partito) %in% ris])
dim(dataM) #  6 partiti x 1327 stem
colnames(dataM)[colSums(dataM) == 1] # 0 nessun hapax
sum(rowSums(dataM) == 0) # 0, nessun partito risulta non rappresentato

# Osservo che 24 stem che compaiono solo in un partito sono utilizzati da un 
# solo partito: vado a vedere le parole
sum(colSums(dataM > 1) == 1)
colnames(dataM)[colSums(dataM > 1) == 1]

# Distribuzione delle frequenze assolute degli stem nel corpus (DA SISTEMARE)
hist(colSums(dataM), nclass = 100, xlim = c(0, 1000), freq = F)

# ---------------------------------------------------------------------------- #

# Ora posso usare la LCA:
#  - graph=T mette sia i punti riga sia i punti colonna ma fa cagare perchè ci
#    sono troppe parole (1327)
res.ca <- CA(dataM, graph = F)

# Faccio il plot solo dei punti riga perche' mi interessano i partiti 
fviz_ca_row(res.ca, repel = TRUE)
# Se considero la prima dimensione:
#   - FI, PD, IV molto vicine, allo stesso livello praticamente
#   - M5S e LSP agli estremi
#   - FdI intermedio tra LSP e il gruppetto di 3
# Se considero la seconda dimensione:
#   - gruppetto di tre da soli all'estremo negativo
#   - LSP e M5S allo stesso livello positivo 

# Stesso plot ma con colori associati ai punti; il colore e' definito in base a 
# l'indice di qualita 'cos2' (cos2 elevato => buono)
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
# note:
#  - valore di cos2 per una data dimensione somma 1, quindi il valore associato
#    a un punto riga (partito) indica quanto e' utile il partito per definire la
#    dimensione latente
#  - e' coerente con i contributi dei partiti sulle prime due dimensioni
#  - somma del coseno al quadrato di quella riga delle prime due dim -> infatti PD e FI sono rossi

# Grafico 'cos2' righe per la prima dimensione:
#  - M5S e Lega danno i maggiori contributi per la prima dimensione
fviz_contrib(res.ca, choice="row", axes=1, top=10)
# Grafico 'cos2' righe per la seconda dimensione:
#  - Lega e IV danno i maggiori contributi per la prima dimensione
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)

# Ottengo gli eigenvalue/autovalori per vedere quanta variabilita' dei dati ogni
# dimensione riesce a spiegare
eig.val <- get_eigenvalue(res.ca)
eig.val
#        eigenvalue variance.percent cumulative.variance.percent
# Dim.1  0.16769498         36.61530                    36.61530
# Dim.2  0.12477814         27.24464                    63.85993
# Dim.3  0.06311873         13.78164                    77.64157
# Dim.4  0.05321652         11.61954                    89.26111
# Dim.5  0.04918320         10.73889                   100.00000

# Da notare che il numero massimo di dimensioni e' 5, ovvero n-1; il seguente
# comando mostra in maniera intuitiva il contributo di ogni dimensione
fviz_screeplot(res.ca, addlabels=TRUE, ylim = c(0, 50))

# Contributi dei partiti sulle dimensioni
row<- get_ca_row(res.ca)
row$contrib
#            Dim 1     Dim 2       Dim 3      Dim 4      Dim 5
# M5S 7.177108e+01 15.764313  0.01119796  0.2708699  0.1206916
# LSP 2.364157e+01 39.497808 11.49244030  0.5270024  0.3600231
# PD  2.958495e-01 13.097252  2.17562481 54.4609424 17.4103904
# FI  3.855467e-04  8.052813  7.47655966  2.6529516 63.7422701
# FdI 4.280838e+00  0.106368 60.47164502  4.8893886 16.5719276
# IV  1.027118e-02 23.481446 18.37253225 37.1988450  1.7946973

#primo fattore PD, FdI, IV e M5S, secondo FI, FdI


# Contributi degli stem sulle dimensioni: visto che sono tanti, vado a vedere solo i primi 50
col <- get_ca_col(res.ca)
col$contrib[1:50,] # 50 stem piu' importanti

# Guardo le 20 parole che forniscono un maggior contributo alla prima dimensione (cos2)
fviz_contrib(res.ca, choice="col", axes=1, top=20)
# 'emote_st' potrebbe essere faccia con le stelle o faccia sorpresa

# Guardo le 20 parole che forniscono un maggior contributo alla seconda dimensione (cos2)
fviz_contrib(res.ca, choice="col", axes=2, top=20)

# ---------------------------------------------------------------------------- #

### Clustering delle parole sulle coordinate colonna

# Dai grafici delle prime due dimensioni sembrano esserci tre gruppi, tuttavia
# il grafico e' un'approssimazione poiche' considera sole prime due dimensioni 
# che insieme spiegano solo il 63% della variabilita'.

# Per verificare la presenza di gruppi, utilizzo metodi di clustering vari:
# se ottengo sempre risultati simili, posso concludere che esistono dei cluster

# HCPC
hc <- HCPC(res.ca, nb.clust=-1)
fviz_dend(hc, show_labels=FALSE)

# Individuals factor map: grafico bellino per indicare i gruppi, riporta anche i centroidi
fviz_cluster(hc, geom = "point", main = "Factor map")

View(hc$data.clust)   # ?
hc$desc.var$category  # ?
View(hc$data.clust)   # ?
hc$desc.var           # ?
hc$desc.axes          # ?
hc$desc.ind           # ?

# Metodi gerarchici
seed.dist <- dist(res.ca$row$coord, method="euclidean") # matrice di distanze
hcc <- hclust(seed.dist, method="complete")
plot(hcc)
rect.hclust(hcc, k=3, border="red") # m5S per i fatti suoi e poi altri due gruppi

# Metodo agglomerativo AGNES
ag <- agnes(res.ca$row$coord, metric="euclidean", method="average", trace.lev=1)
ag
plot(ag, which.plots=2, main="Metodo agglomerativo AGNES")

# Metodo divisivo DIANA
da <- diana(res.ca$row$coord, metric="euclidean", trace.lev=1)
da
plot(da, which.plots=2, main="Metodo divisivo DIANA")

# ---------------------------------------------------------------------------- #

# Tutti i metodi di clustering sono coerenti e uguali nei risultati; anche
# considerando tutte e 5 le dimensioni si conferma quanto visto con 2:
#     - M5S da solo isolato
#     - LSP, FdI insieme
#     - PD, FI, IV insieme con gli ultimi due maggiormente legati

# Come interpreto questi tre gruppi? 
# Sto raggruppando usando le parole che utilizzano, quindi si raggruppano in base a come si esprimono
#     - LSP e FdI sono partiti monoesponente: Salvini da una parte e Crosetto dall'altra.
#     - M5S invece punta sul movimento nell'insieme e non sul singolo.

# Attenzioni:
# LSP e FdI non hanno la pagina del partito, mentre M5S ha solo pagina del partito.
# Potrebbe essere un motivo della separazione così netta tra quei partiti.

# ---------------------------------------------------------------------------- #