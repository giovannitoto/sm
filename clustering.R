# ---------------------------------------------------------------------------- #

# Metodi di clustering sugli stem.
setwd("C:/Users/Giovanni/Desktop/sm")

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
tw <- tw[!(tw$username=="LegaSalvini"),]
tw <- tw[tw$reply_to==0,]

# Riduco dimensionalita' per i test
table(tw$partito)
tw <- tw[tw$partito=="M5S"|tw$partito=="PD"|tw$partito=="FI",]

# Converto il dataset in un corpus
tw <- tw %>% select(tweet, username, partito) %>% mutate(id=1:n()) 
tw_corpus <- corpus(tw$tweet, docnames=tw$id)

# ---------------------------------------------------------------------------- #

# Ottengo matrice degli stem.
stem_matrix <- tokens(tw_corpus, remove_punct=T, remove_symbols=T) %>% dfm
#head(stem_matrix)
dim(stem_matrix)

# Rimuovo gli stem rari
term_fr <- featfreq(stem_matrix)
w_rem <- names(term_fr[term_fr < 10])
stem_matrix <- dfm_remove(stem_matrix, w_rem)
stem_matrix <- dfm_subset(stem_matrix, rowSums(stem_matrix)>1)
dim(stem_matrix)
vett_partito <- tw$partito[as.numeric(docnames(stem_matrix))]
length(vett_partito)

# Salvo la matrice degli stem in csv
write.csv(cbind(vett_partito, as.matrix(stem_matrix)),
          "stem_matrix.csv", row.names=FALSE, fileEncoding="UTF-8")

# ---------------------------------------------------------------------------- #

# REGRESSIONE CON LE COMP. PRINCIPALI (CP)
# Le componenti principali sono ricavate dalla matrice di varianze e covarianze
# poiche' le var. sono gia' standardizzate
# Con cor=TRUE, si usa la matrice di correlazione che porta a risultati diversi

pc <- princomp(stem_matrix[,1:100], cor=FALSE)
summary(pc)
# Ottengo la "Proportion of Variance" (PoV) delle 100 CP
PoV <- pc$sdev^2/sum(pc$sdev^2)
# Valuto i valori comulati per selezionare il numero di CP 
cbind(PoV, cumsum(PoV))[1:65,]
# Guardo screeplot
screeplot(pc, npcs=50)
# Seleziono le prime comp. principali
pc_matrix <- pc$scores[,1:65]

# ---------------------------------------------------------------------------- #

# Analisi delle Corrispondenze (AC)

ca <- textmodel_ca(stem_matrix, smooth=0, nd=NA, sparse=FALSE, residual_floor=0.1)
summary(ca)

ca2 <- textmodel_ca(stem_matrix, smooth=0, nd=2, sparse=FALSE,
                    residual_floor=0.1)
  
# ---------------------------------------------------------------------------- #

# k-MEANS
explained_var <- c()
for (k in 1:7) {
  tmp_km <- kmeans(stem_matrix, centers=k)
  explained_var <- c(explained_var, tmp_km$betweenss/tmp_km$totss)
  rm(tmp_km)
  cat(k, "-means done.\n", sep="")
}
plot(explained_var, xlab="n. clusters", ylab="% var. spiegata", type="l")

tmp_km <- kmeans(stem_matrix, centers=4)
cbind(vett_partito, tmp_km$cluster)[1:10,]

table(vett_partito, tmp_km$cluster)

# ---------------------------------------------------------------------------- #

# Metodi gerarchici

seed.dist <- dist(pc_matrix, method="euclidean") # method="manhattan"
hcc <- hclust(seed.dist, method="complete")
plot(hcc, labels=FALSE)
rect.hclust(hcc, k=10, border="red")

# ---------------------------------------------------------------------------- #

# Metodo agglomerativo AGNES

library(cluster)
ag <- agnes(stem_matrix, metric="euclidean", method="average", trace.lev=1)
ag
plot(da, which.plots=2)

# ---------------------------------------------------------------------------- #

# Metodo divisivo DIANA

library(cluster)
da <- diana(stem_matrix, metric="euclidean", method="average", trace.lev=1)
da
plot(da, which.plots=2)

# ---------------------------------------------------------------------------- #



