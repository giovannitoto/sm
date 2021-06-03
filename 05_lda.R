# ---------------------------------------------------------------------------- #

# LDA

setwd("C:/Users/Giovanni/Desktop/sm")

# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)

# ---------------------------------------------------------------------------- #

rm(list=ls())
load("data/02_tweets_stem.RData")
tw <- tw[tw$reply_to==0,]

# ---------------------------------------------------------------------------- #

tt =  tw %>% select(tweet=tweet_stem, partito=partito, username = username) %>% mutate(id = 1:n())
tt2 = tt %>% unnest_tokens(word,tweet) %>% group_by(id,word) %>% mutate(freq=n())
X = tt2 %>% cast_sparse(row=id, column=word, value=freq)

# Creo il vettore partito
vettore_partito <- tt$partito[as.numeric(rownames(X))]
dim(X)[1]==length(vettore_partito)
table(vettore_partito)
#  FdI   FI   IV  LSP  M5S   PD 
# 4571 6151 7667 8471 3601 4180 

# Filtro le colonne, o meglio rimuovo gli hapax
nomi <- colnames(X)
del <- which(colSums(X)<=1)
X <- X[, -del]
colnames(X) <- nomi[-del]

# Filtro X e vettore_partito rimuovendo i tweet senza stem
vettore_partito <- vettore_partito[rowSums(X)>0]
X <- X[rowSums(X)>0,]
dim(X)[1]==length(vettore_partito)
table(vettore_partito)
#  FdI   FI   IV  LSP  M5S   PD 
# 4566 6151 7667 8470 3601 4177 

# ---------------------------------------------------------------------------- #

# Ora posso stimare la LDA con 7 topic (e' la migliore tra 5, 6, 7 e 8)
q_lda <- LDA(as.matrix(X), k=7, method="Gibbs", control=list(seed=123,
                                                             burnin=1000,
                                                             iter=5000))
beta_topics <- tidy(q_lda, matrix="beta")
dim(beta_topics) # 93177 x 3     nota: 93177 = k*V = 7*13311

# Per ogni topic selezione i 10 termini con peso maggiore, ovvero scelgo i 
# termini a cui corrispondono i valori piu' alti dei parametri beta
termini <- beta_topics %>% group_by(topic) %>% top_n(10, beta) %>% arrange(topic)
# Rapppresento graficamente questi termini
term_pl <- termini %>% ungroup %>% mutate(term=reorder(term,beta))
plot_base <- ggplot(term_pl, aes(term, beta, fill=factor(topic))) +
  geom_col() + facet_wrap(~topic, scales="free", nrow=1)
plot_base + coord_flip()

top_50_words <- beta_topics %>% group_by(topic) %>% top_n(50, beta) %>%
  arrange(topic) %>% ungroup %>% mutate(term=reorder(term,beta)) 
t_list <- list()
for (j in 1:7) {
  t_list[[j]] <- as.data.frame(top_50_words[top_50_words$topic==j,])
}
t_list[[1]][order(t_list[[1]]$beta,decreasing=T),]


# ---------------------------------------------------------------------------- #

# Costruisco la matrice gamma a partire da gamma_doc
gamma_doc <- tidy(q_lda, matrix="gamma")
gamma <- vettore_partito
for (j in 1:max(gamma_doc$topic)) {
  gamma <- cbind(gamma, gamma_doc[gamma_doc$topic==j,"gamma"])
}
colnames(gamma) <-  c("partito", "t1", "t2", "t3", "t4", "t5")
dim(gamma) # 34632 x 6  (n x k)

# Grafici delle distr. a posteriori condizionate al partito di appartenenza
# dei tweet
par(mfrow=c(3,3))
partito_list <- unique(gamma$partito)
for (k in 2:ncol(gamma)) {
  post_tmp <- gamma[gamma$partito==partito_list[1],k]
  post_tmp_dens <- density(post_tmp)
  postmode <- post_tmp_dens$x[which.max(post_tmp_dens$y)]
  # plot
  plot(density(post_tmp), col=1, lwd=2, main=paste("Topic",k-1),
       ylim=c(0,25), xaxt='n')
  abline(v=mean(post_tmp), col=1, lty=2)
  #abline(v=postmode, col=1, lty=2)
  for (p in 2:6) {
    post_tmp <- gamma[gamma$partito==partito_list[p],k]
    post_tmp_dens <- density(post_tmp)
    postmode <- post_tmp_dens$x[which.max(post_tmp_dens$y)]
    # plot
    lines(density(post_tmp), col=p, lwd=2)
    abline(v=mean(post_tmp), col=p, lty=2)
    #abline(v=postmode, col=p, lty=2)
  }
  legend("topright", legend=partito_list, fill=1:6, horiz=F, cex=1)
}

# ---------------------------------------------------------------------------- #