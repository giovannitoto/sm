library(topicmodels)
rm(list=ls())
load("data/02_tweets_stem.RData")
tw <- tw[tw$reply_to==0,]
tt =  tw %>% select(tweet=tweet_stem, partito=partito, username = username) %>% mutate(id = 1:n())
tt2 = tt %>% unnest_tokens(word,tweet) %>% group_by(id,word) %>% mutate(freq = n())
X = tt2 %>% cast_sparse(row=id, column=word, value=freq)
# creiamo il vettore partito
vettore_partito <- tt[rownames(X),]$partito
dim(X)[1]==length(vettore_partito)
# filtriamo le colonne
nomi <- colnames(X)
del = which(colSums(X)<=1)
X = X[,-del]
colnames(X) <- nomi[-del]
# filtriamo X e vettore_partito
vettore_partito <- vettore_partito[rowSums(X)>0]
X <- X[rowSums(X)>0,]
dim(X)[1]==length(vettore_partito)

#per ogni partito faccio la LDA

#a questo punto metto 5 o 7 gruppi? con 7 non si capiscono i topic
q_lda <- LDA(as.matrix(X), k=5, method="Gibbs", control=list(seed=123,
                                                          burnin=500,
                                                          iter=1000))
beta_topics <- tidy(q_lda, matrix="beta")
beta_topics %>% arrange(topic)
dim(beta_topics) # 105784 x 3     nota: 105784 = k*V

# Per ogni topic selezione i 10 termini con peso maggiore, ovvero scelgo i 
# termini a cui corrispondono i valori piu' alti dei parametri beta
termini <- beta_topics %>% group_by(topic) %>% top_n(10, beta) %>% arrange(topic)
termini

# Rapppresento graficamente questi termini
term_pl <- termini %>% ungroup %>% mutate(term=reorder(term,beta))
plot_base <- ggplot(term_pl, aes(term, beta, fill=factor(topic))) +
  geom_col() + facet_wrap(~topic, scales="free", nrow=1)
plot_base + coord_flip()


xx <- beta_topics %>% group_by(topic) %>% top_n(50, beta) %>%
  arrange(topic) %>% ungroup %>% mutate(term=reorder(term,beta)) 
t_list <- list()
for (j in 1:7) {
  t_list[[j]] <- as.data.frame(xx[xx$topic==j,])
}
t_list[[1]][order(t_list[[1]]$beta,decreasing=T),]


# ---------------------------------------------------------------------------- #

gamma_doc <- tidy(q_lda, matrix="gamma")
gamma_doc %>% arrange(document)

gamma_matrice <- vettore_partito
for (j in 1:max(gamma_doc$topic)) {
  gamma_matrice <- cbind(gamma_matrice, gamma_doc[gamma_doc$topic==j,"gamma"])
}
colnames(gamma_matrice) <-  c("partito", "t1", "t2", "t3", "t4", "t5")
dim(gamma_matrice) # 34632     6

par(mfrow=c(2,2))
hist(gamma_matrice[gamma_matrice$partito=="M5S","t1"])
hist(gamma_matrice[gamma_matrice$partito=="IV","t1"])
hist(gamma_matrice[gamma_matrice$partito=="M5S","t1"])
hist(gamma_matrice[gamma_matrice$partito=="M5S","t1"])

ggplot(gamma_matrice[,2]) +
  geom_density(aes(gamma, fill=factor(topic))) +
  facet_wrap(~topic, ncol=3)


ggplot(gamma_doc) +
  geom_density(aes(gamma, fill=factor(topic))) +
  facet_wrap(~topic, ncol=3)
# oss: le 3 distr. sono centrate in 1/3; ciò significa che in media i testi
#      contengono in proporzione piu' o meno uguale tutti e tre i topic

# ---------------------------------------------------------------------------- #