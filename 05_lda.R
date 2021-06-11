# ---------------------------------------------------------------------------- #

# LDA

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

tt <- tw %>% select(tweet=tweet_stem, partito=partito, username = username) %>% mutate(id = 1:n())
tt2 <- tt %>% unnest_tokens(word,tweet) %>% group_by(id,word) %>% mutate(freq=n())
X <- tt2 %>% cast_sparse(row=id, column=word, value=freq)

# Creo il vettore partito
vettore_partito <- tt$partito[as.numeric(rownames(X))]
dim(X)[1]==length(vettore_partito)
table(vettore_partito)
#  FdI   FI   IV  LSP  M5S   PD 
# 4571 6151 7667 8471 3601 4180 

# Filtro le colonne, o meglio rimuovo gli hapax
nomi <- colnames(X)
del <- which(Matrix::colSums(X)<=1)
X <- X[, -del]
colnames(X) <- nomi[-del]

# Filtro X e vettore_partito rimuovendo i tweet senza stem
vettore_partito <- vettore_partito[Matrix::rowSums(X)>0]
X <- X[Matrix::rowSums(X)>0,]
dim(X)[1]==length(vettore_partito)
table(vettore_partito)
#  FdI   FI   IV  LSP  M5S   PD 
# 4566 6151 7667 8470 3601 4177 
dim(X)  # 34632 13307

# ---------------------------------------------------------------------------- #

# Ora posso stimare la LDA con 7 topic (e' la migliore tra 5, 6, 7 e 8)
q_lda <- LDA(as.matrix(X), k=5, method="Gibbs", control=list(seed=123,
                                                             burnin=1000,
                                                             iter=5000))
beta_topics <- tidy(q_lda, matrix="beta")
dim(beta_topics) # 66535 x 3     nota: 66535 = k*V = 5*13307

# Per ogni topic selezione i 10 termini con peso maggiore, ovvero scelgo i 
# termini a cui corrispondono i valori piu' alti dei parametri beta
termini <- beta_topics %>% group_by(topic) %>% top_n(20, beta) %>% arrange(topic)
# Sostituisco le stringhe degli emote con la vera emote
term_pl <- termini %>% ungroup %>% mutate(term=reorder(term,beta))
term_pl$term <- as.character(term_pl$term)
term_pl$term[term_pl$term=="emote_backhand_index_pointing_right"] <- "👉"
term_pl$term[term_pl$term=="emote_italy"] <- "🇮🇹"
term_pl$term[term_pl$term=="emote_red_circle"] <- "🔴"
term_pl$term <- as.factor(term_pl$term)
term_pl <- term_pl %>% mutate(term=reorder(term,beta))

# Rapppresento graficamente questi termini
plot_base <- ggplot(term_pl, aes(term, beta, fill=factor(topic))) +
  geom_col() + facet_wrap(~topic, scales="free", nrow=1)
plot_base + theme(legend.position="none") + coord_flip()


# Per ogni topic ottengo i 50 stem piu' importanti e metto tutto in tabella
top_50_words <- beta_topics %>% group_by(topic) %>% top_n(50, beta) %>%
  arrange(topic) %>% ungroup %>% mutate(term=reorder(term,beta)) 
t_table <- c()
for (j in 1:5) {
  tmp_list <- as.data.frame(top_50_words[top_50_words$topic==j,])
  tmp_list$term <- as.character(tmp_list$term)
  t_table <- cbind(t_table, tmp_list[order(tmp_list$beta,decreasing=T),2])
}
t_table


# ---------------------------------------------------------------------------- #

# Costruisco la matrice gamma a partire da gamma_doc
gamma_doc <- tidy(q_lda, matrix="gamma")
gamma <- vettore_partito
for (j in 1:max(gamma_doc$topic)) {
  gamma <- cbind(gamma, gamma_doc[gamma_doc$topic==j,"gamma"])
}
colnames(gamma) <-  c("partito", "t1", "t2", "t3", "t4", "t5")
dim(gamma) # 34641 x 6  (n x k+1)

# ---------------------------------------------------------------------------- #

# Grafici delle distr. a posteriori condizionate al partito di appartenenza
# dei tweet
par(mfrow=c(1,2))
partito_list <- unique(gamma$partito)
for (k in 2:3) {
  post_tmp <- gamma[gamma$partito==partito_list[1],k]
  post_tmp_dens <- density(post_tmp)
  postmode <- post_tmp_dens$x[which.max(post_tmp_dens$y)]
  # plot
  plot(density(post_tmp), col=1, lwd=2, main=paste("Topic",k-1),
       ylim=c(0,25))
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

# Ottengo valore atteso e varianza per ogni topic fissato il partito
partito_list <- unique(gamma$partito)
partito_tables <- list()
for (p in partito_list) {
  post_tmp <- gamma[gamma$partito==p,-1]
  partito_tables[[p]] <- cbind(paste("Topic", 1:5),
                               round(apply(post_tmp, 2, mean), 4),
                               round(apply(post_tmp, 2, sd), 4))
}
# Ottengo colori utilizzati da ggplot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_colors <- gg_color_hue(5)
plot(1:5, pch=16, cex=2, col=gg_colors)

# Traccio grafico con le distr. dei topic per un fissato partito
for (p in partito_list[1]) {
  post_tmp <- gamma[gamma$partito==p,-1]
  # Costruisco grafico
  p_tmp <- ggplot(post_tmp, aes(x=t1)) +
    geom_density(aes(x=t1, color=gg_colors[1]), lwd=0.8, data=post_tmp) +
    geom_density(aes(x=t2, color=gg_colors[2]), lwd=0.8, data=post_tmp) +
    geom_density(aes(x=t3, color=gg_colors[3]), lwd=0.8, data=post_tmp) +
    geom_density(aes(x=t4, color=gg_colors[4]), lwd=0.8, data=post_tmp) +
    geom_density(aes(x=t5, color=gg_colors[5]), lwd=0.8, data=post_tmp) +
    geom_vline(xintercept=mean(post_tmp[,1]), linetype="dashed", color=gg_colors[1], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,2]), linetype="dashed", color=gg_colors[2], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,3]), linetype="dashed", color=gg_colors[3], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,4]), linetype="dashed", color=gg_colors[4], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,5]), linetype="dashed", color=gg_colors[5], size=0.8) +
    xlab(p) + ylab("Densità")
  # Creo tabella contenente valore atteso e varianza per ogni topic
  table_tmp <- cbind(paste("Topic", 1:5),
                     round(apply(post_tmp, 2, mean), 4),
                     round(apply(post_tmp, 2, sd), 4))
  # Aggiungo la tabella al grafico come legenda
  require(gridExtra)
  table_tmp_grob <- tableGrob(table_tmp, gpar.coretext=gpar(fontsize=8),
                              par.coltext=gpar(fontsize=8), 
                              gpar.rowtext=gpar(fontsize=8))
  pp <- arrangeGrob(p + theme(legend.position = "none"), 
                    arrangeGrob(table_tmp_grob, legend), ncol=2)
  # Salvo grafico
  ggsave(p5, filename="lda_density_legend.png", device="png")
}


  
  
p2 <- ggplot(gamma, aes(x=t2)) +
  geom_density(aes(x=t2, color="Media"), colour ="black", lwd=0.8, data=gamma) +
  geom_density(aes(x=t2, group=partito, color=partito), lwd=0.6) +
  xlab("Topic 2") + ylab("Densità") + theme(legend.position="none")









par(mfrow=c(2,1))
post_tmp <- gamma[gamma$partito=="M5S",-1]
head(post_tmp)
# plot
plot(density(post_tmp$t1), col=1, lwd=2, main="M5S",
     ylim=c(0,25), xlim=c(0.1,0.40))
abline(v=mean(post_tmp[,1]), col=1, lty=2)
cat(mean(post_tmp[,1]),"\n")
for (p in 2:5) {
  lines(density(post_tmp[,p]), col=p, lwd=2)
  abline(v=mean(post_tmp[,p]), col=p, lty=2)
}
legend("topright", legend=paste("Topic",1:5), fill=1:5, horiz=F, cex=1)
for (p in 1:5) {
  cat(mean(post_tmp[,p]),"   ", sd(post_tmp[,p]), "\n")
}




# ---------------------------------------------------------------------------- #



p2 <- ggplot(gamma, aes(x=t2)) +
  geom_density(aes(x=t2, color="Media"), colour ="black", lwd=0.8, data=gamma) +
  geom_density(aes(x=t2, group=partito, color=partito), lwd=0.6) +
  xlab("Topic 2") + ylab("Densità") + theme(legend.position="none")
ggsave(p2, filename="lda_density_2.png", device="png")

p3 <- ggplot(gamma, aes(x=t3)) +
  geom_density(aes(x=t3, color="Media"), colour ="black", lwd=0.8, data=gamma) +
  geom_density(aes(x=t3, group=partito, color=partito), lwd=0.6) +
  xlab("Topic 3") + ylab("Densità") + theme(legend.position="none")
ggsave(p3, filename="lda_density_3.png", device="png")

p4 <- ggplot(gamma, aes(x=t4)) +
  geom_density(aes(x=t4, color="Media"), colour ="black", lwd=0.8, data=gamma) +
  geom_density(aes(x=t4, group=partito, color=partito), lwd=0.6) +
  xlab("Topic 4") + ylab("Densità") + theme(legend.position="none")
ggsave(p4, filename="lda_density_4.png", device="png")

p5 <- ggplot(gamma, aes(x=t5)) +
  geom_density(aes(x=t5, fill=partito, group=partito), lwd=0.6) +
  xlab("Topic 5") + ylab("Densità") +
  theme(legend.position="left",
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30)) +
  labs(fill = "Partito")

p5
ggsave(p5, filename="lda_density_legend.png", device="png")


# ---------------------------------------------------------------------------- #
