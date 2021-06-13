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

# ---------------------------------------------------------------------------- #

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

# Ottengo colori utilizzati da ggplot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_colors <- gg_color_hue(6)
plot(1:5, pch=16, cex=2, col=gg_colors)

# ---------------------------------------------------------------------------- #


# Traccio grafico con le distr. dei topic per un fissato partito
gg_colors <- gg_color_hue(5)
for (p in partito_list) {
  post_tmp <- gamma[gamma$partito==p,-1]
  # Costruisco grafico
  p_tmp <- ggplot(post_tmp, aes(x=t1)) + ylim(0, 20) +
    stat_density(geom="line",position="identity", aes(x=t1), color=gg_colors[1], lwd=0.8, data=post_tmp) +
    stat_density(geom="line",position="identity", aes(x=t2), color=gg_colors[2], lwd=0.8, data=post_tmp) +
    stat_density(geom="line",position="identity", aes(x=t3), color=gg_colors[3], lwd=0.8, data=post_tmp) +
    stat_density(geom="line",position="identity", aes(x=t4), color=gg_colors[4], lwd=0.8, data=post_tmp) +
    stat_density(geom="line",position="identity", aes(x=t5), color=gg_colors[5], lwd=0.8, data=post_tmp) +
    geom_vline(xintercept=mean(post_tmp[,1]), linetype="dashed", color=gg_colors[1], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,2]), linetype="dashed", color=gg_colors[2], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,3]), linetype="dashed", color=gg_colors[3], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,4]), linetype="dashed", color=gg_colors[4], size=0.8) +
    geom_vline(xintercept=mean(post_tmp[,5]), linetype="dashed", color=gg_colors[5], size=0.8) +
    xlab(p) + ylab("Densità") + theme(legend.position="none")
  # Creo tabella contenente valore atteso e varianza per ogni topic
  table_tmp <- cbind(1:5,
                     format(round(apply(post_tmp, 2, mean), 4),nsmall=2),
                     format(round(apply(post_tmp, 2, sd), 4),nsmall=2))
  table_tmp <- rbind(c("topic", "mean", "sd"), table_tmp)
  # Aggiungo la tabella al grafico come legenda
  require(gridExtra); require(grid); require(gtable)
  grob_t1 <- ttheme_minimal(core=list(bg_params=list(fill=c("white",gg_colors[1:5]),col=NA)))
  table_tmp_grob <- tableGrob(table_tmp, theme=grob_t1, rows=NULL)
  table_tmp_grob <- gtable_add_grob(table_tmp_grob,
                       grobs=rectGrob(gp=gpar(fill=NA, lwd=1)),
                       t=1, b=nrow(table_tmp_grob), l=1, r=ncol(table_tmp_grob))
  # Unisco grafico e tabella
  pp_tmp <- arrangeGrob(p_tmp, table_tmp_grob, nrow=1, ncol=2, widths=c(4,1))
  # Salvo grafico
  ggsave(pp_tmp, filename=paste("lda_density_",p,".png",sep=""), device="png")
}

# ---------------------------------------------------------------------------- #

# Traccio grafico con le distr. dei partiti per un fissato topic
gg_colors <- gg_color_hue(6)
t_dict <- list("t1"="Topic 1","t2"="Topic 2","t3"="Topic 3","t4"="Topic 4","t5"="Topic 5")
for (t in c("t1","t2","t3","t4","t5")) {
  # Creo tabella contenente valore atteso e varianza per ogni topic
  means_tmp <- c(mean(gamma[gamma$partito==partito_list[1],t]),
                 mean(gamma[gamma$partito==partito_list[2],t]),
                 mean(gamma[gamma$partito==partito_list[3],t]),
                 mean(gamma[gamma$partito==partito_list[4],t]),
                 mean(gamma[gamma$partito==partito_list[5],t]),
                 mean(gamma[gamma$partito==partito_list[6],t]))
  sd_tmp <- c(sd(gamma[gamma$partito==partito_list[1],t]),
              sd(gamma[gamma$partito==partito_list[2],t]),
              sd(gamma[gamma$partito==partito_list[3],t]),
              sd(gamma[gamma$partito==partito_list[4],t]),
              sd(gamma[gamma$partito==partito_list[5],t]),
              sd(gamma[gamma$partito==partito_list[6],t]))
  table_tmp <- cbind(partito_list,
                     format(round(means_tmp, 4),nsmall=4),
                     format(round(sd_tmp, 4),nsmall=4))
  table_tmp <- rbind(c("partito", "mean", "sd"), table_tmp)
  colnames(table_tmp) <- NULL
  # Costruisco grafico
  p_tmp <- ggplot(gamma, aes(x=!!ensym(t))) + ylim(0, 20) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color="#000000", lwd=1.0, data=gamma) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color=gg_colors[1], lwd=0.8, data=gamma[gamma$partito==partito_list[1],-1]) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color=gg_colors[2], lwd=0.8, data=gamma[gamma$partito==partito_list[2],-1]) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color=gg_colors[3], lwd=0.8, data=gamma[gamma$partito==partito_list[3],-1]) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color=gg_colors[4], lwd=0.8, data=gamma[gamma$partito==partito_list[4],-1]) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color=gg_colors[5], lwd=0.8, data=gamma[gamma$partito==partito_list[5],-1]) +
    stat_density(geom="line",position="identity", aes(x=!!ensym(t)), color=gg_colors[6], lwd=0.8, data=gamma[gamma$partito==partito_list[6],-1]) +
    geom_vline(xintercept=mean(gamma[,t]), linetype="dashed", color="black", size=1.0) +
    geom_vline(xintercept=means_tmp[1], linetype="dashed", color=gg_colors[1], size=0.8) +
    geom_vline(xintercept=means_tmp[2], linetype="dashed", color=gg_colors[2], size=0.8) +
    geom_vline(xintercept=means_tmp[3], linetype="dashed", color=gg_colors[3], size=0.8) +
    geom_vline(xintercept=means_tmp[4], linetype="dashed", color=gg_colors[4], size=0.8) +
    geom_vline(xintercept=means_tmp[5], linetype="dashed", color=gg_colors[5], size=0.8) +
    geom_vline(xintercept=means_tmp[6], linetype="dashed", color=gg_colors[6], size=0.8) +
    xlab(t_dict[[t]]) + ylab("Densità") + theme(legend.position="none")
  # Aggiungo la tabella al grafico come legenda
  require(gridExtra); require(grid); require(gtable)
  grob_t1 <- ttheme_minimal(core=list(bg_params=list(fill=c("white",gg_colors[1:6]),col=NA)))
  table_tmp_grob <- tableGrob(table_tmp, theme=grob_t1, rows=NULL)
  table_tmp_grob <- gtable_add_grob(table_tmp_grob,
                                    grobs=rectGrob(gp=gpar(fill=NA, lwd=1)),
                                    t=1, b=nrow(table_tmp_grob), l=1, r=ncol(table_tmp_grob))
  # Unisco grafico e tabella
  pp_tmp <- arrangeGrob(p_tmp, table_tmp_grob, nrow=1, ncol=2, widths=c(4,1))
  # Salvo grafico
  ggsave(pp_tmp, filename=paste("lda_density_",t,".png",sep=""), device="png")
}

# ---------------------------------------------------------------------------- #
