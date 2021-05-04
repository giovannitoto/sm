# ---------------------------------------------------------------------------- #

# Ottengo lista di hashtag utilizzati

# con lapply (*) si considera anche il numero di hashtag all'interno del
# tweet (#salvini come unico hashtag e #salvini con anche altri hashtag sono
# considerati diversi)

hashtag_to_fix <- unique(tweets$hashtags)
hashtag_list <- c()
for (h in hashtag_to_fix) {
  if (h=="[]") { next }
  h_tmp <- gsub("\\[|\\]|'", "", h)
  if ( grepl(",", h_tmp) ) {
    h_tmp <- strsplit(h_tmp, ",")
    # (*)
    #h_tmp <- lapply(h_tmp, function(x) paste(x,length(h_tmp[[1]]),sep="_"))
  }
  h_tmp <- lapply(h_tmp, trimws)[[1]]
  hashtag_list <- c(hashtag_list, h_tmp)
}
sort(table(hashtag_list), decreasing=T)[1:40]


sum(grepl("coronavirus|COVID-19|pandemia", tweets$tweet))


# ---------------------------------------------------------------------------- #

short_tweet <- tweets[nchar(tweets$tweet)<100,"tweet"]
#View(short_tweet)

# distribuzione della lunghezza dei tweet
hist(nchar(tweets$tweet), nclass=50, col=3)
abline(v=summary(nchar(tweets$tweet))[-4], col=1, lwd=2, lty=2)
abline(v=summary(nchar(tweets$tweet))[4], col=1, lwd=2)

# ----------------------------------------------------------------------------- #

library(forcats)
library(ggplot2)
library(dplyr)
nomi <- unique(tweets$username)
ntweets <- sapply(nomi, function(x) length(tweets$tweet[tweets$username == x]))
data <- as.data.frame(sort(ntweets, decreasing = T)[1:10])
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

ntweets <- sapply(unique(tweets$partito), function(x) length(tweets$tweet[tweets$partito == x]))
data <- as.data.frame(sort(ntweets, decreasing = T))
data$nomi <- rownames(data)
colnames(data) <- c("ntweets", "nomi")

data %>%
  mutate(name = fct_reorder(nomi, desc(ntweets))) %>%
  ggplot( aes(x = name, y = ntweets)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# ----------------------------------------------------------------------------- #

