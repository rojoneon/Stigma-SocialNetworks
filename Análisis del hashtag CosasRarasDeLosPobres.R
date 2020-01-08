######################################################################
#Análisis de la tendencia #CosasRarasDeLosPobres
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################


##############
#Configuración
rm(list = ls())

library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,gmodels,foreign,expss,fishualize, viridis,
       rtweet, wordcloud, igraph, tm, widyr, tidytext, rcorpora, purrr, webshot,wordcloud2, quanteda)

#No sé para qué es esto jeje salu2
knitr::opts_chunk$set(message=FALSE, warning=FALSE)

#Obtención de tweets
tweets_estigma <- rtweet::search_tweets(
  q = "CosasRarasDeLosPobres", 
  n=18000,
  include_rts = TRUE) 

#Gráfica de intensidad de tweets
tweets_estigma %>%
  ts_plot("hours") +
ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

############
#Wordcloud de usuarios más activos con el hashtag
############
users <- toString(tweets_estigma$screen_name) %>%
  str_split(pattern = " ", simplify = TRUE)
set.seed(16995145)
wordcloud(users, colors = viridis::viridis_pal(end = 0.8)(10),
          min.freq = 1000, random.color = TRUE, max.words = 100,
          scale = c(3,.5), rot.per=.3)


############
#Wordcloud de tweets
############

#Look at tweets
head(tweets_estigma)
dim(tweets_estigma)
tweets_estigma$text

#Unnest the words - code via Tidy Text
tweets_estigma_Table <- tweets_estigma %>% 
  unnest_tokens(word, text)
tweets_estigma_Table$word

#Selecciono las palabras comúnes como "el", "es", "la" para que no las tome en cuenta:
data(stop_words)
stop_words <- c(stop_words,corpora("words/stopwords/en")$stopWords)
stop_words <- c(stop_words, corpora("words/stopwords/es")$stopWords, "t.co", "https", "ésto", "te", "ht", "tu", "fueran")
stop_words <- tibble(stop_words)
stop_words$stop_words<- as.character(stop_words$stop_words)

#remove stop words - aka typically very common words such as "the", "of" etc
tweets_estigma_Table <- tweets_estigma_Table %>%
  anti_join(stop_words, by = c("word" = "stop_words"))

#do a word count
tweets_estigma_Table <- tweets_estigma_Table %>%
  count(word, sort = TRUE) 
tweets_estigma_Table

#Remove other nonsense words
tweets_estigma_Table <-tweets_estigma_Table %>%
  filter(!word %in% c('t.co', 'https', "cosasrarasdelospobres", 
                      "fueran", "pobres", "pobreza", "pobre",
                      "tus", "estás", "tuits", "ramirezmmx",
                      "ta9bb30de8"))
tweets_estigma_Table



#Create wordcloud2         
Palette <- c("#50A38C", "#C1375B", "#434371", "#F6E786", "#484041")
set.seed(100)
wordcloud2(tweets_estigma_Table, size=1, 
           color=rep_len( Palette, nrow(tweets_estigma_Table)),
           shape = "circle"
           )

#Create wordcloud2   
set.seed(1234)
wordcloud(words = tweets_estigma_Table$word, freq = tweets_estigma_Table$n, 
          #scale = c(0.1, 1), 
          min.freq = 25, max.words=5000, 
          random.order=FALSE, rot.per=0.35, 
          #colors=brewer.pal(8, "Dark2")
          #color=rep_len( Palette, nrow(tweets_estigma_Table))
          colors = viridis::viridis_pal(end = 0.8, direction=-1)(10)
          )






#DFM
toks_tweets_estigma <- tokens(tweets_estigma$text)
dfmat_tweets_estigma <- dfm(toks_tweets_estigma)
class(dfmat_tweets_estigma)
dfmat_tweets_estigma <- dfm(dfmat_tweets_estigma,
                            remove = stopwords("spanish"),
                            stem = TRUE, remove_punct = TRUE)
dfmat_tweets_estigma <- dfm(dfmat_tweets_estigma,
                            remove = c('t.co', 'https', "cosasrarasdelospobres", 
                                       "fueran", "pobres", "pobreza", "pobre",
                                       "tus", "estás", "/", ",", "'", '"',".", ":", ";"))
                            
                            
set.seed(100)
textplot_wordcloud(dfmat_tweets_estigma, min_freq = 100, random_order = FALSE,
                   min_size = .5, max_size = 8, max_words = 200,
                   rotation = .25,
                   #color=rep_len( Palette, nrow(tweets_estigma_Table))
                   colors = viridis::viridis_pal(end = 0.8, direction=-1)(10)
                   )








#Preparar los datos para armar la red de los mencionados en los Tweets:
mentioned_users <- tweets_estigma %>% 
  mutate(mentions = map_chr(.$mentions_screen_name, paste0, collapse = " ")) %>% 
  select(status_id, mentions, screen_name, reply_to_screen_name)

#Construyo la red de los usuarios que hablaron del # o que fueron referenciados en él
#     y cómo se vincularon entre ellos:
set.seed(3654)
library("ggraph")
data_frame(users = mentioned_users$mentions, status_id = mentioned_users$status_id)  %>% 
  unnest_tokens(user, users) %>% 
  pairwise_count(user, status_id, sort = TRUE, upper = FALSE) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,edge_colour = "red", edge_width = 1,
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "red", size = 5, alpha = .5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines"), vjust = 1, hjust = 1) +
  theme_void()



