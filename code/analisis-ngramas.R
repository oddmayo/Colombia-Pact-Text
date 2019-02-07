#----------------------------------------------------------------------------------------------------------------------
# Intento de word network
#----------------------------------------------------------------------------------------------------------------------

bigram_graph <- bigramas %>%
  separate(bigrama, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  graph_from_data_frame()


set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
x11()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#----------------------------------------------------------------------------------------------------------------------


library(janeaustenr)
base_clean_tibble <- as.tibble(base_clean)

base_clean2 <- data.frame(username=base_clean$Username,descripcion=base_clean$Descripción)
austen_bigrams <- base_clean2 %>%
  unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)

austen_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_counts <- bigrams_separated %>%
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(username, bigram) %>%
  bind_tf_idf(bigram, username, n) %>%
  arrange(desc(tf_idf))


bigram_tf_idf


bigram_graph <- bigram_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
# FUNCIONAAAAAA


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#-----------------------------------------------------------------------

bigram_igraph<- bigram_df$bigram%>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
set.seed(123)
ggraph(bigram_igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = bigramas$freq), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void() +
  ggtitle("Red de bigramas en respuestas que hablan sobre salud")


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)