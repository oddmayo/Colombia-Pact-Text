# para instalar paquetes modificar linea 142 sys.sleep(5.0)
trace(utils:::unpackPkgZip, edit=TRUE)
# Directorio casa
directorio <- 'C:\\Users\\CamiloAndrés\\Desktop\\Colombia-Pact-Text'
# Directorio DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'

# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))

# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','packcircles',
              'viridis','igraph','ggraph'
)
lapply(paquetes, require, character.only = TRUE)

#----------------------------------------------------------------------------------------------------------------------
# Intento de word network
#----------------------------------------------------------------------------------------------------------------------


load(paste0(directorio,"\\data\\base_clean_innovacion.RData"))
#base_clean_tibble <- as.tibble(base_clean)
base_clean2 <- data.frame(username=base_clean$Username,descripcion=base_clean$Descripción)

#----------------------------------------------------------------------------------------------------------------------
# Análisis de toda la base para la pregunta de innovación
#----------------------------------------------------------------------------------------------------------------------
cosas_raras <- c('gov','co','www','magia','negra','com')
base_clean2 %<>% mutate_if(is.factor,as.character)
base_clean2 <- as.data.frame(sapply(base_clean2, function(x) RemoveStopwordsFromText(x,cosas_raras)))

library(qdap)
root <- c('publica','innovacion','medicos','tramites','creacion','tecnologia','pacifico','atencion','informacion','politica','linea','enajenacion','publico','dialogos','peru','mexico','publicos','moviles','investigacion','medica')
objective <- c('pública','innovación','médicos','trámites','creación','tecnología','pacífico','atención','información','política','línea','enajenación','público','diálogos','perú','méxico','públicos','móviles','investigación','médica')
base_clean2$descripcion <- mgsub(pattern = root,replacement = objective,text.var = as.character(base_clean2$descripcion))


# Totalidad de los bigramas
bigramas <- base_clean2 %>%
  unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)

# Frecuencia de bigramas
bigramas %>%
  count(bigram, sort = TRUE)

# Separar cada palabra de los bigramas en columnas
bigramas_separados <- bigramas %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Columna con frecuencia del bigrama
bigramas_conteo <- bigramas_separados %>%
  count(word1, word2, sort = TRUE)

# Unir de nuevo
bigramas_unidos <- bigramas_separados %>%
  unite(bigram, word1, word2, sep = " ")

# tfidf de los bigramas
bigrama_tf_idf <- bigramas_unidos %>%
  count(username, bigram) %>%
  bind_tf_idf(bigram, username, n) %>%
  arrange(desc(tf_idf))


bigrama_tf_idf


bigram_graph <- bigramas_conteo %>%
  filter(n > 2) %>%
  graph_from_data_frame()

bigram_graph

# Gráfico básico
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
# FUNCIONAAAAAA

# Gráfico con color y cadena de Markov
set.seed(11234)
a <- grid::arrow(type = 'closed', length = unit(.15, "inches"))
x11()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a,linemitre = 8, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "firebrick3", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('Red de bigramas más utilizados en respuestas sobre innovación') +
  #scale_x_continuous(limits = c(0, 10)) +
  #coord_cartesian(xlim = c(0, 20)) +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5))

#----------------------------------------------------------------------------------------------------------------------
# Análisis de solo salud
#----------------------------------------------------------------------------------------------------------------------

salud <- base_clean2[grepl("salud", base_clean2$descripcion),]
#base_clean2 <- data.frame(username=salud$Username,descripcion=salud$Descripción)

# Totalidad de los bigramas
bigramas <- salud %>%
  unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)

# Frecuencia de bigramas
bigramas %>%
  count(bigram, sort = TRUE)

# Separar cada palabra de los bigramas en columnas
bigramas_separados <- bigramas %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Columna con frecuencia del bigrama
bigramas_conteo <- bigramas_separados %>%
  count(word1, word2, sort = TRUE)

# Unir de nuevo
bigramas_unidos <- bigramas_separados %>%
  unite(bigram, word1, word2, sep = " ")

# tfidf de los bigramas
bigrama_tf_idf <- bigramas_unidos %>%
  count(username, bigram) %>%
  bind_tf_idf(bigram, username, n) %>%
  arrange(desc(tf_idf))


bigrama_tf_idf


bigram_graph <- bigramas_conteo %>%
  filter(n > 1) %>%
  graph_from_data_frame()

bigram_graph

# Gráfico básico
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
# FUNCIONAAAAAA

# Gráfico con color y cadena de Markov
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "firebrick3", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('Red de bigramas más utilizados en respuestas que hablan sobre salud') +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5))



# Innovation in the whole database
load(paste0(directorio,"\\data\\base_clean_todo.RData"))

para_filtrar <- c('innovacion | innova | innovador | innovadora | innovando | innovar |
                  innovativo')

innovacion_todo <- base_clean[grepl(para_filtrar, base_clean$Descripción),]


