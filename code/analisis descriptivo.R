trace(utils:::unpackPkgZip, edit=TRUE)
# casa
directorio <- 'C:\\Users\\CamiloAndrés\\Desktop\\Colombia-Pact-Text'
# DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'


# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))


# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse'
)
lapply(paquetes, require, character.only = TRUE)

#################################
# Preprocesamiento Excel portal #
#################################

base <- read_excel(paste0(directorio,'/data/propuestas.xlsx'))
base <- base[,-1]
base <- base %>% filter(Título == '¿Cómo podrían las entidades públicas innovar para mejorar procesos y servicios?')

# Stop words
sw <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_spanish.txt"))

# stop words acidionales
sw_a <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_adicionales.txt"))

# todas las stop words
all_stopwords <- unique(c(sw,sw_a))

base_clean <- as.data.frame(sapply(base, function(x) preproctext(x)))
base_clean %<>% mutate_if(is.factor,as.character)

# Remover todas las stopwords
base_clean <- as.data.frame(sapply(base_clean, function(x) RemoveStopwordsFromText(x,all_stopwords)))
base_clean %<>% mutate_if(is.factor,as.character)

# Texto analizar - descripcion
descripcion <- base_clean$Descripción

# cargar texto a lematizar
adicional <- read.table(paste0(directorio,"\\data\\entrada\\lematizacion-adicional.txt"),sep ='\t',header = F,encoding = 'UTF-8',colClasses = 'character')
adicional <- separate(data = adicional,col = 'V1',into = c('V1','V2'))

# Lematización
for (i in 1:nrow(adicional)) {
  descripcion <- gsub(adicional[i,'V1'],adicional[i,'V2'],descripcion)
}

lista_palabras <- unlist(strsplit(descripcion,split = ' ',fixed = T))
lista_palabras <- sort(table(lista_palabras),decreasing = T)

test <- as.data.frame(lista_palabras)
test <- test[which(test$lista_palabras!='co'),]
test <- test[which(test$lista_palabras!='gov'),]
test <- test[which(test$lista_palabras!='anos'),]
test <- test[which(test$lista_palabras!='ano'),]
test2 <- test[1:20,]


#################
# Basic barplot #
#################
theme_update(plot.title = element_text(hjust = 0.5))
p<-ggplot(data=test2, aes(x=test2$lista_palabras, y=test2$Freq)) +
  geom_bar(stat="identity",fill='steelblue') +
  ggtitle('Palabras más comunes en respuestas sobre innovación') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab('Palabras') + ylab('Frecuencia')
p


#############
# Wordcloud #
#############

set.seed(1234)
x11()
wordcloud(words = test$lista_palabras, freq = test$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

####################
# Circular packing #
####################

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data=test2

# calculate the ANGLE of the labels
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$lista_palabras-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Make the plot
p = ggplot(test2, aes(x=as.factor(test2$lista_palabras), y=test2$Freq)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)+
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=test2$Freq+10, label=lista_palabras, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )
p


#########################
# Análisis con bigramas #
#########################

myDfm <- tokens(descripcion) %>%
  tokens_ngrams(n = 3) %>%
  dfm()

bigramas <- topfeatures(myDfm,n = 30)






#######################################
# Respuestas que contienen solo salud #
#######################################

salud <- base_clean[grepl("salud", base$Descripción),]
