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

# libraries
library(packcircles)
library(ggplot2)
library(viridis)
# Create data

data=test2

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$Freq, sizetype='area')

# We can add these packing information to the initial data frame
data = cbind(data, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
plot(data$radius, data$Freq)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=20)

# Make the plot
x11()
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  scale_fill_manual(values = viridis(nrow(data))) + 
  geom_text(data = data, aes(x, y, size=5, label = lista_palabras)) +
  #scale_color_gradient(colours=rainbow(4))+
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


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

myDfm <- tokens(descripcion) %>%
  tokens_ngrams(n = 4) %>%
  dfm()

bigramas <- topfeatures(myDfm,n = 30)
bigramas <- as.data.frame(bigramas)
nombres_row <- rownames(bigramas)
rownames(bigramas) <- NULL
bigramas <- data.frame(bigrama=nombres_row,freq=bigramas$bigramas)
bigramas$bigrama <- gsub("_"," ",bigramas$bigrama)
