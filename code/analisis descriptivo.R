# para instalar paquetes modificar linea 142 sys.sleep(5.0)
trace(utils:::unpackPkgZip, edit=TRUE)
# Directorio casa
directorio <- 'C:\\Users\\CamiloAndrés\\Desktop\\Colombia-Pact-Text'
# Directorio DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'

# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))

# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','packcircles',
              'viridis','igraph','ggraph'
)
lapply(paquetes, require, character.only = TRUE)

#----------------------------------------------------------------------------------------------------------------------
# Preprocesamiento Excel portal
#----------------------------------------------------------------------------------------------------------------------

# Cargo respuestas de las personas
base <- read_excel(paste0(directorio,'/data/propuestas.xlsx'))
base <- base[,-1] # Eliminar columna sin nombre
# Dejar respuestas solo de innovación
base <- base %>% filter(Título == '¿Cómo podrían las entidades públicas innovar para mejorar procesos y servicios?')

# Cargo archivo con stop words
sw <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_spanish.txt"),warn = F)

# Stop words adicionales que surgen después de una primera iteración
sw_a <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_adicionales.txt"))

# Todas las stop words en un objeto
all_stopwords <- unique(c(sw,sw_a))

# Preprocesamiento del texto
base_clean <- as.data.frame(sapply(base, function(x) preproctext(x)))
base_clean %<>% mutate_if(is.factor,as.character)

# Remover todas las stopwords
base_clean <- as.data.frame(sapply(base_clean, function(x) RemoveStopwordsFromText(x,all_stopwords)))
base_clean %<>% mutate_if(is.factor,as.character)

# Texto para analizar - "Descripción" de la base de datos
descripcion <- base_clean$Descripción
# Objeto sin lematizar
save(descripcion, file=paste0(directorio,"\\data\\desc-sin-lematizar.RData"))

# Cargar texto a "lematizar"
adicional <- read.table(paste0(directorio,"\\data\\entrada\\lematizacion-adicional.txt"),sep ='\t',header = F,encoding = 'UTF-8',colClasses = 'character')
adicional <- separate(data = adicional,col = 'V1',into = c('V1','V2')) # Separar en dos columnas

# "Lematización" - Buscar primera palabra en el archivo y reemplazarla por la versión corregida
for (i in 1:nrow(adicional)) {
  descripcion <- gsub(adicional[i,'V1'],adicional[i,'V2'],descripcion)
}
save(descripcion, file=paste0(directorio,"\\data\\desc-lematizada.RData"))
# Obtener cada caracter del texto
lista_palabras <- unlist(strsplit(descripcion,split = ' ',fixed = T))
# Ordenar en orden decreciente
lista_palabras <- sort(table(lista_palabras),decreasing = T)

# Eliminar palabras adicionales tras una segunda iteración
test <- as.data.frame(lista_palabras)
test <- test[which(test$lista_palabras!='co'),]
test <- test[which(test$lista_palabras!='gov'),]
test <- test[which(test$lista_palabras!='anos'),]
test <- test[which(test$lista_palabras!='ano'),]
# Primeras 20 palabras para realizar barplot
test2 <- test[1:20,]

#----------------------------------------------------------------------------------------------------------------------
# Barplot básico
#----------------------------------------------------------------------------------------------------------------------

theme_update(plot.title = element_text(hjust = 0.5))
p<-ggplot(data=test2, aes(x=test2$lista_palabras, y=test2$Freq)) +
  geom_bar(stat="identity",fill='steelblue') +
  ggtitle('Palabras más comunes en respuestas sobre innovación') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab('Palabras') + ylab('Frecuencia')
p

#----------------------------------------------------------------------------------------------------------------------
# Wordcloud
#----------------------------------------------------------------------------------------------------------------------

# Dibujar usando paquete wordcloud
set.seed(1234)
x11()
wordcloud(words = test$lista_palabras, freq = test$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#----------------------------------------------------------------------------------------------------------------------
# Circular packing
#----------------------------------------------------------------------------------------------------------------------

# Datos
data=test2

# Generar diseño, se retorna un dataframe con una línea por burbuja
# Retorna el centro (x e y) y su radio, proprocional al valor
packing <- circleProgressiveLayout(data$Freq, sizetype='area')

# Añadir packing como columna al data frame original
data = cbind(data, packing)

# Comprobar que el radio sea proporcional al valor, no se quiere una relación lineal
plot(data$radius, data$Freq)

# Ir de un centro + un radio a las coordenadas de la burbuja que es dibujado por un montón de líneas rectas
dat.gg <- circleLayoutVertices(packing, npoints=10000) # Creo que entre más npoints más resolución tiene la burbuja

# Dibujar con ggplot
x11()
ggplot() + 
  
  # Construir burbujas
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Añadir texto al centro de cada burbuja y controlar su tamaño
  scale_fill_manual(values = viridis(nrow(data))) + 
  geom_text(data = data, aes(x, y, size=5, label = lista_palabras)) +
  #scale_color_gradient(colours=rainbow(4))+
  scale_size_continuous(range = c(1,4)) +
  
  # Tema general
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


#----------------------------------------------------------------------------------------------------------------------
# Análisis con nmgramas sobre el texto en general
#----------------------------------------------------------------------------------------------------------------------

myDfm <- tokens(descripcion) %>%
  tokens_ngrams(n = 3) %>%
  dfm()

bigramas <- topfeatures(myDfm,n = 30)



#----------------------------------------------------------------------------------------------------------------------
# Respuestas que contienen solo salud + análisis de ngramas
#----------------------------------------------------------------------------------------------------------------------

salud <- base_clean[grepl("salud", base$Descripción),]


myDfm <- tokens(salud$Descripción) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

bigramas <- topfeatures(myDfm,n = 60)
bigramas <- as.data.frame(bigramas)
nombres_row <- rownames(bigramas)
rownames(bigramas) <- NULL
bigramas <- data.frame(bigrama=nombres_row,freq=bigramas$bigramas)
bigramas$bigrama <- gsub("_"," ",bigramas$bigrama)


