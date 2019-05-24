# para instalar paquetes modificar linea 142 sys.sleep(5.0)
trace(utils:::unpackPkgZip, edit=TRUE)

# Directorio DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'

# Funciones preprocesamiento
source(paste0(directorio,'/code/funciones.R'))

# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','packcircles',
              'viridis','igraph','ggraph','wordcloud2'
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
# Objeto para cadenas de markov en script de ngramas
#save(base_clean, file=paste0(directorio,"\\data\\base_clean_innovacion.RData"))
#save(base_clean, file=paste0(directorio,"\\data\\base_clean_todo.RData"))
# Texto para analizar - "Descripción" de la base de datos
descripcion <- base_clean$Descripción
# Objeto sin lematizar
#save(descripcion, file=paste0(directorio,"\\data\\desc-sin-lematizar.RData"))

# Cargar texto a "lematizar"
adicional <- read.table(paste0(directorio,"\\data\\entrada\\lematizacion-adicional.txt"),sep ='\t',header = F,encoding = 'UTF-8',colClasses = 'character')
adicional <- separate(data = adicional,col = 'V1',into = c('V1','V2')) # Separar en dos columnas

# "Lematización" - Buscar primera palabra en el archivo y reemplazarla por la versión corregida
for (i in 1:nrow(adicional)) {
  descripcion <- gsub(adicional[i,'V1'],adicional[i,'V2'],descripcion)
}
#save(descripcion, file=paste0(directorio,"\\data\\desc-lematizada.RData"))

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


#----------------------------------------------------------------------------------------------------------------------
# Barplot básico
#----------------------------------------------------------------------------------------------------------------------
library(qdap)
root <- c('publica','innovacion','medicos','tramites','creacion','tecnologia','pacifico','atencion','informacion','politica','linea')
objective <- c('pública','innovación','médicos','trámites','creación','tecnología','pacífico','atención','información','política','línea')

# Reemplazar palabras
test$lista_palabras <- mgsub(pattern = root,replacement = objective,text.var = as.character(test$lista_palabras))

for(j in seq_along(root)){
  test$lista_palabras <- gsub(root[j], objective[j], as.character(test$lista_palabras))
}

# Primeras 20 palabras para realizar barplot
test2 <- test[1:20,]


p<-ggplot(data=test2, aes(x=reorder(test2$lista_palabras, test2$Freq), y=test2$Freq)) +
  geom_bar(stat="identity",fill='firebrick3') +
  ggtitle('20 palabras más comunes en respuestas sobre innovación') +
  geom_text(aes(label=Freq), vjust=0.5, size=3.5,hjust=-0.5) +
  theme_minimal() +
  xlab('Palabras') + ylab('Frecuencia') +
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),panel.grid = element_blank()) +
  coord_flip()

x11()
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


# Intento de mejorar la cosa
wordcloud2(test,size = 0.7)

# Red palette

custom_colors <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")
# Blue palette
custom_colors <- c("#005073", "#107dac", "#189ad3", "#1ebbd7", "#71c7ec")

#write.csv(test, file = "C:\\Users\\ScmayorquinS\\Desktop\\test.csv")


test$lista_palabras <- gsub('diseno','diseño',test$lista_palabras)


x11()
wordcloud2(test[1:100,], size=0.7, 
           color=rep_len( custom_colors, nrow(test[1:100,])),backgroundColor = "white",shape = 'circle')


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





