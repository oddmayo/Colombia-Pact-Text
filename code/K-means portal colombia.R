trace(utils:::unpackPkgZip, edit=TRUE)
# casa
directorio <- 'C:\\Users\\CamiloAndrés\\Desktop\\portal presidencia'
# DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'
# Temporal
directorio <- 'C:\\Users\\cmayorquin\\Desktop\\Colombia-Pact-Text'

# funciones
source(paste0(directorio,'/code/funciones.R'))
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','NbClust'
              )
lapply(paquetes, require, character.only = TRUE)

# k means - tsne - voronoi

base <- read_excel(path = paste0(directorio,'\\data\\propuestas.xlsx'))
base <- base[,-1]

base <- base %>% filter(Título == '¿Cómo podrían las entidades públicas innovar para mejorar procesos y servicios?')

# Stop words
sw <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_spanish.txt"),warn = F)

# stop words acidionales
sw_a <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_adicionales.txt"))

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


# Bag of words
dtm <- as.matrix(create_matrix(descripcion))
BoW <- as.matrix(create_matrix(descripcion))

# Matriz de probabilidades
BoW <- t(apply(dtm,1,function(x)x/sum(x)))

# PCA con coordenadas ~ 2 horas
BoW.reducido <- PCA(BoW,graph = F)

# PCA con matriz cuadrada ~ 2 horas
BoW.pca <- prcomp(BoW)

# Codo para número óptimo de clusters ~ 21 min
opt = Optimal_Clusters_KMeans(BoW.pca$x, max_clusters = 20, plot_clusters = T,criterion = 'variance_explained',num_init = 1,initializer = 'optimal_init')

perp <- round((5*ncol(BoW.pca$x))/100)
tiesne <- Rtsne(X = BoW.pca$x[,1:5],dims = 2,perplexity = perp,theta = 0.5,check_duplicates = F,pca = F,partial_pca = F,max_iter = 1000,verbose = T)

intento <- NbClust(data = tiesne$Y,distance = 'euclidean',max.nc = 15,method = 'kmeans')
n.clusters <- as.numeric(names(sort(table(intento$Best.nc),decreasing=TRUE)[1]))

n.clusters <- 7
kfit <- kmeans(tiesne$Y, n.clusters)

plot(BoW.pca$x[,1:2], col = kfit$cluster)
plot(tiesne$Y, col = kfit$cluster)
points(kfit$centers, col = 1:5, pch = 2, cex = 2)

plot(tiesne$Y,col=kfit$cluster)


library(cluster)
library(HSAUR)
# Matriz de disimilaridad
dissE <- daisy(tiesne$Y) 
dE2   <- dissE^2
sk2   <- silhouette(kfit$cluster, dE2)
# Gráfico silueta
plot(sk2)
# Se reafirma la presencia de 7 clusters

# Dos componentes
clusplot(tiesne$Y, kfit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


#######################################
############### Voronoi ###############
#######################################

# Para definir el clasificador, creamos polígonos en el espacio de 2 dimensiones, los cuales
# corresponderán a los grupos de productos, o bien, al área en la cual una observación se
# clasificará como perteneciente a cada grupo. Para ello, creamos un diagrama de Voronoi con
# base en los centros de cada grupo.

# Creamos el diagrama de Voronoi por segmentos.

centros <- data.frame(kfit$centers)
colnames(centros) <- c("x","y")
voronoi <- deldir(centros$x, centros$y)

# Definimos los polígonos del diagrama de Voronoi.

# Nota: esta función fue tomada de:
# https://stackoverflow.com/questions/36108812/fill-voronoi-polygons-with-ggplot

vor_pts <- SpatialPointsDataFrame(cbind(centros$x,centros$y),centros, match.ID=TRUE)

vor <- SPointsDF_to_voronoi_SPolysDF(vor_pts)

vor_df <- fortify(vor)

# Creamos un data frame con los resultados del t-SNE para poder graficar el clasificador.

df<-data.frame(X=tiesne$Y[,1],Y=tiesne$Y[,2],
               Cluster=factor(kfit$cluster))

# Creamos un vector con las 2 palabras más frecuentes de cada grupo (cluster).

palabras.por.cluster <- palabras.comunes.cluster(BoW = BoW,kmeansobj = kfit, nPalabras = 10)
palabras.por.cluster <- paste(as.character(1:n.clusters),". ",palabras.por.cluster, sep="")

vor_df$id <- as.factor(as.integer(vor_df$id))

# Configuramos un menor número de colores para colorear el diagrama.
# Nota: Para 4 colores demora en converger
colores<- colorear.mapa(n.clusters, 4, vor)
vor_df$colores<-rep(0,nrow(vor_df))
for (i in unique(colores)){
  vor_df$colores[vor_df$id%in%which(colores==i)]<- i
}
vor_df$colores <- as.factor(vor_df$colores)

palette <- NULL
for (i in 1:length(colores)){
  switch (colores[i],
          "1" = {color.temp <- "gray13"},
          "2" = {color.temp <- "royalblue4"},
          "3" = {color.temp <- "red3"},
          "4" = {color.temp <- "darkorchid4"}
  )
  palette <- c(palette,color.temp)
}


palabras.por.cluster <- c('1. Diseño e ideas sobre innovación.',
                          '2. Desarrollo de tecnología por parte de entidad pública.',
                          '3. Salud pública, centros médicos y deporte.',
                          '4. Procesos atención ciudadano.',
                          '5. Política pública para empleados y contratistas.',
                          '6. Procesos para mejorar trámites (atención y comodidad).',
                          '7. Alianza Pacífico, Colombia, Chile y México.')

library(ggplot2)
plot.voronoi <- ggplot(df, aes(x=X,y=Y)) +
  theme_light() +
  ggtitle("Clasificación de temas en respuestas sobre innovación") +
  geom_point(data=df,aes())+#, alpha = 1))+ #,color=Cluster)) +
  geom_map(data=vor_df, map=vor_df,
           aes(x=long, y=lat, map_id=id, fill = id),
           size=0.25, alpha = 0.7)+
  #scale_color_manual(values = 1:25) +
  scale_fill_manual(values = palette,  labels = palabras.por.cluster) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size = 1,
               data = voronoi$dirsgs,linetype = 1,color= "gray40")+
  ggplot2::annotate("text", x=centros$x, y=centros$y, label= as.character(1:n.clusters), #palabras.por.cluster,#
                    size = 6, col="black", alpha = 0.85) +
  theme(plot.title = element_text(hjust = .5))

X11()
plot.voronoi

