trace(utils:::unpackPkgZip, edit=TRUE)
# casa
directorio <- 'C:/Users/CamiloAndrés/Documents/OneDrive - Departamento Nacional de Planeacion/Proyectos backup/Edwin Duban Torres Garcia - SGR - Proyectos tipo'
# DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\Proyectos backup\\Edwin Duban Torres Garcia - SGR - Proyectos tipo'

# funciones
source(paste0(directorio,'/P tip R/funciones.R'))
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos'
)
lapply(paquetes, require, character.only = TRUE)

# Cargar datos
proyectos <- read_excel(paste0(directorio,"\\Data\\bases\\tabla.xlsx"))
proyectos <- proyectos[order(proyectos$Sector),] %>% select(-c('BPIN','RiesgoDescripempty','Efectosempty','Mitigacionempty','ProductoComplemempty','Estado'))

# Stop words
sw <- readLines(paste0(directorio,"\\Data\\entrada\\stop_words_spanish.txt"))
# Lugares geográficos
geo <- fread(paste0(directorio,"\\Data\\entrada\\geografiaCol.txt"),sep='\t',encoding = 'UTF-8',header = F)
geo <- sapply(geo, function(x) preproctext(x))
geo <- unlist(strsplit(geo,split = ' '))
# Resguardos
res <- readLines(paste0(directorio,"\\Data\\entrada\\resguardos.txt"))
res <- preproctext(res)
res <- unlist(strsplit(res,split = ' '))
res <- as.character(na.omit(res))
# Veredas
ver <- readLines(paste0(directorio,"\\Data\\entrada\\veredas.txt"))
ver <- preproctext(ver)
ver <- unlist(strsplit(ver,split = ' '))
ver <- as.character(na.omit(ver))

all_stopwords <- unique(c(sw,geo,res,ver))

proyectos_limpio <- as.data.frame(sapply(proyectos, function(x) preproctext(x)))

proyectos_limpio %<>%
  mutate_if(is.factor,as.character)
# Remover todas las stopwords
proyectos_limpio <- sapply(proyectos_limpio, function(x) RemoveStopwordsFromText(x,all_stopwords))

# Filtrar por sectores con más de 100 proyectos
sectores <- as.data.frame(table(proyectos_limpio[,2]))
sectores <- sectores[which(sectores$Freq>=100),]

proyectos_definitivo <- as.data.frame(proyectos_limpio[which(proyectos_limpio[,2] %in% sectores$Var1),])
proyectos_definitivo %<>%
  mutate_if(is.factor,as.character)


vor_sector <- function(sector){
  # Filtrar por sector
  proyectos_definitivo2 <- proyectos_definitivo %>% filter(proyectos_definitivo$Sector==sector)
  
  #save(proyectos_definitivo,file = 'C:\\Users\\ScmayorquinS\\Desktop\\Proyectos backup\\Edwin Duban Torres Garcia - SGR - Proyectos tipo\\Data\\bases\\objeto_proyectos.RData')
  
  # Quitar columna sector para el clasificador
  proyectos_definitivo2$Sector <- NULL
  cols <- colnames(proyectos_definitivo2)
  proyectos_definitivo2$x <- apply(proyectos_definitivo2[,cols],1,paste,collapse=" ")
  
  # Texto final
  texto <- proyectos_definitivo2$x
  
  #######################################################
  ################### LEMATIZACIÓN ######################
  #######################################################

  # diccionario con palabras a reemplazar
  diccionario <- fread(paste0(directorio,"\\Data\\entrada\\lemmatization-es.txt"),sep='\t',encoding = 'UTF-8',header = F)
  diccionario <- diccionario[,c(2,1)]
  # Elimino números
  diccionario <- diccionario[-c(1:969),]
  diccionario <- as.data.frame(sapply(diccionario, function(x) preproctext(x)))
  diccionario %<>%
    mutate_if(is.factor,as.character)
  
  # lematización
  palabras <- strsplit(texto,split = ' ')
  
  # Número único de palabras a reemplazar, probablemente decenas por cada una ~ min
  for (i in 1:length(palabras)) {
    coincidentes <- intersect(palabras[[i]],diccionario$V2)  
  }
  
  # Eliminar palabra proyecto pues la cambia por 'proyectar'
  coincidentes <- tm::removeWords(coincidentes,'proyecto')
  coincidentes <- coincidentes[-which(coincidentes=="")]
  
  # Diccionario con palabras a reemplazar únicamente
  diccionario.reducido <- diccionario[match(coincidentes,diccionario$V2),]
  
  # Palabras adicionales que no están en el diccionario
  adicionales <- data.frame(V2=c('vial','culturales','viviendas','disenos','intradomiciliars','proyectos','productores','cultural','deportiva','estudios','artisticas','educativas'),
                            V1=c('via','cultural','vivienda','diseno','intradomicilario','proyecto','produccion','cultura','deporte','estudio','artistica','educativa'))
  adicionales2 <- data.frame(V2=c('vial','culturales','servicios','mejoramiento','instalaciones','viviendas','mejoramiento','inundaciones','ambientales','biodiirrsidad','generarr','inirstigacion','generarcion','estrategias','concretar','contruccion','parques','proyectar','instalarcion','kva','victimars','vehicular','pavimentar','deportiva','intradomiciliars','productores','rurales','productivo','productividad','productivas','urbaño','areas','interirntoria','pruducto','artisticas','construir','actividades','deportivos','practicas','aulas','electricas','luminarias','infantil','educativa'),
                             V1=c('via','cultural','servicio','mejorar','instalacion','vivienda','mejorar','inundacion','ambiental','biodiversidad','generar','investigacion','generacion','estrategia','concreto','construir','parque','proyecto','instalacion','kv','victimas','vehiculo','pavimento','deporte','intradomiciliario','produccion','rural','produccion','produccion','produccion','urbano','area','interventoria','produccion','artistica','construccion','actividad','deportivo','practica','aula','electrica','luminaria','infancia','educacion'))
  diccionario.reducido <- rbind(diccionario.reducido,adicionales,adicionales2)
  
  # Reemplazo palabras por 'lema' en diccionario ~ 29 seg
  for (i in 1:nrow(diccionario.reducido)) {
    texto <- gsub(diccionario.reducido[i,'V2'],diccionario.reducido[i,'V1'],texto)
  }
  
  # Bag of words
  dtm <- as.matrix(create_matrix(texto))
  
  # Matriz de probabilidades
  BoW <- t(apply(dtm,1,function(x)x/sum(x)))
  
  # PCA con matriz cuadrada ~ 2 horas
  BoW.pca <- prcomp(BoW)
  #save(BoW.pca,file = 'C:\\Users\\ScmayorquinS\\Desktop\\Proyectos backup\\Edwin Duban Torres Garcia - SGR - Proyectos tipo\\Data\\bases\\PCA.R')
  #load(paste0(directorio,'\\Data\\bases\\PCA.R'))
  
  perp <- round((5*ncol(BoW.pca$x))/100)
  tiesne <- Rtsne(X = BoW.pca$x[,1:5],dims = 2,perplexity = perp,theta = 0.5,check_duplicates = F,pca = F,partial_pca = F,max_iter = 1000,verbose = T)
  
  
  # Número óptimo de clusters de acuerdo con 26 métodos diferentes
  library(NbClust)
  intento <- NbClust(data = tiesne$Y,distance = 'euclidean',max.nc = 15,method = 'ward.D')
  n.clusters <- as.numeric(names(sort(table(intento$Best.nc),decreasing=TRUE)[1]))

  kfit <- kmeans(tiesne$Y, n.clusters)
  
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
  
  palabras.por.cluster <- palabras.comunes.cluster(BoW = BoW,kmeansobj = kfit, nPalabras = 7)
  
  palabras.por.cluster <- paste(as.character(1:n.clusters),". ",palabras.por.cluster, sep="")
  
  vor_df$id <- as.factor(as.integer(vor_df$id))
  
  # Configuramos un menor número de colores para colorear el diagrama.
  # Nota: Para 4 colores demora en converger
  colores<- colorear.mapa(n.clusters,4,vor)
  vor_df$colores<-rep(0,nrow(vor_df))
  for (i in unique(colores)){
    vor_df$colores[vor_df$id%in%which(colores==i)]<- i
  }
  vor_df$colores <- as.factor(vor_df$colores)
  
  palette <- NULL
  for (i in 1:length(colores)){
    switch (colores[i],
            "1" = {color.temp <- "gold2"},
            "2" = {color.temp <- "dodgerblue3"},
            "3" = {color.temp <- "firebrick3"},
            "4" = {color.temp <- "forestgreen"}
    )
    palette <- c(palette,color.temp)
  }
  
  library(ggplot2)
  plot.voronoi <- ggplot(df, aes(x=X,y=Y)) +
    theme_light() +
    ggtitle("Clasificador por diagrama de Voronoi") +
    geom_point(data=df,aes())+#, alpha = 1))+ #,color=Cluster)) +
    geom_map(data=vor_df, map=vor_df,
             aes(x=long, y=lat, map_id=id, fill = id),
             size=0.25, alpha = 0.7)+
    #scale_color_manual(values = 1:25) +
    scale_fill_manual(values = palette,  labels = palabras.por.cluster) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size = 1,
                 data = voronoi$dirsgs,linetype = 1,color= "gray40")+
    ggplot2::annotate("text", x=centros$x, y=centros$y, label= as.character(1:n.clusters), #palabras.por.cluster,#
                      size = 6, col="black", alpha = 0.85)
  
  X11()
  plot.voronoi
  
}


# voronoi
tic()
vor_sector('cultura')
toc()

tic()
vor_sector('transporte')
toc()

tic()
vor_sector('agricultura desarrollo rural')
toc()

tic()
vor_sector('ambiente desarrollo sostenible')
toc()

tic()
vor_sector('ciencia tecnologia innovacion')
toc()

tic()
vor_sector('deporte recreacion')
toc()

tic()
vor_sector('educacion')
toc()

tic()
vor_sector('energia')
toc()

tic()
vor_sector('inclusion social reconciliacion')
toc()

tic()
vor_sector('salud proteccion social')
toc()

tic()
vor_sector('vivienda ciudad territorio')
toc()


# Gráfico k-means ~ primeras dos componentes
plot(BoW.pca$x[,1:2], col = kfit$cluster)
points(kfit$centers, col = 1:15, pch = 2, cex = 2)

# unCol <- data.frame(x1=BoW.pca$x[,1],x2=BoW.pca$x[,2],y=factor(row.names(BoW.pca$x)))
# ggplot(unCol,aes(x=x1,y=x2)) + geom_point()

# Gráfico t-SNE
set.seed(0)
tiesne <- Rtsne(X = BoW.pca$x[,1:5],dims = 2,perplexity = 20,theta = 0.5,check_duplicates = F,pca = F,partial_pca = F,max_iter = 1000,verbose = T,exaggeration_factor = 4)
save(tiesne,file = paste0(directorio,'\\Data\\bases\\tsne.R'))

plot(tiesne$Y,col=kfit$cluster)




plot(sectores$Freq)
abline(h=600)
