trace(utils:::unpackPkgZip, edit=TRUE)
# casa
directorio <- 'C:/Users/CamiloAndrés/Documents/OneDrive - Departamento Nacional de Planeacion/Proyectos backup/Edwin Duban Torres Garcia - SGR - Proyectos tipo'
# DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\Proyectos backup\\Edwin Duban Torres Garcia - SGR - Proyectos tipo'

# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/P tip R/funciones.R'))
# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos'
)
lapply(paquetes, require, character.only = TRUE)

# Base estándar preprocesada
load('C:\\Users\\ScmayorquinS\\Desktop\\Proyectos backup\\Edwin Duban Torres Garcia - SGR - Proyectos tipo\\Data\\bases\\objeto_proyectos.RData')

# Filtro, procesamiento y lematización
proc_sector <- function(sector){
  
  proyectos_definitivo <- proyectos_definitivo %>% filter(proyectos_definitivo$Sector==sector)
  # Quitar columna sector para el clasificador
  proyectos_definitivo$Sector <- NULL
  cols <- colnames(proyectos_definitivo)
  proyectos_definitivo$x <- apply(proyectos_definitivo[,cols],1,paste,collapse=" ")
  # Texto final (casi)
  texto <- proyectos_definitivo$x
  # lematización
  palabras <- strsplit(texto,split = ' ')
  # diccionario con palabras a reemplazar
  diccionario <- fread(paste0(directorio,"\\Data\\entrada\\lemmatization-es.txt"),sep='\t',encoding = 'UTF-8',header = F)
  diccionario <- diccionario[,c(2,1)]
  # Elimino números
  diccionario <- diccionario[-c(1:969),]
  diccionario <- as.data.frame(sapply(diccionario, function(x) preproctext(x)))
  diccionario %<>%
    mutate_if(is.factor,as.character)
  # Número único de palabras a reemplazar, probablemente decenas por cada una ~ 5 min
  for (i in 1:length(palabras)) {
    coincidentes <- intersect(palabras[[i]],diccionario$V2)  
  }
  
  coincidentes <- tm::removeWords(coincidentes,'proyecto')
  coincidentes <- coincidentes[-which(coincidentes=="")]
  # Diccionario con palabras a reemplazar únicamente
  diccionario.reducido <- diccionario[match(coincidentes,diccionario$V2),]
  # Palabras adicionales que no están en el diccionario
  adicionales <- data.frame(V2=c('vial','culturales','servicios','mejoramiento','instalaciones','viviendas','mejoramiento','inundaciones','ambientales','biodiirrsidad','generarr','inirstigacion','generarcion','estrategias','concretar','contruccion','parques','proyectar','instalarcion','kva','nino','ano','victimars','vehicular','pavimentar','deportiva','diseno','intradomiciliars','productores','rurales','productivo','productividad','productivas','urbaño','areas','interirntoria','pruducto','artisticas','construir','actividades','deportivos','practicas','aulas','electricas','luminarias','ninas','infantil'),
                            V1=c('via','cultural','servicio','mejorar','instalacion','vivienda','mejorar','inundacion','ambiental','biodiversidad','generar','investigacion','generacion','estrategia','concreto','construir','parque','proyecto','instalacion','kv','niño','año','victimas','vehiculo','pavimento','deporte','diseño','intradomiciliario','produccion','rural','produccion','produccion','produccion','urbano','area','interventoria','produccion','artistica','construccion','actividad','deportivo','practica','aula','electrica','luminaria','niñas','infancia'))
  diccionario.reducido <- rbind(diccionario.reducido,adicionales)
  # Reemplazo palabras por 'lema' en diccionario ~ 30 seg
  for (i in 1:nrow(diccionario.reducido)) {
    texto <- gsub(diccionario.reducido[i,'V2'],diccionario.reducido[i,'V1'],texto)
  }
  # Bag of Words binario
  BoW <- BoWveloz(texto)
  # t-SNE
  t <- proyTsneBoW(BoW)
  # Guardar objetos necesarios para gráfico
  assign('t',t,envir = globalenv())
  assign('BoW',BoW,envir = globalenv())

}

# Gráficos en 2 dimensiones del t-sne con palabras y coordenadas de PCA

# agricultura
proc_sector(sector = 'agricultura desarrollo rural')
plot.tsneproy(t$Y,BoW)

# ambiente
proc_sector(sector = 'ambiente desarrollo sostenible')
plot.tsneproy(t$Y,BoW)

# ciencia
proc_sector(sector = 'ciencia tecnologia innovacion')
plot.tsneproy(t$Y,BoW)

# cultura
proc_sector(sector = 'cultura')
plot.tsneproy(t$Y,BoW)

# deporte
proc_sector(sector = 'deporte recreacion')
plot.tsneproy(t$Y,BoW)

# educacion
proc_sector(sector = 'educacion')
plot.tsneproy(t$Y,BoW)

# energia
proc_sector(sector = 'energia')
plot.tsneproy(t$Y,BoW)

# inclusion social
proc_sector(sector = 'inclusion social reconciliacion')
plot.tsneproy(t$Y,BoW)

# salud
proc_sector(sector = 'salud proteccion social')
x11()
plot.tsneproy(t$Y,BoW)

# transporte
proc_sector(sector = 'transporte')
plot.tsneproy(t$Y,BoW)

# vivienda
proc_sector(sector = 'vivienda ciudad territorio')
plot.tsneproy(t$Y,BoW)


