# Para instalar paquetes modificar linea 142 sys.sleep(5.0)
trace(utils:::unpackPkgZip, edit=TRUE)

# Directorio DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'

# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))

# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','readtext',
              'slam','xlsx'
)
lapply(paquetes, require, character.only = TRUE)


# Texto con objetivos del PND sección de innovación
texto_PND <- read_file(paste0(directorio,"\\data\\PND.txt"))

# Separe cada objetivo cuando encuentre un "*."
texto_PND <- unlist(strsplit(texto_PND, "(?<=\\*.)\\s(?=[A-Z])", perl = T))
texto_PND <- texto_PND[2:6]

#----------------------------------------------------------------------------------------------------------------------
# Preprocesamiento
#----------------------------------------------------------------------------------------------------------------------

# Cargar stopwords
sw <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_spanish.txt"),warn = F)


# Todas las stop words - No hay necesidad de adicionales
all_stopwords <- unique(c(sw))

# A continuación se usa una serie de "fors" puesto que los "sapply" duplican por algún motivo la información

# Preprocesamiento del texto
for (i in texto_PND) {
  base_clean <- NULL
  base_clean <- preproctext(texto_PND)
}

# Remover stopwords
for (i in base_clean) {
  sin_sw <- NULL
  sin_sw <- RemoveStopwordsFromText(base_clean,all_stopwords)
}

# Fucnión para dejar solo la palabras únicas de cada objetivo
rem_dup.one <- function(x){
  paste(unique(tolower(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T))))),collapse = " ")
}
rem_dup.vector <- Vectorize(rem_dup.one,USE.NAMES = F)

# Palabras únicas para cada objetivo
for (i in sin_sw) {
  best <- rem_dup.vector(sin_sw)  
}

# Cargar objeto que contiene respuestas
# Versión lematizada
load(paste0(directorio,"\\data\\desc-lematizada.RData"))

# Versión sin lematizar
load(paste0(directorio,"\\data\\desc-sin-lematizar.RData"))

# Dejar palabras únicas para cada respuesta
for (i in descripcion) {
  best2 <- rem_dup.vector(descripcion)  
}



#----------------------------------------------------------------------------------------------------------------------
# Matriz de similitud del coseno
#----------------------------------------------------------------------------------------------------------------------
# dtm con quanteda
# Matriz de entrenamiento con vocabulario los objetivos y de las respuestas
dtm_nueva <- as.matrix(dfm(c(best,best2)))

#--------------------------------------------
# Número de líneas
dtm1 <- as.simple_triplet_matrix(dtm_nueva[1:5,]) 
# Número de respuestas
dtm2 <-as.simple_triplet_matrix(dtm_nueva[6:111,]) 
# Matriz cruzada similitud coseno
cosine_sim <- tcrossprod_simple_triplet_matrix(dtm1, dtm2)/sqrt(row_sums(dtm1^2) %*% t(row_sums(dtm2^2)))
# Invertir matriz para visibilidad
veamos <- t(cosine_sim)
# Nombres más cortos para las líneas
colnames(veamos) <- 1:5
# Matriz final
prueba <- veamos[1:106,1:5]

# Dataframe 
base <- as.data.frame(prueba)

# Nombres de los objetivos / líneas
objetivos <- readLines(paste0(directorio,"\\data\\PND-titulos.txt"),warn = F)
# Objetivos con identificador
objetivos_numero <- data.frame(nombre=objetivos,numero=1:5)

# Crear columna con número del objetivo con el que mayor tiene similitud
base$numero <- as.numeric(colnames(base)[apply(base,1,which.max)])

# Pegar Texto del objetivo a su respectivo número por fila
funciona <- full_join(base,objetivos_numero,"numero")
# Nombres originales
rownames(funciona) <- rownames(base)

# Similitud con texto limpio de las respuestas
funciona <- funciona[,6:7]


# Cargo respuestas de las personas
base_original <- read_excel(paste0(directorio,'/data/propuestas.xlsx'))
base_original <- base_original[,-1] # Eliminar columna sin nombre
# Dejar respuestas solo de innovación
base_original <- base_original %>% filter(Título == '¿Cómo podrían las entidades públicas innovar para mejorar procesos y servicios?')


similitud <- funciona
rownames(similitud) <- base_original$Descripción
colnames(similitud) <- c("Número","Objetivo")
similitud <- cbind(Respuesta = rownames(similitud), similitud)
# Similitud con texto original de las respuestas
rownames(similitud) <- NULL

#write.xlsx(similitud,paste0(directorio,'\\output\\coseno-respuestas-objetivos.xlsx'))




