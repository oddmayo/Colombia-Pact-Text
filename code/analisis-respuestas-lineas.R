# Para instalar paquetes modificar linea 142 sys.sleep(5.0)
trace(utils:::unpackPkgZip, edit=TRUE)

# Directorio DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'

# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))

# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','readtext',
              'slam'
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
# SVM
#----------------------------------------------------------------------------------------------------------------------

# Asignarle una clase a cada línea
nuevo_dataset <- data.frame(text=best,class=1:5)

# Creaer document term matrix
dtMatrix <- create_matrix(nuevo_dataset["text"])

# Datos de entrenamiento en container
container <- create_container(dtMatrix, nuevo_dataset$class, trainSize=1:5, virgin=FALSE)

# Entrenar modelo
model <- train_model(container, "SVM", kernel = "polynomial", cost=10)

# Datos a predecir - Clasificar respuestas en los objetivos
predictionData <- descripcion
# Cambiar "Acronym" por "acronym"
trace("create_matrix", edit=T)
# Crear document term matrix de predicción
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)

# Datos a predecir en container - Test data
predSize = length(predictionData)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

# Clasificación y resultados
results <- classify_model(predictionContainer, model)
results





#############
# Similitud #
#############

dtm_nueva <- create_matrix(c(best,best2))
# Número de líneas
dtm1 <- dtm_nueva[1:5,]
# Número de respuestas
dtm2 <- dtm_nueva[6:111,]
# Matriz cruzada similitud coseno
cosine_sim <- tcrossprod_simple_triplet_matrix(dtm1, dtm2)/sqrt(row_sums(dtm1^2) %*% t(row_sums(dtm2^2)))
# Invertir matriz para visibilidad
veamos <- t(cosine_sim)
# Nombres más cortos para las líneas
colnames(veamos) <- c('uno','dos','tres','cuatro','cinco')
# Matriz final
prueba <- veamos[1:106,1:5]
