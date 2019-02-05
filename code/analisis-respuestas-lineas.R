directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'


# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))


# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','readtext'
)
lapply(paquetes, require, character.only = TRUE)


#prueba <- readtext("C:\\Users\\ScmayorquinS\\Downloads\\05. PND CTI_13 enero 2019.docx")

texto_PND <- read_file("C:\\Users\\ScmayorquinS\\Desktop\\PND.txt")

# Separe cada texto después de un "*."
# EXCELENTE
texto_PND <- unlist(strsplit(texto_PND, "(?<=\\*.)\\s(?=[A-Z])", perl = T))
texto_PND <- texto_PND[2:6]

# Limpieza
# Stop words
sw <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_spanish.txt"),warn = F)

# todas las stop words
all_stopwords <- unique(c(sw))

# Preprocesar texto antes de stopwords
for (i in texto_PND) {
  base_clean <- NULL
  base_clean <- preproctext(texto_PND)
}
#base_clean <- sapply(texto_PND, function(x) preproctext(x))
#base_clean %<>% mutate_if(is.factor,as.character)

# Remover todas las stopwords
#base_clean <- sapply(base_clean, function(x) RemoveStopwordsFromText(x,all_stopwords))

for (i in base_clean) {
  sin_sw <- NULL
  sin_sw <- RemoveStopwordsFromText(base_clean,all_stopwords)
}

#base_clean %<>% mutate_if(is.factor,as.character)


# Dejar palabras únicas por cada lista
unos <- "hola como estas"
uno <- sin_sw[1]
dos
tres
cuatro
cinco


rem_dup.one <- function(x){
  paste(unique(tolower(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T))))),collapse = " ")
}
rem_dup.vector <- Vectorize(rem_dup.one,USE.NAMES = F)
rem_dup.vector(sin_sw[1])


best <- sapply(sin_sw,function(x) rem_dup.vector(x))



######### Otro intento
nuevo_dataset <- data.frame(text=sin_sw,class=1:5)

# Create the document term matrix
dtMatrix <- create_matrix(nuevo_dataset["text"])

# Configure the training data
container <- create_container(dtMatrix, nuevo_dataset$class, trainSize=1:5, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel = "linear", cost=1000000)

# new data
predictionData <- list("sunny sunny sunny rainy rainy", "rainy sunny rainy rainy", "hello", "", "this is another rainy world")
predictionData <- descripcion
trace("create_matrix", edit=T)
# create a prediction document term matrix
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)

# create the corresponding container
predSize = length(predictionData)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

# predict
results <- classify_model(predictionContainer, model)
results
