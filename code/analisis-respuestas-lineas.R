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
base_clean <- sapply(texto_PND, function(x) preproctext(x))
#base_clean %<>% mutate_if(is.factor,as.character)

# Remover todas las stopwords
base_clean <- sapply(base_clean, function(x) RemoveStopwordsFromText(x,all_stopwords))
#base_clean %<>% mutate_if(is.factor,as.character)


data <- c('Cats like to chase mice.', 'Dogs like to eat big bones.')
corpus <- VCorpus(VectorSource(data))
corpus <- VCorpus(VectorSource(base_clean))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus)

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, 1:5)
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Train.
library(caret)
tic()
fit <- train(y ~ ., data = train, method = 'bayesglm')
toc()
# Check accuracy on training.
predict(fit, newdata = train)

# Test data.
data2 <- c('Bats eat bugs.')
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
predict(fit, newdata = test)
