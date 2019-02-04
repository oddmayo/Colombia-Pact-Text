trace(utils:::unpackPkgZip, edit=TRUE)
# casa
directorio <- 'C:\\Users\\CamiloAndrés\\Desktop\\portal presidencia'
# DNP
directorio <- 'C:\\Users\\cmayorquin\\Desktop\\portal presidencia'


# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/cluster/funciones.R'))


# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr'
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



# Bag of Words binario
BoW <- BoWveloz(descripcion)

proyTsneBoW <- function(B){
  require(Rtsne)
  # t-sne perplexity automatizado
  perp <- 15
  pca <- PCA(B,graph = F)
  PVC <- pca$var$coord
  ts2 <- Rtsne(X = PVC[,1:5],dims = 2,perplexity = perp,pca = F,max_iter = 1000,verbose = T,pca_center = F,check_duplicates = F)
}

  # t-SNE
  t <- proyTsneBoW(BoW)
  tic()
  plot.tsneproy(t$Y,BoW)
  toc()


  
