trace(utils:::unpackPkgZip, edit=TRUE)
# casa
directorio <- 'C:\\Users\\CamiloAndrés\\Desktop\\portal presidencia'
# DNP
directorio <- 'C:\\Users\\ScmayorquinS\\OneDrive - Departamento Nacional de Planeacion\\DIDE\\2019\\Data Science Projects\\Colombia-Pact-Text'


# Funciones preprocesamiento y t-sne
source(paste0(directorio,'/code/funciones.R'))


# Cargar paquetes
paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','wordcloud'
)
lapply(paquetes, require, character.only = TRUE)

#################################
# Preprocesamiento Excel portal #
#################################

base <- read_excel(paste0(directorio,'/data/propuestas.xlsx'))
base <- base[,-1]
base <- base %>% filter(Título == '¿Cómo podrían las entidades públicas innovar para mejorar procesos y servicios?')

# Stop words
sw <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_spanish.txt"),warn = F)

# stop words acidionales
sw_a <- readLines(paste0(directorio,"\\data\\entrada\\stop_words_adicionales.txt"),warn = F)

# todas las stop words
#all_stopwords <- unique(c(sw,sw_a))
all_stopwords <- unique(c(sw))

base_clean <- as.data.frame(sapply(base, function(x) preproctext(x)))
base_clean %<>% mutate_if(is.factor,as.character)

# Remover todas las stopwords
base_clean <- as.data.frame(sapply(base_clean, function(x) RemoveStopwordsFromText(x,all_stopwords)))
base_clean %<>% mutate_if(is.factor,as.character)

# Texto analizar - descripcion
descripcion <- base_clean$Descripción

# cargar texto a lematizar
adicional <- read.table(paste0(directorio,"\\data\\entrada\\verbos-lematizar.txt"),sep ='\t',header = F,encoding = 'UTF-8',colClasses = 'character')
adicional <- separate(data = adicional,col = 'V1',into = c('V1','V2'))

# Lematización
for (i in 1:nrow(adicional)) {
  descripcion <- gsub(adicional[i,'V1'],adicional[i,'V2'],descripcion)
}

# Segunda iteración para corregir lematizada
adicional2 <- read.table(paste0(directorio,"\\data\\entrada\\correccion-lematizada-verbos.txt"),sep ='\t',header = F,encoding = 'UTF-8',colClasses = 'character')
adicional2 <- separate(data = adicional2,col = 'V1',into = c('V1','V2'))

# Lematización
for (i in 1:nrow(adicional2)) {
  descripcion <- gsub(adicional2[i,'V1'],adicional2[i,'V2'],descripcion)
}


lista_palabras <- unlist(strsplit(descripcion,split = ' ',fixed = T))
lista_palabras <- sort(table(lista_palabras),decreasing = T)

test <- as.data.frame(lista_palabras)

test2 <- as.data.frame(grep(pattern = 'r$',test$lista_palabras,value = T))
colnames(test2) <- 'lista_palabras'

definitiva <- inner_join(test,test2,by='lista_palabras')

# Remover últimas stop word que no son verbos en infinitivo
no_verbs <- readLines(paste0(directorio,"\\data\\entrada\\stop-words-no-verbs.txt"),warn = F)

definitiva %<>% mutate_if(is.numeric,as.character)
definitiva <- as.data.frame(sapply(definitiva, function(x) RemoveStopwordsFromText(x,no_verbs)))

definitiva %<>% mutate_if(is.factor,as.character)
definitiva_final <- definitiva[-which(definitiva$lista_palabras==''),]
definitiva_final$Freq <- as.numeric(definitiva_final$Freq) 


barplot <- definitiva_final[1:20,]
barplot <- barplot[order(-barplot$Freq),]
#barplot %<>% mutate_if(is.character,as.factor)
barplot$lista_palabras <- factor(barplot$lista_palabras, levels=unique(barplot$lista_palabras))

#################
# Basic barplot #
#################
p<-ggplot(data=barplot, aes(x=barplot$lista_palabras, y=barplot$Freq)) +
  geom_bar(stat="identity",fill='firebrick3') +
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5,hjust=-0.2)+
  theme_minimal() +
  xlab('Palabras') + ylab('Frecuencia') +
  ggtitle('Verbos más comunes en respuestas sobre innovación') +
  coord_flip()
  
x11()
p




set.seed(1234)
x11()
wordcloud(words = definitiva_final$lista_palabras, freq = definitiva_final$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

redPalette <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")
write.csv(test, file = "C:\\Users\\ScmayorquinS\\Desktop\\test.csv")

wordcloud2(definitiva_final[1:50,], size=0.7, 
           color=rep_len( redPalette, nrow(definitiva_final[1:50,])),backgroundColor = "white",shape = 'circle')


