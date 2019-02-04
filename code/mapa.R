library(sp)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(scales)
library(readxl)

# DNP
directorio <- 'C:\\Users\\cmayorquin\\Desktop\\portal presidencia'


paquetes <- c('dplyr','readxl','data.table','magrittr','RTextTools','tictoc','ggplot2','tm',
              'ClusterR','factoextra','FactoMineR','beepr','quanteda','Rtsne','deldir','sp',
              'rgeos','reshape','tidyr','tidytext','stringr','wordcloud','tidyverse','utf8'
)


lapply(paquetes, require, character.only = TRUE)

# Base con ordenamiento de departamentos de acuerdo a id del shape
DatosPau <- read_excel("C:\\Users\\cmayorquin\\Desktop\\portal presidencia\\data\\mapa\\datoos.xlsx", range = "A1:B33")

# Base de datos 
base <- read_excel(paste0(directorio,'/data/propuestas.xlsx'))
base <- base[,-1]
base <- base %>% filter(Título == '¿Cómo podrían las entidades públicas innovar para mejorar procesos y servicios?')

# Cambiar todos los 'no hay registro' y 'Bogotá' por Cundinamarca
base$Departamento <- gsub('no hay registro','Cundinamarca',base$Departamento)
base$Departamento <- gsub('Bogota D.C.','Cundinamarca',base$Departamento)

# Dejar solo departamentos de Colombia
base <- base[ ! base$Departamento %in% c('Braga','Cochabamba','Grevenmacher'), ]
# Reducir a usuarios y departamento
base <- data.frame(usuario=base$Username,depto=base$Departamento)
# Dejar total por cada depto en orden
base2 <- unique(base)
base2 <- as.data.frame(base2[,-1])
base2 <- as.data.frame(table(base2))
base2 <- base2[order(-base2$Freq),]
# Igualo los nombres de las bases a mergear
base2$base2 <- gsub('Valle del Cauca','Valle',base2$base2)
base2$base2 <- gsub('Narino','Nariño',base2$base2)
#base2$base2 %in% DatosPau$depto

# matching names para ambos data frames
colnames(base2) <- c('depto','Freq')
# hail dplyr
definitiva <- full_join(DatosPau, base2,"depto")
definitiva$Cuartil <- NULL

# Cambiar NAs por ceros
definitiva$Freq[is.na(definitiva$Freq)] <- 0

####################
# Contrucción mapa #
####################
ohsCol2 <- readShapeSpatial('C:\\Users\\cmayorquin\\Desktop\\portal presidencia\\data\\mapa\\geo_export_98f694ca-e5a7-40e0-a59f-523ec74b2f25.shp')
ohsColI2 <- fortify(ohsCol2)
grupo2 <- data.frame(id = unique(ohsColI2[ , c("id")]))
#grupo2 <- head(grupo2,-1)
grupo2[ , "Porcentaje"] <- factor(definitiva$Freq,levels = c("31","15","5","2",'1','0'))
ohsColI2 <- merge(ohsColI2, grupo2, by = "id")


mapColDep <- ggplot() +
  geom_polygon(data=ohsColI2, aes(x=long, y=lat, group = group,
                                  fill = Porcentaje), colour ="black", size = 0.1) +
  theme_void()+
  scale_fill_manual(values = c("midnightblue","dodgerblue4","dodgerblue3","dodgerblue2",'deepskyblue3','dimgray'))+
  # scale_fill_gradient(low="#56B1F7",high="#132B43")+
  labs(title = "Respuestas por departamento en Colombia", fill = "") +
  labs(x="",y="",title="Respuestas por departamento en Colombia") +
  scale_x_continuous(limits=c(-80,-60))+
  scale_y_continuous(limits=c(-5,13))

x11()
mapColDep





