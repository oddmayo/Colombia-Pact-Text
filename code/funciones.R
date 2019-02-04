mejorDeVariosKmeans<-function(datos,k,iteraciones){
  minwithin<-Inf
  for (i in 1:iteraciones){
    set.seed(i)
    mikmeans<-kmeans(datos,k)
    if(mikmeans$tot.withinss<minwithin){
      out<-mikmeans
    }
  }
  out
}

preproctext <- function(x){
  require(magrittr)
  x[which(is.na(x))] <- ""
  y <- x %>% 
    iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(y)
}

RemoveStopwordsFromText <- function(texto, # texto
                                    swords # términos a remover
){
  sapply(texto, function(x){
    y <- strsplit(x," ",T)[[1]]
    paste(y[!(y%in%swords)],collapse=" ")
  }, USE.NAMES = FALSE
  )
}

BoWveloz <- function(M){
  if(!is.null(ncol(M))) M <- c(M)
  i <- which(M!="")
  M <- M[i]
  terminosComunes <- names(head(sort(table(unlist(sapply(M, strsplit," "),use.names = FALSE)),decreasing=TRUE),50))
  
  JNSW <- sapply(M,strsplit," ",USE.NAMES = FALSE)
  
  indicesBoW <- sapply(JNSW, function(x){which(terminosComunes%in%x)})
  
  BoW <- t(sapply(1:length(M),
                  function(x){a <- rep(0,50); a[indicesBoW[[x]]] <- 1;return(a)},
                  USE.NAMES = FALSE))
  
  #terminosComunes[which(terminosComunes=="anos")] <- "años"
  colnames(BoW) <- terminosComunes
  return(BoW)
}

#euc_dist <- function(m) {mtm <- Matrix::tcrossprod(m); sq <- rowSums(m*m);  sqrt(outer(sq,sq,"+") - 2*mtm)}

proyTsneBoW <- function(B){
  require(Rtsne)
  pca <- PCA(B,graph = F)
  PVC <- pca$var$coord
  ts2 <- Rtsne(X = PVC[,1:5],dims = 2,perplexity = 15,pca = FALSE,partial_pca = FALSE,max_iter = 2000,verbose = T,pca_center = F,check_duplicates = F)
}



proyTsneBoW2 <- function(B){
  require(Rtsne)
  PVC <- B$var$coord
  ts2 <- Rtsne(X = PVC[,1:5],dims = 2,perplexity = 15,pca = FALSE,partial_pca = FALSE,max_iter = 2000,verbose = T,pca_center = F,check_duplicates = F)
}


plot.tsneproy <- function(t,B){
  par(mar=rep(0,4))
  lp1a <- min(t[,1])*1.05
  lp1b <- max(t[,1])*1.05
  lp2a <- min(t[,2])*1.05
  lp2b <- max(t[,2])*1.05
  
  plot(t,pch=20,type="n",xlim=c(lp1a,lp1b),ylim=c(lp2a,lp2b),
       bty="o",xaxt="n",yaxt="n",xlab="",ylab="")
  
  conts <- matrix(0,50,50)
  for(i in 1:nrow(B)){
    cc <- which(B[i,]>0)
    l <- length(cc)
    if(l>1){
      for(i1 in 1:(l-1)){
        c1 <- cc[i1]
        for(i2 in (i1+1):l){
          c2 <- cc[i2]
          conts[c1,c2] <- conts[c1,c2]+1
        }
      }
    }
  }
  
  AI <- arrayInd(order(conts),dim(conts))[-which(sort(conts)==0),]
  
  for(i1 in 1:nrow(AI)){
    v <- AI[i1,]
    lines(t[c(v[1],v[2]),1],t[c(v[1],v[2]),2],col=grey(1-conts[v[1],v[2]]/max(conts,na.rm = TRUE)))
  }
  
  text(t,colnames(B),cex=.1+2*sqrt(rowSums(B)/max(rowSums(B))))
}


SPointsDF_to_voronoi_SPolysDF <- function(sp) {
  
  # tile.list extracts the polygon data from the deldir computation
  vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2]))
  
  lapply(1:(length(vor_desc)), function(i) {
    
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    
    # now we can make the Polygon(s)
    Polygons(list(Polygon(tmp)), ID=i)
    
  }) -> vor_polygons
  
  # hopefully the caller passed in good metadata!
  sp_dat <- sp@data
  
  # this way the IDs _should_ match up w/the data & voronoi polys
  rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons),
                                  'polygons'),
                             slot, 'ID')
  
  SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                           data=sp_dat)
  
}

# 2.4 Identificación de palabras más frecuentes en un cluster

# La función "palabras.comunes.cluster" permite identificar las palabras que aparecen
# mayor número de veces en los textos de los productos de un mismo grupo.

palabras.comunes.cluster <- function(BoW, kmeansobj, nPalabras){
  frecuencias_clusters<-apply(BoW, 2, function(x){tapply(x, kmeansobj$cluster, sum)})
  apply(frecuencias_clusters,1,function(x){paste(head(names(sort(x,decreasing = TRUE)),nPalabras),collapse = " ")})
}


# 2.6 Cuatro colores en figuras adyacentes

# La función inicial.posibles.colores se utiliza en la función colorear.mapa (2.7)

inicial.posibles.colores <- function(i,colores,n.colores,poligonos){
  cuatro.colores <- rep(FALSE,n.colores)
  adyacentes <- which(gTouches(poligonos, byid = TRUE)[,i])
  for(k in 1:n.colores){
    if(!any(colores[adyacentes]==k)){cuatro.colores[k]<-TRUE}
  }
  cuatro.colores
}


# 2.7 Colorear polígonos con colores distintos a los de sus adyacentes

# La función colorear.mapa permite colorear una serie de polígonos bidimensionales
# de forma tal que dos polígonos adyacentes no tengan el mismo color.
# Por el teorema de los 4 colores, el algoritmo siempre convergerá si se utilizan
# 4 colores, mas demorará menos en hacerlo si se permiten más colores.



colorear.mapa <- function(n.poligonos,n.colores,poligonos){
  colores <- rep(0,n.poligonos)
  posibles.colores <- NULL
  i<-1
  while (i <= n.poligonos){
    if (sum(colores!=0)==(i-1)){
      posibles.colores <- rbind(posibles.colores, inicial.posibles.colores(i,colores,n.colores,poligonos))
    }
    if (any(posibles.colores[i,])){
      colores[i]<-which(posibles.colores[i,])[1]
      i <- i+1
    }
    else {
      posibles.colores <- head(posibles.colores,-1)
      colores[i]<-0
      i <- i-1
      posibles.colores[i,which(posibles.colores[i,])[1]]<-FALSE
    }
  }
  colores
}

