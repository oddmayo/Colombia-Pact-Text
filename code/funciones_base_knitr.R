#### FUnciones base para la generación automática de los reportes
#### Se relacionan básicamente con organización de vectores, estilo y
#### graficación.

##########
recount <- function(x){
  names(x)[which(names(x)=="")] <- "NINGUNO"
  if(length(x)==1) return(x)
  ordo <- order(x,decreasing = TRUE)
  imas <- which(x>sum(x)/100)
  if(length(x)<6){
    return(x[ordo])
  } else if(length(imas)>=6){
    iout <- imas
  } else {
    iout <- ordo[1:5]
  }
  y <- c(x[iout],sum(x[-iout]))
  names(y)[length(iout)+1] <- paste("OTRO (", length(x)-length(iout), ")",sep="")
  return(sort(y,decreasing = TRUE))
}

##########
recount.con.pacientes <- function(datos,mi.variable){
  # La función recibe como input (1) la base de datos y (2) la variable en 
  # función de la cual se realizará el reconteo (y agrupamiento). En todos los
  # casos, la función retornará un dataframe con el conteo de la variable de
  # interés en la primera columna y el número de pacientes a quien corresponde
  # dicha frecuencia en la segunda columna
  
  # Creamos una tabla de frecuencias con los pacientes en las filas y la
  # variable en las columnas
  mi.tabla <- table(datos$ID_Paciente,mi.variable,useNA = "ifany")
  
  # Titulamos como "Ninguno" aquellas categorías no registradas
  colnames(mi.tabla)[which(is.na(colnames(mi.tabla)))] <- "NINGUNO"
  rownames(mi.tabla)[which(is.na(rownames(mi.tabla)))] <- "NINGUNO"
  
  mis.frecuencias <- colSums(mi.tabla) #Equivale a hacer table(mi.variable)
  
  # Si nos interesa el número de pacientes a los cuales corresponde la
  # frecuencia de la variable de interés, lo podemos calcular a partir de una
  # tabla que identifique si un paciente tiene al menos una observación. Por
  # ejemplo, si la variable de interés son prescripciones y se realizaron 3
  # prescripciones al paciente A, 0 al B y 5 al C, en la nueva tabla se tendrá
  # A=1, B=0 y C=1. Sumando, veremos que las 8 prescripciones corresponden a
  # solo 2 pacientes.
  mi.tabla <- ifelse(mi.tabla>0,1,0)
  mis.pacientes<-colSums(mi.tabla)
  
  #Juntamos las listas obtenidas
  mi.df<-data.frame("Variable"=mis.frecuencias, "Pacientes"=mis.pacientes)
  
  # Identificamos el orden en que debemos organizar la tabla, en función de la
  # variable de interés
  ordo <- order(mis.frecuencias,decreasing = TRUE)
  # La variable de interés tiene menos de 6 categorías, las mostramos todas. Si
  # no, identificamos aquellas que contribuyen en más del 1%. Si estas son más
  # de 5, guarda los índices de todas en el vector iout, y si son 5 o menos,
  # guarda las 5 con frecuencias más altas, incluso si contribuyen en menos del
  # 1%.
  imas <- which(mis.frecuencias>sum(mis.frecuencias)/100)
  if(length(mis.frecuencias)<6){
    return(mi.df[ordo,])
  } else if(length(imas)>=6){
    iout <- imas
  } else {
    iout <- ordo[1:5]
  }
  #Luego, se agrupan las variables "descartadas"
  mi.df.agrupado <- rbind(mi.df[iout,],colSums(mi.df[-iout,]))
  rownames(mi.df.agrupado)[length(iout)+1] <-
    paste("OTRO (",   length(mis.frecuencias)-length(iout), ")",sep="")
  ordo <- order(mi.df.agrupado$Variable,decreasing = TRUE) 
  return(mi.df.agrupado[ordo,])
}

##########
otro.barplot <- function(y){
  par(mar=c(4,ceiling(0.45*max(nchar(names(y)))+3.25),0,8)+.1,
      cex.axis=1.1)
  
  flagTR <- FALSE
  xl <- c(0,y[1])
  if(names(y)[1]=="NINGUNO" | substr(names(y)[1],1,4)=="OTRO"){
    flagTR <- TRUE
    xl[2] <- y[2]*1.5
  }
  
  par(lwd=4)
  p <- barplot(rev(y),xlim=xl,col="grey",las=1,horiz=TRUE,xpd=FALSE,border = "black",axes=FALSE)
  axis(side=1)
  title(xlab="Frecuencia",cex.lab=1.8)
  tt <- paste(y," (", sprintf("%.2f",y/sum(y)*100),"%)",sep="")
  if(flagTR){
    y[1] <- xl[2]
    tt[1] <- paste("...",tt[1])
  }
  text(y,rev(p),tt,pos=4,xpd=TRUE)
}

##########
otro.barplot2.0 <- function(z){
  y <- z$Variable
  ny <- ifelse(nchar(rownames(z))>50,paste(substr(rownames(z),1,50),"...",sep=""),rownames(z))
  y <- setNames(y,ny)
  par(mar=c(4.5,ceiling(0.45*max(nchar(names(y)))+5.05),0,14)+.1,
      cex.axis=1.1)
  
  flagTR <- FALSE
  xl <- c(0,y[1])
  if(names(y)[1]=="NINGUNO" | substr(names(y)[1],1,4)=="OTRO"){
    flagTR <- TRUE
    xl[2] <- y[2]*1.5
  }
  if((nrow(z)==0||nrow(z)==1)&z[1,1]==0)  y <- setnames(1,"NINGUNO")
  par(lwd=4)
  p <- barplot(rev(y), xlim=xl, col="grey", las=1, horiz=TRUE, xpd=FALSE,
               border="black", axes=FALSE)
  axis(side=1)
  title(xlab="Frecuencia",cex.lab=1.8)
  tt <- paste(form.numero(y), "Rx", " (",
              sprintf("%.2f",y/sum(y)*100), "%, ",
              form.numero(z$Pacientes),
              "P)", sep="")
  if(flagTR){
    y[1] <- xl[2]
    tt[1] <- paste("...",tt[1])
  }
  text(y,rev(p),tt,pos=4,xpd=TRUE)
}

##########
tabla <- function(datos,var_llave=''){
  key <- count_(datos,c('justificacion','ID_Medico',var_llave),sort = TRUE)
  llave <- key[which(key$ID_Medico %in% llave$ID_Medico),]
  # cambiar justificaciones limpias por originales
  pil <- NULL
  for (i in 1:nrow(llave)) {
    options(warn = -1)
    dfp <- datos[which(prescrps$justificacion==llave$justificacion[i]),]
    pil[i] <- dfp$JustificacionNoPBS
    options(warn = 1)}
  llave$justificacion <-c(tolower(pil))
  # lista del total del campo
  num <- NULL
  for (i in 1:length(medmas)) {
    dfp <- llave[which(llave$justificacion==justmas[[i]] & llave$ID_Medico==medmas[[i]]),]
    num[[i]] <- dfp
  }
  # columna del total
  numero <- NULL
  for (i in 1:length(medmas)) {
    dfp <- c(nrow(num[[i]]))
    numero[[i]] <- dfp
  }
  # lista de data frames con top 3
  top <- NULL
  for (i in 1:length(justmas)) {
    dfp <- data.frame(head(llave[which(llave$justificacion==justmas[[i]] & llave$ID_Medico==medmas[[i]]),],3))
    top[[i]] <- dfp
  }
  # Columna de cada top 3
  pegado <- NULL
  for (i in 1:length(justmas)) {
    dfp <- paste(top[[i]][[3]], top[[i]][[4]], sep = ' : ', collapse = c(", "))
    pegado[[i]] <- dfp
  }
  top3 <<- pegado
  total <<- numero
}

##########
form.numero <- function(a){
  format(a, big.mark = ".", decimal.mark = ",")
}

##########
form.decimal <- function(a, d){
  format(a, big.mark = ".", decimal.mark = ",", nsmall=d, digits=0)
}

##########
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

##########
proyTsneBoW <- function(B){
  require(FactoMineR)
  pc <- PCA(B, graph=F)
  PVC <- pc$var$coord
  ts2 <- tsne(X = PVC[,1:5], #initial_config = PVC[,1:2],
              k = 2, perplexity = 15, max_iter = 1000, whiten = FALSE,
              epoch = 10, min_cost=0.4)
}

##########
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
  
  text(t,colnames(B),cex=.5+2*sqrt(colSums(B)/max(colSums(B))))
}

##########
#Similitud de frases considerando longitud (salida booleana)
phrsim <- function(a0,b0){
  la <- length(a0)
  lb <- length(b0)
  hi <- floor((la-1)/2)
  lw <- la - floor((la-1)/4)
  lh <- la + floor((la-1)/3)
  if((lb<lw) | (lb>lh)) return(FALSE)
  mtch <- sum(a0 %in% b0)
  if(abs(la-mtch)<=hi) return(TRUE) else return(FALSE)
}

##########
#Lista de conteos de justificaciones similares
conteoJustsSimilares <- function(J){
  
  tjl <- sort(table(J),decreasing=TRUE)
  ntjl <- unname(sapply(names(tjl), function(x){
    unique(unlist(strsplit(x,split=" ")))
  }))
  
  n <- min(length(tjl),1000)
  
  justs <- rep("",n)
  conts <- rep(NA,n)
  indxs <- vector("list",n)
  
  i <- 1
  done <- FALSE
  while(!done){
    inic <- 1
    nntjl <- names(tjl)
    a0 <- ntjl[[inic]]
    a <- nntjl[inic]
    indic <- which(J==a)
    indiu <- inic
    for(j in (inic+1):length(tjl)){
      b0 <- ntjl[[j]]
      b <- nntjl[j]
      cc <- phrsim(a0, b0)
      if(cc){
        iwi <- which(J==b)
        indic <- c(indic, iwi)
        indiu <- c(indiu, j)
      }
    }
    justs[i] <- a
    conts[i] <- length(indic)
    indxs[[i]] <- sort(indic)
    
    tjl <- tjl[-indiu]
    ntjl <- ntjl[-indiu]
    
    if(length(tjl)==1) done <- TRUE
    i <- i+1
    print(i-1)
    print(length(tjl))
    if(i>n) done <- TRUE
  }
  
  ord <- order(conts, decreasing=TRUE)
  justs <- justs[ord]
  conts <- conts[ord]
  indxs <- indxs[ord]
  
  if(any(is.na(conts))){
    ii <- head(which(is.na(conts)),1)-1
    justs <- justs[1:ii]
    conts <- conts[1:ii]
    indxs <- indxs[1:ii]
  }
  
  return(list(js = justs, cs = conts, Ix = indxs))
}

##########
printhable <- function(x){
  p <- ""
  for(i in 1:length(x)){
    p <- paste(p,names(x)[i],": ",x[i],"Rx, ",sep="")
  }
  p <- substr(p, 1, nchar(p)-2)
  p <- paste(p,".",sep="")
}