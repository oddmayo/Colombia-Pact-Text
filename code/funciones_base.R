#### Funciones base para el preprocesamiento de texto.

##########
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

##########
RemoveStopwordsFromText <- function(texto, # texto
                                    swords # términos a remover
){
  sapply(texto, function(x){
    y <- strsplit(x," ",T)[[1]]
    paste(y[!(y%in%swords)],collapse=" ")
  }, USE.NAMES = FALSE
  )
}



