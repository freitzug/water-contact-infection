

require(stringi)
require(stringr)
require(dplyr)

odk_clean <- function(df){
  
  out1 <- str_replace_all(names(df),"_","0") %>% str_replace_all("[:digit:]{2,}","_") %>% 
    str_split("_{1}",simplify = TRUE)
  
  col.nom <- rep(0,nrow(out1))
  for(i in 1:ncol(df)){ #get non-empty, last part of string
    vec <- max(which(nchar(out1[i,])>0))
    col.nom[i] <- out1[i,vec] %>% as.character() %>% str_replace_all("0","_")
  }
  
  #remove digits (just inserted) to get rid of first part
  if(any(str_detect(col.nom,"hh_rname" ))){
    ind <- which(!is.na(str_match( col.nom,"hh_rname" )))
    col.nom[ind] <- names(df)[ind]
  }
  
  #remove first parts of strings containing dots
  start <- stri_locate_last(col.nom,regex=c('\\.'))[,1]
  end <- nchar(col.nom)
  col.nom <- ifelse(!is.na(start),substr(col.nom,start+1,end),col.nom)
  col.nom <- col.nom %>% make.names(unique = TRUE) %>% str_to_lower()
  
  return(col.nom)
}



