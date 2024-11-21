
noms <- dict$name
df0 <- df #save unlabelled dataframe for GAMs
df <- as.data.frame(df) #as labeller does not work with tibble

noms_mut <- noms[nchar(dict$labels)>0 & dict$type!="Binary"]
for(i in 1:length(noms_mut)){
  df[,noms_mut[i]] <- as.factor(df[,noms_mut[i]])
  levels(df[,noms_mut[i]]) <- str_split(dict$labels[which(dict$name==noms_mut[i])],",")[[1]]
}

#apply var name labels
for(i in 1:length(noms)){
  Hmisc::label(df[,noms[i]]) <- dict$variable[which(dict$name==noms[i])]
}

rm_space <- function(x){ #rm space
  x <- str_replace_all(x,"\\( ","(")
  return(x)
}



