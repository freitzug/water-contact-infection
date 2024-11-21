#Author: Fabian Reitzug
#Date: 2022-06-24
#Purpose: prepare hh and clinical data

# Notes: df_techu has five extra exams (were not of final participants)

# Get paths ----------------------------------------------------------------
source("path/path.R") #paths


# Load packages -----------------------------------------------------------
.libPaths(paste0(path.git,"library"))

library(ggpubr)
library(Rmisc)
library(readr)
library(stringr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(table1)
library(tableone)
library(jstable)
library(flextable)
library(officer)

# Source scripts ----------------------------------------------------------
source("prep/prep_hh+clinical.R")

# Read data ---------------------------------------------------------------
dict <- read.csv("dict/dict.csv")

#sort df by variable level
dict$level_cat <- as.factor(dict$level)
levels(dict$level_cat) <- c(4,2,1,3); dict$level_cat <- as.numeric(as.character(dict$level_cat))

dict$type_var <- as.factor(dict$type_var)
levels(dict$type_var) <- c(5,4,0,2,1,3); dict$type_var <- as.numeric(as.character(dict$type_var))
dict %<>% dplyr::arrange(type_var,level_cat)

dict %<>% filter(raw.variables=="No") %>% filter(!name %in% c("time_spent_water","trips_to_water","tbd","n_lact_hh",
                                                              "n_lact_ind","mem_pregnant","tribe","yearsvillage_ind"))

#join df's
#df %<>% left_join(df_lact,by="hh_mem_id")

#apply transformation
noms <- dict$name
df %<>% select(c("barcode",noms))


# indented code to check if length of levels and labels is same
#l1 <- rep(0,length(noms))
#l2 <- rep(0,length(noms))
noms_mut <- noms[nchar(dict$labels)>0]
for(i in 1:length(noms_mut)){
  df[,noms_mut[i]] <- as.factor(df[,noms_mut[i]])
  levels(df[,noms_mut[i]]) <- str_split(dict$labels[which(dict$name==noms_mut[i])],",")[[1]]
  #l1[i] <- length(levels(df[,noms_mut[i]]))
  #l2[i] <- length(str_split(dict$labels[which(dict$name==noms_mut[i])],",")[[1]])
}
#View(cbind.data.frame(l1,l2,(l1-l2)))

#apply var name labels
for(i in 1:length(noms)){
  label(df[,noms[i]]) <- dict$variable[which(dict$name==noms[i])]
}

rm_space <- function(x){ #rm space
  x <- str_replace_all(x,"\\( ","(")
  return(x)
}

t1 <- CreateTableOne2(
  noms[!noms %in% c("kk_yes_no","tribe","poc_cca_yes_nop","poc_cca_yes_nom","district","sma")], #select names
  strata = "kk_yes_no",data=df,Labels = T,showAllLevels =F)
t1[,2:ncol(t1)] <- apply(t1[,2:ncol(t1)],2,FUN=rm_space)
write.csv(t1,paste0(path.git,"tabs/table1_kk.csv")) #need to save fist to get df
t1 <- read.csv(paste0(path.git,"tabs/table1_kk.csv")) %>% select(!c(test,sig))
names(t1) <- c("n (%) or mean (SD)",paste0("positive (n=",t1[1,2],")"),paste0("negative (n=",t1[1,3],")"),"p-value")
t1 <- t1[-1,]
write.csv(t1,paste0(path.git,"tabs/table1_kk.csv")) #overwrite

t1 %<>% flextable(cwidth = 1.5) %>% theme_booktabs()
t1 %<>% set_table_properties(layout = "fixed", width = 0) %>% add_header_lines("Characteristics of respondents by KK infection status")
t1 %<>% fontsize(12) %>% font(fontname = "Times New Roman",part='all')
t1

t2 <- CreateTableOne2(
  noms[!noms %in% c("district")], #select names
  strata = "district",data=df,Labels = T,showAllLevels =F)
t2[,2:ncol(t2)] <- apply(t2[,2:ncol(t2)],2,FUN=rm_space)
write.csv(t2,paste0(path.git,"tabs/table1_dis.csv")) #need to save fist to get df
t2 <- read.csv(paste0(path.git,"tabs/table1_dis.csv")) %>% select(!c(test,sig))
names(t2) <- c("n (%) or mean (SD)",paste0("Mayuge (n=",t2[1,2],")"),
                                    paste0("Buliisa (n=",t2[1,3],")"),
                                    paste0("Pakwach (n=",t2[1,4],")"),"p-value")
t2 <- t2[-1,]
write.csv(t2,paste0(path.git,"tabs/table1_dis.csv")) #overwrite

t2 %<>% flextable(cwidth = 1.5) %>% theme_booktabs()
t2 %<>% set_table_properties() %>% add_header_lines("Characteristics of respondents by district")
t2

save_as_docx("Differences between KK positive and negative respondents"=t1, path=paste0(path.git,"tabs/table1.docx"))













