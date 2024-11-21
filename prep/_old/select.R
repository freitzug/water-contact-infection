
#Author: Fabian Reitzug
#Date: 2022-07-19
#Purpose: variable selection

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
library(lmtest)
library(multiwayvcov)
library(table1)

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

dict %<>% filter(!name %in% c("time_spent_water","trips_to_water","tbd"))

noms <- dict$name
df %<>% select(c("barcode",noms))

for(i in 1:length(noms)){
  label(df[,noms[i]]) <- dict$variable[which(dict$name==noms[i])]
}

noms_mut <- noms[nchar(dict$labels)>0]
for(i in 1:length(noms_mut)){
  df[,noms_mut[i]] <- as.factor(df[,noms_mut[i]])
  levels(df[,noms_mut[i]]) <- str_split(dict$labels[which(dict$name==noms_mut[i])],",")[[1]]
}

#implement lr tests: select ind. var an dep. vars for lr tests
dvar <- dict$name[dict$uga_vars=="Yes"]
names <- dict$variable[dict$name %in% dvar]
ivar <- "kk_yes_no"

m0 <- glm(paste0(ivar,  "~ 1"), data = df, family = binomial("logit")) #null model
p_val <- rep(NA,length(dvar))
coeff <- rep(NA,length(dvar))
ci_mat <- matrix(ncol=2,nrow=length(dvar),data=NA) %>% as.data.frame()
names(ci_mat) <- c("ci.lb","ci.ub")
for(i in 1:length(dvar)){
  m1 <- glm(paste0(ivar,  "~",dvar[[i]]), data = df, family = binomial("logit"))
  p_val[i] <- lrtest(m0,m1)$`Pr(>Chisq)`[[2]] %>% round(3)
  #vcov1<- cluster.vcov(m1, factor(df$hh_id)) #post-estimation commands to get robust clustered SEs
  #coeff[i] <- exp(m1$coefficients[[2]])
  #ci_mat[i,] <- exp(as.matrix.data.frame(coefci(m1, vcov = vcov1, cluster = ~hh_id)))[2,]
}

lrtest.out <- cbind.data.frame(dvar,names, p_val)#,coeff,ci_mat)
sel <- lrtest.out$dvar[lrtest.out$p_val<0.05]

# Run adjusted model with selected covariates
m2 <- glm(paste0(ivar,  "~",paste0(sel,collapse = " + ")), data = df, family = binomial("logit"))
vcov2<- cluster.vcov(m2, factor(df$hh_id)) #post-estimation commands to get robust clustered SEs
coeff <- coeftest(m2, vcov = vcov2)[,1] %>% exp() %>% round(2)
ci_mat <- exp(as.matrix.data.frame(coefci(m2, vcov = vcov2, cluster = ~hh_id))) %>% round(2)
names <- dict$variable[dict$name %in% sel]
mod.out <- cbind.data.frame(coeff,ci_mat)
names(mod.out) <- c("coeff","ci.lb","ci.ub")
mod.out$names <- rownames(mod.out)
mod.out$ord <- 
#forest plot
ggplot(data=mod.out, aes(x=names, y=coeff, ymin=ci.lb, ymax=ci.ub)) +
  geom_pointrange() +
  geom_hline(yintercept=1, lty=2) +
  geom_hline(yintercept=4, lty=1) +
  scale_y_continuous(breaks =c(0,0.5,1,2,3,4))+
  scale_x_discrete(expand = c(-.1,3))+
  coord_flip(xlim=c(0,23),ylim=c(0,5))+
  labs(y="Odds ratio",x="",title="Predictors of Schistosoma infection status (based on \nstool microscopy)") +
  geom_text(aes(label = paste0(round(coeff,2)," [",round(ci.lb,2), ", ",round(ci.ub,2),"]"), y=4.6, vjust = 1))+
  geom_text(aes(x=23, y=4.6,label="Odds ratio \n[95% Confidence Interval]"))+
  theme_classic(base_size = 20)

df %>% group_by(occupation) %>% summarise(length(kk_yes_no[kk_yes_no=="negative"])/n())

