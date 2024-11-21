

#select vars for summary table
noms <- sel_df %>% arrange(sort) %>% distinct(name,.keep_all = T) %>% pull(name)

#restrict df to selected vars
dft <- df %>% dplyr::select(all_of(c(noms,"lact_ind")))

#fun for cleaning up table
rm_space <- function(x){ #rm space
  x <- str_replace_all(x,"\\( ","(")
  return(x)
}

#create table
t <- CreateTableOne2(
  vars=noms, #select names
  factorVars= dict %>% filter(dict$name %in% noms,type=="Binary") %>% pull(name), #treat 0/1 vars as factors
  nonnormal = dict %>% filter(dict$name %in% noms,type!="Binary",type_var=="Water contact") %>% pull(name),
  contDigits = 1,
  pDigits = 2,
  testNonNormal = list(NULL),
  strata = "lact_ind",data=dft,Labels = T,showAllLevels =F)
#format variable column
rownames <- rownames(t) %>% unlist()
t %<>% as.data.frame(); rownames(t) <- NULL
t$variable <- rownames; t %<>% dplyr::relocate(variable) %>% dplyr::select(-c(test,sig))
t[,2:ncol(t)] <- apply(t[,2:ncol(t)],2,FUN=rm_space)
names(t) <- c("variable",
               paste0("no water contact (n=",t[1,2],")"),
               paste0("water contact    (n=",t[1,3],")"),"p-value")
t <- t[-1,]
rownames(t) <- NULL

#manually adjust columns
t$variable <- str_replace_all(t$variable,"= 1","")
t$`p-value` <- str_replace_all(t$`p-value`,"    NA","-")
t$`p-value`[str_detect(t[,2],"(0.0)")] <- "-"
t[,1] <- ifelse(str_detect(t[,2],"±"),paste0(t[,1]," (Mean ± SD)"),t[,1])

names(t) <- str_to_sentence(names(t))
write.csv(t,"out/s_tabs/tab_2.csv",row.names = F)

t  %>%
  kable('latex', booktabs = T, longtable=T, label = "pchar",
                 caption = "Key participant characteristics" ) %>%
  add_indent(which(str_detect(t$Variable, "\\(")==FALSE), level_of_indent = 2) %>%
  column_spec(1, width = "9.5cm") %>% #adj col width
  column_spec(2:4, width = "2.5cm") %>%
  column_spec(ncol(t), width = "1.5cm") %>%
  kable_styling(latex_options = c("striped", "hold_position", "repeat_header")) %>%
  footnote(general ="For binary variables, number and proportions [N (%)] are shown and Chi-squared tests were used to test for group differences. For all normally distributed continuous variables, mean and standard deviation (mean +/- sd) are shown and two sample t-tests were performed to test for group differences. For all non-normally distributed variables, median and interquartile range (median [IQR]) are shown and Kruskal-Wallis Rank Sum Test were performed to test for group differences. Abbreviations: HH = household. Prop. = proportion.",
           threeparttable=T) %>%
  save_kable(file = "out/s_tabs/tab_2.tex")

rm(t)
