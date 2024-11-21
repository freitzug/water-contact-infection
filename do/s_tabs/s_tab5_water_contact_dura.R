
# Duration ----------------------------------------------------------------

#dplyr::select vars for table 3
noms <- paste0(c(occ,dom,rec),"_time_spent")

#restrict df to dplyr::selected vars
dft <- df %>% dplyr::select(all_of(c(noms,"gender")))
dft[,-ncol(dft)] <- dft[,-ncol(dft)]*dft0 #mask df

#create table
t <- CreateTableOne2(
  vars=noms , #dplyr::select names
  nonnormal = noms,
  contDigits = 1,
  pDigits = 2,
  strata = "gender",data=dft,Labels = T,showAllLevels =F)

#Manually create overall column
overall <- summarise_all(dft[,noms], funs(quantile(., c(0.25, 0.5, 0.75), na.rm = TRUE)))
overall <- format(overall, nsmall = 1, digits=1,trim = F)
overall <- apply(overall, 2, function(x) {
  paste0(x[2], " [", x[1], ", ", x[3], "]")
})

#format variable column
rownames <- rownames(t) %>% unlist()
t %<>% as.data.frame(); rownames(t) <- NULL
t$variable <- rownames
t$overall <- c(NA,overall) #add overall column
t %<>% relocate(variable,overall) %>% dplyr::select(-c(test,sig)) %>% rename("p-value" = p)
t[,2:ncol(t)] <- apply(t[,2:ncol(t)],2,FUN=rm_space)
t <- t[-1,]; t <- arrange(t,desc(overall))
names(t) <- str_to_sentence(names(t))

#write output
write.csv(t,"out/s_tabs/tab_5.csv",row.names = F)

t %>% kable('latex', booktabs = T, longtable=T, 
             caption = "Water contact duration per activity (in hrs per week)" ) %>%
  column_spec(1, width = "8cm") %>% #adj col width
  column_spec(2:ncol(t), width = "2cm") %>%
  kable_styling(latex_options = c("striped", "hold_position", "repeat_header")) %>%
  footnote(general="Median and interquartile range (median [IQR]) are shown. Kruskal-Wallis Rank Sum Test were performed to test for group differences.",
           threeparttable = T) %>%
  save_kable(file = "out/s_tabs/tab_5.tex")

rm(t)
