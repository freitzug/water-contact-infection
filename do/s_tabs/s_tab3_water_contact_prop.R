

# Proportion engaging in activity ---------------------------------------------------------------

#def
occ <- c("collecting_papyrus",
         "fishing",
         "fishmongering",
         "collecting_shells")
rec <- c("swimming_or_plays")
dom <-  c("getting_drinking_water",
          "washing_clothes_without_soap",
          "bathing_with_soap",
          "bathing_without_soap",
          "washing_jerry_cans_or_other_household_items",
          "washing_clothes_with_soap")


#needed to mask dft to respondents doing that activity
noms0 <- c(occ,dom,rec)
dft0 <- df %>% dplyr::select(all_of(c(noms0))) #restrict to ppl wit water contact
dft0[dft0==0] <- NA

#dplyr::select vars for table 2
noms <- paste0(c(occ,dom,rec))

#restrict df to dplyr::selected vars
dft <- df %>% dplyr::select(all_of(c(noms,"gender")))
#dft[,-ncol(dft)] <- dft[,-ncol(dft)]*dft0 #mask df

#create table
t <- CreateTableOne2(
  vars=noms ,
  factorVars = noms,
  strata = "gender",data=dft,Labels = T,showAllLevels =F)
overall <- CreateTableOne( #separate model needed because not implemented in CreateTableOne2
  vars=noms , #dplyr::select names
  strata = "gender",
  factorVars = noms,
  data=dft,T,
  addOverall=T) %>% print() %>% as.data.frame() # %>% dplyr::select()
#format variable column
rownames <- rownames(t) %>% unlist()
t %<>% as.data.frame(); rownames(t) <- NULL
t$variable <- rownames
t$overall <- unlist(overall$Overall) #add overall column
t %<>% relocate(variable,overall) %>% dplyr::select(-c(test,sig)) %>% rename("p-value" = p)
t[,2:ncol(t)] <- apply(t[,2:ncol(t)],2,FUN=rm_space)
t <- t[-1,]; t <- arrange(t,desc(overall))
#manually adjust columns
t$variable <- str_replace_all(t$variable,"= 1","")
names(t) <- str_to_sentence(names(t))
#write output
write.csv(t,"out/s_tabs/tab_3.csv",row.names = F)

t %>% kable('latex', booktabs = T, longtable=T,
             caption = "Water contact per activity" ) %>%
  column_spec(1, width = "8cm") %>% #adj col width
  column_spec(2:ncol(t), width = "2cm") %>%
  kable_styling(latex_options = c("striped", "hold_position", "repeat_header")) %>%
  footnote(general="Number and percentage of participants with water contact [n (%)] per activity. Chi-squared tests were used to test for group differences.",
           threeparttable = T) %>%
  save_kable(file = "out/s_tabs/tab_3.tex")

rm(t)
