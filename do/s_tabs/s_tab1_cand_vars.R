
var_set <- unique(c(inf_cand,exp_cand))
t <- dict %>% filter(name %in% var_set) %>% arrange(sort,type_var) %>%
  select(name,variable,definition,type,levels=labels,
         "type of variable"=type_var,
         "level of variable"=level) %>% mutate(levels = str_replace_all(levels,",",", "),
         "candidate variable for water contact" = if_else(name %in% exp_cand, "X","-" ),
         "candidate variable for infection" = if_else(name %in% inf_cand, "X","-" )   
                                               ) %>% select(!name)
names(t) <- str_to_sentence(names(t))
write.csv(t,"out/s_tabs/tab_1.csv",row.names = F)

t %>% kable('latex', booktabs = T, longtable=T,
             caption = "Variable definitions" ) %>%
  column_spec(c(1,2,4), width = "4cm")  %>%
  column_spec(6:ncol(t), width = "1.5cm") %>% #adj col width
  kable_styling(latex_options = c("striped", "hold_position", "repeat_header")) %>%
  footnote(general = "Abbreviations: HH = household. Ind.-level = individual-level. Vill.-level = village-level. KK = Kato-Katz stool microscopy. Prop. = proportion. WASH = Water, sanitation, and hygiene.",
           threeparttable = T) %>%
  save_kable(file = "out/s_tabs/tab_1.tex")
  
rm(t)  
