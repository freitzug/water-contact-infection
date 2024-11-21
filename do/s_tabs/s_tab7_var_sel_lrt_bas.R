
#note: vars_exp_lrt and vars_inf_lrt generated in ROC script in main figures

t <- dict %>% arrange(sort) %>% select(variable,name) %>% mutate(
                "Infection status (LRT)" = if_else(name %in% vars_inf_lrt,"X", "-"),
                "Infection status (BAS)" = if_else(name %in% inf_vars,"X", "-"),
                "Water contact (LRT)" = if_else(name %in% vars_exp_lrt,"X", "-"),
                "Water contact (BAS)" = if_else(name %in% exp_vars,"X", "-")
                ) %>%  
      filter_at(vars("Infection status (LRT)","Infection status (BAS)",
               "Water contact (LRT)", "Water contact (BAS)"),
               any_vars(. =="X")) %>%
      select(-name)

write.csv(t,"out/s_tabs/tab_7.csv",row.names = F)

t %>% kable('latex', booktabs = T, longtable=T, label = "selection",
             caption = "Comparison of variable selection between LRTs and BVS" ) %>%
  column_spec(1, width = "6cm") %>% #adj col width
  column_spec(2:ncol(t), width = "1.5cm") %>%
  kable_styling(latex_options = c("striped", "hold_position", "repeat_header")) %>%
  footnote(general = "Comparison of variable sets selected using likelihood ratio tests (LRTs) at pr <0.05 with the variable sets selected via Bayesian variable selection (BVS) with marginal inclusion probabilities p >= 0.5 (i.e., variables included in the median probability model).",
           threeparttable = T) %>%
  save_kable(file = "out/s_tabs/tab_7.tex")

rm(t)
