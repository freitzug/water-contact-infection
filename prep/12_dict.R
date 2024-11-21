

dict %<>% filter(raw.variables=="No",ready=="Yes") #filter to relevant vars

# variable selection ------------------------------------------------------

#candidate vars for exposure
exp_cand <- dict %>% filter(type_var %in% c("Environmental","Biomedical","Socio-demographic","WASH") & outcome!="Outcome") %>%
  arrange(sort) %>%
  pull(name)

#candidate vars for infection
inf_cand <- dict %>% 
  filter((type_var %in% c("Environmental","Biomedical","Socio-demographic","WASH") | exposure=="Exposure") & outcome!="Outcome") %>%
  arrange(sort) %>%
  pull(name)

#additional candidate exposure vars for infection models with more exposure vars
exp_add_cand <- dict %>% 
  filter((type_var %in% c("Water contact")) & outcome!="Outcome") %>%
  arrange(sort) %>%
  pull(name)

#additional candidate exposure vars for infection models with more exposure vars
exp_add_snail <- dict %>% 
  filter((type_var %in% c("Malacology")) & outcome!="Outcome") %>%
  arrange(sort) %>%
  pull(name)

# Make long-form dict for use in forest plot labels ----------------------------
dict_long <- dict %>% select(variable,name,labels,sort,type_var,level) %>%
  tidyr::separate_rows(labels, sep = ",") %>%
  mutate(modlabs = paste0(name,labels))

#replace in dict_long for correct labelling of polynomial age
dict_long_bas <- dict_long
dict_long[dict_long=="age2"] <- "poly(age, 2, raw = TRUE)2"
dict_long[dict_long=="age"] <- "poly(age, 2, raw = TRUE)1"
