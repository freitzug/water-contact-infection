
## Type of activity

#restrict to people with only one lact type
df$nact <- df %>% dplyr::select("lact_ind_dom","lact_ind_occ","lact_ind_rec") %>% rowSums()

frm5a <- formula(paste("lact_ind_occ ~", paste0(c(tp(exp_vars),"district"), collapse = " + ")))
frm5b <- formula(paste("lact_ind_dom ~", paste0(c(tp(exp_vars),"district"), collapse = " + ")))
frm5c <- formula(paste("lact_ind_rec ~", paste0(c(tp(exp_vars[exp_vars!="site_type" & exp_vars!="occupation_cat"]),"district"), collapse = " + ")))

mod5a <- glm(frm5a,
             data = df %>% filter(nact < 2),
             family="binomial")

mod5b <- glm(frm5b,
             data = df %>% filter(nact < 2),
             family="binomial")

mod5c <- glm(frm5c,
             data = df %>% filter(nact < 2),
             family="binomial")

out5a <- glm_forest_prep_clse(mod5a,clust_var="hh_id",exp=T)
out5b <- glm_forest_prep_clse(mod5b,clust_var="hh_id",exp=T)
out5c <- glm_forest_prep_clse(mod5c,clust_var="hh_id",exp=T)

s_fig5 <- forest_fun(base_s = 16,
  modf = list(out5a,out5b,out5c),
  rm_names = "Intercept",
  x_trans="log",
  data = df,
  labs = c("Occupational","Domestic","Recreational"),
  mini = 0.1,
  maxi = 65,
  set_lims = c(0.2, 0.5,1, 2, 4, 8,16,32,64),
  clean_labs_df=dict_long
)

ggsave(filename ="out/s_figs/s_fig5.png",plot=s_fig5,device = png, type = "cairo",
       width =47,height =26,units="cm",dpi = 200)







