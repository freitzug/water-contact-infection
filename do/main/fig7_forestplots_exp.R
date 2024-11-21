
## Any water contact -------------------------------------------------------

# Run model
frm1 <- formula(paste("lact_ind ~", paste0(c(tp(exp_vars),"district"),  collapse = " + ")))
mod1 <- glm(frm1,
      data = df,
      family = 'binomial' )

# Format forest plot
out1 <- glm_forest_prep_clse(mod1,clust_var="hh_id",exp=T)
fig7 <- forest_fun(
  modf = list(out1),
  rm_names = c("Intercept"),
  data = df,
  x_trans="log",
  h_line = 1,
  maxi = 20,
  mini=0.15,
  set_lims = c(0.2, 0.5, 1, 2, 4,8,16),
  clean_labs_df=dict_long
)

#VIF for models ------
vif_mod1 <- vif(mod1)

ggsave(filename ="out/main/fig7.png",plot=fig7,device = png, type = "cairo",
       width = 20,height = 18,units="cm",dpi = 400)

