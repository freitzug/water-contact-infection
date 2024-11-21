

## Any water contact + add snail variables -------------------------------------------------------

mod15 <- glm(frm6r2,
            data = df,
            family = 'binomial' )

out15 <- glm_forest_prep_clse(mod15,clust_var="hh_id",exp=T)
fig15 <- forest_fun(
  modf = list(out15),
  rm_names = c("Intercept"),
  data = df,
  x_trans="log",
  h_line = 1,
  maxi = 4,
  mini=0.15,
  set_lims = c(0.2, 0.5, 1, 2, 4),
  clean_labs_df=dict_long
)

#VIF for models ------
vif_mod15 <- vif(mod15)

ggsave(filename ="out/s_figs/s_fig15.png",plot=fig15,device = png, type = "cairo", 
       width = 21,height = 18,units="cm",dpi = 400)
