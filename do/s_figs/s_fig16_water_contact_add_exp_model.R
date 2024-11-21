

## Any water contact + add wc variables -------------------------------------------------------

mod16 <- glm(frm6r,
            data = df,
            family = 'binomial' )

out16 <- glm_forest_prep_clse(mod16,clust_var="hh_id",exp=T)
fig16 <- forest_fun(
  modf = list(out16),
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
vif_mod16 <- vif(mod16)

ggsave(filename ="out/s_figs/s_fig16.png",plot=fig16,device = png, type = "cairo", 
       width = 21,height = 20,units="cm",dpi = 400)
