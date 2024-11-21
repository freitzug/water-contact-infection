


# ------------------------------------------------------------------------#
# EXPOSURE MODELS W GENDER REMOVED  ---------------------------------------
# ------------------------------------------------------------------------#

frm4b <- formula(paste("lact_ind ~", paste0(c(tp(exp_vars[exp_vars!="occupation_cat"]),"district"),  collapse = " + ")))
mod4b <- glm(frm4b,
            data = df,
            family = 'binomial' )

out4b <- glm_forest_prep_clse(mod4b,clust_var="hh_id",exp=T)

frm4c <- formula(paste("lact_ind ~", paste0(c(tp(exp_vars[exp_vars!="gender_cat"]),"district"),  collapse = " + ")))
mod4c <- glm(frm4c,
             data = df,
             family = 'binomial' )

out4c <- glm_forest_prep_clse(mod4c,clust_var="hh_id",exp=T)


fig4 <- forest_fun(
  labs = c("main model","occupation removed","gender removed"),
  x_trans="log",
  h_line = 1,
  maxi = 16,
  mini=0.15,
  set_lims = c(0.2, 0.5, 1, 2, 4,8),
  modf = list(out1,out4b,out4c), 
  clean_labs_df=dict_long
)
fig4
#VIFs for models ------
vif_mod4b <- vif(mod4b)

ggsave(filename ="out/s_figs/s_fig4.png",plot=fig4,device = png, type = "cairo", 
       width = 40,height = 24,units="cm",dpi = 400)



