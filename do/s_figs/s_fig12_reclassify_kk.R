
## KK infection and relassified KK infection status-------------------------------------------

### KK models

frm12 <-
  formula(
    paste("kk_yes_no_corr ~",paste(c(tp(inf_vars),"district"), collapse = " + "))
  )

mod12 <- glm(frm12,
            data = df,
            family = 'binomial' )

out12 <- glm_forest_prep_clse(mod12,clust_var="hh_id",exp=T)


s_fig12 <- forest_fun(base_s = 16,
  labs = c("infection","infection reclassified"),
  h_line = 1,
  mini = -.5,
  maxi = 6.5,
  set_lims = c(0,1,2,4,6),
  modf = list(out5,out12),
  clean_labs_df=dict_long
)

ggsave(filename ="out/s_figs/s_fig12.png",plot=s_fig12,device = png, type = "cairo", 
       width =43,height =28,units="cm",dpi = 400)




