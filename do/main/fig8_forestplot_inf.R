
## Infection and heavy infection -------------------------------------------

# Run models
frm5 <-
  formula(
    paste("kk_yes_no ~",paste(c(tp(inf_vars),"district"), collapse = " + "))
  )

mod5 <- glm(frm5,
            data = df,
            family = 'binomial' )

frm5b <-
  formula(
    paste("kk_heavy ~",paste(c(tp(inf_vars),"district"), collapse = " + "))
  )

mod5b <- glm(frm5b,
             data = df,
             family = 'binomial')

# Format forest plot
out5 <- glm_forest_prep_clse(mod5,clust_var="hh_id",exp=T)
out5b <- glm_forest_prep_clse(mod5b,clust_var="hh_id",exp=T)


fig8 <- forest_fun(
  labs = c("infection","heavy infection"),
  h_line = 1,
  mini = -.5,
  maxi = 4.5,
  set_lims = c(0,1,2,3,4),
  modf = list(out5,out5b),
  clean_labs_df=dict_long
)

#VIFs for models ------
vif_mod5 <- vif(mod5)
vif_mod5b <- vif(mod5b)


# Extract the p-value for the F-test

ggsave(filename ="out/main/fig8.png",plot=fig8,device = png, type = "cairo",
       width = 31.5,height = 17,units="cm",dpi = 400)


