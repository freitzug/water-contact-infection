
## Infection intensity model -------------------------------------------------------
frm10 <- formula(paste("sma ~", paste0(c(tp(inf_vars),"district"), collapse = " + ")))

# Test whether poisson, negative binomial or zero-inflated model most appropriate
mod01 <- glm(frm10,
               data = df, family = "poisson")

mod02 <- glm.nb(frm10,link="log",
                 data = df)

lrtest(mod01,mod02) #check poisson vs negative binomial -> neg bin better
check_zeroinflation(mod02, tolerance = 0.1) #check zero-inflation in negative binomial model

out10 <- glm_forest_prep_clse(mod02,clust_var="hh_id",exp=T)
s_fig10 <- forest_fun(
  modf = list(out10),
  rm_names = "Intercept",
  data = df,
  maxi = 3.5,
  set_lims = c(0,1, 2,3),
  clean_labs_df=dict_long
)

ggsave(filename ="out/s_figs/s_fig10.png",plot= s_fig10,device = png, type = "cairo", 
       width =23,height =16,units="cm",dpi = 400)
