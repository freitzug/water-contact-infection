

## Frequency -------------------------------------------------------

frm6 <- formula(paste("lact_freq ~", paste0(c(tp(exp_vars),"district"), collapse = " + ")))

# Test whether poisson, negative binomial or zero-inflated model most appropriate
mod601 <- glm(frm6,
             data = df, family = "poisson")

mod602 <- glm.nb(frm6,link="log",
                data = df)

lrtest(mod601,mod602) #check poisson vs negative binomial -> neg bin better
check_zeroinflation(mod602, tolerance = 0.1) #check zero-inflation in negative binomial model

#use nb model
mod6 <- glm.nb(frm6,
      data = df %>% filter(lact_ind==1))

out6 <- glm_forest_prep_clse(mod6,clust_var="hh_id",exp=T)
s_fig6 <- forest_fun(
  modf = list(out6),
  rm_names = "Intercept",
  data = df,
  maxi = 3.5,
  set_lims = c(0, 1, 2, 3),
  clean_labs_df=dict_long
)

ggsave(filename ="out/s_figs/s_fig6.png",plot= s_fig6,device = png, type = "cairo", 
       width =23,height =18,units="cm",dpi = 400)




