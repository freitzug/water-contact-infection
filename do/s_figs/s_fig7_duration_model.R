
## Duration -------------------------------------------

s_frm7 <- formula(paste("lact_hrs ~", paste0(exp_vars, collapse = " + ")))

# Test whether poisson, negative binomial or zero-inflated model most appropriate
s_mod701 <- glm(s_frm7,
              data = df, family = "poisson")

s_mod702 <- glm.nb(s_frm7,link="log",
                 data = df)

lrtest(s_mod701,s_mod702) #check poisson vs negative binomial -> neg bin better
check_zeroinflation(s_mod702, tolerance = 0.1) #check zero-inflation in negative binomial model

#use nb model
s_mod7 <- glm.nb(s_frm7,
                 data = df  %>% 
                   mutate(lact_hrs = as.integer(lact_hrs)))

s_out7 <- glm_forest_prep_clse(s_mod7,clust_var="hh_id",exp=T)
s_fig7 <- forest_fun(
  modf = list(s_out7),
  rm_names = "Intercept",
  data = df,
  maxi = 3.5,
  set_lims = c(0, 1, 2, 3),
  clean_labs_df=dict_long
)

ggsave(filename ="out/s_figs/s_fig7.png",plot=s_fig7,device = png, type = "cairo", 
       width =23,height =18,units="cm",dpi = 400)
