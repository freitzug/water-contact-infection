

#set number of iterations
#niter = 2*10^7

#NOT RUN- only runs on confidential raw data

# Exposure model ----------------------------------------------------------

# frm1 <- formula(paste("lact_ind ~", paste0(exp_cand, collapse = " + ")))
#
# exp_sel <- bas.glm(frm1,
#                    data = df,
#                    include.always = as.formula("~district"),
#                    family  = binomial(link = "logit"),
#                    method = "MCMC", #for large number of predictors
#                    MCMC.iterations = niter,
#                    update = TRUE, #use for large model space
#                    modelprior = uniform(), initprobs = "marg-eplogp",
#                    force.heredity = FALSE
# )
#
# #plot(exp_sel, ask = F, which = 2)
# #image(exp_sel, rotate = F)
#
# saveRDS(exp_sel,paste0(path.out, "exp_sel_bas.rds"))

# Infection model ----------------------------------------------------------

# frm2 <- formula(paste("kk_yes_no ~", paste0(inf_cand, collapse = " + ")))
#
# inf_sel <- bas.glm(frm2,
#                    data = df,
#                    include.always = as.formula("~district"),
#                    family  = binomial(link = "logit"),
#                    method = "MCMC", #for large number of predictors
#                    MCMC.iterations = niter,
#                    update = TRUE, #use for large model space
#                    modelprior = uniform(), initprobs = "marg-eplogp",
#                    force.heredity = FALSE
# )

#saveRDS(inf_sel,paste0(path.out, "inf_sel_bas.rds"))

#plot(inf_sel, ask = F, which = 2)
#image(inf_sel, rotate = F)

# format inclusion probabilities as data frames-----------------------------
exp_sel <- readRDS(paste0(path.out, "exp_sel_bas.rds"))
inf_sel <- readRDS(paste0(path.out, "inf_sel_bas.rds"))

#exp
exp_sel_df <- as.data.frame(as.data.frame(summary(exp_sel))) %>%
  select(1) %>%
  rename(value =`P(B != 0 | Y)`) %>% na.omit()
exp_sel_df$var <- rownames(exp_sel_df)
exp_sel_df <- exp_sel_df[order(-exp_sel_df$value), ]
exp_sel_df %<>% filter(var!="Intercept")
exp_sel_df <- left_join(exp_sel_df,dict_long_bas,by=c("var"="modlabs"))

#inf
inf_sel_df <- as.data.frame(as.data.frame(summary(inf_sel))) %>%
  select(1) %>%
  rename(value =`P(B != 0 | Y)`) %>% na.omit()
inf_sel_df$var <- rownames(inf_sel_df)
inf_sel_df <- inf_sel_df[order(-inf_sel_df$value), ]
inf_sel_df %<>% filter(var!="Intercept")
inf_sel_df <- left_join(inf_sel_df,dict_long_bas,by=c("var"="modlabs"))

#joint df
sel_df <- full_join(inf_sel_df,
                    exp_sel_df %>% select(value,var),
                    by="var") %>% relocate(var)
sel_df %<>% rename(infection = value.x, exposure = value.y)
sel_df[is.na(sel_df)] <- 0
sel_df$diff <- (sel_df$infection - sel_df$exposure)

#Join with label dict
exp_vars <- exp_sel_df %>% filter(value>=.5) %>% arrange(sort) %>%  pull(name) %>% na.omit() %>% unique()
inf_vars <- inf_sel_df %>% filter(value>=.5) %>% arrange(sort) %>%  pull(name) %>% na.omit() %>% unique()

# infection model with add. exposure covars for ROC -----------------------

# frm3 <- formula(paste("kk_yes_no ~", paste0(exp_add_cand, collapse = " + ")))
#
# inf_sel2 <- bas.glm(frm3,
#                    include.always = as.formula(paste0("~", paste0(c(inf_vars,"district"), collapse = " + "))),
#                    data = df,
#                    family  = binomial(link = "logit"),
#                    method = "MCMC", #for large number of predictors
#                    MCMC.iterations = niter,
#                    update = TRUE, #use for large model space
#                    modelprior = uniform(), initprobs = "marg-eplogp",
#                    force.heredity = FALSE
# )

#saveRDS(inf_sel2,paste0(path.out, "inf_sel_add_exp_bas.rds"))
inf_sel2 <- readRDS(paste0(path.out, "inf_sel_add_exp_bas.rds"))

#inf
inf_sel_df2 <- as.data.frame(as.data.frame(summary(inf_sel2))) %>%
  select(1) %>%
  rename(value =`P(B != 0 | Y)`) %>% na.omit()
inf_sel_df2$var <- rownames(inf_sel_df2)
inf_sel_df2 <- inf_sel_df2[order(-inf_sel_df2$value), ]
inf_sel_df2 %<>% filter(var!="Intercept")
inf_sel_df2 <- left_join(inf_sel_df2,dict_long_bas,by=c("var"="modlabs"))

#Join with label dict
inf_vars_exp_add <- inf_sel_df2 %>% filter(value>=.5) %>% arrange(sort) %>%  pull(name) %>% na.omit() %>% unique()

# infection model with add. snail covars for ROC -----------------------

# frm3 <- formula(paste("kk_yes_no ~", paste0(exp_add_snail, collapse = " + ")))
#
# inf_sel3 <- bas.glm(frm3,
#                     include.always = as.formula(paste0("~", paste0(c(inf_vars,"district"), collapse = " + "))),
#                     data = df,
#                     family  = binomial(link = "logit"),
#                     method = "MCMC", #for large number of predictors
#                     MCMC.iterations = niter,
#                     update = TRUE, #use for large model space
#                     modelprior = uniform(), initprobs = "marg-eplogp",
#                     force.heredity = FALSE
# )

#saveRDS(inf_sel3,paste0(path.out, "inf_sel_add_snail_bas.rds"))
inf_sel3 <- readRDS(paste0(path.out, "inf_sel_add_snail_bas.rds"))

#inf
inf_sel_df3 <- as.data.frame(as.data.frame(summary(inf_sel3))) %>%
  select(1) %>%
  rename(value =`P(B != 0 | Y)`) %>% na.omit()
inf_sel_df3$var <- rownames(inf_sel_df3)
inf_sel_df3 <- inf_sel_df3[order(-inf_sel_df3$value), ]
inf_sel_df3 %<>% filter(var!="Intercept")
inf_sel_df3 <- left_join(inf_sel_df3,dict_long_bas,by=c("var"="modlabs"))

#Join with label dict
inf_vars_snail_add <- inf_sel_df3 %>% filter(value>=.5) %>% arrange(sort) %>%  pull(name) %>% na.omit() %>% unique()

# Exposure model with add. snail covars for ROC -----------------------

# frm1b <- formula(paste("lact_ind ~", paste0(exp_add_snail, collapse = " + ")))
#
# exp_sel2 <- bas.glm(frm1b,
#                     include.always = as.formula(paste0("~", paste0(c(exp_vars,"district"), collapse = " + "))),
#                     data = df,
#                     family  = binomial(link = "logit"),
#                     method = "MCMC", #for large number of predictors
#                     MCMC.iterations = niter,
#                     update = TRUE, #use for large model space
#                     modelprior = uniform(), initprobs = "marg-eplogp",
#                     force.heredity = FALSE
# )

#saveRDS(exp_sel2,paste0(path.out, "exp_sel_add_snail_bas.rds"))
exp_sel2 <- readRDS(paste0(path.out, "exp_sel_add_snail_bas.rds"))

#inf
exp_sel_df2 <- as.data.frame(as.data.frame(summary(exp_sel2))) %>%
  select(1) %>%
  rename(value =`P(B != 0 | Y)`) %>% na.omit()
exp_sel_df2$var <- rownames(exp_sel_df2)
exp_sel_df2 <- exp_sel_df2[order(-exp_sel_df2$value), ]
exp_sel_df2 %<>% filter(var!="Intercept")
exp_sel_df2 <- left_join(exp_sel_df2,dict_long_bas,by=c("var"="modlabs"))

#Join with label dict
exp_vars_snail_add <- exp_sel_df2 %>% filter(value>=.5) %>% arrange(sort) %>%  pull(name) %>% na.omit() %>% unique()

#Write output
write.csv(inf_sel_df,"out/var_sel/inf_sel_bas.csv",row.names = F)
write.csv(exp_sel_df,"out/var_sel/exp_sel_bas.csv",row.names = F)
write.csv(inf_sel_df2,"out/var_sel/inf_sel_exp_add_bas.csv",row.names = F)
write.csv(inf_sel_df3,"out/var_sel/inf_sel_snail_add_bas.csv",row.names = F)

