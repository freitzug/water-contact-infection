
# LRTs for water contact variables -------------------------------------------------

#NOT RUN- only runs on confidential raw data

# df_lrt_lact_ind <- data.frame()
# null_mod <- glm("lact_ind ~ village_ref",  data=df,family = "binomial")
# for(i in 1:length(exp_cand)){
#   mod1 <- glm(paste0("lact_ind ~",exp_cand[i], "+ village_ref"), data=df,family = "binomial")
#   lr_out <- as_tibble(lrtest(null_mod,mod1)[2,]); lr_out %<>% mutate(var = exp_cand[i],prob = `Pr(>Chisq)`, out = "lact_ind")
#   df_lrt_lact_ind <- rbind.data.frame(df_lrt_lact_ind,lr_out)
# }
#
# # LRTs for infection outcomes ---------------------------------------------------------
#
# df_lrt_kk <- data.frame()
# null_mod <- glm("kk_yes_no ~ village_ref",  data=df,family = "binomial")
# for(i in 1:length(inf_cand)){
#   mod1 <- glm(paste0("kk_yes_no ~",inf_cand[i], "+ village_ref"), data=df,family = "binomial")
#   lr_out <- as_tibble(lrtest(null_mod,mod1)[2,]); lr_out %<>% mutate(var = inf_cand[i],prob = `Pr(>Chisq)`, out = "kk_yes_no")
#   df_lrt_kk <- rbind.data.frame(df_lrt_kk,lr_out)
# }
#
# #lrt vars
# write.csv(df_lrt_kk ,"out/var_sel/inf_sel_lrt.csv",row.names = F)
# write.csv(df_lrt_lact_ind ,"out/var_sel/exp_sel_lrt.csv",row.names = F)

#load results of variable selection

df_lrt_kk <- read.csv("out/var_sel/inf_sel_lrt.csv")
df_lrt_lact_ind <- read.csv("out/var_sel/exp_sel_lrt.csv")

vars_exp_lrt <- df_lrt_lact_ind %>% filter(`Pr..Chisq.`<.05) %>% pull(var)
vars_inf_lrt <- df_lrt_kk %>% filter(`Pr..Chisq.`<.05) %>% pull(var)




