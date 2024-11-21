
# Create a joint data frame with direct obs and survey data ---------------

#prepare df for merging: make comparable to direct obs
dftmp <- df[rep(seq_len(nrow(df)), times = df$nlact_ind), ] #create df repeating ind. by nr of wc activities so same structure as observation data
dftmp %<>% mutate(age_cat3 = case_when(
  age < 1 ~ 1,
  age > 0 & age < 5 ~ 2,
  age > 4 & age < 10 ~ 3,
  age > 9 & age < 16 ~ 4,
  age > 15 & age < 20 ~ 5,
  age > 19  ~ 6))

dftmp <- dftmp[dftmp$village_ref %in% c(unique(wctmp$village_ref)),] #filter to villages in obs data

# Bootstrap ---------------------------------------------------------------

# Function to perform sampling, aggregation, and merging
# Number of iterations
num_iterations <- 1000

bootstrap_iteration <- function(df, niter =num_iterations ) {
  df %>%
    sample_n(size = niter, replace = TRUE) %>%
    group_by(village_ref, age_cat3) %>%
    summarise(n = n()) %>%
    mutate(proportion = n / sum(n),
           N = n / proportion %>% as.double())
}



# List to store results
results_list <- map(1:num_iterations, ~bootstrap_iteration(dftmp))
results_list2 <- map(1:num_iterations, ~bootstrap_iteration(wctmp))

# Combine the list of dataframes into a single dataframe
dfwctmp <- bind_rows(
  bind_rows(results_list, .id = "id") %>% mutate(data = "survey"),
  bind_rows(results_list2, .id = "id") %>% mutate(data = "observation")
) %>% mutate(data = as.factor(data))

#get 95% CIs via SE method
dfwctmp$se <- sqrt((dfwctmp$proportion * (1 - dfwctmp$proportion)) / dfwctmp$N) #calc SEs

#calc lci and uci
dfwc <- dfwctmp %>% group_by(age_cat3=as.factor(age_cat3),data) %>%
  summarise(fit = mean(proportion),
            lci = fit - 1.96 * mean(se),
            uci = fit + 1.96 * mean(se)
  )

#correct sorting of df
levels(dfwc$age_cat3) <- c("5-9","10-15","16-19","20+")
dfwc$sort <- as.numeric(dfwc$age_cat3)

# Create the bar plot with error bars
s_fig11 <- ggplot(dfwc, aes(x = reorder(age_cat3,sort), y = fit,group=data,fill=data)) +
  geom_bar(position = "dodge",stat="identity") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4,
                position = position_dodge(0.9), color = "grey30",linewidth=.5) +
  xlab("Age group") + ylab("Proportion")+
  ylim(0,1)+
  scale_fill_manual(values = c("survey" = "#CD6090", "observation" = "#9999FF")) +
  theme_base() +
  theme(legend.title = element_blank())


ggsave(filename ="out/s_figs/s_fig11.png",plot=s_fig11,device = png, type = "cairo",
       width =20,height =14,units="cm",dpi = 400)
