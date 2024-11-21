
# Type of variable --------------------------------------------------------

outtmp <- bind_rows(
  dict %>% select(type_var,level,name) %>%
    filter(name %in% vars_exp_lrt) %>% group_by(type_var) %>%
    summarise(value = n()/length(vars_exp_lrt),
           group = "LRT \n(n=10 selected)", sorted=2),
  dict %>% select(type_var,level,name) %>%
    filter(name %in% exp_vars) %>% group_by(type_var) %>%
    summarise(value = n()/length(exp_vars),
           group = "BVS \n(n=10 selected)", sorted=1),
)

outtmp <- outtmp %>% group_by(group,type_var,sorted) %>% summarise(value = sum(value))
outtmp$group <- stats::reorder(outtmp$group, outtmp$sorted) #sort df

p1 <- ggplot(outtmp, aes(x = group, y = value, fill = type_var)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(value,2), group = type_var),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Type of predictors",fill="Variable type")+
  xlab("")+
  ylab("Proportion") +
  theme_base() +
  scale_fill_manual(values= c("#F8766D","#A3A500","#00B0F6","#E76BF3"))+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right",
        legend.text.align = 0,
        title = element_text(size = 12))

# Level of variable --------------------------------------------------------

outtmp <- bind_rows(
  dict %>% select(type_var,level,name,sort) %>%
    filter(name %in% vars_exp_lrt) %>% group_by(level) %>%
    summarise(value = n()/length(vars_exp_lrt),sort=mean(sort),
              group = "LRT \n(n=10 selected)", sorted=2),
  dict %>% select(type_var,level,name,sort) %>%
    filter(name %in% exp_vars) %>% group_by(level) %>%
    summarise(value = n()/length(exp_vars),sort=mean(sort),
              group = "BVS \n(n=10 selected)", sorted=1),
)

# sort levels and groups
outtmp <- outtmp %>% group_by(group,level,sorted) %>% summarise(value = sum(value),
                                                                sort = mean(sort))
outtmp$level <- stats::reorder(outtmp$level, outtmp$sort)
outtmp$group <- stats::reorder(outtmp$group, outtmp$sorted)

p2 <- ggplot(outtmp, aes(x = group, y = value, fill = level)) +
  geom_bar(stat = "identity", width = .7) +
  geom_text(aes(label = round(value,2), group = level),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Level of predictors",fill="Levels")+
  xlab("")+
  ylab("Proportion") +
  theme_base() +
  scale_fill_manual(values= c("#FF8C00", "#CDC0B0", "#CD6090"))+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.text.align = 0,
        legend.position = "right",
        title = element_text(size = 12))


s_fig14b <- plot_grid(p1,
                   align = 'hv',
                   ncol=2,
                   nrow=2,
                   rel_widths = c(.8,.4))


s_fig14a <- plot_grid(p1 + theme(legend.position = "none"),
                   p2 + theme(legend.position = "none"),
                   align = 'hv',
                   ncol=2,
                   labels = c("A","B"))

s_fig14 <- plot_grid(p1,
                     p2,
                 align = 'hv',
                 ncol=2)

ggsave(filename ="out/s_figs/s_fig14.png",plot=s_fig14,device = png, type = "cairo",
       width = 30,height = 15,units="cm",dpi = 400)



