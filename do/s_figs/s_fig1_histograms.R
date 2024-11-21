


p1 <- ggplot(df, aes(x = lact_freq)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(
       x = "Frequency (times per week)",
       y = "Number of participants") +
  geom_vline(xintercept = median(df$lact_freq), linetype = "dashed", color = "black", size = 1)+
         theme_base()

p2 <-  ggplot(df, aes(x = lact_hrs)) +
  geom_histogram(binwidth = 1, fill = "darkblue", alpha = 0.7) +
  labs(
       x = "Duration (hours per week)",
       y = "Number of participants") +
  geom_vline(xintercept = median(df$lact_hrs), linetype = "dashed", color = "black", size = 1)+
  theme_base()

p3 <- ggplot(df %>% filter(lact_freq>0), aes(x = lact_freq)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(
       x = "Frequency (times per week)",
       y = "Number of participants") +
  geom_vline(xintercept = median(df$lact_freq[df$lact_freq>0]), linetype = "dashed", color = "black", size = 1)+
  theme_base()

p4 <-  ggplot(df %>% filter(lact_hrs>0), aes(x = lact_hrs)) +
  geom_histogram(binwidth = 1, fill = "darkblue", alpha = 0.7) +
  labs(
       x = "Duration (hours per week)",
       y = "Number of participants") +
  geom_vline(xintercept = median(df$lact_hrs[df$lact_hrs>0]), linetype = "dashed", color = "black", size = 1)+
  theme_base()


s1_fig <-  plot_grid(p1,
          p2,
          p3,
          p4,
          labels = c("A","B","C","D"),
          align = "hv",
          axis = "t",
          ncol = 2)


ggsave(filename ="out/s_figs/s_fig1.png",plot=s1_fig,device = png, type = "cairo", 
       width =30,height =30,units="cm",dpi = 400)

