
# Any water contact -------------------------------------------------

## Any water contact males -------------------------------------------------

# Summarize the data
summary_df <- df %>% filter(gender == "male") %>%
  group_by(age_cat) %>%
  summarise(
    `Occupational` = sum(lact_ind_occ) / n(),
    `Recreational` = sum(lact_ind_rec) / n(),
    `Domestic` = sum(lact_ind_dom) / n()
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(`Occupational`, `Recreational`, `Domestic`), names_to = "Activity type")

## Create the stacked area plot
vec = seq(5,55,5)
age_grps <- c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55+")
fig5a <- ggplot(summary_df, aes(x = age_cat, y = value, fill = `Activity type`)) +
  geom_area(position = "fill") + #position= fill if area should fill 100%
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#4F94CD","#CDC0B0","#CD6090"))+
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq_along(vec), labels = age_grps,expand = c(0, 0)) + #avoid x/y expansion
  scale_y_continuous(expand = c(0, 0))+ #avoid x/y expansion
  theme_base()+

  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Any water contact females -------------------------------------------------

# Summarize the data
summary_df <- df %>% filter(gender == "female") %>%
  group_by(age_cat) %>%
  summarise(
    `Occupational` = sum(lact_ind_occ) / n(),
    `Recreational` = sum(lact_ind_rec) / n(),
    `Domestic` = sum(lact_ind_dom) / n()
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(`Occupational`, `Recreational`, `Domestic`), names_to = "Activity type")

# Create the stacked area plot
vec = seq(5,55,5)
age_grps <- c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55+")
fig5b <-ggplot(summary_df, aes(x = age_cat, y = value, fill = `Activity type`)) +
  geom_area(position = "fill") + #position= fill if area should fill 100%
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#4F94CD","#CDC0B0","#CD6090"))+
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq_along(vec), labels = age_grps,expand = c(0, 0)) + #avoid x/y expansion
  scale_y_continuous(expand = c(0, 0))+ #avoid x/y expansion
  theme_base()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Frequency ---------------------------------------------------------------

## Frequency males --------------------------------------------------------

# Summarize the data
summary_df <- df %>% filter(gender == "male") %>%
  group_by(age_cat) %>%
  summarise(
    `Occupational` = mean(lact_freq_occ) ,
    `Recreational` = mean(lact_freq_rec) ,
    `Domestic` = mean(lact_freq_dom)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(`Occupational`, `Recreational`, `Domestic`), names_to = "Activity type")

# Create the stacked area plot
fig5c <-ggplot(summary_df, aes(x = age_cat, y = value, fill = `Activity type`)) +
  geom_area(position = "fill") + #position= fill if area should fill 100%
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#4F94CD","#CDC0B0","#CD6090"))+
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq_along(vec), labels = age_grps,expand = c(0, 0)) + #avoid x/y expansion
  scale_y_continuous(expand = c(0, 0))+ #avoid x/y expansion
  theme_base()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Frequency females --------------------------------------------------------

# Summarize the data
summary_df <- df %>% filter(gender == "female") %>%
  group_by(age_cat) %>%
  summarise(
    `Occupational` = mean(lact_freq_occ) ,
    `Recreational` = mean(lact_freq_rec) ,
    `Domestic` = mean(lact_freq_dom)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(`Occupational`, `Recreational`, `Domestic`), names_to = "Activity type")

# Create the stacked area plot
fig5d <- ggplot(summary_df, aes(x = age_cat, y = value, fill = `Activity type`)) +
  geom_area(position = "fill") + #position= fill if area should fill 100%
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#4F94CD","#CDC0B0","#CD6090"))+
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq_along(vec), labels = age_grps,expand = c(0, 0)) + #avoid x/y expansion
  scale_y_continuous(expand = c(0, 0))+ #avoid x/y expansion
  theme_base()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Duration ---------------------------------------------------------------

## Duration males --------------------------------------------------------

# Summarize the data
summary_df <- df %>% filter(gender == "male") %>%
  group_by(age_cat) %>%
  summarise(
    `Occupational` = mean(lact_hrs_occ) ,
    `Recreational` = mean(lact_hrs_rec) ,
    `Domestic` = mean(lact_hrs_dom)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(`Occupational`, `Recreational`, `Domestic`), names_to = "Activity type")

# Create the stacked area plot
fig5e <-ggplot(summary_df, aes(x = age_cat, y = value, fill = `Activity type`)) +
  geom_area(position = "fill") + #position= fill if area should fill 100%
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#4F94CD","#CDC0B0","#CD6090"))+
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq_along(vec), labels = age_grps,expand = c(0, 0)) + #avoid x/y expansion
  scale_y_continuous(expand = c(0, 0))+ #avoid x/y expansion
  theme_base()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Duration females --------------------------------------------------------

# Summarize the data
summary_df <- df %>% filter(gender == "female") %>%
  group_by(age_cat) %>%
  summarise(
    `Occupational` = mean(lact_hrs_occ) ,
    `Recreational` = mean(lact_hrs_rec) ,
    `Domestic` = mean(lact_hrs_dom)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(`Occupational`, `Recreational`, `Domestic`), names_to = "Activity type")

# Create the stacked area plot
fig5f <-ggplot(summary_df, aes(x = age_cat, y = value, fill = `Activity type`)) +
  geom_area(position = "fill") + #position= fill if area should fill 100%
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#4F94CD","#CDC0B0","#CD6090"))+
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq_along(vec), labels = age_grps,expand = c(0, 0)) + #avoid x/y expansion
  scale_y_continuous(expand = c(0, 0))+ #avoid x/y expansion
  theme_base()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Create multipanel figure ------------------------------------------------

legend <- get_legend(fig5b)

fig5a <- fig5a + theme(legend.position = "none",
                       axis.title = element_text(face = "bold"),
                       axis.text.x = element_blank(),
                       plot.title = element_text(
                         size = 14,
                         hjust = 0.5))

fig5b <- fig5b + theme(legend.position = "none",
                       axis.title = element_text(face = "bold"))

fig5c <- fig5c + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       plot.title = element_text(size = 14,
                                                 hjust = 0.5))

fig5d <- fig5d + theme(legend.position = "none",
                       axis.text.y = element_blank(),)
fig5e <- fig5e + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       plot.title = element_text(size = 14,
                                                 hjust = 0.5)) + ggtitle("Duration")
fig5f <- fig5f + theme(legend.position = "none",
                       axis.text.y = element_blank(),)




fig5 <- plot_grid(fig5a  + ggtitle("Any water contact") + ylab("Males"),
                  fig5b + ylab("Females"),
                  fig5c + ggtitle("Frequency"),
                  fig5d,
                  fig5e +  ggtitle("Duration"),
                  fig5f,
                  legend,
                  byrow = F,
                  labels = c("A","B","C","","D","E","F"),
                  label_size = 16,
                  rel_widths = c(1,.9,.9,.5),
                  rel_heights = c(.9,.9),
                  align = 'hv',
                  axis = "bt",
                  nrow = 2,
                  ncol = 4)

fig5 <- plot_grid(fig5a  + ggtitle("Any water contact") + ylab("Males"),
                  fig5c + ggtitle("Frequency"),
                  fig5e +  ggtitle("Duration"),
                  legend,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  fig5b + ylab("Females"),
                  fig5d + xlab("Age"),
                  fig5f,
                  NULL,
                  byrow = T,
                  nrow = 3,
                  ncol = 4,
                  labels = c("A", "B","C","","","","","","D","E","F"),
                  label_size = 16,
                  rel_widths = c(1.1,1,1,.5),
                  rel_heights = c(.9,-.2,.9),
                  align = 'hv',
                  axis = "bt")


# Create a Cairo device to save the plot as a PNG
ggsave(filename ="out/main/fig5.pdf",plot=fig5,
       width = 27,height = 16.8,units="cm",dpi = 400)




