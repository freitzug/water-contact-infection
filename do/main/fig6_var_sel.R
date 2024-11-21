
# Plot by type of variable ------------------------------------------------

outtmp <- bind_rows(
  exp_sel_df %>%
    filter(!is.na(type_var)) %>%
    mutate(value = value / sum(value), group = "Water contact \n(all)", sorted = 1),
  exp_sel_df %>%
    filter(!is.na(type_var), value >= .5) %>%
    mutate(value = value / sum(value), group = "Water contact \n(selected)", sorted = 1),
  inf_sel_df %>%
    filter(!is.na(type_var)) %>%
    mutate(value = value / sum(value), group = "Infection \n(all)", sorted = 2),
  inf_sel_df %>%
    filter(!is.na(type_var), value >= .5) %>%
    mutate(value = value / sum(value), group = "Infection \n(selected)", sorted = 2)
)

outtmp <- outtmp %>% group_by(group, type_var, sorted) %>% summarise(value = sum(value))
outtmp$group <- stats::reorder(outtmp$group, outtmp$sorted) # Sort groups

p1 <- ggplot(outtmp, aes(x = group, y = value, fill = type_var)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(value, 2), group = type_var), position = position_stack(vjust = 0.5)) +
  geom_vline(xintercept = 2.5) +
  labs(title = "Type of predictors", fill = "Variable type") +
  xlab("") + ylab("Proportion") +
  theme_base() +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right",
        title = element_text(size = 12))

# Plot by level of variable ------------------------------------------------

outtmp <- bind_rows(
  exp_sel_df %>%
    filter(!is.na(level)) %>%
    mutate(value = value / sum(value), group = "Water contact \n(all)", sorted = 1, width = .1),
  exp_sel_df %>%
    filter(!is.na(level), value >= .5) %>%
    mutate(value = value / sum(value), group = "Water contact \n(selected)", sorted = 1, width = .1),
  inf_sel_df %>%
    filter(!is.na(level)) %>%
    mutate(value = value / sum(value), group = "Infection \n(all)", sorted = 2, width = .5),
  inf_sel_df %>%
    filter(!is.na(level), value >= .5) %>%
    mutate(value = value / sum(value), group = "Infection \n(selected)", sorted = 2, width = .5)
)

# Sort levels and groups
outtmp <- outtmp %>% group_by(group, level, sorted) %>% summarise(value = sum(value), sort = mean(sort))
outtmp$level <- stats::reorder(outtmp$level, outtmp$sort)
outtmp$group <- stats::reorder(outtmp$group, outtmp$sorted)

p2 <- ggplot(outtmp, aes(x = group, y = value, fill = level)) +
  geom_bar(stat = "identity", width = .7) +
  geom_text(aes(label = round(value, 2), group = level), position = position_stack(vjust = 0.5)) +
  geom_vline(xintercept = 2.5) +
  labs(title = "Level of predictors", fill = "Levels") +
  xlab("") + ylab("Proportion") +
  theme_base() +
  scale_fill_manual(values = c("#FF8C00", "#CDC0B0", "#CD6090")) +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.position = "right",
        title = element_text(size = 12))

# Dumbbell plot ------------------------------------------------------------

# Format the dataframe
sel_df2 <- bind_rows(
  mutate(inf_sel_df, outcome = "Infection") %>%
    left_join(exp_sel_df %>% dplyr::select(value, var) %>% rename(other_val = value), by = "var"),
  mutate(exp_sel_df, outcome = "Water contact") %>%
    left_join(inf_sel_df %>% dplyr::select(value, var) %>% rename(other_val = value), by = "var")
) %>% filter(!is.na(other_val), value > 0 & other_val > 0)

sel_df2 <- sel_df2 %>% mutate(variable = if_else(labels != "", paste0(variable, " (", labels, ")"), variable))

# Reorder variables
sel_df2$variable <- stats::reorder(sel_df2$variable, sel_df2$value)
sel_df2$variable <- stats::reorder(sel_df2$variable, sel_df2$outcome)

# Create dumbbell plots
tvars <- unique(sel_df2$type_var) %>% na.omit() %>% sort()
plist <- list()

for (i in 1:length(tvars)) {
  sel_df_tmp2 <- sel_df2 %>% filter(type_var == tvars[i])
  sel_df_tmp_wide <- sel_df_tmp2 %>% filter(type_var == tvars[i], outcome == "Water contact")

  if (nrow(sel_df_tmp2) > 0) {
    p <- ggplot() +
      geom_segment(data = sel_df_tmp_wide, aes(x = variable, xend = variable, y = value, yend = other_val), color = "gray", lwd = 2.5) +
      geom_point(data = sel_df_tmp2, aes(x = variable, y = value, shape = outcome, fill = outcome), size = 2.5) +
      geom_hline(yintercept = .5, linetype = 'dotted', col = 'gray') +
      coord_flip() +
      labs(title = tvars[i], x = "Groups", y = "Values") +
      scale_fill_manual(name = "", values = c("red", "blue")) +
      scale_shape_manual(name = "", values = c(21, 24)) +
      ylim(0, 1) +
      theme(legend.title = element_blank()) +
      theme_base()
    plist[[i]] <- p
  }
}

# Combine plots into multi-panel figure -----------------------------------

fig6a <- plot_grid(
  plist[[3]] + theme(legend.position = "none", title = element_text(size = 12), axis.text.y = element_text(size = 9)) + xlab("") + ylab(""),
  plist[[1]] + theme(legend.position = "none", title = element_text(size = 12), axis.text.y = element_text(size = 9)) + xlab("") + ylab(""),
  plist[[4]] + theme(legend.position = "none", title = element_text(size = 12), axis.text.y = element_text(size = 9)) + xlab("") + ylab(""),
  plist[[2]] + theme(legend.position = "none", title = element_text(size = 12), axis.text.y = element_text(size = 9)) + xlab("") + ylab("Inclusion probability"),
  byrow = FALSE, labels = c("A", "B", "C", "D"), align = 'hv', axis = "bt", rel_heights = c(1, .4, .8, 1), scale = 1, ncol = 1
)

fig6b <- plot_grid(get_legend(plist[[4]]), align = 'hv', nrow = 4)

fig6c <- plot_grid(p1, p2, align = 'hv', nrow = 2, labels = c("E", "F"))

fig6 <- plot_grid(fig6a, fig6b, fig6c, nrow = 1, rel_widths = c(.9, .3, 1.1))

ggsave(filename = "out/main/fig6.png", plot = fig6, device = png, type = "cairo", width = 42, height = 28, units = "cm", dpi = 400)

