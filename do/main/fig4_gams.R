

# Exposure and Infection over age -----------------------------------------

df_mod1 <- df
df_mod2 <- df
group1_lab <- "Any water contact"
group2_lab <- "<i>S. mansoni</i> infection"
lab_y <- "Proportion with water contact"
lab_x <- "Age (years)"
ylims <- c(0, 1)

mod1 <- gam(lact_ind ~ s(age, k = 8, m = 2, bs = "tp"), data = df_mod1, family = "binomial")
mod2 <- gam(kk_yes_no ~ s(age, k = 8, m = 2, bs = "tp"), data = df_mod2, family = "binomial")

gam1 <- gam_fun(xvar = "age", mod1 = mod1, mod2 = mod2, df_mod1 = df_mod1, df_mod2 = df_mod2,
                group1_lab = group1_lab, group2_lab = group2_lab, lab_y = lab_y, lab_x = lab_x,
                ylims = ylims, cols = c("black", "blue")) +
  theme(legend.text = element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)
  ) +
  scale_x_continuous(breaks = seq(5, 90, 5)) +
  scale_y_continuous(breaks = seq(0, 1, .2))

# Water contact frequency -------------------------------------------------

df_mod1 <- df %>% filter(gender == "male")
df_mod2 <- df %>% filter(gender == "female")
lab_y <- "Frequency"
lab_x <- "Age (years)"
ylims <- c(0, 11.5)
xvar <- "age"

mod1 <- gam(lact_freq ~ s(age, k = 7, m = 2, bs = "tp"), data = df_mod1, family = "poisson")
mod2 <- gam(lact_freq ~ s(age, k = 7, m = 2, bs = "tp"), data = df_mod2, family = "poisson")

gam2 <- gam_fun(xvar = "age", mod1 = mod1, df_mod1 = df_mod1, mod2 = mod2, df_mod2 = df_mod2,
                group1_lab = "Males", group2_lab = "Females", lab_y = lab_y, lab_x = lab_x,
                ylims = ylims, cols = c("#FF1493", "#008B45")) +
  theme(panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)
  ) +
  scale_x_continuous(breaks = seq(5, 90, 10)) +
  scale_y_continuous(breaks = seq(0, 11, 3))

# Water contact duration -------------------------------------------------

df_mod1 <- df %>% filter(gender == "male")
df_mod2 <- df %>% filter(gender == "female")
lab_y <- "Duration"
lab_x <- "Age (years)"
ylims <- c(0, 11.5)
xvar <- "age"

mod1 <- gam(lact_hrs ~ s(age, k = 7, m = 2, bs = "tp"), data = df_mod1, family = "poisson")
mod2 <- gam(lact_hrs ~ s(age, k = 7, m = 2, bs = "tp"), data = df_mod2, family = "poisson")

gam3 <- gam_fun(xvar = "age", mod1 = mod1, df_mod1 = df_mod1, mod2 = mod2, df_mod2 = df_mod2,
                group1_lab = "Males", group2_lab = "Females", lab_y = lab_y, lab_x = lab_x,
                ylims = ylims, cols = c("#FF1493", "#008B45")) +
  theme(panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)
  ) +
  scale_x_continuous(breaks = seq(5, 90, 10)) +
  scale_y_continuous(breaks = seq(0, 11, 3))

# Make multi-panel figure -------------------------------------------------

# Format and retrieve legends
gam1 <- gam1 + theme(legend.position = "top", legend.title = element_blank())
gam2 <- gam2 + theme(legend.position = "none")
gam3 <- gam3 + theme(legend.position = "top", legend.title = element_blank())

# Create large panel figure
fig4a <- plot_grid(gam1 + theme(legend.position = "none"),
                   get_plot_component(gam1, 'guide-box-top', return_all = TRUE),
                   label_size = 16,
                   labels = "A",
                   rel_heights = c(3, .5),
                   nrow = 2)

# Create two small panel figures
fig4bc <- plot_grid(gam2 + theme(legend.position = "none"),
                    gam3 + theme(legend.position = "none"),
                    get_plot_component(gam3, 'guide-box-top', return_all = TRUE),
                    labels = c("B", "C"),
                    label_size = 16,
                    rel_heights = c(1.5, 1.5, .5),
                    align = 'hv',
                    axis = "bt",
                    nrow = 3,
                    ncol = 1)

# Create multi-panel figure
fig4 <- plot_grid(fig4a, fig4bc, align = 'bottom', rel_widths = c(.8, .5), ncol = 2, nrow = 1)

# Save the figure
ggsave(filename = "out/main/fig4.png", plot = fig4, device = png, type = "cairo",
       width = 27, height = 17, units = "cm", dpi = 400)

