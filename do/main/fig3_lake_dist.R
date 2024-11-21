
# Type of activity over distance ------------------------------------------

df_mod1 <- df
df_mod2 <- df
df_mod3 <- df
group1_lab <- "Domestic water contact"
group2_lab <- "Occupational water contact"
group3_lab <- "Recreational water contact"
lab_y <- "Proportion with water contact"
lab_x <- "Household distance to closest water site (km)"
ylims <- c(0, 0.8)
xlims <- c(0, 2)
xvar <- "dist_site"
mod1 <- as.formula(lact_ind_dom ~ s(dist_site, k = 5, m = 2, bs = "tp"))
mod2 <- as.formula(lact_ind_occ ~ s(dist_site, k = 6, m = 2, bs = "tp"))
mod3 <- as.formula(lact_ind_rec ~ s(dist_site, k = 5, m = 2, bs = "tp"))
xvar <- "dist_site"
replicas <- 1000
cols <- c("black", "blue", "red")
fam <- "binomial"

gam2a <- gam_fun_boot(xvar = "dist_site", mod1 = mod1, mod2 = mod2, df_mod1 = df_mod1,
                      df_mod2 = df_mod2, group1_lab = group1_lab, group2_lab = group2_lab,
                      replicas = replicas, fam = fam, mod3 = mod3, df_mod3 = df_mod3,
                      group3_lab = group3_lab, lab_y = lab_y, lab_x = lab_x, ylims = ylims,
                      xlims = xlims, cols = c("#4F94CD", "#8f867b", "#FF1493"),
                      line_size = 1.5) +
  theme(legend.position = c(0.25, 0.9), legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)) +
  scale_x_continuous(breaks = seq(0, 2.2, .5)) +
  scale_y_continuous(breaks = seq(0, .8, .2))

# Different distance metrics ----------------------------------------------

## Household-level distance -----------------------------------------------

df_mod1 <- df
lab_y <- "Proportion"
lab_x <- "Household distance (km)"
ylims <- c(0, 0.8)
xvar <- "dist_site"

mod1 <- gam(lact_ind ~ s(dist_site, k = 4, m = 2, bs = "tp"), data = df_mod1, family = "binomial")

gam2b <- gam_fun_single(xvar = "dist_site", mod1 = mod1, df_mod1 = df_mod1, lab_y = lab_y, lab_x = lab_x,
                        ylims = ylims, cols = c("blue")) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)) +
  scale_x_continuous(breaks = seq(0, 2.2, .5)) +
  scale_y_continuous(breaks = seq(0, .8, .2))

## Village-level distance -----------------------------------------------

df_mod4 <- df
lab_y <- "Proportion"
lab_x <- "Village distance (km)"
ylims <- c(0, 0.8)
xlims <- c(0, 2)
xvar <- "dist_site_villc"

mod4c <- gam(lact_ind ~ s(dist_site_villc, k = 4, m = 2, bs = "tp"), data = df_mod4, family = "binomial")

gam4c <- gam_fun_single(xvar = "dist_site_villc", mod1 = mod4c, df_mod1 = df_mod1, lab_y = lab_y, lab_x = lab_x,
                        ylims = ylims, xlims = xlims, cols = c("blue")) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)) +
  scale_x_continuous(breaks = seq(0, 2.2, .5)) +
  scale_y_continuous(breaks = seq(0, .8, .2))

## School-level distance -----------------------------------------------

df_mod4d <- df %>% filter(!is.na(dist_site_school))
lab_y <- "Proportion"
lab_x <- "School distance (km)"
ylims <- c(0, 0.8)
xlims <- c(0, 2)
xvar <- "dist_site_school"

mod4d <- gam(lact_ind ~ s(dist_site_school, k = 4, m = 2, bs = "tp"), data = df_mod4d, family = "binomial")

gam4d <- gam_fun_single(xvar = "dist_site_school", mod1 = mod4d, df_mod1 = df_mod4d, lab_y = lab_y, lab_x = lab_x,
                        ylims = ylims, xlims = xlims, cols = c("blue")) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey95", size = 1, linetype = 1),
        panel.grid.major.y = element_line(color = "grey95", size = 1, linetype = 1)) +
  scale_x_continuous(breaks = seq(0, 2.2, .5)) +
  scale_y_continuous(breaks = seq(0, .8, .2))

# Make multi-panel figure -------------------------------------------------

fig3_right <- plot_grid(gam2b, gam4c, gam4d, labels = c("B", "C", "D"), nrow = 3)

fig3_left <- plot_grid(gam2a)

fig3 <- plot_grid(fig3_left, fig3_right, ncol = 2, rel_widths = c(1.5, .8), labels = c("A"))

ggsave(filename = "out/main/fig3.png", plot = fig3, device = png, type = "cairo", width = 34 * 0.8,
       height = 22 * 0.8, units = "cm", dpi = 400)
