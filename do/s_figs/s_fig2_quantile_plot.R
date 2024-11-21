
# Summarise data by distance to the site
p <- df %>%
  select(lact_ind, dist_site) %>%
  group_by(dist_site) %>%
  summarise(
    lact_ind = sum(lact_ind),
    hh_prop = n()
  )

# Calculate cumulative proportions
p$lact_ind_prop <- cumsum(p$lact_ind) / sum(p$lact_ind)
p$hh_prop <- cumsum(p$hh_prop / sum(p$hh_prop))

# Identify the distance at which the lact_ind proportion exceeds 0.8
p$dist_site[which(p$lact_ind_prop > .8)[1]]

# Create a plot and save it as a PNG
png("out/s_figs/s_fig2.png", width = 15, height = 16, units = "cm", res = 400, type = "cairo")

# Plot household proportion by distance
plot(p$dist_site, p$hh_prop, col = "grey50",
     ylab = "Cumulative proportion",
     xlab = "Household distance from closest water site (km)")

# Add points for water contact proportion
points(p$dist_site, p$lact_ind_prop, col = "blue")

# Add a vertical dashed line at 0.43 km
abline(v = 0.43, lty = 2)

# Add a legend
legend("bottomright",
       legend = c("Water contact", "Households"),
       col = c("blue", "grey"),
       pch = 19,
       title = "Groups")

# Close the PNG device
dev.off()
