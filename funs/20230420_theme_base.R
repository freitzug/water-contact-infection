
#modified base_theme (no black box around)
# from here: https://github.com/jrnold/ggthemes/tree/main/R

theme_foundation <- function(base_size=12, base_family="") {
  thm <- theme_grey(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + theme(panel.border = element_rect(fill = NA),
              legend.background = element_rect(colour = NA),
              line = element_line(colour = "black"),
              rect = element_rect(fill = "white", colour = "black"),
              text = element_text(colour = "black"))
}


theme_base <- function(base_size = 16, base_family = "") {
  theme_foundation() +
    theme(line = element_line(colour = "black",
                              lineend = "round",
                              linetype = "solid"),
          rect = element_rect(fill = "white",
                              colour = "black",
                              linetype = "solid"),
          text = element_text(colour = "black",
                              face = "plain",
                              family = base_family,
                              size = base_size,
                              vjust = 0.5,
                              hjust = 0.5,
                              lineheight = 1),
          plot.background = element_rect(color = NA, size = 2),
          panel.grid = element_blank(),
          strip.background = element_rect(colour = NA),
          legend.key = element_rect(colour = NA),
          title = element_text(size = rel(1)),
          plot.title = element_text(size = rel(1.2), face = "bold"),
          strip.text = element_text(),
          axis.ticks.length = unit(0.5, "lines")
    )
}
