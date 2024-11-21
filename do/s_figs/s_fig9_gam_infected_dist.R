

df_mod1 <- df
df_mod2 <- df
df_mod3 <- df
group1_lab <- "Water contact"
group2_lab <- "Heavily infected"
group3_lab <- "Infected"
lab_y <- "Proportion of participants"
lab_x <- "household distance to closest water site (km)"
ylims <- c(0,1)

mod1 <- gam(lact_ind ~ s(dist_site,k=4,m=2, bs="tp"), data=df_mod1, family="binomial")
mod2 <- gam(kk_heavy ~ s(dist_site,k=4,m=2, bs="tp"), data=df_mod2, family="binomial")
mod3 <- gam(kk_yes_no ~ s(dist_site,k=4,m=2, bs="tp"), data=df_mod2, family="binomial")

s9_gam <- gam_fun(xvar="dist_site",mod1=mod1,mod2=mod2,mod3=mod3,df_mod1=df_mod1,df_mod2=df_mod2,df_mod3=df_mod3,
                  group1_lab=group1_lab,group2_lab=group2_lab,group3_lab=group3_lab,
                  lab_y=lab_y,lab_x=lab_x,ylims=ylims,cols=c("brown","red","blue"))  + 
                  theme(legend.text = element_markdown(),
                        panel.grid.major.x = element_line(
                          color = "grey95",
                          size = 1,
                          linetype = 1
                        ),
                        panel.grid.major.y = element_line(
                          color = "grey95",
                          size = 1,
                          linetype = 1
                        )
                  ) +
                  scale_x_continuous(breaks = seq(0,2.2,.5)) +
                  scale_y_continuous(breaks = seq(0,1,.2))

ggsave(filename ="out/s_figs/s_fig9.png",plot=s9_gam,device = png, type = "cairo", 
       width =20,height =16,units="cm",dpi = 400)
