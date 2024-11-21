

# Water contact over gender -----------------------------------------

df_mod1 <- df %>% filter(gender_cat==0)
df_mod2 <- df %>% filter(gender_cat==1)
group1_lab <- "Male"
group2_lab <- "Female"
lab_y <- "Proportion of participants with water contact"
lab_x <- "Age (years)"
ylims <- c(0,1)

mod1 <- gam(kk_yes_no ~ s(age,k=8,m=2, bs="tp"), data=df_mod1, family="binomial")
mod2 <- gam(kk_yes_no ~ s(age,k=8,m=2, bs="tp"), data=df_mod2, family="binomial")

s3_gam <- gam_fun(xvar="age",mod1=mod1,mod2=mod2,df_mod1=df_mod1,df_mod2=df_mod2,group1_lab=group1_lab,group2_lab=group2_lab,
                lab_y=lab_y,lab_x=lab_x,ylims=ylims,cols=c("#FF1493","#008B45")) +
                    theme(
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
                scale_x_continuous(breaks = seq(5,90,5)) +
                scale_y_continuous(breaks = seq(0,1,.2))



ggsave(filename ="out/s_figs/s_fig3.png",plot=s3_gam,device = png, type = "cairo",
       width =20,height =16,units="cm",dpi = 400)
