
# Any water contact and exposure ------------------------------------------

roc_exp <- roc_fun(mod_frm = frm1, repeats = 100, col= "blue", y=df$lact_ind)
roc_inf <- roc_fun(mod_frm = frm5, repeats = 100, col= "blue", y=df$kk_yes_no)

# t-test for group difference
roc_exp_mean <- mean(roc_exp[["auc"]])
roc_inf_mean <- mean(roc_inf[["auc"]])
t.test(roc_exp[["auc"]], roc_inf[["auc"]], var.equal=F)

roc_exp_lab <- paste0("Water contact (BVS, auROC = ", round(roc_exp_mean, 3), ")")
roc_inf_lab <- paste0("Infection status (BVS, auROC = ", round(roc_inf_mean, 3), ")")

p0 <- ggplot(data=NULL, aes(x=x, y=y)) +
  geom_path(data=roc_exp[["values"]], aes(group=id), col="#87CEFF", size = .1, alpha = 0.5) +
  geom_path(data=roc_inf[["values"]], aes(group=id, col=roc_inf_lab), col="#FFB6C1", size = .1, alpha = 0.5) +
  geom_smooth(data=bind_rows(roc_exp[["values"]] %>% mutate(group=roc_exp_lab),
                             roc_inf[["values"]] %>% mutate(group=roc_inf_lab)),
              aes(col=group), se = F) +
  xlab("1 - Specificity") + ylab("Sensitivity") +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), color = "grey70", size = .7, linetype="dashed") +
  scale_color_manual(name=NULL, aesthetics = c("colour", "fill"),
                     breaks=c(roc_exp_lab, roc_inf_lab), values=c("blue", "red")) +
  theme_base() +
  theme(legend.position = c(.98, 0.045),
        legend.justification = "right",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())

# Water contact vs water contact with added snail variables ------------------------------------------

frm1b <- formula(paste("lact_ind ~", paste(c(tp(exp_vars), exp_vars_snail_add, "district"), collapse = " + ")))

roc_exp_snail <- roc_fun(mod_frm = frm1b, repeats = 100, col= "blue", y=df$lact_ind)

# t-test for group difference
roc_exp_mean_snail <- mean(roc_exp_snail[["auc"]])
t.test(roc_exp[["auc"]], roc_exp_snail[["auc"]], var.equal=F)

roc_exp_snail_lab <- paste0("Water contact * (BVS, auROC = ", round(roc_exp_mean_snail, 3), ")")

p1 <- ggplot(data=NULL, aes(x=x, y=y)) +
  geom_path(data=roc_exp[["values"]], aes(group=id), col="#FFB6C1", size = .1, alpha = 0.5) +
  geom_path(data=roc_exp_snail[["values"]], aes(group=id, col=roc_exp_snail_lab), col="#b7cad2", size = .1, alpha = 0.5) +
  geom_smooth(data=bind_rows(roc_exp[["values"]] %>% mutate(group=roc_exp_lab),
                             roc_exp_snail[["values"]] %>% mutate(group=roc_exp_snail_lab)),
              aes(col=group), se = F) +
  xlab("1 - Specificity") + ylab("Sensitivity") +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), color = "grey70", size = .7, linetype="dashed") +
  scale_color_manual(name=NULL, aesthetics = c("colour", "fill"),
                     breaks=c(roc_exp_lab, roc_exp_snail_lab), values=c("red", "#105069")) +
  theme_base() +
  theme(legend.position = c(.98, 0.1),
        legend.justification = "right",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())

# Infection vs infection with additional exposure/snail variables ------------------------------------------

frm6r <- formula(paste("kk_yes_no ~", paste(c(tp(inf_vars), inf_vars_exp_add, "district"), collapse = " + ")))
frm6r2 <- formula(paste("kk_yes_no ~", paste(c(tp(inf_vars), inf_vars_snail_add, "district"), collapse = " + ")))

roc_inf_add_exp <- roc_fun(mod_frm = frm6r, repeats = 100, col= "blue", y=df$kk_yes_no)
roc_inf_add_snail <- roc_fun(mod_frm = frm6r2, repeats = 100, col= "blue", y=df$kk_yes_no)

# t-test for group difference
roc_inf_add_exp_mean <- mean(roc_inf_add_exp[["auc"]])
roc_inf_add_snail_mean <- mean(roc_inf_add_snail[["auc"]])
t.test(roc_inf[["auc"]], roc_inf_add_exp[["auc"]], var.equal=F)
t.test(roc_inf[["auc"]], roc_inf_add_snail[["auc"]], var.equal=F)

roc_inf_add_exp_lab <- paste0("Infection status + (BVS, auROC = ", round(roc_inf_add_exp_mean, 3), ")")
roc_inf_add_snail_lab <- paste0("Infection status * (BVS, auROC = ", round(roc_inf_add_snail_mean, 3), ")")

p2 <- ggplot(data=NULL, aes(x=x, y=y)) +
  geom_path(data=roc_inf[["values"]], aes(group=id), col="#FFB6C1", size = .1, alpha = 1) +
  geom_path(data=roc_inf_add_exp[["values"]], aes(group=id, col=roc_inf_add_exp_lab), col="#7D7DFF", size = .1, alpha = 0.2) +
  geom_path(data=roc_inf_add_snail[["values"]], aes(group=id, col=roc_inf_add_snail_lab), col="grey80", size = .1, alpha = 0.2) +
  geom_smooth(data=bind_rows(roc_inf[["values"]] %>% mutate(group=roc_inf_lab),
                             roc_inf_add_exp[["values"]] %>% mutate(group=roc_inf_add_exp_lab),
                             roc_inf_add_snail[["values"]] %>% mutate(group=roc_inf_add_snail_lab)),
              aes(col=group), se = F) +
  xlab("1 - Specificity") + ylab("Sensitivity") +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), color = "grey70", size = .7, linetype="dashed") +
  scale_color_manual(name=NULL, aesthetics = c("colour", "fill"),
                     breaks=c(roc_inf_lab, roc_inf_add_exp_lab, roc_inf_add_snail_lab),
                     values=c("red", "#4B4B99", "black")) +
  theme_base() +
  theme(legend.position = c(.98, 0.1),
        legend.justification = "right",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())

# Any water contact: BAS vs LRT  ------------------------------------------

frm1_lrt <- formula(paste("lact_ind ~", paste(c(tp(vars_exp_lrt), "district"), collapse = " + ")))

roc_exp_lrt <- roc_fun(mod_frm = frm1_lrt, repeats = 100, col= "blue", y=df$kk_yes_no)

# t-test for group difference
roc_exp_lrt_mean <- mean(roc_exp_lrt[["auc"]])
t.test(roc_exp[["auc"]], roc_exp_lrt[["auc"]], var.equal=F)

roc_exp_lrt_lab <- paste0("Water contact (LRT, auROC = ", round(roc_exp_lrt_mean, 3), ")")

p3 <- ggplot(data=NULL, aes(x=x, y=y)) +
  geom_path(data=roc_exp[["values"]], aes(group=id), col="#87CEFF", size = .2, alpha = 0.1) +
  geom_path(data=roc_exp_lrt[["values"]], aes(group=id, col=roc_exp_lrt_lab), col="#66CDAA", size = .2, alpha = 0.1) +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), color = "grey70", size = .7, linetype="dashed") +
  geom_smooth(data=bind_rows(roc_exp[["values"]] %>% mutate(group=roc_exp_lab),
                             roc_exp_lrt[["values"]] %>% mutate(group=roc_exp_lrt_lab)),
              aes(col=group), se = F) +
  xlab("1 - Specificity") + ylab("Sensitivity") +
  scale_color_manual(name=NULL, aesthetics = c("colour", "fill"),
                     breaks=c(roc_exp_lab, roc_exp_lrt_lab),
                     values=c("blue", "#167c76")) +
  theme_base() +
  theme(legend.position = c(.98, 0.1),
        legend.justification = "right",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())

# Infection BAS vs LRT  ------------------------------------------

frm5_lrt <- formula(paste("kk_yes_no ~", paste(c(tp(vars_inf_lrt), "district"), collapse = " + ")))

roc_inf_lrt <- roc_fun(mod_frm = frm5_lrt, repeats = 100, col= "blue", y=df$kk_yes_no)

# t-test for group difference
roc_inf_lrt_mean <- mean(roc_inf_lrt[["auc"]])
t.test(roc_inf[["auc"]], roc_inf_lrt[["auc"]], var.equal=F)

roc_inf_lrt_lab <- paste0("Infection status (LRT, auROC = ", round(roc_inf_lrt_mean, 3), ")")

p4 <- ggplot(data=NULL, aes(x=x, y=y)) +
  geom_path(data=roc_inf[["values"]], aes(group=id), col="#FFB6C1", size = .2, alpha = 1) +
  geom_path(data=roc_inf_lrt[["values"]], aes(group=id, col=roc_inf_lrt_lab), col="#aea785", size = .2, alpha = 0.1) +
  geom_smooth(data=bind_rows(roc_inf[["values"]] %>% mutate(group=roc_inf_lab),
                             roc_inf_lrt[["values"]] %>% mutate(group=roc_inf_lrt_lab)),
              aes(col=group), se = F) +
  xlab("1 - Specificity") + ylab("Sensitivity") +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), color = "grey70", size = .7, linetype="dashed") +
  scale_color_manual(name=NULL, aesthetics = c("colour", "fill"),
                     breaks=c(roc_inf_lab, roc_inf_lrt_lab),
                     values=c("red", "#8e8e68")) +
  theme_base() +
  theme(legend.position = c(.98, 0.1),
        legend.justification = "right",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())

# Make multipanel figure --------------------------------------------------

fig9a <- plot_grid(p0, labels="A")
fig9b <- plot_grid(p1, p2, p3, p4, align = "hv", labels = c("B", "C", "D", "E"), ncol = 2)

fig9 <- plot_grid(fig9a, fig9b, ncol = 2, rel_widths = c(0.85, 1))

ggsave(filename = "out/main/fig9.png", plot = fig9, device = png, type = "cairo",
       width = 30 * 1.7, height = 17 * 1.5, units = "cm", dpi = 400)
