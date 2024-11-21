
source("funs/20230420_theme_base.R")


#input parameters:

  #df_mod1: input model from gam()
  #df_mod2: input model from gam()
  #group1_lab: label for group from df_mod1
  #group2_lab: label for group from df_mod1
  #xvar:
  #lab_y: Y-axis label
  #lab_x: X-axis label
  #ylims: Y-lims
  #expon: exponentiate: needed for binary models
  #cols: colors of lines, vector of length equal to number of models
  #replicas: iterations of bootstrap
  #fam: family for GAM
  #line_size: size of lines in plot


#CHANGES to GAM
# -> mod is a function
# -> need replicas

gam_fun_boot <- function(mod1=NULL,mod2=NULL,
                    xvar=NULL,replicas=NULL,
                    fam="binomial",
                    df_mod1=NULL,df_mod2=NULL,
                    group1_lab=NULL,group2_lab=NULL,
                    lab_y=NULL,lab_x=NULL,ylims=NULL,xlims=NULL,
                    mod3=NULL,df_mod3=NULL,group3_lab=NULL, #if third model
                    cols = c("blue","black"),
                    line_size=.9
                    ){

  require(mgcv)

  nom <- xvar #get nom for predict object
  xvar <- df_mod1[,xvar] %>% remove_labels() %>% as.numeric() #get values

  #define boot function
  boot_fun <- function(replicas, data, form, fam, xvar, nom) {
    out <- replicate(
      replicas, {
        boot <- data[sample.int(nrow(data), replace = TRUE), ]
        model <- gam(form, data = boot, family = fam)
        if (fam == "binomial") {
          out <- data.frame(tmp = xvar); names(out)[names(out) == "tmp"] <- nom
          out <- predict(model, out) %>% plogis()
        } else {
          out <- data.frame(tmp = xvar); names(out)[names(out) == "tmp"] <- nom
          out <- predict(model, out)
        }
        return(out)
      }
    )
    return(out)
  }

  mod1_pred <- boot_fun(replicas, df_mod1, mod1, fam, xvar, nom)
  mod2_pred <- boot_fun(replicas, df_mod1, mod2, fam, xvar , nom)
  fit = predict.gam(gam(mod1, df_mod1, family = fam)) %>% plogis()

  if(!is.null(mod3)){
    mod3_pred <- boot_fun(replicas, df_mod3, mod3, fam, xvar, nom)
  }

    dfp1 <- data.frame(
                    #fit = apply(mod1_pred, 1, quantile, .5),
                    fit = predict.gam(gam(mod1, df_mod1, family = fam)) %>% plogis(),
                    lci = apply(mod1_pred, 1, quantile, .025),
                    uci = apply(mod1_pred, 1, quantile, .975))

    dfp2 <- data.frame(
      #fit = apply(mod2_pred, 1, quantile, .5),
      fit = predict.gam(gam(mod2, df_mod2, family = fam)) %>% plogis(),
      lci = apply(mod2_pred, 1, quantile, .025),
      uci = apply(mod2_pred, 1, quantile, .975))

    if(!is.null(mod3)){
      dfp3 <- dfp3 <- data.frame(
        #fit = apply(mod3_pred, 1, quantile, .5),
        fit = predict.gam(gam(mod3, df_mod3, family = fam)) %>% plogis(),
        lci = apply(mod3_pred, 1, quantile, .025),
        uci = apply(mod3_pred, 1, quantile, .975))
      dfp3$Group <- group3_lab
      dfp3$xvar <- xvar
    }

  dfp1$Group <- group1_lab
  dfp2$Group <- group2_lab
  dfp1$xvar <- xvar
  dfp2$xvar <- xvar

  if(!is.null(mod3)){
    dfp <- rbind.data.frame(dfp1,dfp2,dfp3)
  } else {
    dfp <- rbind.data.frame(dfp1,dfp2)
  }

  #CairoPNG("tmp_out/ICC_PCA_LRTs/figs/gams/fig6.png",width = 1500,height = 1500*0.618,unit="px",dpi=250)
  p1 <- ggplot(data=dfp, aes(x=xvar, y=fit,group=Group,col=Group)) +
    scale_colour_manual(values=cols)+
    geom_ribbon(aes(ymin=lci, ymax = uci), alpha = 0.3,col="grey") +
    geom_rug(aes(x=xvar), sides = "b",alpha = 1/2,col="black",position = "jitter")+
    geom_line(aes(x=xvar,y=fit),size=line_size) +
    coord_cartesian(ylim = ylims,xlim = xlims)+
    ylab(lab_y) + xlab(lab_x)+
    theme_base()
  return(p1)
}


