
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

gam_fun_single <- function(mod1=NULL,
                    xvar=NULL,
                    df_mod1=NULL,
                    lab_y=NULL,lab_x=NULL,xlims=NULL,ylims=NULL,
                    cols = c("blue")
                    ){
  
  
  require(mgcv)
  
  mod1_pred <- predict.gam(mod1, se=TRUE)
  
  dfp <- transform(df_mod1,
                    fit = mod1$family$linkinv(mod1_pred$fit),
                    lci = mod1$family$linkinv(mod1_pred$fit - 1.96 * mod1_pred$se.fit),
                    uci = mod1$family$linkinv(mod1_pred$fit + 1.96 * mod1_pred$se.fit))
  
  xvar <- dfp[,xvar]
  
  #CairoPNG("tmp_out/ICC_PCA_LRTs/figs/gams/fig6.png",width = 1500,height = 1500*0.618,unit="px",dpi=250)
  ggplot(data=dfp, aes(x=xvar, y=fit)) +
    scale_colour_manual(values=cols)+
    geom_ribbon(aes(ymin=lci, ymax = uci), alpha = 0.3,col="grey") +
    geom_rug(aes(x=xvar), sides = "b",alpha = 1/2, position = "jitter",col="black") +
    geom_line(aes(x=xvar,y=fit),size=0.9) +
    #geom_vline(aes(xintercept = pkexp), linetype="dashed",col="grey40") +
    coord_cartesian(ylim = ylims,xlim = xlims)+
    ylab(lab_y) + xlab(lab_x)+
    theme_base()
  #dev.off()
}


