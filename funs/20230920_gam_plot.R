
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
  #line_size: size of lines in plot

gam_fun <- function(mod1=NULL,mod2=NULL,
                    xvar=NULL,
                    df_mod1=NULL,df_mod2=NULL,
                    group1_lab=NULL,group2_lab=NULL,
                    lab_y=NULL,lab_x=NULL,ylims=NULL,xlims=NULL,
                    mod3=NULL,df_mod3=NULL,group3_lab=NULL, #if third model
                    cols = c("blue","black"),
                    line_size=.9
                    ){
  
  
  require(mgcv)
  
  mod1_pred <- predict.gam(mod1, se=TRUE)
  mod2_pred <- predict.gam(mod2, se=TRUE)
  
  if(!is.null(mod3)){
    mod3_pred <- predict.gam(mod3, se=TRUE)
  }
  
  
    dfp1 <- transform(df_mod1,
                    fit = mod1$family$linkinv(mod1_pred$fit),
                    lci = mod1$family$linkinv(mod1_pred$fit - 1.96 * mod1_pred$se.fit),
                    uci = mod1$family$linkinv(mod1_pred$fit + 1.96 * mod1_pred$se.fit))
    
    dfp2 <- transform(df_mod2,
                      fit = mod2$family$linkinv(mod2_pred$fit),
                      lci = mod2$family$linkinv(mod2_pred$fit - 1.96 * mod2_pred$se.fit),
                      uci = mod2$family$linkinv(mod2_pred$fit + 1.96 * mod2_pred$se.fit))
    
    if(!is.null(mod3)){
      dfp3 <- transform(df_mod3,
                        fit = mod3$family$linkinv(mod3_pred$fit),
                        lci = mod3$family$linkinv(mod3_pred$fit - 1.96 * mod3_pred$se.fit),
                        uci = mod3$family$linkinv(mod3_pred$fit + 1.96 * mod3_pred$se.fit))
      dfp3$Group <- group3_lab
    }
  
  dfp1$Group <- group1_lab
  dfp2$Group <- group2_lab
  
  dfp <- rbind.data.frame(dfp1,dfp2)
  
  if(!is.null(mod3)){
    dfp <- rbind.data.frame(dfp1,dfp2,dfp3)
  }
  xvar <- dfp[,xvar]
  #CairoPNG("tmp_out/ICC_PCA_LRTs/figs/gams/fig6.png",width = 1500,height = 1500*0.618,unit="px",dpi=250)
  ggplot(data=dfp, aes(x=xvar, y=fit,group=Group,col=Group)) +
    scale_colour_manual(values=cols)+
    geom_ribbon(aes(ymin=lci, ymax = uci), alpha = 0.3,col="grey") +
    geom_rug(aes(x=xvar), sides = "b",alpha = 1/2, position = "jitter",col="black") +
    geom_line(aes(x=xvar,y=fit),size=line_size) +
    #geom_vline(aes(xintercept = pkexp), linetype="dashed",col="grey40") +
    coord_cartesian(ylim = ylims,xlim = xlims)+
    ylab(lab_y) + xlab(lab_x)+
    theme_base()
  #dev.off()
}


