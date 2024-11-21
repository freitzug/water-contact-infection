
#select pca variables
pca_vars <- dict %>% filter(pca=="Yes") %>% pull(name)

df_pca <- df %>% select(all_of(pca_vars))
res.pca <- princomp(df_pca, cor = TRUE, scores = TRUE)

#viz 
#CairoPNG("tmp_out/ICC_PCA_LRTs/figs/pca/scree_plot.png",width = 1000,height = 0.8*1000,unit="px",dpi=250)
#fviz_eig(res.pca)
#dev.off()


#CairoPNG("tmp_out/ICC_PCA_LRTs/figs/pca/contrib_plot.png",width = 1500,height = 1500,unit="px",dpi=250)
#fviz_pca_var(res.pca,
             #col.var = "contrib", # Color by contributions to the PC
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             #repel = TRUE     # Avoid text overlapping
#)
#dev.off()


#notes:

#var.coord = loadings * the component standard deviations
#var.cos2 = var.coord^2
#var.contrib. The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)

# Helper function 
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
loadings <- res.pca$loadings
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord)

# Compute Cos2
var.cos2 <- var.coord^2
head(var.cos2)

# Compute contributions
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2)) %>% 
  as.matrix.data.frame(rownames.force=TRUE) %>% as.data.frame() %>% tibble::rownames_to_column( "var")
names(var.contrib) <- c("var",paste0("Comp.",2:ncol(var.contrib)-1))

pca.out <- predict(res.pca,newdata = df_pca,type="scores") %>% as.data.frame()
df$prcomp1 <- pca.out[,1]
df$prcomp2 <- pca.out[,2]
df$prcomp1_raw <- df$prcomp1
#minMax <- function(x) {
  #(x - min(x)) / (max(x) - min(x))
#}

#df %<>% mutate_at(c("prcomp1","prcomp2"), ~minMax(.))


#gen district level quantiles

df$prcomp1_rank <- df %>% group_by(district) %>% reframe(prcomp_rank = percent_rank(prcomp1)) %>% pull(prcomp_rank)
df$prcomp2_rank <- df %>% group_by(district) %>% reframe(prcomp_rank = percent_rank(prcomp2)) %>% pull(prcomp_rank)

#quantiles
df %<>% mutate(prcomp1 = case_when(prcomp1_rank < 1/3   ~ 1,
                                   prcomp1_rank >= 1/3 & prcomp1_rank < 2/3 ~ 2,
                                   prcomp1_rank >= 2/3 ~3)
  )

df %<>% mutate(prcomp2 = case_when(prcomp2_rank < 1/3   ~ 1,
                                   prcomp2_rank >= 1/3 & prcomp2_rank < 2/3 ~ 2,
                                   prcomp2_rank >= 2/3 ~3)
  )

df %<>% mutate(prcomp2 = case_when(prcomp2 < quantile(df$prcomp2, probs = seq(0,1,1/3))[2] ~ 1,
                                   (prcomp2 >= quantile(df$prcomp2, probs = seq(0,1,1/3))[2]) & 
                                     (prcomp2 < quantile(df$prcomp2, probs = seq(0,1,1/3))[3]) ~ 2,
                                   prcomp2 >= quantile(df$prcomp2, probs = seq(0,1,1/3))[3] ~ 3))


#High exposure (top tercile of exposure index)
df$prcomp1_top <- ifelse(df$prcomp1==3,1,0)


