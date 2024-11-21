
frm1x <- formula(paste("lact_ind ~", paste0(c(exp_vars,"(1 | vill_id)"), collapse = " + ")))

# estimate the model and store results in m
m <- glmer(frm1x, data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)

sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

#Now we will resample our data and take 10 replicates
tmp <- sampler(slice_sample(df,prop=1) , "hh_id", reps = 10)
bigdata <- cbind(tmp, df[tmp$RowID, ])

f <- fixef(m)
r <- getME(m, "theta")

cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))

myboot <- function(i) {
  object <- try(glmer(lact_ind ~ age + age2 + gender_cat + occupation_cat + water_improved_vill + 
                        contamination + nsites + site_type + prev_cat_vill + roads_vill + 
                        (1 | vill_id), data = bigdata, subset = Replicate == i, family = binomial,
                      nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  if (class(object) == "try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

# calculate proportion of models that successfully converged
success <- length(res)
mean(success)


out <- list()
for(i in 1:length(res)){
  out[[i]] <- t(as.data.frame(res[[i]])) %>% as.data.frame()
}

bigres <- do.call(bind_rows, out)
coef <- apply(bigres,2,median)
ci <- t(apply(bigres, 2, quantile, probs = c(0.025, 0.975),na.rm=T))
out <- cbind.data.frame(coef,ci) %>% exp()
out <- tibble::rownames_to_column(out, "names")

# Rename columns
out <- select(out,names,lci=`2.5%`,uci=`97.5%`,coef)

# Merge the data frames
#out <- left_join(out, noms, by = "names")

# Add empty space column for forest plot
out$" " <- paste(rep(" ", 20), collapse = " ")

# Replace missing variable levels with empty strings
#out$var <- ifelse(is.na(out$var), "", out$var)

# Format exponentiated coefficients and confidence intervals
out$or_ci <- ifelse(is.na(out$lci), "", sprintf("%.2f (%.2f to %.2f)", out$coef, out$lci, out$uci))
out <- out[-c(1,nrow(out)) ,]

