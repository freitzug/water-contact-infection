
#ROC function

roc_fun <- function(mod_frm = NULL,col= NULL,y=NULL,k=10,repeats=5){
  
  library(pROC)
  library(caret)
  
  df$y <- y
  
  # Create an empty list to store the cross-validation results
  cv_results <- list()
  auc <- list()
  stats <- list()
  out_df <- data.frame()
  ctr = 0
  
  for (rep in 1:repeats) {
    
  folds <- createFolds(df$y, k = k)
  ctr <- ifelse(k<10,ctr + 1,ctr) #init counter for id col
  
  # Loop through each fold
  for (i in 1:k) {
    # Extract the current fold's training and test indices
    ctr <- ctr + 1 #add to counter  
    
    train_indices <- unlist(folds[-i])
    test_indices <- folds[[i]]
    
    # Split the df into training and test sets
    train_df <- df[train_indices, ]
    test_df <- df[test_indices, ]
    
    model <-
      glm(
        mod_frm,
        data = train_df,
        family = 'binomial' 
      )
    
    # Make predictions on the test df
    predictions <- predict(model,test_df)
    
    # Calculate RMSE and store it in the results list
    rmse <- sqrt(mean((test_df$y - predictions)^2))
    cv_results[[ctr]] <- rmse
    
    ### get ROC curve ------
    
    # Calculate the AUC
    roc_obj <- roc(test_df$y, predictions)
    auc_value <- auc(roc_obj)
    
    # Print the AUC value
    cat("AUC:", auc_value, "\n")
    auc[ctr] <- auc_value
    
    #create df
    out_tmp <- data.frame(id = ctr,
                          x = (1- roc_obj[["specificities"]]),
                          y = roc_obj[["sensitivities"]])
    out_df <- rbind(out_df,out_tmp, by="id")
    
  }
  
}
  
  #clean up df
  out_df %<>% na.omit() %>% mutate_all(as.numeric) %>% suppressWarnings()
  
  #gen df with bootstrapped 95% CIs 
  sequence_for_bins <- seq(0, 1, .02) #.02 increments
  out_cis <- out_df %>%
    mutate(bin = cut(x, breaks = sequence_for_bins, include.lowest = TRUE, labels = FALSE)) %>%
    group_by(bin) %>%
    summarise(
      lci = quantile(y,.025,na.rm=T),
      uci = quantile(y,.975,na.rm=T)
    )
  
  out_cis$x <- sequence_for_bins
  out_cis$y <- 1
  
  # Calculate the mean and standard deviation of the performance metric across folds
  stats[[1]] <- unlist(cv_results) #rmse
  stats[[2]] <- base::matrix(data=unlist(auc),nrow=k,ncol=repeats) %>% colMeans() #average across folds
  stats[[3]] <- out_df
  stats[[4]] <- out_cis
  names(stats) <- c("rsme","auc","values","confidence_ints")
  return(stats)
}






