

# Prep lme4 object for use in forest plotter function ---------------------

glm_forest_prep_clse <- function(mod,clust_var=hh_id,exp=T){
  
  require(dplyr)
  require(stringr)
  require(Hmisc)
  require(sandwich)
  
  modx <- mod
  
  
  #get var-cov matrix for robust SEs
  vcovCL <- vcovCL(modx)
  
  # Calculate confidence intervals and convert to a data frame
  if(exp==T){
    
    out <- confint(coeftest(modx,
                     vcov = vcovCL,
                     type = "HC1",
                     cluster = ~clust_var)) %>% 
      exp() %>%
      as.data.frame()
    
  }else{
    
    out <- confint(coeftest(modx,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~clust_var)) %>% 
      as.data.frame()
  }
  
  # Add row names as a column
  out <- tibble::rownames_to_column(out, "names")
  
  # Calculate (exponentiated) coefficients
  if(exp==T){
    out$coef <- exp(modx$coefficients)
  }else{
    out$coef <- modx$coefficients
  }
  
  # Rename columns
  names(out) <- c("names", "lci", "uci", "coef")
  
  # Remove the first row (intercept)
  out <- out[-1, ]
  
 
  # Merge the data frames
  #out <- left_join(out, noms, by = "names")
  
  # Add empty space column for forest plot
  out$" " <- paste(rep(" ", 20), collapse = " ")
  
  # Replace missing variable levels with empty strings
  #out$var <- ifelse(is.na(out$var), "", out$var)
  
  # Format exponentiated coefficients and confidence intervals
  out$or_ci <- ifelse(is.na(out$lci), "", sprintf("%.2f (%.2f to %.2f)", out$coef, out$lci, out$uci))
  
  return(out)
}
