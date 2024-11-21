

# Prep lme4 object for use in forest plotter function ---------------------

glm_forest_prep_mlogit <- function(mod,exp=T,filter_cat=NULL){
  
  require(dplyr)
  require(stringr)
  require(Hmisc)
  require(sandwich)
  
  modx <- mod #data frame with SE, CI, etc,
  out <- modx
  filter_cat <- paste0(filter_cat,"~")
  
  #filter coefficients
  out <- confint(mod) %>% as.data.frame()
  out$names <- rownames(out) #add var names
  out$Estimate <- mod$coefficients #add column with model coefficients
  rownames(out) <- NULL 
  out <- out[str_detect(out$names, filter_cat),] #keep selected multinomial level
  out$names <- str_remove(out$names,filter_cat) #rm filter from variable name
  
  # exposentiate if needed
  if(exp==T){
    out$`2.5 %` <- exp(out$`2.5 %`)
    out$`97.5 %` <- exp(out$`97.5 %`)
    out$Estimate <- exp(out$Estimate)
  }
  
  # Calculate (exponentiated) coefficients
  out <- out %>% select(c("names","2.5 %","97.5 %","Estimate"))
  
  names(out) <- c("names", "lci", "uci", "coef")
  # Remove the first row (intercept)
  out <- out[-1, ]
  
  rownames(out) <- NULL
  
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
