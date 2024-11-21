
  
forest_fun <- function(modf=list(),mini=0, coef_name = "OR",clean_labs_df=NULL,ttl=NULL,base_s=12, x_trans="none",
                         maxi = 4, labs = "", h_line=1, rm_names=NULL, data=NA,set_lims=c(0.5,1:3)){
  
  
  # Load required packages
  require(forestploter)
  require(grid)
  require(dplyr)
  require(magrittr)
  require(Hmisc)
  require(stringr)
  
  # Initialize an empty data frame
  outdf <- data.frame()
  
  # Get the length of the mod list
  len <- length(modf)
  
  # Loop through the models
  for(i in 1:len) {
    
    out <- modf[[i]]
    
    # Append the data frame if there are multiple models
    if(i > 1) {
      
      if(labs[1]==""){
        warning("Define labels for each model using the `labs` argument")
      }
      
      print(head(out))
      noms <- out %>% dplyr::select(names)
      out <- out %>% dplyr::select(lci, uci, coef, or_ci)
      names(out) <- paste0(names(out), i)
      out <- bind_cols(noms,out)
      outdf <- full_join(outdf, out, by = c("names"))
    } else {
      outdf <- out %>% rename(or_ci1 = or_ci)
    }
    
  }
  
  
  # Remove NA values for prettier display
  outdf %<>% mutate_at(vars(starts_with("or_ci")), ~ifelse(is.na(.), "", .)) %>%  
    rename(lci1 = lci, uci1 = uci, coef1=coef)
  
  #Join in clean variable names
  #Join with dict of all variable names
  outdf <- left_join(outdf,clean_labs_df,by=c("names"="modlabs"))
  #Get headers for categorical vars (format as rows with no data)
  headers <- outdf %>% filter(labels!="") %>% distinct(names,.keep_all = T) %>%
    mutate_at(vars(starts_with("or_ci")), ~replace(.,is.character(.),NA)) %>% 
    mutate_at(vars(starts_with("lci")), ~replace(.,is.numeric(.),NA)) %>%
    mutate_at(vars(starts_with("uci")), ~replace(.,is.numeric(.),NA)) %>% 
    mutate_at(vars(starts_with("coef")), ~replace(.,is.numeric(.),NA))
  #Join reference category so can append (Ref=X) to header
  sel_vars_tmp <- headers %>% distinct(variable) %>% pull()
  sel_vars_tmp<- clean_labs_df %>% filter(variable %in% sel_vars_tmp) %>% 
    distinct(variable,.keep_all = T)
  #Replace variable with (Ref=X) information
  headers <- left_join(headers %>% dplyr::select(!labels), 
                       sel_vars_tmp %>% dplyr::select(variable,labels), by="variable" ) %>% 
    mutate(variable = paste0(variable, " (ref=",labels,")")) %>% 
    distinct(variable,.keep_all=T)
  #Indent category labels
  outdf  %<>%  mutate(variable = if_else(labels!="",paste0("           ",labels),variable))
  #Join in headers
  outdf <- bind_rows(headers,outdf) %>% arrange(sort,type_var,name)
  
  rm(headers)
  
  # Gen lists of uci,lci and coef for forestploter fun
  lci<- list()
  uci<- list()
  coef<- list()
  
  for (i in 1:len) {
    lci[[i]] <- outdf %>% pull(paste0("lci",i))
    uci[[i]] <- outdf %>% pull(paste0("uci",i))
    coef[[i]] <- outdf %>% pull(paste0("coef",i))
  }
  
  # Assign labels and rename columns for use in forestploter
  grp_labs <- labs
  outdf <- outdf %>%
    rename_at(vars(starts_with("or_ci")), ~paste0(coef_name, " (95% CI) ", grp_labs))
  
  # Set the theme 
  tm <- forest_theme(base_size = base_s,
    ci_lwd = 2,
    ci_pch = 19,
    legend_name = "",
    legend_value = grp_labs)
  
  #dplyr::select vars and rename for pretty display
  
  outdf$` ` <- paste(rep(" ", 20), collapse = " ")
  outdf <- outdf[, names(outdf) %in% c("variable", " ", paste0(coef_name, " (95% CI) ", grp_labs))] %>% 
    rename(Variable = variable) %>% relocate(Variable)
  
  #Replace NA's with empty strings
  outdf <- as.data.frame(outdf)
  outdf[is.na(outdf)] <- ""
  p <- forest(outdf,
              est = coef,
              lower = lci, 
              upper = uci,
              ci_column = 2,
              ref_line = h_line,
              x_trans = x_trans,
              xlim = c(mini, maxi),
              ticks_at = set_lims,
              title = ttl,
              theme = tm)
  p
  
  
}
    
  
  
 
  