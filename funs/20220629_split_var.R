#Author: Fabian Reitzug
#Date: 2022-06-29
#Purpose: Splits variables with multi-select options saved as a character string into on var per category


require(stringr)
require(stringi)
require(magrittr)
require(dplyr)

split_var <- function(df,x){
  
  df0 <- df
  inp <- df0[,x] %>% str_replace_all("/","_") #need to replace invalid character
  vec <- str_split(inp," ") %>% unlist() %>% unique() %>% str_replace_all("/","_")
  
  #loop over each unique string in vec to generate variable with name x_vec[i]
  for(i in 1:length(vec)){
    var <- str_detect(inp,vec[i])
    var <- ifelse(var==TRUE,1,0)
    nom <- paste0(x,"_",vec[i])
    df0 %<>% dplyr::mutate(!! nom := var)
  }
  df <- df0
  return(df)
}

