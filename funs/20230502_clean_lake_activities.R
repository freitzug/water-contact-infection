
#logic of script:

#outputs generated

#df_lake -> ind. lake activities

#Vars
#nlact_hh - number of distinct lake activities per hh
#nlact_ind - number of distinct lake activities per ind.


# Get paths ----------------------------------------------------------------
#source("path/path.R") #paths

# load packages -----------------------------------------------------------

# set wd ------------------------------------------------------------------


# load data ---------------------------------------------------------------

#df <- read_csv("8_clean_master_data/df_hh_identifiable.csv")
#df2 <- read.csv(paste0(path.dbl,"df_hh.csv"))


# Individual-level lake activity matrix -----------------------------------

require(stringr)
require(stringi)
require(magrittr)
require(dplyr)

clean_lact <- function(df){
  
  df2 <- df_hh #create copy
  
  sel <- names(df2)[str_detect(names(df2),".hh_13_|h_g5.hh_cname|h_g5.hh_fname_value")]
  df.hh <- df2[,str_detect(names(df2),"barcode|hh_mem_id|.hh_13_")]
  
  
  #lake activities from h_g20.hh_13_1_activity_lake
  lact <- c("washing clothes without soap",
            "washing clothes with soap",
            "washing jerry cans or other household items",
            "getting drinking water",
            "bathing without soap",
            "bathing with soap",
            "swimming or plays",
            "fishing",
            "fishmongering",
            "collecting papyrus",
            "collecting shells")
  
  #format for string matching
  lact_mem <- paste0("mem_act__",lact) %>% str_replace_all(" ","_")
  
  #select all relevant columns
  df_key <- df2[,str_detect(names(df2),str_flatten(lact_mem,"|"))]
  
  #empty data frame
  df_lact <- df_key
  df_lact[!is.na(df_lact)] <- NA
  
  #create df of zero's and one's for water contact
  for(i in 1:nrow(df_key)){
    for(j in 1:length(lact)){
      df_lact[i,j] <- str_detect(df_key[i,j],df2$hh_mem_id[i]) 
    }
  }
  
  df_lact[is.na(df_lact)] <- 0
  df_lact[df_lact==FALSE] <- 0
  df_lact[df_lact==TRUE] <- 1
  
  df_lact %<>% mutate_each(as.numeric)
  
  # gen nr of lake activities per individual
  df_lact$nlact_ind <- rowSums(df_lact)
  
  df_lact %<>% mutate(hh_mem_id = df2$hh_mem_id,nlact = df2$h_g20.hh_13_1_activity_lake)
  
  str_c <- function(x){
    str_split(x,pattern = " ", simplify = T) %>% unlist() %>% length()
  }
  
  df_lact$nlact_hh <- lapply(df_lact$nlact, str_c) %>% unlist
  df_lact$nlact_hh <- ifelse(df2$h_g20.hh_13_1_activity_lake=="none",0,df_lact$nlact_hh)
  df_lact$h_g20.hh_13_1_activity_lake <- df2$h_g20.hh_13_1_activity_lake
  
  #lake time spent
  lact_time <- paste0("time_spent_water__",lact) %>% str_replace_all(" ","_")
  df_lact_time <- df2[,str_detect(names(df2),str_flatten(lact_time,"|"))]
  names(df_lact_time) <- paste0(names(df_lact_time),"_time_spent")
  df_lact_time %<>% mutate(hh_mem_id = df2$hh_mem_id)
  df_lact_time[is.na(df_lact_time)] <- "none"
  
  #lake trips
  lact_trips <- paste0("trips_to_water__",lact) %>% str_replace_all(" ","_")
  df_lact_trips <- df2[,str_detect(names(df2),str_flatten(lact_trips,"|"))]
  names(df_lact_trips) <- paste0(names(df_lact_trips),"_trips_to_water")
  df_lact_trips %<>% mutate(hh_mem_id = df2$hh_mem_id)
  df_lact_trips[is.na(df_lact_trips)] <- 0
  
  #time of day
  lact_daytime <- paste0("time_of_day_in_water__",lact) %>% str_replace_all(" ","_")
  df_lact_daytime <- df2[,str_detect(names(df2),str_flatten(lact_daytime,"|"))]
  names(df_lact_daytime) <- paste0(names(df_lact_daytime),"_time_of_day_in_water")
  df_lact_daytime %<>% mutate(hh_mem_id = df2$hh_mem_id)
  df_lact_daytime[is.na(df_lact_daytime)] <- "none"
  
  rm(df2,df.hh)
  
  noms <- c("df_lact","df_lact_time","df_lact_trips","df_lact_daytime")
  out <- list(noms,df_lact,df_lact_time,df_lact_trips,df_lact_daytime)
  return(out)

}
