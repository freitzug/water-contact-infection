

#Nearest neighbor distance: returns df of distance in m to k nearest neighbor and columns with row number of nearest neighbor in the points df


#load packages
require(nngeo)
require(sf)
require(magrittr)

nn_calc <- function(df=df,df_points=df_points,k=k,`vars from df_points to include in output df`=vars,`names of lon/lat cols`=coords,
                    buffer=`search buffer in m`){
  
  if(! exists("buffer")){ #assign buffer
    buffer <- Inf
  }else{
    buffer <- buffer
  }
  
  #set k
  k = k
  #df to sf df
  df_sf <- df
  df_sf %<>% select(all_of(c("barcode",coords))) %>% st_as_sf(coords=coords)
  st_crs(df_sf) <- 4326
  
  #df with points to sf df
  df_points_sf <- df_points
  df_points_sf %<>% filter_at(coords, ~ !is.na(.x)) %>% st_as_sf(coords=coords)
  st_crs(df_points_sf) <- 4326
  
  #nn claculation
  nearest_features <- st_nn(df_sf, df_points_sf,k = k, returnDist = TRUE,sparse = T)
  
  #rlist function
  list.do <- function(.data, fun, ...) {
    do.call(what = fun, args = as.list(.data), ...)
  }
  list.rbind <- function(.data) {
    list.do(.data, "rbind")
  }
  
  #create output df
  ks <- list.rbind(nearest_features[[1]]) %>% as.data.frame()
  kdist <- list.rbind(nearest_features[[2]]) %>% as.data.frame()
  
  df_chars <- df_points %>% slice(ks[,1]) %>% select(all_of(vars)) #additional features based on nearest neighbor
  
  if(buffer<Inf){
    ks[kdist>buffer] <- NA
    kdist[kdist>buffer] <- NA
  }
  
  assign("nsites_in_buffer", rowSums(!is.na(ks)))
  noms_k <- paste0("k",1:k,"_obj")
  noms_dist <- paste0("dist_k",1:k)
  df_out <- cbind.data.frame(df_sf$barcode,df_chars,ks,kdist,nsites_in_buffer)
  names(df_out) <- c("barcode",paste0("k1_",vars),noms_k,noms_dist,"nsites_in_buffer")
  
  return(df_out)
  rm(buffer)
}

