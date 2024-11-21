
#Author: Fabian Reitzug
#Date: 2022-07-28
#Purpose: get lake distance 

#note: run after prep_hh+clinical.R


# Get paths ---------------------------------------------------------------
source("path/path.R") #paths


# Load packages -----------------------------------------------------------
.libPaths(paste0(path.git,"library"))

library(ggpubr)
library(readr)
library(stringr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(sf)
library(nngeo)
library(rgeos)

# Source scripts ----------------------------------------------------------
source("prep/prep_hh+clinical.R")


#load data ----------------------------------------------------------------
lk_sf <- st_read(paste0(path.sha,"data/waterbodies_wri/Ug_Waterbodies.shp"))
df_sf <- st_as_sf(df, coords = c("longitude","latitude"),crs=4326) #create sf obj
sn <- read.csv(paste0(path.dbl,"df_snail_site.csv"))
names(sn) <- odk_clean(sn)
sn_sf <- st_as_sf(sn %>% filter(!is.na(longitude)), coords = c("longitude","latitude"),crs=4326) #create sf obj
#processing village infrastructure boundary pts
vi <- read.csv(paste0(path.dbl,"df_infrastructure.csv"))
names(vi) <- odk_clean(vi)
vi1 <- vi %>% select(village,latitude.1,latitude.2,latitude.3,latitude.4,latitude.5,latitude.6,latitude.7,latitude.8,latitude.9,latitude.10)
vi2 <- vi %>% select(village,longitude.1,longitude.2,longitude.3,longitude.4,longitude.5,longitude.6,longitude.7,longitude.8,longitude.9,longitude.10)
vi1 <- gather(vi1,key="key",value = "latitude",-village)
vi2 <- gather(vi2,key="key",value = "longitude",-village)
vi <- bind_cols(vi1 %>% select(-key),vi2 %>% select(-village,-key)) %>% rename(village_name)  %>% na.omit()

#get distance to nearest point on shore
m.project <- "+proj=utm +zone=36 +north +datum=WGS84 +units=m +no_defs"


ldist <- rgeos::gDistance(as_Spatial(st_transform(df_sf,m.project)),as_Spatial(st_transform(lk_sf,m.project)),byid=TRUE)
df$lakedist_line <- ldist[1,]
#plot result
plot(df$lakedist,df$lakedist2)
lines(0:3000,0:3000,col="red")

#plot pts with 0m lake dist
leaflet() %>% addTiles() %>% addCircleMarkers(data=df[df$lakedist2==0,],fill = "red",radius=1.5 ) %>% addPolygons(data=lk_sf)

#get distance to nearest water site -----------------------------------------------
nn= nngeo::st_nn(df_sf,sn_sf, returnDist = TRUE)
l =st_connect(df_sf,sn_sf, ids = nn[[1]])
leaflet(l) %>% addTiles() %>% addPolylines() %>% addMarkers(data=sn_sf) %>% addCircleMarkers(data=df_sf,fill = "red",radius=1.5 )

df$lakedist_ws <- unlist(nn[[2]])
df$vill_nearsite <- sn$village[unlist(nn[[1]])]
df$nearsite_in_vill <- ifelse(df$village_name==df$vill_nearsite,1,0)

pts <- bind_rows(df %>% select(latitude,longitude,village_name),sn %>% 
                     rename(village_name=village) %>% select(latitude,longitude,village_name)) %>% 
 filter(!is.na(longitude))  %>% 
  st_as_sf(coords = c("longitude","latitude"),crs=4326)


df_bn <- st_sf(village_name = 1:length(unique(df$village_name)), geometry = st_sfc(lapply(1:length(unique(df$village_name)), function(x) st_geometrycollection())))

for(i in 1:length(unique(df$village_name))){
  shp <- st_polygon(list(st_convex_hull(st_union(pts$geometry[pts$village_name==unique(df$village_name)[i],] ))[[1]][[1]]))
  df_bn$village_name[i] <- unique(df$village_name)[i]
  df_bn$geometry[i] <- shp
}


test <- st_convex_hull(st_union(bound$geometry[bound$village_name=="jakok",] ))
plot(test)

leaflet() %>% addTiles() %>% 
  addPolygons(data=df_bn) %>% addMarkers(data=sn_sf)




