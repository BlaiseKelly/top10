3#library(raster)
library(sf)
library(dplyr)
library(stringr)
#library(RCurl)


defz <- read.csv("defs/land_ids.csv")

dir.create('outputs/dat/RDS/', recursive = TRUE)
dir.create('outputs/dat/zip/', recursive = TRUE)
dir.create('outputs/dat/temp/', recursive = TRUE)

##KAARTBLADDEN IS THE OLD DUTCH GEOSPATIAL GRID USED BY SOME FORMATS. THIS SCRIPT GENERATED A GEOJSON AND SHAPE FILE
##to be used for applications such as the dutch terrain data download

seq_num <-  sprintf("%02d", seq(1:99))

O299 <- seq_num

## get TOP10 
for(Z in O299){
  tryCatch({
    
    download.file(paste0("http://geodata.nationaalgeoregister.nl/top10nlv2/extract/kaartbladen/TOP10NL_", Z, "W.zip?formaat=gml"), destfile = paste0("outputs/dat/zip/", Z, "W.zip"), mode = "wb")
    unzip(zipfile = paste0("outputs/dat/zip/", Z, "W.zip") , exdir = paste0("outputs/dat/temp"))
    
    land <- st_read(paste0("outputs/dat/temp/TOP10NL_", Z, "W.gml"), layer = "Terrein")
    water <- st_read(paste0("outputs/dat/temp/TOP10NL_", Z, "W.gml"), layer = "Waterdeel")
    asphalt <- st_read(paste0("outputs/dat/temp/TOP10NL_", Z, "W.gml"), layer = "Wegdeel")
    
    land_w <- land %>% 
      transmute(gml_id, type = typeLandgebruik, geometry)
    water_w <- water %>% 
      transmute(gml_id, type = typeWater, geometry)
    asphalt_w <- asphalt %>% 
      transmute(gml_id, type = verhardingstype, geometry)
    
    all_w <- rbind(land_w, water_w, asphalt_w)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "/n")})
  
  tryCatch({
    download.file(paste0("http://geodata.nationaalgeoregister.nl/top10nlv2/extract/kaartbladen/TOP10NL_", Z, "O.zip?formaat=gml"), destfile = paste0("outputs/dat/zip/", Z, "O.zip"), mode = "wb")
    unzip(zipfile = paste0("outputs/dat/zip/", Z, "O.zip") , exdir = paste0("outputs/dat/temp"))
    
    land <- st_read(paste0("outputs/dat/temp/TOP10NL_", Z, "O.gml"), layer = "Terrein")
    water <- st_read(paste0("outputs/dat/temp/TOP10NL_", Z, "O.gml"), layer = "Waterdeel")
    asphalt <- st_read(paste0("outputs/dat/temp/TOP10NL_", Z, "O.gml"), layer = "Wegdeel")
    
    land_o <- land %>% 
      transmute(gml_id, type = typeLandgebruik, geometry)
    water_o <- water %>% 
      transmute(gml_id, type = typeWater, geometry)
    asphalt_o <- asphalt %>% 
      transmute(gml_id, type = verhardingstype, geometry)
    
    all_o <- rbind(land_o, water_o, asphalt_o)
    
    all <- rbind(all_w, all_o)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "/n")})
  
  if(exists('all_o') & exists('all_w')){
    all_lu <- all %>% 
      left_join(defz, by = c('type' = 'top10_nam_nl')) %>% 
      mutate(g_type = st_geometry_type(.)) %>% 
      filter(g_type == "POLYGON")
    
    saveRDS(all_lu, paste0("outputs/dat/RDS/", Z, ".RDS"))
  }
  
  if(exists('all_o') & !exists('all_w')){
    all_lu <- all_o %>% 
      left_join(defz, by = c('type' = 'top10_nam_nl')) %>% 
      mutate(g_type = st_geometry_type(.)) %>% 
      filter(g_type == "POLYGON")
    
    saveRDS(all_lu, paste0( "outputs/dat/RDS/", Z, ".RDS"))
  }
  
  if(exists('all_w') & !exists('all_o')){
    all_lu <- all_w %>% 
      left_join(defz, by = c('type' = 'top10_nam_nl')) %>% 
      mutate(g_type = st_geometry_type(.)) %>% 
      filter(g_type == "POLYGON")
    
    saveRDS(all_lu, paste0("outputs/dat/RDS/", Z, ".RDS"))
  }
  
  
  rm(all_o)
  rm(all_w)
  rm(all_lu)
  
  unlink("outputs/dat/temp/", recursive = TRUE)
  unlink("outputs/dat/zip/", recursive = TRUE)
  
  
}
