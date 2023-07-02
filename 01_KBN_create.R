#library(raster)
library(sf)
library(dplyr)
library(stringr)
library(RCurl)

##KAARTBLADDEN IS THE OLD DUTCH GEOSPATIAL GRID USED BY SOME FORMATS. THIS SCRIPT GENERATED A GEOJSON AND SHAPE FILE
##to be used for applications such as the dutch terrain data download

Oneto99 <- seq(1:99)
Oneto99 <- sprintf("%02d", Oneto99)

polygon_list <- list()

##downloads from the main server, not all variations exist so errors will be generated, however, ignore them
for (O in Oneto99){
  
  Ls <- LETTERS[1:8]
  
  for(L in Ls){
    tryCatch({
      
      r <- read.csv(paste0("https://data.nlextract.nl/opentopo/worldfiles/800pixkm/800-", O,L, ".wld"), header = FALSE)
      closeAllConnections()
      xmin <- r[5,]
      xmax <- xmin+10000
      ymax <- r[6,]
      ymin <- ymax-12500
      
      df <- data.frame(X = c(xmin, xmax, xmax, xmin), 
                       Y = c(ymax, ymax, ymin, ymin))
      df_area <- df %>%
        st_as_sf(coords = c("X", "Y"), crs = 28992) %>%
        dplyr::summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      
      df_area$ID <- paste0(O,L)
      
      
      dT <- st_make_grid(df_area, square = T, n = c(2, 2)) %>% # the grid, covering bounding box
        st_sf()
      
      dT$NS <- c("Z1", "Z2", "N1", "N2")
      
      dT$KBN <- paste0(O,L, dT$NS)
      
      nam <- paste0(O,L)
      polygon_list[[nam]] <- dT
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  closeAllConnections()
}
closeAllConnections()
all_800s <- do.call(rbind, polygon_list)
KBN_grid <- dplyr::select(all_800s, -NS)

## define codes
KBN_grid$KBN3 <- str_sub(KBN_grid$KBN, 3, -3)

KBN_grid$OW <- ifelse(KBN_grid$KBN3 == "A" | KBN_grid$KBN3 == "B" | KBN_grid$KBN3 == "C" | KBN_grid$KBN3 == "D", "W", "O")

## write output
dir.create('outputs/dat/', recursive = TRUE)
saveRDS(KBN_grid, "outputs/dat/KBN_grid.RDS")
