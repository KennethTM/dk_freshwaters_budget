library(tidyverse);library(sf)

#Initial processing of GIS data read from the rawdata files. Files saved as .rds objects in data folder. 
#Files added to .gitignore because of size.

data_path <- paste0(getwd(), "/data/")
rawdata_path <- paste0(getwd(), "/rawdata/")

#Spatial data on lakes and streams downloaded from https://download.kortforsyningen.dk/ 

#Read from spatial files and save as .rds objects
#Denmark lakes
lakes <- st_read(paste0(data_path, "DK_PhysicalWaters_GML_UTM32-EUREF89/DK_StandingWater.gml")) %>%
  select(gml_id, area = surfaceArea)
saveRDS(lakes, paste0(data_path, "lakes.rds"))

#Denmark streams
streams <- st_read(paste0(rawdata_path, "DK_PhysicalWaters_GML_UTM32-EUREF89/DK_Watercourse.gml")) %>%
  select(gml_id, length)
saveRDS(streams, paste0(data_path, "streams.rds"))

#Process stream layer to calculate slope
streams <- readRDS(paste0(data_path, "streams.rds")) %>% 
  rownames_to_column(var = "L1") %>% 
  filter(length > 0)

#Extract coordinated to data.frame and use z-values to calculate slope as drop/distance
streams_slope <- streams %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  mutate(Z_na = ifelse(Z == -999 | Z < -20, NA, Z),
         L1 = as.character(L1)) %>% 
  group_by(L1) %>% 
  summarise(drop = abs(diff(range(Z_na, na.rm = TRUE)))) %>% 
  right_join(streams) %>% 
  mutate(slope = drop/length)
saveRDS(streams_slope, paste0(data_path, "streams_slope.rds"))
