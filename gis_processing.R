#Initial processing of GIS data read from the rawdata files. 
#Spatial files read, subsetted and saved as .rds objects in data folder for faster loading
#Files added to .gitignore because of size.
#Spatial data on lakes and streams is open to and can be downloaded from https://download.kortforsyningen.dk/ 

#Load libs and set paths to data
library(tidyverse);library(sf)

data_path <- paste0(getwd(), "/data/")
rawdata_path <- paste0(getwd(), "/rawdata/")

#Read from spatial files and save as .rds objects
#Denmark lakes
lakes <- st_read(paste0(rawdata_path, "DK_PhysicalWaters_GML_UTM32-EUREF89/DK_StandingWater.gml")) %>%
  select(gml_id, area = surfaceArea)
saveRDS(lakes, paste0(data_path, "lakes.rds"))

#Denmark streams
streams <- st_read(paste0(rawdata_path, "DK_PhysicalWaters_GML_UTM32-EUREF89/DK_Watercourse.gml")) %>%
  select(gml_id, length)
saveRDS(streams, paste0(data_path, "streams.rds"))

#Read stream layer to calculate stream channel slope using the supplied XYZ coordinates
#Add rownames to id stream segments
streams <- readRDS(paste0(data_path, "streams.rds")) %>% 
  rownames_to_column(var = "L1") %>% 
  filter(length > 0)

#Extract coordinates to data.frame and use z-values to calculate slope as drop/distance
#Negative values are replaced by NA
#Drop calculated from the range in Z values per stream segment
#Slope is calculated as drop over distance
#Slopes are joined with stream geometries and saved to file
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
