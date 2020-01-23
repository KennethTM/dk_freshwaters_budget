library(tidyverse);library(sf)

data_path <- paste0(getwd(), "/data/")
rawdata_path <- paste0(getwd(), "/rawdata/")

# #dk standingwater
# lakes <- st_read(paste0(data_path, "DK_PhysicalWaters_GML_UTM32-EUREF89/DK_StandingWater.gml")) %>%
#   select(gml_id, area = surfaceArea)
# 
# saveRDS(lakes, paste0(data_path, "lakes.rds"))
# 
# #dk streams
# #calc slope by drop/distance
# streams <- st_read(paste0(rawdata_path, "DK_PhysicalWaters_GML_UTM32-EUREF89/DK_Watercourse.gml")) %>% 
#   select(gml_id, length) 
# 
# saveRDS(streams, paste0(data_path, "streams.rds"))


###
#prepare .sqlite file for extraction of elevation values (dtm 10 m)
#try two approaches: vector extract or rasterize/extract/vectorize - grass gis, or pktools max/min/other function?
readRDS(paste0(data_path, "streams.rds")) %>% 
  st_zm() %>% 
  st_write(paste0(data_path, "streams.sqlite"))
###


#process stream layer to calculate slope
streams <- readRDS(paste0(data_path, "streams.rds")) %>% 
  rownames_to_column(var = "L1") %>% 
  filter(length > 0)

# streams_clean <- streams %>% 
#   st_cast("MULTILINESTRING") %>% 
#   st_union() %>% 
#   st_line_merge() %>% 
#   st_cast("LINESTRING") %>% 
#   st_as_sf() %>% 
#   mutate(length = st_length(x))
# 
# saveRDS(streams_clean, paste0(data_path, "streams_clean.rds"))

#st_cast(st_line_merge(st_union(st_cast(tmp, "MULTILINESTRING"))), "LINESTRING")
#st_node
#arrange by descending z values for correct order?
#rasterize streams, extract z from dem or maybe pktools

streams_slope <- streams %>% 
  #slice(1:10000) %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  #filter(L1 == 306)
  mutate(Z_na = ifelse(Z == -999 | Z < -20, NA, Z),
         L1 = as.character(L1)) %>% 
  group_by(L1) %>% 
  #add_tally() %>%
  #filter(n > 3) %>%
  #arrange(Z_na) %>% 
  summarise(drop = abs(diff(range(Z_na, na.rm = TRUE)))) %>% 
  right_join(streams) %>% 
  mutate(slope = drop/length)

saveRDS(streams_slope, paste0(data_path, "streams_slope.rds"))
