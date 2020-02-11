#Read data from jupiter xl database
library(raster)
library(fields)
library(tidyverse)
library(lubridate)
library(sf)

#Read extracted data
grw_ph_alk <- readRDS(paste0(getwd(), "/data/grw_ph_alk.rds"))

#Subset data and convert to spatial sf object
alk_ph_sf <- grw_ph_alk %>%
  mutate(datetime = ymd_hms(SAMPLEDATE)) %>%
  filter(!is.na(XUTM32EUREF89), !is.na(YUTM32EUREF89),
         datetime >= ymd_hm("1990-01-01 00:00"),
         AMOUNT > 0, 
         UNIT %in% c(3, 8), 
         TOP < 30 | BOTTOM < 30) %>%
  group_by(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89, LONG_TEXT) %>%
  summarise(value = mean(AMOUNT)) %>%
  rename(variable = LONG_TEXT) %>% 
  st_as_sf(coords = c("XUTM32EUREF89", "YUTM32EUREF89"), crs = 25832)

alk_sf <- alk_ph_sf %>% 
  filter(variable == "Alkalinitet,total TA")

ph_sf <- alk_ph_sf %>% 
  filter(variable == "pH")

dk_polygon <- getData(country = "DNK", level = 0, path = paste0(getwd(), "/data")) %>% 
  st_as_sf() %>% 
  st_transform(25832)

dk_raster_empthy <- raster(dk_polygon, res = c(1000, 1000))

alk_tps <- Tps(st_coordinates(alk_sf), alk_sf$value)

alk_raster <- interpolate(dk_raster_empthy, alk_tps)

alk_raster_mask <- mask(alk_raster, as(dk_polygon, "Spatial"))

plot(alk_raster_mask)

ph_tps <- Tps(st_coordinates(ph_sf), ph_sf$value)

ph_raster <- interpolate(dk_raster_empthy, ph_tps)

ph_raster_mask <- mask(ph_raster, as(dk_polygon, "Spatial"))

plot(ph_raster_mask)

library(seacarb)

pco2_raster <- dk_raster_empthy

pco2_raster[] <- carb(flag = 8, var1 = ph_raster_mask[], var2 = alk_raster_mask[]/1000, S = 0, T = 10,
                      k1k2 = "w14", kf = "dg", ks = "d")$pCO2

plot(pco2_raster)

saveRDS(list("alk" = alk_raster_mask, 
             "ph" = ph_raster_mask,
             "pco2" = pco2_raster), 
        paste0(getwd(), "/data/grw_alk_ph_pco2_rasters.rds"))
