#Use data extracted from the GEUS JupiterXL database to calculate groundwater pCO2
#Create groundwater rasters covering Denmark

#Load libraries
library(raster)
library(fields)
library(tidyverse)
library(lubridate)
library(sf)
library(seacarb)

set.seed(100)

rawdata_path <- paste0(getwd(), "/rawdata/")

#Read extracted data
grw_ph_alk <- readRDS(paste0(rawdata_path, "grw_ph_alk.rds"))

#Subset data, filter observations after 1990, calculate station mean values and convert to spatial sf object
alk_ph_sf <- grw_ph_alk %>%
  mutate(datetime = ymd_hms(SAMPLEDATE)) %>%
  filter(!is.na(XUTM32EUREF89), !is.na(YUTM32EUREF89),
         datetime >= ymd_hm("1990-01-01 00:00"),
         AMOUNT > 0, 
         UNIT %in% c(3, 8)) %>%
  group_by(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89, LONG_TEXT) %>%
  summarise(value = mean(AMOUNT)) %>%
  rename(variable = LONG_TEXT) %>% 
  st_as_sf(coords = c("XUTM32EUREF89", "YUTM32EUREF89"), crs = 25832)

#Alkalinity spatial object
alk_sf <- alk_ph_sf %>% 
  filter(variable == "Alkalinitet,total TA")

#pH spatial object
#Subset to match observations of alkalinity and reduce computation time during interpolation
ph_sf <- alk_ph_sf %>% 
  filter(variable == "pH") %>% 
  sample_n(nrow(alk_sf))

#Get country polygon for Denmark
dk_polygon <- getData(country = "DNK", level = 0, path = rawdata_path) %>% 
  st_as_sf() %>% 
  st_transform(25832)

#Create empthy raster as template 
dk_raster_empthy <- raster(dk_polygon, res = c(1000, 1000))

#Do thin plate spline smoother for alkalinity using x and y coordinates
alk_tps <- Tps(st_coordinates(alk_sf), alk_sf$value)

#Interpolate to Denmark
alk_raster <- interpolate(dk_raster_empthy, alk_tps)

#Apply country polygon as mask
alk_raster_mask <- mask(alk_raster, as(dk_polygon, "Spatial"))

#Replace negative values (interpolation artifacts) with low value
alk_raster_mask[alk_raster_mask<0] <- 0.01

#Plot raster
plot(alk_raster_mask)

#Do thin plate spline smoother for pH  using x and y coordinates
ph_tps <- Tps(st_coordinates(ph_sf), ph_sf$value)

#Interpolate to Denmark
ph_raster <- interpolate(dk_raster_empthy, ph_tps)

#Apply country polygon as mask
ph_raster_mask <- mask(ph_raster, as(dk_polygon, "Spatial"))

#Plot raster
plot(ph_raster_mask)

#Use national groundwater alkalinity and pH raster to calculate pCO2 using a temperature of 10 degrees
#Create empthy raster
pco2_raster <- dk_raster_empthy

#Fill empthy raster with calculated pCO2 values
pco2_raster[] <- carb(flag = 8, var1 = ph_raster_mask[], var2 = alk_raster_mask[]/1000, S = 0, T = 10,
                      k1k2 = "w14", kf = "dg", ks = "d")$pCO2

#Plot raster 
plot(pco2_raster)

#Save raster for plotting
saveRDS(list("alk" = alk_raster_mask, 
             "ph" = ph_raster_mask,
             "pco2" = pco2_raster), 
        paste0(rawdata_path, "grw_alk_ph_pco2_rasters.rds"))
