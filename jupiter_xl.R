library(odbc)
library(tidyverse)
library(lubridate)
library(sf)

#Connect to local copy of GEUS jupiter xl
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "kenneth_martinsen_bio_ku_dk_14207319_pcjupiter_xl", 
                      Trusted_Connection = "True")

#DBI::dbDisconnect(con)

tables <- DBI::dbListTables(con)

compoundlist <- tbl(con, "COMPOUNDLIST") %>% 
  select(COMPOUNDNO, LONG_TEXT) %>% 
  collect()

grep("COMPOUND*", tables, value = TRUE)

analysis <- tbl(con, "GRWCHEMANALYSIS") %>% 
  select(SAMPLEID, COMPOUNDNO, AMOUNT, UNIT)

sample <- tbl(con, "GRWCHEMSAMPLE") %>% 
  select(SAMPLEID, BOREHOLENO, SAMPLEDATE)

compounds <- tbl(con, "COMPOUNDLIST") %>% 
  select(COMPOUNDNO, LONG_TEXT)
  
borehole <- tbl(con, "BOREHOLE") %>% 
  select(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89)

all <- analysis %>% 
  left_join(sample) %>% 
  left_join(compounds) %>% 
  left_join(borehole)

alk <- all %>% 
  filter(COMPOUNDNO == 291) %>% 
  collect()

ph <- all %>% 
  filter(COMPOUNDNO == 41) %>% 
  collect()

alk_sf <- alk %>% 
  na.omit() %>% 
  mutate(datetime = ymd_hms(SAMPLEDATE)) %>% 
  filter(datetime >= ymd_hm("1990-01-01 00:00"),
         AMOUNT > 0) %>% 
  group_by(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89) %>% 
  summarise(alk = mean(AMOUNT)) %>% 
  st_as_sf(coords = c("XUTM32EUREF89", "YUTM32EUREF89"), crs = 25832)

ph_sf <- ph %>% 
  na.omit() %>% 
  mutate(datetime = ymd_hms(SAMPLEDATE)) %>% 
  filter(datetime >= ymd_hm("1990-01-01 00:00"),
         between(AMOUNT, 0, 14)) %>% 
  group_by(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89) %>% 
  summarise(ph = mean(AMOUNT)) %>% 
  st_as_sf(coords = c("XUTM32EUREF89", "YUTM32EUREF89"), crs = 25832)

saveRDS(list(alk_sf, ph_sf), "grw_ph_alk.rds")




library(raster)
library(fields)

dk_polygon <- getData(country = "DNK", level = 0) %>% 
  st_as_sf() %>% 
  st_transform(25832)

dk_raster_empthy <- raster(dk_polygon, res = c(1000, 1000))

alk_tps <- Tps(st_coordinates(alk_sf), alk_sf$alk)

alk_raster <- interpolate(dk_raster_empthy, alk_tps)

alk_raster_mask <- mask(alk_raster, as(dk_polygon, "Spatial"))

ph_tps <- Tps(st_coordinates(ph_sf), ph_sf$ph)

ph_raster <- interpolate(dk_raster_empthy, ph_tps)

ph_raster_mask <- mask(ph_raster, as(dk_polygon, "Spatial"))

library(seacarb)

pco2_raster <- dk_raster_empthy

pco2_raster[] <- carb(flag = 8, var1 = ph_raster_mask[], var2 = alk_raster_mask[], S = 0, T = 10)
