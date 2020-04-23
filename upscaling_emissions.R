#Upscaling to national freshwater emissions from lake and stream CO2 data
#Script also used for creating figures by sourcing

#Load libraries, set paths and seed for reproducability
library(raster);library(tidyverse);library(lubridate);library(sf)
library(seacarb);library(lwgeom);library(openxlsx)

set.seed(9999)

rawdata_path <- paste0(getwd(), "/rawdata/")

#EPSG number for UTM zone 32 for Denmark
dk_epsg <- 3044 #25832

#Set data range
time_start <- ymd("1989-01-01")
time_end <- ymd("2010-12-31")

#####Load and set up nescessary data

#Load data and exclude outliers or low pH values which may bias CO2 calculation
dk_carb <- readRDS(paste0(rawdata_path, "dk_carb.rds")) %>% 
  tbl_df() %>% 
  filter(between(date, time_start, time_end)) %>% 
  filter(alk > 0 & alk < 10, 
         wtr < 40,
         ph > 5.4,
         pCO2_uatm < 40000)

#Stream and lake pco2 data
vl_pco2 <- dk_carb %>%
  filter(system == "vl") %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), crs = dk_epsg)

so_pco2 <- dk_carb %>%
  filter(system == "so") %>%
  select(-q_m3_s) %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), crs = dk_epsg)

#Denmark climate grid 20x20 km 1990-2010 downloaded from DMI
wnd_grid <- read.delim(paste0(rawdata_path, "tr12-10_20x20km/daily_20x20km_wind speed_1989-2010.txt"),
                       sep = ";", stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  rename(wnd = X20.20km_Mean_wind_speed_.m.s., 
         dmi_cell = gridcelle) %>%
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  select(date, dmi_cell, utm_zone, eastings, northings, wnd)

#Wind grid as spatial object
wnd_grid_sf <- wnd_grid %>%
  select(dmi_cell, eastings, northings) %>%
  distinct() %>%
  st_as_sf(coords = c("eastings", "northings"), crs = 23032) %>% 
  st_transform(dk_epsg)

#Denmark lakes
lakes <- readRDS(paste0(rawdata_path, "lakes.rds"))

#Denmark lakes CO2 data joined with wind and area data
so_pco2_sf <- so_pco2 %>%
  st_join(wnd_grid_sf, join = st_nearest_feature) %>%
  st_join(lakes) %>%
  left_join(wnd_grid[, c("date", "dmi_cell", "wnd")])

#Denmark streams segments with slope
streams_slope <- readRDS(paste0(rawdata_path, "streams_slope.rds")) %>% 
  st_as_sf()

#Join slope values to CO2 data
vl_pco2_sf <- vl_pco2 %>%
  st_join(streams_slope, join = st_nearest_feature)

##### Upscaling lake emissions
#Define size categories
lake_size_cats <- c(0, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8)

#Summarise count and area of danish lakes
lake_size <- lakes %>% 
  st_set_geometry(NULL) %>% 
  mutate(sizecat = cut(area, breaks = lake_size_cats, right = FALSE)) %>% 
  group_by(sizecat) %>% 
  summarise(n = n(), total_area = sum(area))

#Cut area into size categories
so_pco2_sizecat <- so_pco2_sf %>% 
  mutate(sizecat = cut(area, breaks = lake_size_cats, right = FALSE)) %>% 
  st_set_geometry(NULL)

#Calculate flux using empirical parameterizations of gas exchange velocity from wind speed (and area for one)
#Use gas exchange velocity to calculate CO2 flux
so_co2_flux <- so_pco2_sizecat %>% 
  mutate(k600_cole = 2.07 + 0.215 * wnd^1.7, 
         k600_crusius_1 = 0.228 * wnd^2.2 + 0.168, 
         k600_crusius_2 = ifelse(wnd < 3.7, 0.72*wnd, 4.33*wnd-13.3),
         k600_vachon_1 = 1.41 + 2.58 * wnd,
         k600_vachon_2 = 2.51 + 1.48 * wnd + 0.39 * wnd * log10(area),
         sc = 1742 + -91.24*wtr + 2.208*wtr^2 + -0.0219*wtr^3,
         k600_mean = (k600_cole + k600_crusius_1 + k600_crusius_2 + k600_vachon_1 + k600_vachon_2)/5,
         kgas = (sc/600)^-0.5 * k600_mean) %>%  #k600 og kgas in cm/h
  mutate(co2_eq_mol_kg = 400 * 10^-6 * K0(S = 0, T = wtr)) %>% 
  mutate(co2_flux_mmol_m2_h = kgas/100*(CO2_mol_kg-co2_eq_mol_kg)*10^6) #%>%  #flux in mmol CO2/m2/h

#Summarise fluxes per lake size category
so_co2_flux_sizeclass <- so_co2_flux %>% 
  group_by(sizecat) %>% 
  summarise(n_samples = n(), 
            n_sites = length(unique(site_id)), 
            mean = mean(co2_flux_mmol_m2_h), 
            sd = sd(co2_flux_mmol_m2_h)) %>% 
  na.omit()

#Monte-Carlo simulation of uncertainty by 1000 random draws from a normal distribution defined by the estimated mean and standard deviation 
so_upscale <- so_co2_flux_sizeclass %>% 
  left_join(lake_size) %>% 
  mutate(flux_rnorm = map2(mean, sd, rnorm, n = 1000)) %>% 
  unnest(flux_rnorm) %>% 
  mutate(flux_year = flux_rnorm * total_area * 24 * 365 * 10^-3,
         flux_gigagram = flux_year * 12 * 10^-9)  #flux_year = mol/år, flux_gigagram = gigagram C/år

#Summarise simulated data
so_upscale_sizecat <- so_upscale %>% 
  select(sizecat, flux_gigagram) %>% 
  group_by(sizecat) %>% 
  summarise(q_05 = quantile(flux_gigagram, 0.05),
            q_25 = quantile(flux_gigagram, 0.25),
            q_50 = quantile(flux_gigagram, 0.5),
            q_75 = quantile(flux_gigagram, 0.75),
            q_95 = quantile(flux_gigagram, 0.95),
            mean = mean(flux_gigagram))

#Compute sums from simulated data
so_upscale_total <- so_upscale %>% 
  select(sizecat, flux_gigagram) %>% 
  add_column(id = rep(1:1000, 6)) %>% 
  spread(sizecat, flux_gigagram) %>% 
  mutate(flux_gigagram_sum = rowSums(.[-1])) %>% 
  summarise(q_05 = quantile(flux_gigagram_sum, 0.05),
            q_25 = quantile(flux_gigagram_sum, 0.25),
            q_50 = quantile(flux_gigagram_sum, 0.5),
            q_75 = quantile(flux_gigagram_sum, 0.75),
            q_95 = quantile(flux_gigagram_sum, 0.95),
            mean = mean(flux_gigagram_sum))

##### Upscaling stream emissions
#Define size categories and their length and average size (from national report on nitrogen export)
vl_area <- tibble(sizecat = c("small", "medium", "large"), 
                  width = c("0-2.5 m", "2.5-12 m", ">12 m"), 
                  length = c(45480, 16600, 2660),
                  avg_width = c(1.23, 5.15, 16.61),
                  area = length*1000*avg_width)

#Use stream hydraulic to calculate width, velocity and depth from discharge (Raymond 2012)
vl_pco2_hydro <- vl_pco2_sf %>% 
  st_set_geometry(NULL) %>% 
  mutate(v = 0.19*q_m3_s^0.285, 
         d = 0.4*q_m3_s^0.294, 
         w = 12.88*q_m3_s^0.423, 
         fr = v/(9.82*d)^0.5)

#Add size categories to stream data
vl_stat_sizecat <- vl_pco2_hydro %>% 
  group_by(site_id) %>% 
  summarise(site_id_w = mean(w, na.rm = TRUE)) %>% 
  mutate(sizecat = cut(site_id_w, breaks = c(0, 2.5, 12, 100), 
                       right = FALSE, labels = c("small", "medium", "large"))) %>% 
  na.omit()

#Calculate mean width for stream categories
vl_sizecat_area <- vl_stat_sizecat %>% 
  group_by(sizecat) %>% 
  summarise(sizecat_w = mean(site_id_w, na.rm = TRUE)) %>% 
  left_join(vl_area)

#Use empirical relatinoship in Raymond 2012 (eq. 5) to estimate gas transfer velocity and flux
vl_co2_flux <- vl_pco2_hydro %>% 
  left_join(vl_stat_sizecat) %>% 
  mutate(sc = 1742 + -91.24*wtr + 2.208*wtr^2 + -0.0219*wtr^3,
         k600_5 = v*slope*2841+2.02) %>% #k600 in m/d
  mutate(kgas = (sc/600)^-0.5*k600_5) %>% 
  mutate(co2_eq_mol_kg = 400*10^-6*K0(S = 0, T = wtr)) %>% 
  mutate(co2_flux_mmol_m2_h = kgas/24*(CO2_mol_kg-co2_eq_mol_kg)*10^6) #flux in mmol CO2/m2/h

#Summarise fluxes per stream size category
vl_co2_flux_sizeclass <- vl_co2_flux %>% 
  group_by(sizecat) %>% 
  summarise(n_samples = n(), 
            n_sites = length(unique(site_id)), 
            mean = mean(co2_flux_mmol_m2_h, na.rm = TRUE), 
            sd = sd(co2_flux_mmol_m2_h, na.rm = TRUE)) %>% 
  na.omit()

#Monte-Carlo simulation of uncertainty by 1000 random draws from a normal distribution defined by the estimated mean and standard deviation 
vl_upscale <- vl_co2_flux_sizeclass %>% 
  left_join(vl_sizecat_area) %>% 
  mutate(flux_rnorm = map2(mean, sd, rnorm, n = 1000)) %>%
  unnest(flux_rnorm) %>% 
  mutate(flux_year = flux_rnorm * area * 24 * 365 * 10^-3,
         flux_gigagram = flux_year * 12 * 10^-9)  #flux_year = mol/år, flux_gigagram = giga C/year

#Summarise simulated data
vl_upscale_sizecat <- vl_upscale %>% 
  select(sizecat, flux_gigagram) %>% 
  group_by(sizecat) %>% 
  summarise(q_05 = quantile(flux_gigagram, 0.05),
            q_25 = quantile(flux_gigagram, 0.25),
            q_50 = quantile(flux_gigagram, 0.5),
            q_75 = quantile(flux_gigagram, 0.75),
            q_95 = quantile(flux_gigagram, 0.95),
            mean = mean(flux_gigagram))

#Compute sums from simulated data
vl_upscale_total <- vl_upscale %>% 
  select(sizecat, flux_gigagram) %>% 
  add_column(id = rep(1:1000, 3)) %>% 
  spread(sizecat, flux_gigagram) %>% 
  mutate(flux_gigagram_sum = rowSums(.[-1])) %>% 
  summarise(q_05 = quantile(flux_gigagram_sum, 0.05),
            q_25 = quantile(flux_gigagram_sum, 0.25),
            q_50 = quantile(flux_gigagram_sum, 0.5),
            q_75 = quantile(flux_gigagram_sum, 0.75),
            q_95 = quantile(flux_gigagram_sum, 0.95),
            mean = mean(flux_gigagram_sum))

##### Total stream emissions
#Combined table for lake and streams
vl_so_samples_sites <- bind_rows(add_column(vl_co2_flux_sizeclass, system = "stream"), 
                                 add_column(so_co2_flux_sizeclass, system = "lake")) %>% 
  select(sizecat, system, n_sites, n_samples)

upscale_total <- bind_rows(bind_rows(add_column(so_upscale_sizecat, system = "lake"), 
                                     add_column(so_upscale_total, sizecat = "total", system = "lake")),
                           bind_rows(add_column(vl_upscale_sizecat, system = "stream"), 
                                     add_column(vl_upscale_total, sizecat = "total", system = "stream"))) %>% 
  left_join(vl_so_samples_sites) %>% 
  mutate(mean_label = paste0(round(mean, 1), " (", round(q_25, 1), " - ", round(q_75, 1), ")"),
         site_sample_label = paste0(n_sites, "/", n_samples))

#Write to excel
#Units in table is gigagram C per year
write.xlsx(upscale_total, "co2_budget_table.xlsx")

