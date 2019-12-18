library(tidyverse);library(lubridate);library(rkt)

files <- list.files("C:/Users/kenne/Documents/Submerged macrophyte MLR/odaforalle/", full.names = TRUE)

#nationalt ferskvands biodiversitets kort, sø og vandløb alle grupper/samlet

#vandføring, interpolerede dagsmålinger
discharge <- read.csv2(files[grepl("vl_vandføring", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(date = ymd(Dato)) #%>% 
  select(ObservationsStedNr, ObservationsStedNavn, date, Q_m3_s = Resultat)

library(sf)
discharge_avg <- discharge %>%
  na.omit() %>%
  mutate(year = year(ymd(Dato))) %>% 
  group_by(ObservationsStedNr, year, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32) %>%
  summarise(Q_m3_s = mean(Resultat), n = n()) %>%
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs") %>%
  rename(vl_id = ObservationsStedNr)
#st_write(discharge_avg, "C:/Users/kenne/Documents/DK DEM temp/all_mean_q.shp")

#pco2 ifht oplandstørrelse og landuse 1990-2010
#husk dato/måned, jordtyper/land use også downloades, nedbør fra dmi!, gam models?
library(seacarb)
vl_kemi_ph_alk <- read.csv2(files[grepl("vl_kemi_ph_alk_allobs", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(Parameter_Enhed = paste0(Parameter, "_", Enhed),
         date = ymd(Startdato),
         Resultat = parse_number(Resultat, locale = locale(decimal_mark = ","))) %>% 
  na.omit() %>% 
  filter(Parameter_Enhed %in% c("pH_pH", "Alkalinitet,total TA_mmol/l")) %>% 
  distinct() %>% 
  group_by(ObservationsStedNr, ObservationsStedNavn, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date, Parameter_Enhed) %>% 
  summarise(Resultat = mean(Resultat)) %>% 
  ungroup() %>% 
  spread(Parameter_Enhed, Resultat)

#####få _allobs fil til at loade uden EOF within quote?!?!###
vl_felt_temp_ph <- read.csv2(files[grepl("vl_felt_temp_ph.csv", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(Parameter_Enhed = paste0(Parameter, "_", Enhed),
         date = ymd(Startdato)) %>% 
  group_by(ObservationsStedNr, ObservationsStedNavn, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date, Parameter_Enhed) %>% 
  summarise(Resultat = mean(Resultat)) %>%
  ungroup() %>% 
  spread(Parameter_Enhed, Resultat) %>% 
  rename(pH_pH_felt = pH_pH) %>% 
  mutate(Xutm_Euref89_Zone32 = parse_character(Xutm_Euref89_Zone32),
         Yutm_Euref89_Zone32 = parse_character(Yutm_Euref89_Zone32))

vl_kemi_felt <- left_join(vl_kemi_ph_alk, vl_felt_temp_ph) %>% 
  mutate(pH_pH_combined = coalesce(pH_pH, pH_pH_felt)) %>% 
  select(-pH_pH, -pH_pH_felt) %>% 
  na.omit() %>% 
  rename(ph = pH_pH_combined, alk = `Alkalinitet,total TA_mmol/l`, wtr = `Temperatur_grader C`) %>% 
  filter(alk > 0, ph > 5) %>% 
  mutate(pco2_uatm = carb(8, ph, alk/1000, S = 0, T = wtr)$pCO2,
         co2_mol_kg = carb(8, ph, alk/1000, S = 0, T = wtr)$CO2,
         DIC_mol_kg = carb(8, ph, alk/1000, S = 0, T = wtr)$DIC) %>% 
  left_join(discharge)
#saveRDS(vl_kemi_felt, "C:/Users/kenne/Documents/DK DEM temp/vl_pco2.rds")

vl_ais <- read.csv2(files[grepl("vl_ais_lvl0", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  #select(ObservationsStedNr, ObservationsStedNavn, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, Oplandsareal, Arealanvendelsesnavn, Pct.af.areal) %>% 
  select(ObservationsStedNr, Oplandsareal, Arealanvendelsesnavn, Pct.af.areal) %>% 
  spread(Arealanvendelsesnavn, Pct.af.areal)
vl_ais[is.na(vl_ais)] <- 0

vl_kemi_felt_ais <- vl_kemi_felt %>% 
  group_by(ObservationsStedNr) %>% 
  summarise(pco2_med = median(pco2)) %>% 
  left_join(vl_ais) %>% 
  na.omit()

vl_kemi_felt_ais %>% 
  select(-ObservationsStedNr) %>% 
  lm(pco2_med~., data = .) %>% 
  summary()










#plantedata
#sø morfometri data? (slope osv), fetch
#plant cover/height i punk, machine learning/ensemble models
so_planter_pkt <- read.csv2(files[grepl("so_planter_pkt", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(date = ymd(StartDato),
         year = year(date),
         month = month(date)) 

library(raster);library(sp);library(sf)
dk_map <- getData("GADM", country = "DNK", level = 0)
dk_map_utm <- spTransform(dk_map, "+init=epsg:32632")
plot(dk_map_utm)
dk_raster_temp <- raster(crs = crs(dk_map_utm), res = 10000, ext = extent(dk_map_utm))
dk_raster_grid <- rasterize(dk_map_utm, dk_raster_temp, 0)
#dk_raster_grid[!is.na(dk_raster_grid)] <- 1:length(na.omit(dk_raster_grid[]))
#plot(dk_raster_grid)
#text(dk_raster_grid)

so_planter_pkt_sp <- so_planter_pkt %>% 
  dplyr::select(Punkt.X.UTM, Punkt.Y.UTM, Latinsk.navn) %>% 
  distinct() %>% 
  st_as_sf(coords = c("Punkt.X.UTM", "Punkt.Y.UTM"), crs = 32632)
  
dk_raster_grid_lake_plants <- rasterize(as(so_planter_pkt_sp, "Spatial"), dk_raster_grid, field = "Latinsk.navn", update = TRUE, fun=function(x, ...){length(unique(na.omit(x)))})
plot(dk_raster_grid_lake_plants)

so_planter_pkt %>% 
  select(ObservationsStedNr, year, Latinsk.navn) %>% 
  distinct() %>% 
  filter(Latinsk.navn != "") %>% View()
  group_by(ObservationsStedNr, year) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#so_planter_pkt$Latinsk.navn %>% unique() %>% sort()
  
so_planter_overblik <- read.csv2(files[grepl("so_planter_overblik", files)], stringsAsFactors = FALSE) %>% 
  tbl_df()

so_planter_supp <- read.csv2(files[grepl("so_planter_supp", files)], stringsAsFactors = FALSE) %>% 
  tbl_df()

so_planter_supp %>% 
  group_by(ObservationsStedNavn, StartDato) %>% 
  summarise(n=length(unique(Latinsk.navn))) %>% 
  arrange(desc(n))

so_kemi <- read.csv2(files[grepl("so_kemi_2", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(Parameter = ifelse(Parameter == "Chlorophyl (ukorrigeret)", "Chlorophyl A", Parameter),
         Parameter_Enhed = paste0(Parameter, "_", Enhed),
         date = ymd(Startdato)) %>% 
  distinct() %>% 
  group_by(ObservationsStedNr, ObservationsStedNavn, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date, Parameter_Enhed) %>% 
  summarise(Resultat = mean(Resultat)) %>% 
  ungroup() %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(ObservationsStedNr, Parameter_Enhed, year) %>% 
  summarise(mean = mean(Resultat), n = n())
  
so_felt_sigt <- read.csv2(files[grepl("so_felt_sigt", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(Parameter_Enhed = paste0(Parameter, "_", Enhed),
         date = ymd(Startdato)) %>% 
  distinct() %>% 
  filter(Parameter_Enhed == "Sigtdybde_m") %>% 
  group_by(ObservationsStedNr, ObservationsStedNavn, date, Parameter_Enhed) %>% 
  summarise(Resultat = mean(Resultat)) %>% 
  ungroup() %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(ObservationsStedNr, Parameter_Enhed, year) %>% 
  summarise(mean = mean(Resultat), n = n())
  
bind_rows(so_kemi, so_felt_sigt)

so_planter_pkt %>% 
  left_join(so_kemi) %>% View()
  



#temp og iltprofiler
so_profil <- read.csv2(files[grepl("profil", files)], stringsAsFactors = FALSE) %>% 
  tbl_df()

so_profil_nest <- so_profil %>% 
  filter(Parameter == "Temperatur") %>% 
  filter(between(Resultat, -2, 35),
         between(MåledybdeM, 0, 40)) %>% 
  mutate(dato = ymd(Startdato),
         Startklok_fill = case_when(str_length(Startklok) == 1 ~ "0000",
                                    str_length(Startklok) == 2 ~ paste0("00", Startklok),
                                    str_length(Startklok) == 3 ~ paste0("0", Startklok),
                                    TRUE ~ as.character(Startklok)),
         datetime = ymd_hm(paste(Startdato, Startklok_fill))) %>% 
  select(ObservationsStedNr, ObservationsStedNavn, dato, datetime, MåledybdeM, Resultat) %>% 
  arrange(ObservationsStedNr, ObservationsStedNavn, dato, datetime, MåledybdeM) %>% 
  nest(-ObservationsStedNr, -ObservationsStedNavn, -dato)








#farvetal platin enheder
so_kemi_farvetal <- read.csv2(files[grepl("so_kemi_farvetal", files)], stringsAsFactors = FALSE) %>% 
  tbl_df()

temp <- so_kemi_farvetal %>% 
  mutate(dato = ymd(Startdato),
         Startklok_fill = case_when(str_length(Startklok) == 1 ~ "0000",
                                    str_length(Startklok) == 2 ~ paste0("00", Startklok),
                                    str_length(Startklok) == 3 ~ paste0("0", Startklok),
                                    TRUE ~ as.character(Startklok)),
         datetime = ymd_hm(paste(Startdato, Startklok_fill))) %>% 
  select(ObservationsStedNr, ObservationsStedNavn, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, datetime, GennemsnitsDybde.i.m, Resultat)

temp %>% 
  group_by(ObservationsStedNr) %>% 
  add_tally() %>% 
  ungroup() %>% 
  nest(-ObservationsStedNr, -n) %>% 
  filter(n > 50) %>% 
  mutate(trend = map(data, ~rkt(as.numeric(.x$datetime), .x$Resultat, rep = "a")),
         slope = map_dbl(trend, ~.x$B))



  




#so pco2
#overvej kun at bruge enkeltprøver og ikke blandingsprøver
so_kemi_2 <- read.csv2(files[grepl("so_kemi_2", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  mutate(date = ymd(Startdato)) %>% 
  filter(Parameter %in% c("Alkalinitet,total TA", "pH")) %>% 
  arrange(ObservationsStedNavn, ObservationsStedNr, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date, Parameter, GennemsnitsDybde.i.m) %>% 
  group_by(ObservationsStedNavn, ObservationsStedNr, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date, Parameter) %>% 
  summarise(Resultat = first(Resultat), Resultat_Dybde = first(GennemsnitsDybde.i.m)) %>% 
  spread(Parameter, Resultat) %>% 
  na.omit()

so_profil <- read.csv2(files[grepl("profil", files)], stringsAsFactors = FALSE) %>% 
  tbl_df() %>% 
  filter(Parameter == "Temperatur") %>% 
  mutate(date = ymd(Startdato)) %>% 
  arrange(ObservationsStedNavn, ObservationsStedNr, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date, MåledybdeM) %>% 
  group_by(ObservationsStedNavn, ObservationsStedNr, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, date) %>% 
  summarise(Temp = first(Resultat), Temp_Dybde = first(MåledybdeM))

#alk, ph og temp for søer
so_pco2 <- so_kemi_2 %>% 
  left_join(so_profil) %>% 
  na.omit() %>% 
  rename(alk = `Alkalinitet,total TA`, ph = pH, wtr = Temp, wtr_depth = Temp_Dybde, kemi_depth = Resultat_Dybde) %>% 
  filter(between(alk, 0, 20), 
         between(ph, 5,  14),
         between(kemi_depth, 0, 5),
         between(wtr_depth, 0, 5)) %>% 
  mutate(pco2_uatm = carb(8, ph, alk/1000, S = 0, T = wtr)$pCO2,
         co2_mol_kg = carb(8, ph, alk/1000, S = 0, T = wtr)$CO2,
         DIC_mol_kg = carb(8, ph, alk/1000, S = 0, T = wtr)$DIC)
saveRDS(so_pco2, "C:/Users/kenne/Documents/DK DEM temp/so_pco2.rds")
###beregn co2 og join med polygon, lav fil magen til vl_pco2.rds


temp <- read.csv2(files[grepl("so_ph_alk_ox_prod", files)], stringsAsFactors = FALSE) %>% 
  filter(Parameter %in% c("Primær produktion assimilieret Carbon"))


