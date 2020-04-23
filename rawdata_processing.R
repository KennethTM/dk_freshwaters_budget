#Processing of rawdata downloaded from https://odaforalle.au.dk/
#Rawdata is open and free to access
#For both lakes and streams data contains pH and alkalinity
#Water temperature are in contained in "field" for streams or "profile" for lakes depending on how the samples were collected
#For streams discharge data is also processed

#Load libs and set path to data
library(data.table);library(seacarb)

rawdata_path <- paste0(getwd(), "/rawdata/")

#Data processing for streams
#Read chemistry data
dk_chem_vl <- fread(paste0(rawdata_path, "vl_vandkemi.csv"), encoding = "UTF-8")

#Clean names and variables for chemistry data
dk_chem_vl_clean <- dk_chem_vl[,.(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                                  date = as.Date(as.character(Startdato), format = "%Y%m%d"), var_unit = paste0("chem_", Parameter, "_", Enhed), 
                                  value = as.numeric(sub(",", ".", Resultat, fixed = TRUE)))]

#Read field data
dk_field_vl <- fread(paste0(rawdata_path, "vl_felt.csv"), encoding = "UTF-8")

#Clean names and variables for field data
dk_field_vl_clean <- dk_field_vl[,.(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                                    date = as.Date(as.character(Startdato), format = "%Y%m%d"), var_unit = paste0("field_", Parameter, "_", Enhed), 
                                    value = as.numeric(sub(",", ".", Resultat, fixed = TRUE)))]

#Bind tables
dk_all <- rbindlist(list(dk_chem_vl_clean, dk_field_vl_clean))

#Cast to wide format with a column for each variable
dk_all_wide <- dcast(dk_all, site_id + x_coord + y_coord + date ~ var_unit, value.var = "value", fun = mean, fill = NA)

#Clean names
dk_all_clean <- dk_all_wide[, .(site_id, date,
                                x_coord = as.integer(x_coord), 
                                y_coord = as.integer(y_coord), 
                                alk = `chem_Alkalinitet,total TA_mmol/l`,
                                ph = fcoalesce(field_pH_pH, chem_pH_pH),
                                wtr = `field_Temperatur_grader C`,
                                sys_coord = 'EUREF89_UTMZONE32',
                                system = "vl")]

#Data processing for lakes
#Read chemistry and profile data
dk_chem_so <- fread(paste0(rawdata_path, "so_vandkemi.csv"), dec = ",")
dk_profil_so <- fread(paste0(rawdata_path, "so_profil.csv"), dec = ",")

#Clean names and variables for chemistry data
dk_chem_so_clean <- dk_chem_so[, .(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                               date = as.Date(as.character(Startdato), format = "%Y%m%d"), depth_sample = `GennemsnitsDybde i m`,
                               var_unit = paste0("chem_", Parameter, "_", Enhed), value = Resultat)]

#Cast to wide format with a column for each variable
dk_chem_so_clean_wide <- dcast(dk_chem_so_clean[order(site_id, date, depth_sample)], site_id + x_coord + y_coord + date ~ var_unit, value.var = "value", fun = first, fill = NA)

#Set key for chemistry table to speed up later processing
setkey(dk_chem_so_clean_wide, site_id, date)

#Clean names and variables for profile data
dk_profil_so_clean <- dk_profil_so[, .(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                                   date = as.Date(as.character(Startdato), format = "%Y%m%d"), depth = MåledybdeM, 
                                   var_unit = paste0("profil_", Parameter, "_", Enhed), value = Resultat)]

#Cast to wide format with a column for each variable
dk_profil_so_clean_wide <- dcast(dk_profil_so_clean[order(site_id, date, depth)], site_id + date ~ var_unit, value.var = "value", fun = first, fill = NA) #sort obs, og brug first istedet for

#Set key for chemistry table
setkey(dk_profil_so_clean_wide, site_id, date)

#Join chemistry and profile data
dk_all_clean_so <- dk_profil_so_clean_wide[dk_chem_so_clean_wide][, .(site_id, date, 
                                                                      x_coord = as.integer(x_coord), 
                                                                      y_coord = as.integer(y_coord),
                                                                      wtr = `profil_Vandtemperatur_grader C`,
                                                                      ph = fcoalesce(profil_pH_pH, chem_pH_pH),
                                                                      alk = `chem_Alkalinitet,total TA_mmol/l`,
                                                                      sys_coord = 'EUREF89_UTMZONE32',
                                                                      system = "so")]

#Combine stream and lake tables and calculate pCO2 and DIC
dk_samlet <- rbindlist(list(dk_all_clean, dk_all_clean_so), use.names = TRUE)

#Remove observation with missing values
dk_samlet_na <- na.omit(dk_samlet)

#Calculate inorganic carbon system from pH, temperature and alkalinity
dk_carb <- carb(flag = 8, dk_samlet_na$ph, dk_samlet_na$alk/1000, T = dk_samlet_na$wtr, S = 0, Patm = 1, k1k2 = "w14", kf = "dg", ks = "d")

#Select CO2 and DIC
dk_carb_sub <- dk_carb[, c("pCO2", "DIC", "CO2")]

#Set names
names(dk_carb_sub) <- c("pCO2_uatm", "DIC_mol_kg", "CO2_mol_kg")

#Bind to table
dk_samlet_carb <- cbind(dk_samlet_na, dk_carb_sub)

#Set key for table
setkey(dk_samlet_carb, system, site_id, date)

#Read stream discharge data
dk_vl_q <- fread(paste0(rawdata_path, "vl_discharge.csv"), dec = ",")

#Clean and filter out negative values
dk_vl_q_sub <- dk_vl_q[, .(site_id = ObservationsStedNr,
                           date = as.Date(as.character(Dato), format = "%Y%m%d"),
                           q_m3_s = Resultat,
                           system = "vl")][q_m3_s > 0]

#Set key for table
setkey(dk_vl_q_sub, system, site_id, date)

#Join discharge data to stream and lake table
dk_samlet_carb_with_q <- dk_vl_q_sub[dk_samlet_carb]

#Save complete table to file in data folder
saveRDS(dk_samlet_carb_with_q, paste0(getwd(), "/data/", "dk_carb.rds"))
