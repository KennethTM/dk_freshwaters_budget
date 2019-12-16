library(data.table);library(seacarb)

#data downloaded fra odaforalle
rawdata_path <- paste0(getwd(), "/rawdata/")

#vandløb
dk_chem_df <- fread(paste0(rawdata_path, "vl_vandkemi.csv"), encoding = "UTF-8")

dk_chem_df_clean <- dk_chem_df[,.(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                                  date = as.Date(as.character(Startdato), format = "%Y%m%d"), var_unit = paste0("chem_", Parameter, "_", Enhed), 
                                  value = as.numeric(sub(",", ".", Resultat, fixed = TRUE)))]

dk_field_df <- fread(paste0(rawdata_path, "vl_felt.csv"), encoding = "UTF-8")

dk_field_df_clean <- dk_field_df[,.(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                                    date = as.Date(as.character(Startdato), format = "%Y%m%d"), var_unit = paste0("field_", Parameter, "_", Enhed), 
                                    value = as.numeric(sub(",", ".", Resultat, fixed = TRUE)))]

dk_all <- rbindlist(list(dk_chem_df_clean, dk_field_df_clean))

dk_all_wide <- dcast(dk_all, site_id + x_coord + y_coord + date ~ var_unit, value.var = "value", fun = mean, fill = NA)

dk_all_clean <- dk_all_wide[, .(site_id, date,
                                x_coord = as.integer(x_coord), 
                                y_coord = as.integer(y_coord), 
                                alk = `chem_Alkalinitet,total TA_mmol/l`,
                                ph = fcoalesce(field_pH_pH, chem_pH_pH),
                                wtr = `field_Temperatur_grader C`,
                                sys_coord = 'EUREF89_UTMZONE32',
                                system = "vl")]

#sø
dk_chem_so <- fread(paste0(rawdata_path, "so_vandkemi.csv"), dec = ",")
dk_profil_so <- fread(paste0(rawdata_path, "so_profil.csv"), dec = ",")

dk_chem_so_clean <- dk_chem_so[, .(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                               date = as.Date(as.character(Startdato), format = "%Y%m%d"), depth_sample = `GennemsnitsDybde i m`,
                               var_unit = paste0("chem_", Parameter, "_", Enhed), value = Resultat)]

dk_chem_so_clean_wide <- dcast(dk_chem_so_clean, site_id + x_coord + y_coord + date ~ var_unit, value.var = "value", fun = mean, fill = NA)

setkey(dk_chem_so_clean_wide, site_id, date)

dk_profil_so_clean <- dk_profil_so[, .(site_id = ObservationsStedNr, x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                                   date = as.Date(as.character(Startdato), format = "%Y%m%d"), depth = MåledybdeM, 
                                   var_unit = paste0("profil_", Parameter, "_", Enhed), value = Resultat)]

dk_profil_so_clean_wide <- dcast(dk_profil_so_clean, site_id + date ~ var_unit, value.var = "value", fun = mean, fill = NA)

setkey(dk_profil_so_clean_wide, site_id, date)

dk_all_clean_so <- dk_profil_so_clean_wide[dk_chem_so_clean_wide][, .(site_id, date, 
                                                                      x_coord = as.integer(x_coord), 
                                                                      y_coord = as.integer(y_coord),
                                                                      wtr = `profil_Vandtemperatur_grader C`,
                                                                      ph = fcoalesce(profil_pH_pH, chem_pH_pH),
                                                                      alk = `chem_Alkalinitet,total TA_mmol/l`,
                                                                      sys_coord = 'EUREF89_UTMZONE32',
                                                                      system = "so")]

#vandløb og sø samlet, beregn pco2
dk_samlet <- rbindlist(list(dk_all_clean, dk_all_clean_so), use.names = TRUE)

dk_samlet_na <- na.omit(dk_samlet)

dk_carb <- carb(flag = 8, dk_samlet_na$ph, dk_samlet_na$alk/1000, T = dk_samlet_na$wtr, S = 0)

dk_carb_sub <- dk_carb[, c("pCO2", "DIC", "CO2")]

names(dk_carb_sub) <- c("pCO2_uatm", "DIC_mol_kg", "CO2_mol_kg")

dk_samlet_carb <- cbind(dk_samlet_na, dk_carb_sub)

setkey(dk_samlet_carb, system, site_id, date)

#vandløb discharge 
dk_vl_q <- fread(paste0(rawdata_path, "vl_discharge.csv"), dec = ",")

dk_vl_q_sub <- dk_vl_q[, .(site_id = ObservationsStedNr,
                           date = as.Date(as.character(Dato), format = "%Y%m%d"),
                           q_m3_s = Resultat,
                           system = "vl")][q_m3_s > 0]

setkey(dk_vl_q_sub, system, site_id, date)

dk_samlet_carb_with_q <- dk_vl_q_sub[dk_samlet_carb]

#save to file
saveRDS(dk_samlet_carb_with_q, paste0(getwd(), "/data/", "dk_carb.rds"))
