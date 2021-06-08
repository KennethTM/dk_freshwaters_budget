#Figures for popular science article in danish (Vand & Jord) on the danish freshwater carbon budget

#Run and load upscaling script
source("upscaling_emissions.R")

library(patchwork);library(ggridges)

fig_path <- paste0(getwd(), "/figures/")

##### Figure 1
#Read groundwater rasters and convert to dataframe for plotting
grw_list <- readRDS(paste0(getwd(), "/rawdata/grw_alk_ph_pco2_rasters.rds"))
grw_df <- lapply(grw_list, function(rast){as.data.frame(rast, xy = TRUE)})

#Plot alkalinity, pH and CO2 raster
alk <- ggplot(grw_df$alk, aes(x, y, fill = layer))+
  geom_raster()+
  scale_fill_viridis_c(name = expression("Alkalinitet (meq "*L^{-1}*")"), na.value = "white", 
                       breaks = seq(0, 10, 2), direction = 1, limits = c(0, 10))+
  theme_void()+
  coord_equal()+
  theme(legend.position = c(0.8, 0.7), legend.justification = "left")

ph <- ggplot(grw_df$ph, aes(x, y, fill = layer))+
  geom_raster()+
  scale_fill_viridis_c(name = "pH", na.value = "white", breaks = seq(4, 8, 0.5), 
                       limits = c(6, 8), direction = 1, option = "C")+
  theme_void()+
  coord_equal()+
  theme(legend.position = c(0.8, 0.7), legend.justification = "left")

pco2 <- grw_df$pco2 %>% 
  mutate(bin = cut(layer, breaks = c(seq(0, 25000, 5000), 120000),
                   labels = c("0-5000", "5000-10000", "10000-15000", "15000-20000", "20000-25000", ">25000"))) %>% 
  na.omit() %>% 
  ggplot(aes(x, y, fill = layer))+
  geom_raster()+
  scale_fill_viridis_c(name = expression("pCO"[2]*" ("*mu*atm*")"), na.value = "white", option = "E", 
                       breaks = seq(0, 40000, 10000), direction = 1, limits = c(0, 40000))+
  theme_void()+
  coord_equal()+
  theme(legend.position = c(0.8, 0.7), legend.justification = "left")

#Layout plots and save to file
ggsave(paste0(fig_path, "grw_alk_ph_pco2.png"), alk/ph/pco2, width = 129, height = 200, units = "mm")

##### Figure 2
#Plot of density distributions of pco2 and flux intensities
#Combine stream and lake CO2 flux data and cast to long format for plotting
so_vl_flux_rates <- bind_rows(so_co2_flux, vl_co2_flux) %>% 
  select(site_id, system, sizecat, co2_flux_mmol_m2_h, pCO2_uatm) %>% 
  gather(variable, value, co2_flux_mmol_m2_h, pCO2_uatm)

#Labels for plotting
sizecat_labels <- data.frame(sizecat = sort(unique(so_vl_flux_rates$sizecat)),
                             label = c("0-10^3^~m^2^", "10^3^-10^4^~m^2^", "10^4^-10^5^~m^2^", 
                                       "10^5^-10^6^~m^2^", "10^6^-10^7^~m^2^", "10^7^-10^8^~m^2^",
                                       "0-2.5~m", "2.5-12~m", "'>'*12~m")) %>% 
  mutate(label = factor(label, levels = rev(c("0-2.5~m", "2.5-12~m", "'>'*12~m",  
                                              "0-10^3^~m^2^", "10^3^-10^4^~m^2^", "10^4^-10^5^~m^2^",
                                              "10^5^-10^6^~m^2^", "10^6^-10^7^~m^2^", "10^7^-10^8^~m^2^"))))

lab_expressions <- rev(c(expression(Vandløb~0-2.5~m), expression(Vandløb~2.5-12~m), expression(Vandløb~">12"~m),
                         expression(Søer~0-10^3~m^2), expression(Søer~10^3-10^4~m^2), 
                         expression(Søer~10^4-10^5~m^2), expression(Søer~10^5-10^6~m^2),
                         expression(Søer~10^6-10^7~m^2), expression(Søer~10^7-10^8~m^2)))

ridge_pressure <- so_vl_flux_rates %>% 
  left_join(sizecat_labels) %>% 
  na.omit() %>% 
  filter(variable != "co2_flux_mmol_m2_h") %>% 
  ggplot(aes(value, label, fill = stat(x))) +
  geom_vline(xintercept = 410, linetype = 1, col = "grey", size = 1)+
  geom_density_ridges_gradient(scale = 3)+
  scale_fill_viridis_c(direction = -1, option = "B")+
  scale_y_discrete(labels = lab_expressions)+
  xlim(xlim = c(0, 15000))+
  theme_ridges()+
  xlab(expression("CO"[2]~partialtryk~"("*mu*"atm)"))+
  ylab("Størrelseskategori")+
  guides(fill = guide_colorbar(title = NULL, barwidth = unit(8, "mm"), barheight = unit(40, "mm"), ticks = FALSE))+
  theme(axis.text.y = element_text(hjust = 0))

ridge_flux <- so_vl_flux_rates %>% 
  left_join(sizecat_labels) %>% 
  na.omit() %>% 
  filter(variable == "co2_flux_mmol_m2_h") %>% 
  mutate(value = value*10^-3*12*24) %>% #change unit to g C/m2/day
  ggplot(aes(value, label, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3)+
  scale_fill_viridis_c(direction = -1, limits = c(-2, 8), breaks = seq(-2, 8, 2))+
  scale_y_discrete(labels = lab_expressions)+
  scale_x_continuous(breaks=seq(-2, 8, 2), limits = c(-2, 8))+
  theme_ridges()+
  xlab(expression("CO"[2]~flux~"("*g~C~m^{-2}~d^{-1}*")"))+
  ylab("Størrelseskategori")+
  guides(fill = guide_colorbar(title = NULL, barwidth = unit(8, "mm"), barheight = unit(40, "mm"), ticks = FALSE))+
  theme(axis.text.y = element_text(hjust = 0))

#Save to file
ggsave(paste0(fig_path, "ridge_pressure_flux.png"), ridge_pressure/ridge_flux, width = 174, height = 200, units = "mm")

##### Figure 3
#Data read from previous project (Stream watersheds, COWIFonden) added to current directory
#Data contains stream pco2 (same data as here) and watershed characteristics for many danish streams
watershed_proj_folder <- paste0(getwd(), "/rawdata/watershed_proj/")

proj_watersheds <- readRDS(paste0(watershed_proj_folder, "watershed_stats.rds"))

proj_pco2 <- readRDS(paste0(watershed_proj_folder,"vl_pco2.rds")) %>% 
  mutate(month = month(date),
         vl_id = factor(ObservationsStedNr)) 

#Calculate DIC transport and make watershed NPP (MODIS-derived data) same unit
proj_dic_trans <- proj_pco2 %>% 
  mutate(DIC_trans_mol_s = DIC_mol_kg*1000*Q_m3_s) %>% 
  group_by(vl_id) %>% 
  summarise(DIC_trans_mol_s = mean(DIC_trans_mol_s, na.rm = TRUE), n = n()) %>% 
  na.omit() %>% 
  left_join(cowi_watersheds %>% 
              select(vl_id, area, npp_mean, strahler_order)) %>% 
  na.omit() %>% 
  mutate(npp_mean_mol_watershed_year = npp_mean/12*area,
         DIC_trans_mol_year = DIC_trans_mol_s*60*60*24*365) 

proj_dic_trans %>% 
  mutate(npp_mean_g_C_watershed_year = npp_mean_mol_watershed_year*12,
         DIC_trans_g_C_year = DIC_trans_mol_year*12) %>%
  ggplot(aes(npp_mean_mol_watershed_year, DIC_trans_mol_year))+
  geom_smooth(method = "lm", col = "black")+
  geom_point(aes(col = log10(area))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_color_viridis_c(name = expression(log[10]*"(oplandsareal [m"^{2}*"])"),
                        guide = guide_colorbar(direction = "horizontal", title.position = "top"))+
  theme_classic()+
  ylab(expression("DIC transport (g C"~y^{-1}*")"))+
  xlab(expression("Terrestrisk netto primærproduktion (g C"~y^{-1}*")"))+
  theme(axis.text = element_text(colour = "black"), legend.position = c(0.8, 0.25))

ggsave(paste0(fig_path, "dic_trans_terr_npp.png"), width = 129, height = 90, units = "mm")

##### Figure in english for datainwater.com of only fluxes
lab_expressions_eng <- rev(c(expression(Streams~0-2.5~m), expression(Streams~2.5-12~m), expression(Streams~">12"~m),
                             expression(Lakes~0-10^3~m^2), expression(Lakes~10^3-10^4~m^2),
                             expression(Lakes~10^4-10^5~m^2), expression(Lakes~10^5-10^6~m^2),
                             expression(Lakes~10^6-10^7~m^2), expression(Lakes~10^7-10^8~m^2)))

so_vl_flux_rates %>%
  left_join(sizecat_labels) %>%
  na.omit() %>%
  filter(variable == "co2_flux_mmol_m2_h") %>%
  mutate(value = value*10^-3*12*24) %>% #change unit to g C/m2/day
  ggplot(aes(value, label, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3)+
  scale_fill_viridis_c(direction = -1, limits = c(-2, 10), breaks = seq(-2, 10, 2), option = "A")+
  scale_y_discrete(labels = lab_expressions_eng)+
  scale_x_continuous(breaks=seq(-2, 10, 2), limits = c(-2, 10))+
  theme_ridges()+
  xlab(expression("CO"[2]~flux~"("*g~C~m^{-2}~d^{-1}*")"))+
  ylab("Size category")+
  guides(fill = guide_colorbar(title = NULL, barwidth = unit(8, "mm"), barheight = unit(40, "mm"), ticks = FALSE))+
  theme(axis.text.y = element_text(hjust = 0))

ggsave(paste0(fig_path, "ridge_flux_english.png"), width = 174, height = 100, units = "mm")

##### Figure in danish for chronic
so_vl_flux_rates %>% 
  left_join(sizecat_labels) %>% 
  na.omit() %>% 
  filter(variable != "co2_flux_mmol_m2_h") %>% 
  ggplot(aes(value, label, fill = stat(x))) +
  geom_vline(xintercept = 410, linetype = 1, col = grey(0.6), size = 2)+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.005)+
  scale_fill_viridis_c(direction = -1, option = "D", limits =c(0, 12000))+
  scale_y_discrete(labels = lab_expressions)+
  scale_x_continuous(limits = c(0, 12000), expand = c(0, 0))+
  theme_ridges()+
  xlab(expression("CO"[2]~partialtryk~"("*mu*"atm)"))+
  ylab("Størrelseskategori")+
  guides(fill = guide_colorbar(title = NULL, barwidth = unit(8, "mm"), barheight = unit(40, "mm"), ticks = FALSE))+
  theme(axis.text.y = element_text(hjust = 0))

ggsave(paste0(fig_path, "ridge_flux_chronic.png"), width = 174, height = 100, units = "mm")

#########################################
### Figure 2/Ridge plot in english 
so_vl_flux_rates <- bind_rows(so_co2_flux, vl_co2_flux) %>% 
  select(site_id, system, sizecat, co2_flux_mmol_m2_h, pCO2_uatm) %>% 
  gather(variable, value, co2_flux_mmol_m2_h, pCO2_uatm)

#Labels for plotting
sizecat_labels <- data.frame(sizecat = sort(unique(so_vl_flux_rates$sizecat)),
                             label = c("0-10^3^~m^2^", "10^3^-10^4^~m^2^", "10^4^-10^5^~m^2^", 
                                       "10^5^-10^6^~m^2^", "10^6^-10^7^~m^2^", "10^7^-10^8^~m^2^",
                                       "0-2.5~m", "2.5-12~m", "'>'~12~m")) %>% 
  mutate(label = factor(label, levels = rev(c("0-2.5~m", "2.5-12~m", "'>'~12~m",  
                                              "0-10^3^~m^2^", "10^3^-10^4^~m^2^", "10^4^-10^5^~m^2^",
                                              "10^5^-10^6^~m^2^", "10^6^-10^7^~m^2^", "10^7^-10^8^~m^2^"))))

lab_expressions <- rev(c(expression(Streams~0-2.5~m), expression(Streams~2.5-12~m), expression(Streams~"> 12"~m),
                         expression(Lakes~0-10^3~m^2), expression(Lakes~10^3-10^4~m^2), 
                         expression(Lakes~10^4-10^5~m^2), expression(Lakes~10^5-10^6~m^2),
                         expression(Lakes~10^6-10^7~m^2), expression(Lakes~10^7-10^8~m^2)))

ridge_pressure <- so_vl_flux_rates %>% 
  left_join(sizecat_labels) %>% 
  na.omit() %>% 
  filter(variable != "co2_flux_mmol_m2_h") %>% 
  ggplot(aes(value, label, fill = stat(x))) +
  geom_vline(xintercept = 410, linetype = 1, col = "grey", size = 1)+
  geom_density_ridges_gradient(scale = 3)+
  scale_fill_viridis_c(direction = -1)+
  scale_y_discrete(labels = lab_expressions)+
  scale_x_continuous(breaks=seq(0, 12000, 2000), limits = c(0, 12000))+
  theme_ridges()+
  xlab(expression("CO"[2]~partial~pressure~"("*mu*"atm)"))+
  ylab("Size category")+
  guides(fill = guide_colorbar(title = NULL, barwidth = unit(8, "mm"), barheight = unit(40, "mm"), ticks = FALSE))+
  theme(axis.text.y = element_text(hjust = 0))

ridge_flux <- so_vl_flux_rates %>% 
  left_join(sizecat_labels) %>% 
  na.omit() %>% 
  filter(variable == "co2_flux_mmol_m2_h") %>% 
  mutate(value = value*10^-3*12*24) %>% #change unit to g C/m2/day
  ggplot(aes(value, label, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3)+
  scale_fill_viridis_c(direction = -1, limits = c(-2, 8), breaks = seq(-2, 8, 2), option = "B")+
  scale_y_discrete(labels = lab_expressions)+
  scale_x_continuous(breaks=seq(-2, 8, 2), limits = c(-2, 8))+
  theme_ridges()+
  xlab(expression("CO"[2]~flux~"("*g~C~m^{-2}~d^{-1}*")"))+
  ylab("Size category")+
  guides(fill = guide_colorbar(title = NULL, barwidth = unit(8, "mm"), barheight = unit(40, "mm"), ticks = FALSE))+
  theme(axis.text.y = element_text(hjust = 0))

ridge_pressure/ridge_flux

#Save to file
ggsave(paste0(fig_path, "ridge_pressure_flux_english.png"), width = 210, height = 230, units = "mm")
