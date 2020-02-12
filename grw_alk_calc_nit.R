library(raster)
library(fields)
library(tidyverse)
library(lubridate)
library(sf)

library(mgcv)
library(gratia)

grw_data <- readRDS(paste0(getwd(), "/data/grw_alk_calc_nit.rds"))

nit_data <- grw_data %>%
  mutate(datetime = ymd_hms(SAMPLEDATE)) %>% 
  filter(COMPOUNDNO == 1176,
         !is.na(XUTM32EUREF89),
         datetime > ymd_hm("1900-01-01 00:00"),
         AMOUNT > 0) %>% 
  mutate(year = year(datetime),
         doy = yday(datetime),
         depth_mean = (TOP+BOTTOM)/2,
         depth = coalesce(depth_mean, BOTTOM, TOP),
         site = factor(BOREHOLENO)) %>% 
  filter(!is.na(depth) & depth > 0) %>% 
  select(site, datetime, year, doy, x = XUTM32EUREF89, y = YUTM32EUREF89, depth, nitrate = AMOUNT, elev = ELEVATION)

summary(nit_data)

# library(openxlsx)
# grw_data %>%
#   mutate(datetime = ymd_hms(SAMPLEDATE),
#          year = year(datetime)) %>%
#   filter(COMPOUNDNO == 1176 & year > 1900) %>%
#   ggplot(aes(datetime, AMOUNT))+
#   geom_smooth()+
#   coord_cartesian(ylim=c(0, 30))
# write.xlsx(grw_data, paste0(getwd(), "/data/grw_alk_calc_nit.xlsx"))

glob_model <- gam(log(nitrate) ~ 
                    s(x, y) + s(year) + s(x, y, year)
                  #s(x, y) + s(year) + s(depth) + s(depth, year)
                  #s(doy, bs = "cc") + s(year, doy)+
                  #s(elev) + s(elev, year) + s(elev, doy) 
                  #s(site, bs = "re")
                  , data = nit_data %>% sample_n(500))

summary(glob_model)

draw(glob_model)
appraise(glob_model)


