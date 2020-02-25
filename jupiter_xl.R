library(odbc)
library(tidyverse)
library(lubridate)
library(sf)

#Connect to local copy of GEUS jupiter xl database
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

grep("long*", tables, value = TRUE)

analysis <- tbl(con, "GRWCHEMANALYSIS") %>% 
  select(SAMPLEID, COMPOUNDNO, AMOUNT, UNIT)

sample <- tbl(con, "GRWCHEMSAMPLE") %>% 
  select(SAMPLEID, BOREHOLENO, TOP, BOTTOM, SAMPLEDATE)

compounds <- tbl(con, "COMPOUNDLIST") %>% 
  select(COMPOUNDNO, LONG_TEXT)
  
borehole <- tbl(con, "BOREHOLE") %>% 
  select(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89, ELEVATION, DRILLDEPTH)

all <- analysis %>% 
  left_join(sample) %>% 
  left_join(compounds) %>% 
  left_join(borehole)

#Extract and save alk and ph data
alk_ph <- all %>% 
  filter(COMPOUNDNO %in% c(41, 291)) %>% 
  collect()
saveRDS(alk_ph, paste0(getwd(), "/data/grw_ph_alk.rds"))

#Extract and save alk, calcium and nitrate data
alk_calc_nit <- all %>% 
  filter(COMPOUNDNO %in% c(291, 1551, 1176)) %>% 
  collect()
saveRDS(alk_calc_nit, paste0(getwd(), "/data/grw_alk_calc_nit.rds"))
