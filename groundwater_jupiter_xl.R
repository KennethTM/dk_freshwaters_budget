#Extract groundwater chemistry data from GEUS JupiterXL database
#The database is downloaded from https://www.geus.dk/produkter-ydelser-og-faciliteter/data-og-kort/national-boringsdatabase-jupiter/adgang-til-data/
#The database is downloaded a MSSQL backup file (.bak). It is restored SQL server management studio.

#Load libraries
library(odbc)
library(tidyverse)
library(lubridate)
library(sf)

#Connect to local copy of GEUS JupiterXL database
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "kenneth_martinsen_bio_ku_dk_14207319_pcjupiter_xl", 
                      Trusted_Connection = "True")

#Get tables
tables <- DBI::dbListTables(con)

#Get list of compounds so they can be extracted later
compoundlist <- tbl(con, "COMPOUNDLIST") %>% 
  select(COMPOUNDNO, LONG_TEXT) %>% 
  collect()

#Search for relevant tables
grep("GRW*", tables, value = TRUE)

#Table with groundwater analysis
analysis <- tbl(con, "GRWCHEMANALYSIS") %>% 
  select(SAMPLEID, COMPOUNDNO, AMOUNT, UNIT)

#Table with sample info
sample <- tbl(con, "GRWCHEMSAMPLE") %>% 
  select(SAMPLEID, BOREHOLENO, TOP, BOTTOM, SAMPLEDATE)

#Table with compound info
compounds <- tbl(con, "COMPOUNDLIST") %>% 
  select(COMPOUNDNO, LONG_TEXT)
  
#Table with boreholde info
borehole <- tbl(con, "BOREHOLE") %>% 
  select(BOREHOLENO, XUTM32EUREF89, YUTM32EUREF89, ELEVATION, DRILLDEPTH)

#Join tables
all <- analysis %>% 
  left_join(sample) %>% 
  left_join(compounds) %>% 
  left_join(borehole)

#Extract and save alk and ph data
alk_ph <- all %>% 
  filter(COMPOUNDNO %in% c(41, 291)) %>% 
  collect()

#Save to file
saveRDS(alk_ph, paste0(getwd(), "/rawdata/grw_ph_alk.rds"))

#Close connection to database
DBI::dbDisconnect(con)
