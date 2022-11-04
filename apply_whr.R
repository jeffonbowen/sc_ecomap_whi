## Apply Wildlife Habitat Ratings From Table to TEM/PEM/CEM ## 

# Libraries
library(tidyverse)
library(readxl)

options(scipen = 999)

# Read TEM/PEM/CEM attribute file.

# The attribute file is a csv export the original shapefile or other source. 

# Note how the site modifier fields have been renamed to match the syntax of 
# the other attributes. 

# fname <- "dat/CEM_Oct10_13.csv"
# eco_dat <- read_csv(fname, 
#                   col_types = cols(.default = "c",
#                                    'AREA' = col_double(),
#                                    'SDEC_1' = col_integer(), 
#                                    'SDEC_2' = col_integer(), 
#                                    'SDEC_3' = col_integer()))
# 

# Revised footprint intersected with CEM
eco_dat <- read_xlsx("dat/Intersect_Footprint_20200901_CEM.xlsx")


# Original footprint intersected with CEM
eco_dat <- read_xlsx("dat/Clearing_Strategies_PAZ_CEM_Intersect.xlsx") %>% 
  rename(Area_ha = Shape_Area)
sum(eco_dat$Area_ha)/10000


# Conduct QA of codes and values
table(eco_dat$BGC_ZONE, eco_dat$BGC_SUBZON)

table(eco_dat$SDEC_1)
table(eco_dat$SITEMC_S1, eco_dat$SERAL_1)
table(eco_dat$SITE_M1A)
table(eco_dat$SITE_M1B)
table(eco_dat$STRCT_S1, eco_dat$STRCT_M1)

table(eco_dat$SDEC_2)
table(eco_dat$SITEMC_S2, eco_dat$SERAL_2)
table(eco_dat$STRCT_S2)
table(eco_dat$STRCT_S2, eco_dat$STRCT_M2)

table(eco_dat$SDEC_3)
table(eco_dat$SITEMC_S3, eco_dat$SERAL_3)
table(eco_dat$STRCT_S3)
table(eco_dat$STRCT_S3, eco_dat$STRCT_M3)

# Cleanup for the CEM data. May not be needed for the others. 
# eco_dat$STRCT_M1[eco_dat$STRCT_M1 == "i"] <- ""
# eco_dat$STRCT_M1[eco_dat$STRCT_M1 == "s"] <- ""
# eco_dat$STRCT_M3[eco_dat$STRCT_M3 == "3"] <- NA
# eco_dat$STRCT_S3[eco_dat$CEM_ECP == "87238"] <- "3"


# Need to work with the data in long format. Specifically, pivoting the 
# deciles to longer. 
# To overcome the complication of inconsistent syntax for for the attribute names,
# using a hack to unite the attributes and then separate after pivoting.
eco_dat_long <- eco_dat %>% 
  unite("eco_1", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
                       SITEMC_S1, SITE_M1A, SITE_M1B, STRCT_S1, STRCT_M1, 
                       SERAL_1, remove = FALSE) %>% 
  unite("eco_2", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
        SITEMC_S2, SITE_M2A, SITE_M2B, STRCT_S2, STRCT_M2, 
        SERAL_2, remove = FALSE) %>%
  unite("eco_3", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
        SITEMC_S3, SITE_M3A, SITE_M3B, STRCT_S3, STRCT_M3, 
        SERAL_3, remove = FALSE) %>%
  mutate(Area_1 = Area_ha*SDEC_1/10,
         Area_2 = Area_ha*SDEC_2/10, 
         Area_3 = Area_ha*SDEC_3/10) %>% 
  select(FID, ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
         SDEC_1, eco_1, Area_1, 
         SDEC_2, eco_2, Area_2, 
         SDEC_3, eco_3, Area_3) %>% 
  pivot_longer(SDEC_1:Area_3, names_to = c(".value", "DECILE"), names_sep = "_") %>% 
  separate (eco, c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT",
        "SITEMC_S", "SITE_MA", "SITE_MB", "STRCT_S", "STRCT_M", 
        "SERAL"), remove = FALSE) %>% 
  na_if("NA") %>% 
  filter(Area > 0)

sum(eco_dat_long$Area)
  
mc_list <- eco_dat_long %>% 
  group_by(eco, ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
           SITEMC_S, SITE_MA, SITE_MB, STRCT_S, STRCT_M, 
           SERAL) %>% 
  summarize(Area = sum(Area))

sum(mc_list$Area)


# WHR ---------------------------------------------------------------------

fname_whr <- "dat/20080425 Peace Wildlife Studies Ratings Table - Keystone.csv"

rat <- read_csv(fname_whr,
                col_types = cols(.default = "c"))

rat_seow <- rat %>% select(ECO_SEC:SERAL, BSEOW_LI) %>% 
  unite("eco", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
        SITEMC_S, SITE_MA, SITE_MB, STRCT_S, STRCT_M, 
        SERAL, remove = FALSE)

eco_seow <- left_join(eco_dat_long, rat_seow, by = "eco", keep = FALSE) %>% 
  select(FID, DECILE, eco, Area, BSEOW_LI)

sum(eco_seow$Area)



# make a version for QA
eco_seow_qa <- left_join(eco_dat_long, rat_seow, by = "eco", keep = FALSE) %>%
  filter(Area != 0) %>% 
  filter(is.na(BSEOW_LI)) %>% 
  select(1:16, 28) %>% 
  group_by(eco) %>% 
  summarize(Area = sum(Area)) %>% 
  separate (eco, c("ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT",
                   "SITEMC_S", "SITE_MA", "SITE_MB", "STRCT_S", "STRCT_M", 
                   "SERAL"), remove = FALSE)
write_csv(eco_seow_qa, "out/BSEOW_LI_polyrat_qa.csv")



# WHR Updated -------------------------------------------------------------

rat <- read_xlsx("dat/BSEOW_lrat_updated.xlsx")

rat_seow <- rat %>% select(ECO_SEC:SERAL, BSEOW_LI) %>% 
  unite("eco", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,
        SITEMC_S, SITE_MA, SITE_MB, STRCT_S, STRCT_M, 
        SERAL, remove = FALSE) 

eco_seow_revised <- left_join(eco_dat_long, rat_seow, by = "eco") %>% 
  select(FID, DECILE, eco, Area, BSEOW_LI)

sum(eco_seow_revised$Area)

# All deciles:
eco_seow_revised %>% 
  group_by(BSEOW_LI) %>% 
  summarize(Area_tot = sum(Area)/10000)


# First deciles:
eco_seow_revised %>% group_by(BSEOW_LI)


  

write_csv(eco_seow_wide, "out/BSEOW_LI_polyrat.csv")


eco_seow_wide <- eco_seow %>% 
  select(FID, DECILE, BSEOW_LI) %>% 
  pivot_wider(names_from = DECILE, names_prefix = "Decile_", values_from = BSEOW_LI)

write_csv(eco_seow_wide, "out/BSEOW_LI_polyrat.csv")

