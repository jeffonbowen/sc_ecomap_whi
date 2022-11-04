## Apply Wildlife Habitat Ratings From Table to TEM/PEM/CEM ## 

# Libraries
library(tidyverse)
library(readxl)
library(sf)
library(tmap)

options(scipen = 999)

CEM <- st_read("dat_spatial/CEM_Oct10_13_nooverlap.shp")

tmap_options(check.and.fix = TRUE)
tmap_mode("view") +
  tm_basemap("Esri.WorldTopoMap") +
tm_shape(CEM) + 
  tm_polygons(alpha = 0.5)






# Read TEM/PEM/CEM attribute file.

fname <- "dat/CEM_Oct10_13.csv"

# The attribute file is a csv export the original shapefile or other source. 

# Note how the site modifier fields have been renamed to match the syntax of 
# the other attributes. 

eco_dat <- read_csv(fname, 
                  col_types = cols(.default = "c",
                                   'AREA' = col_double(),
                                   'SDEC_1' = col_integer(), 
                                   'SDEC_2' = col_integer(), 
                                   'SDEC_3' = col_integer()))


# This was from the earlier approach
# select(CEM_ECP, ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,  
#        SDEC_S1 = SDEC_1, SITE_S1, SITEMC_S1, SITE_MA_S1 = SITE_M1A, SITE_MB_S1 = SITE_M1B, STRCT_S1, STRCT_M_S1 = STRCT_M1, SERAL_S1 = SERAL_1, 
#        SDEC_S2 = SDEC_2, SITE_S2, SITEMC_S2, SITE_MA_S2 = SITE_M2A, SITE_MB_S2 = SITE_M2B, STRCT_S2, STRCT_M_S2 = STRCT_M2, SERAL_S2 = SERAL_2, 
#        SDEC_S3 = SDEC_3, SITE_S3, SITEMC_S3, SITE_MA_S3 = SITE_M3A, SITE_MB_S3 = SITE_M3B, STRCT_S3, STRCT_M_S3 = STRCT_M3, SERAL_S3 = SERAL_3,
#        AREA)

# Conduct QA of codes and values
table(eco_dat$BGC_ZONE, eco_dat$BGC_SUBZON)

table(eco_dat$SDEC_S1)
table(eco_dat$SITEMC_S1, eco_dat$SERAL_S1)
table(eco_dat$SITE_MA_S1)
table(eco_dat$SITE_MB_S2)
table(eco_dat$STRCT_S1, eco_dat$STRCT_M1)

table(eco_dat$SDEC_S2)
table(eco_dat$SITEMC_S2, eco_dat$SERAL_S2)
table(eco_dat$STRCT_S2)
table(eco_dat$STRCT_S2, eco_dat$STRCT_M2)

table(eco_dat$SDEC_S3)
table(eco_dat$SITEMC_S3, eco_dat$SERAL_S3)
table(eco_dat$STRCT_S3)
table(eco_dat$STRCT_S3, eco_dat$STRCT_M3)

# Cleanup for the CEM data. May not be needed for the others. 
eco_dat$STRCT_M1[eco_dat$STRCT_M1 == "i"] <- ""
eco_dat$STRCT_M1[eco_dat$STRCT_M1 == "s"] <- ""
eco_dat$STRCT_M3[eco_dat$STRCT_M3 == "3"] <- NA
eco_dat$STRCT_S3[eco_dat$CEM_ECP == "87238"] <- "3"


# Simplest approach is to create eco identifier. Not pretty, but simple.
eco_dat_label <- eco_dat %>% 
  unite("eco_1", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, SITE_S1, 
                       SITEMC_S1, SITE_M1A, SITE_M1B, STRCT_S1, STRCT_M1, 
                       SERAL_1, remove = FALSE) %>% 
  unite("eco_2", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, SITE_S2, 
        SITEMC_S2, SITE_M2A, SITE_M2B, STRCT_S2, STRCT_M2, 
        SERAL_3, remove = FALSE) %>%
  unite("eco_3", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, SITE_S3, 
        SITEMC_S3, SITE_M3A, SITE_M3B, STRCT_S3, STRCT_M3, 
        SERAL_3, remove = FALSE) %>%
  select(CEM_ECP, eco_1, eco_2, eco_3)


# WHR ---------------------------------------------------------------------

fname_whr <- "dat/20080425 Peace Wildlife Studies Ratings Table - Keystone.csv"

whr <- read_csv(fname_whr,
                col_types = cols(.default = "c"))

whr_seow <- whr %>% select(ECO_SEC:SERAL, BSEOW_LI) %>% 
  unite("eco", ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, SITE_S, 
        SITEMC_S, SITE_MA, SITE_MB, STRCT_S, STRCT_M, 
        SERAL, remove = FALSE)


eco_whi <- left_join(eco_dat_label, whr_seow, by = c("eco_1" = "eco")) 
  





## Save this for another time
## Pivot the deciles in to longer form to allow for summaries ----
eco_dat_long <- eco_dat %>% 
  select(AREA, everything()) %>% 
  pivot_longer(!(AREA:BGC_VRT), 
               names_to = c(".value", "DECILE"), 
               names_sep = "_S"
               ) %>% 
  mutate(Area_decile_ha = SDEC/10 * AREA / 10000) %>% 
  select(-AREA)
#  filter(!is.na(SITEMC))   # Keep all deciles for now

# Load the ecosystem names and other ref data
eco_ref <- read_xlsx("dat/SiteC_EcosystemUnits.xlsx", 
                     sheet = "MapCodes", skip = 1)


## EU Summary Table
# This is a tally of the ecosystems present within the mapping area. 

eu_summary <- eco_dat_long %>% 
  mutate("BEC Variant" = str_c(`BGC_ZONE`, `BGC_SUBZON`, `BGC_VRT`)) %>% 
  mutate(Ecosystem = ifelse(is.na(`SERAL`), 
                            `SITEMC`,
                            str_c(`SITEMC`, ":", SERAL))) %>%
  group_by(`BEC Variant`, Ecosystem, `STRCT`) %>% 
  summarise("Total_area" = sum(Area_decile_ha)) %>% 
  mutate("unit" = str_c(`BEC Variant`, Ecosystem)) %>% 
  left_join(eco_ref, by = "unit") %>% 
  mutate(structural_class = case_when(
    STRCT == 1 ~ "Non/sparsely vegetated",
    STRCT == 2 ~ "Herbaceous",
    STRCT == 3 ~ "Shrub",
    STRCT %in% c(4,5) ~ "Young Forest", 
    STRCT %in% c(6,7) ~ "Mature Forest", 
    TRUE ~ "NA"
  )) %>% 
  select("BEC Variant" = `BEC Variant.x`, Ecosystem, "Site Series Name", Type, 
         STRCT, structural_class, Total_area) 
  
# write_csv(eu_summary, here("eco_mapping_summary", "out", "Footprint_CEM_EU.csv"))

