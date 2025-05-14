## Summarize Ecosystems from CEM for original PAZ## 


# An ecosystem unit is the combination of site series/map code, seral 
# association and structural stage.

## Import and organize ----

# Libraries
library(tidyverse)
library(readxl)

# Read attribute file.

fname <- "dat/Clearing_Strategies_PAZ_CEM_intersect.xlsx"

# Use this if attribute file is csv. 
# The attribute file is a csv export the original shapefile or other source. 
eco_dat <- read_csv(fname, 
                  col_types = cols(.default = "c",
                                   'Shape_Area' = col_double(),
                                   'SDEC_1' = col_integer(), 
                                   'SDEC_2' = col_integer(), 
                                   'SDEC_3' = col_integer())) %>% 

  
eco_dat <- read_xlsx(fname)

, 
                      col_types = cols(.default = "c",
                                       'Shape_Area' = col_double(),
                                       'SDEC_1' = col_integer(), 
                                       'SDEC_2' = col_integer(), 
                                       'SDEC_3' = col_integer())) %>% 
  
  
  
  
  # Select the attributes of interest.
select(CEM_ECP, ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT,  
         SDEC_S1 = SDEC_1, SITE_S1, SITEMC_S1, STRCT_S1, SERAL_S1 = SERAL_1, 
         SDEC_S2 = SDEC_2, SITE_S2, SITEMC_S2, STRCT_S2, SERAL_S2 = SERAL_2, 
         SDEC_S3 = SDEC_3, SITE_S3, SITEMC_S3, STRCT_S3, SERAL_S3 = SERAL_3,
       AREA) 

# Conduct QA of codes and values
table(eco_dat$BGC_ZONE, eco_dat$BGC_SUBZON)

table(eco_dat$SDEC_1)
table(eco_dat$SITEMC_S1)
table(eco_dat$SERAL_1)
table(eco_dat$STRCT_S1)

table(eco_dat$SDEC_2)
table(eco_dat$SITEMC_S2)
table(eco_dat$SERAL_2)
table(eco_dat$STRCT_S2)

table(eco_dat$SDEC_3)
table(eco_dat$SITEMC_S3)
table(eco_dat$SERAL_3)
table(eco_dat$STRCT_S3)

## Pivot the deciles in to longer form to allow for summaries ----
eco_dat_long <- eco_dat %>% 
  pivot_longer(!(PAZ:BGC_VRT), 
               names_to = c(".value", "DECILE"), 
               names_sep = "_"
               ) %>% 
  mutate(DECILE = str_replace(DECILE, "S", "Decile"),
         Area_decile_ha = SDEC/10 * Area_ha) %>% 
  select(-Area_ha) %>% 
  filter(!is.na(SITEMC))


# Load the ecosystem names and other ref data
eco_ref <- read_xlsx(here("eco_mapping_summary", "dat", "SiteC_EcosystemUnits.xlsx"), 
                     sheet = "MapCodes", skip = 1)


## EU Summary Table
# This is a tally of the ecosystems present within the footprint. 
# The "before" case.
eu_summary <- eco_dat_long %>% 
  mutate("BEC Variant" = str_c(`BGC_ZONE`, `BGC_SUBZON`, `BGC_VRT`)) %>% 
  mutate(Ecosystem = ifelse(is.na(`SERAL`), 
                            `SITEMC`,
                            str_c(`SITEMC`, ":", SERAL))) %>%
  group_by(`PAZ`, `BEC Variant`, Ecosystem, `STRCT`) %>% 
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
  select(PAZ, "BEC Variant" = `BEC Variant.x`, Ecosystem, "Site Series Name", Type, 
         STRCT, structural_class, Total_area) 
  
write_csv(eu_summary, here("eco_mapping_summary", "out", "Footprint_CEM_EU.csv"))

mc_summary <- eu_summary %>% 
  group_by(`PAZ`, `BEC Variant`, `Ecosystem`) %>%
  summarize("Total_area" = sum(Total_area)) %>%
  mutate("unit" = str_c(`BEC Variant`, Ecosystem)) %>% 
  filter(!is.na(Ecosystem)) %>% 
  pivot_wider(id_cols = c(`BEC Variant`, Ecosystem, unit), names_from = PAZ, values_from = `Total_area`) %>% 
  left_join(eco_ref, by = "unit") %>% 
  arrange(Sort) %>% 
  select(Sort, "BEC Variant" = `BEC Variant.x`, Ecosystem, `Site Series`, `Site Series Name`,
         Type, Dam, Reservoir, "TL" = `Transmission Line ROW`, `Hwy 29`, Road, Quarry, 
         "Erosion Impact" = `Erosion Impact Area`)

write_csv(mc_summary, here("eco_mapping_summary", "out", "Footprint_CEM_MC.csv"))

## Ecosystem Changes
# Below  is a tally of the ecosystems present after construction within the footprint. 
# The "after" case.
# It differs from the footprint ecosystem calculations in that impacts along the TL
# are not total loss of vegetation, rather conversion of forested areas to shrub.

# The difference between before and after is the impact to ecosystems.

# This needs to be limited to natural ecosystems (vegetated and non-vegetated).

# We don't really care what part of the footprint it turns into (that is actually 
# complicated in the dam area).
# Just want tally.

# Logic:
  # For ecosystems with structural stage shrub and lower, impact area is 0. 
  # For ecosystems pole-sapling to mature, change structural stage to shrub 
  # and leave impact area same. 

eu_after <- eu_summary %>% 
  mutate(structural_class_after = case_when(
    PAZ == "Transmission Line ROW" & STRCT %in% c(4,5,6,7) ~ "Shrub",
    PAZ == "Transmission Line ROW" & STRCT %in% c(1,2,3,NA) ~ structural_class,
    TRUE ~ "Anthropogenic/Reservoir")
    ) %>% 
  filter(!Type %in% c("Anthropogenic Areas", "Water")) %>% 
  filter(structural_class_after != "Anthropogenic/Reservoir") %>% 
  group_by(Type, structural_class_after) %>% 
  summarize(Total_area = sum(Total_area))

write_csv(eu_after, here("eco_mapping_summary", "out", "eu_after.csv"))



## For later when using BHC

level_order <- c("CSH","CYF","CMF","DSH","DYF", "DMF", "RSH", "RYF", "RMF", "FBS", "FBT", "WGR", "WSH", "WRI", "DSG", "DSS", "CUL", "NVE", "ANT", "WAT")
g <- ggplot(summary, aes(factor(BHC20, level = level_order), Total)) +
      geom_col()
g



