## Summarize Ecosystems from TEM/PEM/CEM ## 

# This tool creates a list of ecosystem units and calculates area mapped 
# across all deciles.

# An ecosystem unit is the combination of site series/map code, seral 
# association and structural stage.

## Import and organize ----

# Libraries
library(tidyverse)
library(readxl)
options(scipen = 999)

# Read attribute file.

fname <- "dat/2025_May/BCH_Select_Parcels_ProjectFootprint_CEM_Intersect.xlsx"

# Use this if attribute file is xlsx.
eco_dat <- read_excel(fname) |> 
  select(ParcelID_t,  Dist_Type, CEM_ECP, ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, 
         SDEC_S1 = SDEC_1, SITE_S1, SITEMC_S1, STRCT_S1, SERAL_S1 = SERAL_1, 
         SDEC_S2 = SDEC_2, SITE_S2, SITEMC_S2, STRCT_S2, SERAL_S2 = SERAL_2, 
         SDEC_S3 = SDEC_3, SITE_S3, SITEMC_S3, STRCT_S3, SERAL_S3 = SERAL_3,
         AREA = Shape_Area) 

# Conduct QA of codes and values
table(eco_dat$BGC_ZONE, eco_dat$BGC_SUBZON)

table(eco_dat$SDEC_S1)
table(eco_dat$SITEMC_S1)
table(eco_dat$SERAL_S1)
table(eco_dat$STRCT_S1)

table(eco_dat$SDEC_S2)
table(eco_dat$SITEMC_S2)
table(eco_dat$SERAL_S2)
table(eco_dat$STRCT_S2)

table(eco_dat$SDEC_S3)
table(eco_dat$SITEMC_S3)
table(eco_dat$SERAL_S3)
table(eco_dat$STRCT_S3)


## Pivot the deciles in to longer form to allow for summaries ----
eco_dat_long <- eco_dat %>% 
  pivot_longer(!c(ParcelID_t,  Dist_Type, ECO_SEC:BGC_VRT, AREA), 
               names_to = c(".value", "DECILE"), 
               names_sep = "_"
               ) %>% 
  mutate(DECILE = str_replace(DECILE, "S", "Decile"),
         Area_decile = SDEC/10 * AREA) %>% 
  mutate(eco = ifelse(is.na(`SERAL`), 
                            `SITEMC`,
                            str_c(`SITEMC`, ":", SERAL))) %>%
#  select(-AREA) %>% 
  filter(!is.na(SITEMC))

# Create a summary of parcel sizes and amount of footprint.
area_sum <- eco_dat_long |> 
  mutate(Dist_Type = replace_na(Dist_Type, "Not footprint")) |> 
  group_by(ParcelID_t, Dist_Type) |> 
  summarize(area_ha = sum(Area_decile)/10000) |> 
  pivot_wider(id_cols = "ParcelID_t", names_from = "Dist_Type", 
              values_from = area_ha, values_fill = 0) |> 
  rowwise() |> 
  mutate(parcel_area = sum(c_across(1:2))) |> 
  ungroup()

write_csv(area_sum, "out/2025_May/parcel_area_sum.csv")



# Load the ecosystem names and other ref data
eco_ref <- read_xlsx("dat/SiteC_EcosystemUnits-v2.xlsx", 
                     sheet = "MapCodes", skip = 1) |> 
  mutate(unit = paste0(`BEC Variant`, `Map Code`))



## EU Summary Table
eu_summary <- eco_dat_long %>% 
  filter(is.na(Dist_Type)) |> 
  mutate("BEC Variant" = str_c(`BGC_ZONE`, `BGC_SUBZON`, `BGC_VRT`)) %>% 
  mutate(Ecosystem = ifelse(is.na(`SERAL`), 
                            `SITEMC`,
                            str_c(`SITEMC`, ":", SERAL))) %>%
  group_by(ParcelID_t, `BEC Variant`, Ecosystem, `STRCT`) %>% 
  summarise("Total_area" = sum(Area_decile)/10000) %>% 
  mutate("unit" = str_c(`BEC Variant`, Ecosystem),
         "Ecosystem Unit" = if_else(!is.na(STRCT), 
                                    paste0(Ecosystem, STRCT),
                                    paste0(Ecosystem))) %>% 
  left_join(eco_ref, by = "unit") %>% 
  mutate(structural_class = case_when(
    STRCT == 1 ~ "Non/sparsely vegetated",
    STRCT == 2 ~ "Herbaceous",
    STRCT == 3 ~ "Shrub",
    STRCT %in% c(4,5) ~ "Young Forest", 
    STRCT %in% c(6,7) ~ "Mature Forest", 
    TRUE ~ "NA"
  )) %>% 
  select(ParcelID_t, "BEC Variant" = `BEC Variant.x`, Ecosystem, 
         "Site Series Name", Type, STRCT, structural_class, "Ecosystem Unit", 
         Total_area) 

# Add BHC

EUs_BHC <- read_xlsx("dat/SiteC_EcosystemUnits-v2.xlsx", 
                     sheet = "EUs with Habitat Category", skip = 3)

BHC_levels <- c("CMF", "CYF", "CSH",
                "DMF", "DYF", "DSH", 
                "RMF", "RYF", "RSH",  
                "FBS", "FBT", 
                "WRI", "WSH", "WGR",
                "DSS", "DSG", 
                "CUL", "NVE", "ANT", 
                "WAT")

EUs_BHC <- EUs_BHC |> 
  mutate(BHC20 = factor(BHC20, levels = BHC_levels), ordered = TRUE)

eu_summary <- eu_summary |> 
  left_join(EUs_BHC, by = "Ecosystem Unit") |> 
  select(ParcelID_t, 'BEC Variant', Ecosystem, 
         'Site Series Name' = 'Site Series Name.x', Type, STRCT, 
         structural_class, 'Ecosystem Unit', BHC20, Total_area)

BHC_summary <- eu_summary |> 
  group_by(ParcelID_t, BHC20) |>
  summarize(Total_area = sum(Total_area)) |> 
  pivot_wider(id_cols = "ParcelID_t", names_from = "BHC20", 
              values_from = "Total_area", values_fill = 0) |> 
  left_join(area_sum, by = "ParcelID_t")


write_csv(BHC_summary, "out/2025_May/BCH_parcels_BHC_Summary.csv", )
