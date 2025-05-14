## Summarize Ecosystems from TEM/PEM/CEM ## 

# This tool creates a list of ecosystem units and calculates area mapped 
# across all deciles.

# An ecosystem unit is the combination of site series/map code, seral 
# association and structural stage.

## Import and organize ----

# Libraries
library(tidyverse)
library(readxl)

# Read attribute file.

fname <- "dat/2025_May/MitigationProperty_MarlFen_TEM.xlsx"

# Use this if attribute file is xlsx.
eco_dat <- read_excel(fname) |> 
  select(ECP_TAG, ECO_SEC = Eco_Sec, BGC_ZONE = Bgc_Zone, 
         BGC_SUBZON = Bgc_Subzon, BGC_VRT = Bgc_Vrt,  
       SDEC_S1 = Sdec_1, SITE_S1 = Site_S1, SITEMC_S1 = SiteMC_S1, STRCT_S1 = Strct_S1, 
       SERAL_S1 = Seral_1, 
       SDEC_S2 = Sdec_2, SITE_S2 = Site_S2, SITEMC_S2 = SiteMC_S2, STRCT_S2 = Strct_S2, 
       SERAL_S2 = Seral_2, 
       SDEC_S3 = Sdec_3, SITE_S3 = Site_S3, SITEMC_S3 = SiteMC_S3, STRCT_S3 = Strct_S3, 
       SERAL_S3 = Seral_3, 
       AREA = Area_ha) 

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
  pivot_longer(!c(ECP_TAG, ECO_SEC:BGC_VRT, AREA), 
               names_to = c(".value", "DECILE"), 
               names_sep = "_"
               ) %>% 
  mutate(DECILE = str_replace(DECILE, "S", "Decile"),
         Area_decile_ha = SDEC/10 * AREA) %>% 
  mutate(eco = ifelse(is.na(`SERAL`), 
                            `SITEMC`,
                            str_c(`SITEMC`, ":", SERAL))) %>%
#  select(-AREA) %>% 
  filter(!is.na(SITEMC))


# add BWBS old codes to SiteMC. 
old_to_new <- read_xlsx("dat/SiteC_EcosystemUnits-v2.xlsx", 
                        sheet = "Marl_crosswalk", skip = 2)

eco_dat_long <- eco_dat_long |> 
  left_join(old_to_new, by = "eco") |> 
  select(-SITEMC) |> 
  rename(SITEMC = SITEMC_replace)

# Load the ecosystem names and other ref data
eco_ref <- read_xlsx("dat/SiteC_EcosystemUnits-v2.xlsx", 
                     sheet = "MapCodes", skip = 1) |> 
  mutate(unit = paste0(`BEC Variant`, `Map Code`))



## EU Summary Table
eu_summary <- eco_dat_long %>% 
  mutate("BEC Variant" = str_c(`BGC_ZONE`, `BGC_SUBZON`)) %>% 
  mutate(Ecosystem = SITEMC) %>%
  group_by(`BEC Variant`, Ecosystem, `STRCT`) %>% 
  summarise("Total_area" = sum(Area_decile_ha)) %>% 
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
  select("BEC Variant" = `BEC Variant.x`, Ecosystem, "Site Series Name", Type, 
         STRCT, structural_class, "Ecosystem Unit", Total_area) 

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
  select('BEC Variant', Ecosystem, 'Site Series Name' = 'Site Series Name.x', Type, STRCT, 
         structural_class, 'Ecosystem Unit', BHC20, Total_area)

write_csv(eu_summary, "out/2025_May/Marl_2025_03_TEM_EU.csv")


# Check area
sum(eu_summary$Total_area)

# Summarize BHC for Offset Accounting
bhc_summary <- eu_summary |> 
  group_by(BHC20) |> 
  summarise(Total_area = as.numeric(sum(Total_area)))

write_csv(bhc_summary, "out/2025_May/Marl_2025_03_BHC_Summary.csv")
