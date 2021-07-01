
# 2. Relative Abundance (RA)

library(tidyverse)

# RA_Load files ----

fticr_tes_drought_data_key = read.csv("Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz")

fticr_tes_drought_meta = read.csv("Processed Data/Processed_FTICR_DATA/fticr_tes_drought_processedmeta.csv")

# RA Calculation ----

RA_tes_drought_cores = fticr_tes_drought_data_key %>%
  left_join(dplyr::select(fticr_tes_drought_meta, formula, class), by = "formula") %>%
  group_by(FTICR_ID, class) %>%
  dplyr::summarise(abund = sum(presence)) %>%
  filter(!is.na(class)) %>%
  ungroup %>%
  group_by(FTICR_ID) %>%
  dplyr::mutate(total = sum(abund), relabund = round((abund/total)*100,2))

# Calculating Mean RA ----

RA_tes_drought_trt = RA_tes_drought_cores %>%
  group_by(FTICR_ID,class) %>%
  dplyr::summarize(relabund2 = mean(relabund), 
                   
                   se = sd(relabund/sqrt(n())),
                   
                   relative_abundance = paste(relabund2, "\u00b1", se))

