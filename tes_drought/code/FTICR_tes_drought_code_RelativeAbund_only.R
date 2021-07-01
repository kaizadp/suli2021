
# 2. Relative Abundance (RA)

library(tidyverse)

  # RA_Load files ----

fticr_data_key = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz")

fticr_meta = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv")

  # RA Calculation ----

RA_cores = fticr_data_key %>%
  left_join(dplyr::select(fticr_meta, formula, class), by = "formula") %>%
  group_by(DOC_ID, depth, Site, treatment, class) %>%
  dplyr::summarise(abund = sum(presence)) %>%
  filter(!is.na(class)) %>%
  ungroup %>%
  group_by(Site, DOC_ID) %>%
  dplyr::mutate(total = sum(abund), relabund = round((abund/total)*100,2))

  # Calculating Mean RA ----

RA_trt = RA_cores %>%
  group_by(DOC_ID, depth, Site, treatment, class) %>%
  dplyr::summarize(relabund2 = mean(relabund), 
                   
                   se = sd(relabund/sqrt(n())),
                   
                   relative_abundance = paste(relabund2, "\u00b1", se))

  # Output ----

write.csv(RA_cores, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_cores.csv", row.names=FALSE)
write.csv(RA_trt, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_trt.csv", row.names=FALSE)


