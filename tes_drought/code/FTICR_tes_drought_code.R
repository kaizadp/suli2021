
library(tidyverse)

1. #PRocessing TES Drought Data

  #Loading Files ----

fticr_tes_drought_data = read.csv("tes_drought/data/fticr_data/fticr_data.csv")

fticr_tes_drought_meta = read.csv("tes_drought/data/fticr_data/fticr_meta.csv")

fticr_tes_drought_report = read.csv("tes_drought/data/fticr_data/fticr_report.csv")


  #fticr key cleaning(SKIPPED due to different column names) ----

fticr_tes_drought_data_cleaned = fticr_tes_drought_data %>%
  mutate(SampleAssignment = paste0(Suction, "-", Soil_Moisture, "-", Rewetting)) %>%
  select(FTICR_ID, Core, Suction, Soil_Moisture, Rewetting, Amendments, SampleAssignment)


  # fticr_tes_drought_meta processing ----

meta = fticr_tes_drought_meta %>%
  
  filter(Mass>200 & Mass<900) %>%
  
  filter(C13==0) %>%
  
  filter(C>0) %>%
  
  dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                HC = round(H/C,2),
                OC = round(O/C,2)) %>%
  
  dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                formula = str_replace_all(formula,"NA","")) %>%
  
  dplyr::mutate(element_c = if_else(C>0,paste0("C"),as.character(NA)),
                element_h = if_else(H>0,paste0("H"),as.character(NA)),
                element_o = if_else(O>0,paste0("O"),as.character(NA)),
                element_n = if_else(N>0,paste0("N"),as.character(NA)),
                element_s = if_else(S>0,paste0("S"),as.character(NA)),
                element_p = if_else(P>0,paste0("P"),as.character(NA)),
                element_comp = paste0(element_c,element_h, element_o, element_n, element_s, element_p),
                element_comp = str_replace_all(element_comp,"NA","")) %>%
  mutate(
    class = case_when(AImod > 0.66 ~ "condensed_arom",
                      AImod <=0.66 & AImod >= 0.50 ~ "aromatic",
                      AImod < 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                      HC >= 1.5 & N==0 ~ "aliphatic",
                      HC >= 1.5 & N>0 ~ "aliphatic",
                      HC >= 2 ~ "aliphatic"),
    
    class = if_else(is.na(class)&!is.na(formula), "other", class)) %>%
  filter(!class=="other") %>%
  
  dplyr::select(Mass, formula, element_comp, class, HC, OC, AImod, NOSC, C:P, -C13)

mass_list = 
  meta %>% pull(Mass)

  #fticr_tes_drought_data_long ----

fticr_tes_drought_data_long_key = fticr_tes_drought_data %>%
  filter(Mass %in% mass_list) %>%
  pivot_longer(-Mass, names_to = "FTICR_ID", values_to = "intensity") %>%
  mutate(presence = if_else(intensity>0, 1, 0)) %>%
  filter(presence==1) %>%
  left_join(select(meta, Mass, formula), by = "Mass") %>%
  distinct(FTICR_ID, formula, presence) %>%
  na.omit() %>%
  group_by(formula) %>%
  mutate(n = n())

  # Rep Filter ----

reps = fticr_tes_drought_data_long_key %>%
  dplyr::summarise(reps = n())

fticr_tes_drought_data_long_key2 = fticr_tes_drought_data_long_key %>%
  left_join(reps, by = "formula") %>%
  ungroup() %>%
  mutate(keep = n >= (2/3)*reps) %>%
  filter(keep)

fticr_tes_drought_data_long_trt = fticr_tes_drought_data_long_key2 %>%
  distinct(formula, presence)

meta_HCOC = meta %>%
  select(formula, HC, OC)


  # Processed Data_Outputs ----

write.csv(meta, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_processedmeta.csv", row.names = F)

crunch::write.csv.gz(fticr_tes_drought_data_long_key, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_tes_drought_data_long_key2, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key2.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_tes_drought_data_long_trt, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz", row.names = F, na="")


