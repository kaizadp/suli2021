
library(tidyverse)

1. #PRocessing TES Drought Data

  #Loading Files ----

fticr_tes_drought_data = read.csv("tes_drought/data/fticr_data/fticr_data.csv")

fticr_tes_drought_meta = read.csv("tes_drought/data/fticr_data/fticr_meta.csv")

core_keys = read.csv("tes_drought/data/corekey.csv")

 #Renaming columns in the FTICR core_keys

fticr_tes_drought_data_renamed = rename(fticr_tes_drought_data,
       "DOC-001" = "Fansler_51618_DOC001_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55581",
       "DOC-002" = "Fansler_51618_DOC002_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55582",
       "DOC-003" = "Fansler_51618_DOC003_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55583",
       "DOC-009" = "Fansler_51618_DOC009_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55588",
       "DOC-073" = "Fansler_51618_DOC073_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55723",
       "DOC-074" = "Fansler_51618_DOC074_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55724",
       "DOC-075" = "Fansler_51618_DOC075_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55725",
       "DOC-079" = "Fansler_51618_DOC079_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55729",
       "DOC-080" = "Fansler_51618_DOC080_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55730",
       "DOC-081" = "Fansler_51618_DOC081_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55731",
       "DOC-177" = "Fansler_DOC_177_SPE_18Dec20_Alder_300SA_p1_1_01_57099",
       "DOC-178" = "Fansler_DOC_178_SPE_18Dec20_Alder_300SA_p1_1_01_57100",
       "DOC-179" = "Fansler_DOC_179_SPE_18Dec20_Alder_300SA_p1_1_01_57101",
       "DOC-180" = "Fansler_DOC_180_SPE_18Dec20_Alder_300SA_p1_1_01_57102",
       "DOC-185" = "Fansler_DOC_185_SPE_18Dec20_Alder_300SA_p1_1_01_57107",
       "DOC-186" = "Fansler_DOC_186_SPE_18Dec20_Alder_300SA_p1_1_01_57108",
       "DOC-187" = "Fansler_DOC_187_SPE_18Dec20_Alder_300SA_p1_1_01_57109",
       "DOC-037" = "Fansler_51618_DOC037_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55617",
       "DOC-038" = "Fansler_51618_DOC038_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55618",
       "DOC-039" = "Fansler_51618_DOC039_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55619",
       "DOC-043" = "Fansler_51618_DOC043_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55622",
       "DOC-044" = "Fansler_51618_DOC044_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55625",
       "DOC-045" = "Fansler_51618_DOC045_Alder_Inf_02Oct2020_300SA_IATp1_1_01_55626",
       "DOC-107" = "Fansler_51618_DOC107_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55760",
       "DOC-108" = "Fansler_51618_DOC108_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55761",
       "DOC-109" = "Fansler_51618_DOC109_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55762",
       "DOC-113" = "Fansler_51618_DOC113_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55766",
       "DOC-114" = "Fansler_51618_DOC114_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55767",
       "DOC-115" = "Fansler_51618_DOC115_Alder_Inf_05Oct2020_300SA_IATp1_1_01_55768",
       "DOC-181" = "Fansler_DOC_181_SPE_18Dec20_Alder_300SA_p1_1_01_57103",
       "DOC-182" = "Fansler_DOC_182_SPE_18Dec20_Alder_300SA_p1_1_01_57104",
       "DOC-183" = "Fansler_DOC_183_SPE_18Dec20_Alder_300SA_p1_1_01_57105",
       "DOC-184" = "Fansler_DOC_184_SPE_18Dec20_Alder_300SA_p1_1_01_57106",
       "DOC-188" = "Fansler_DOC_188_SPE_18Dec20_Alder_300SA_p1_1_01_57110",
       "DOC-189" = "Fansler_DOC_189_SPE_18Dec20_Alder_300SA_p1_1_01_57111",
       "DOC-190" = "Fansler_DOC_190_SPE_18Dec20_Alder_300SA_p1_1_01_57112")


 #Merging fticr data and corekey ----

merge(core_keys, fticr_tes_drought_data_renamed, by.x = "DOC-", by.y = "DOC-")


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

crunch::write.csv.gz(fticr_tes_drought_data_long_key, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_core.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_tes_drought_data_long_key2, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_tes_drought_data_long_trt, "Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz", row.names = F, na="")
