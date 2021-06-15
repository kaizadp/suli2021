## SPATIAL ACCESS
## KAIZAD F. PATEL
## 2-JULY-2020

## FTICR-PROCESSING

## 2021-06-07: modify code for CC, SULI-2021


# load packages -----------------------------------------------------------
library(tidyverse)

# load files --------------------------------------------------------------
# the data file contains m/z and intensity data for each peak
fticr_data = read.csv("fticr_test/data/SpatAccess_testdata.csv")
# the meta file contains information about each peak
fticr_meta = read.csv("fticr_test/data/SpatAccess_eMeta.csv")
# the key file contains information about each sample (sample key)
fticr_key = read.csv("fticr_test/data/fticr_key.csv")

# fticr_key -----------------------------------------------------------------
fticr_key_cleaned =
  fticr_key %>% 
  # create a new column that includes the entire name
  mutate(SampleAssignment = paste0(Suction, "-", Moisture, "-", Wetting)) %>% 
  # subset only the columns needed
  select(FTICR_ID, Core, Suction, Moisture, Wetting, Amendments, SampleAssignment)

#
# fticr_meta ---------------------------------------------------------
## processing the meta data 

meta = 
  fticr_meta %>% 
  # filter appropriate mass range
  filter(Mass>200 & Mass<900) %>% 
  # remove isotopes
  filter(C13==0) %>% 
  # remove peaks without C assignment
  filter(C>0) %>% 

  # create columns for indices
  dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                HC = round(H/C,2),
                OC = round(O/C,2)) %>% 
  
  # create column/s for formula
  # first, create columns for individual elements
  # then, combine
  dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                formula = str_replace_all(formula,"NA","")) %>% 
  
  # elemental composition (CHONS, etc)
  # create column/s for formula
  dplyr::mutate(element_c = if_else(C>0,paste0("C"),as.character(NA)),
                element_h = if_else(H>0,paste0("H"),as.character(NA)),
                element_o = if_else(O>0,paste0("O"),as.character(NA)),
                element_n = if_else(N>0,paste0("N"),as.character(NA)),
                element_s = if_else(S>0,paste0("S"),as.character(NA)),
                element_p = if_else(P>0,paste0("P"),as.character(NA)),
                element_comp = paste0(element_c,element_h, element_o, element_n, element_s, element_p),
                element_comp = str_replace_all(element_comp,"NA","")) %>%
  
  #  dplyr::select(Mass, formula, El_comp, Class, HC, OC, AImod, NOSC, C:P)
  
  # assign compound classes
  mutate(
    class = case_when(AImod > 0.66 ~ "condensed_arom",
                      AImod <=0.66 & AImod >= 0.50 ~ "aromatic",
                      AImod < 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                      HC >= 1.5 & N==0 ~ "aliphatic",
                      HC >= 1.5 & N>0 ~ "aliphatic",
                      HC >= 2 ~ "aliphatic"),
    
    class = if_else(is.na(class)&!is.na(formula), "other", class)) %>%
  filter(!class=="other") %>% 
  
  # select only required columns
  dplyr::select(Mass, formula, element_comp, class, HC, OC, AImod, NOSC, C:P, -C13)

mass_list = 
  meta %>% pull(Mass)

#
# fticr_data --------------------------------------------------------------------
## make a new file called data_long, which will take the wide-form data
## and convert to long-form

data_long = 
  fticr_data %>% 
  filter(Mass %in% mass_list) %>% 
  pivot_longer(-Mass,
               names_to = "FTICR_ID",
               values_to = "intensity") %>% 
  mutate(presence = if_else(intensity>0, 1, 0)) %>% 
  filter(presence==1) %>% 
  # add the molecular formula column
  left_join(select(meta, Mass, formula), by = "Mass") %>% 
  # some formulae have multiple m/z. drop the multiples
  distinct(FTICR_ID, formula, presence)

data_long_key =
  data_long %>% 
  left_join(fticr_key_cleaned, by = "FTICR_ID") %>%
  group_by(SampleAssignment, formula) %>% 
  mutate(n = n())

## Now, create a replication filter for the peaks, 
## following Sleighter et al. 2012 (10.1021/ac3018026) and Payne et al. 2009 (10.1021/jasms.8b03484)
## keep only peaks seen in 2/3 of replicates within that treatment


# first, create a separate file that gives us the no. of reps per treatment
reps = 
  data_long_key %>% 
  ungroup() %>% 
  distinct(Core, SampleAssignment) %>% 
  group_by(SampleAssignment) %>% 
  dplyr::summarise(reps = n())

# second, join the `reps` file to the long_key file
# and then use the replication filter  
data_long_key2 = 
  data_long_key %>% 
  left_join(reps, by = "SampleAssignment") %>% 
  ungroup() %>% 
  mutate(keep = n >= (2/3)*reps) %>% 
  filter(keep)

data_long_trt = 
  data_long_key2 %>% 
  group_by(SampleAssignment, Suction, Moisture, Wetting, Amendments) %>% 
  distinct(formula, presence)

#
meta_hcoc = 
  meta %>% 
  select(formula, HC, OC)

#
# outputs -----------------------------------------------------------------
write.csv(meta, "fticr_test/data/processed/fticr_meta.csv", row.names = F)
write.csv(fticr_key_cleaned, "fticr_test/data/processed/fticr_key.csv", row.names = F)
crunch::write.csv.gz(data_long, "fticr_test/data/processed/fticr_long_core.csv.gz", row.names = F, na="")
crunch::write.csv.gz(data_long_key2, "fticr_test/data/processed/fticr_long_key.csv.gz", row.names = F, na="")
crunch::write.csv.gz(data_long_trt, "fticr_test/data/processed/fticr_long_trt.csv.gz", row.names = F, na="")

# -----------------------------------------------------------------

