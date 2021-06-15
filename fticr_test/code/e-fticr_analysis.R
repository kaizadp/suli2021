

# load packages -----------------------------------------------------------
library(tidyverse)

# load files --------------------------------------------------------------
data_long_trt = read.csv("fticr_test/data/processed/fticr_long_trt.csv.gz")
meta = read.csv("fticr_test/data/processed/fticr_meta.csv")
meta_hcoc = meta %>% 
  dplyr::select(formula, HC, OC)


# UNIQUE PEAKS ------------------------------------------------------------

data_unique =  
  data_long_trt %>% 
  # group by all columns 
  # and then create a new column that counts how many times each formula was seen
  group_by(formula, Moisture, Wetting) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)# %>% filter(n==1)


# plot the common peaks
gg_vankrev(data_unique %>% filter(count == 2), aes(x = OC, y = HC))+
  labs(title = "common peaks")+
  theme_classic()

# plot the unique peaks
gg_vankrev(data_unique %>% filter(count == 1), 
           aes(x = OC, y = HC, color = as.character(Suction)))+
  labs(title = "unique peaks")+
  theme_classic()
