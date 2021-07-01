# 4. STATS - Multivariate of Suction and Rewetting Graphs ----

library(dplyr)
library(tidyr)

RA_cores = read.csv("fticr_test_data2/Processed_Data2/fticr2_RA_cores.csv")

  # STATS - Processing Data ----

RA_wide=
  RA_cores %>%
  ungroup %>% 
  dplyr::select(-c(abund, total)) %>% 
  mutate_all(as.character) %>% 
  mutate(relabund = as.numeric(relabund)) %>% 
  spread(class, relabund) %>% 
  replace(.,is.na(.),0)  %>% 
  dplyr::select(-1)

num = 
  RA_wide %>%
  dplyr::select(where(is.numeric))

grp = 
  RA_wide %>% 
  dplyr::select(-where(is.numeric)) %>%
  dplyr::mutate(row = row_number())

pca_int = prcomp(num, scale. = T)
plot(pca_int)


  # STATS - Biplots ----

library(ggbiplot)

ggbiplot(pca_int,
         obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Suction), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = grp$Soil_Moisture, color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()

ggbiplot(pca_int,
         obs.scale = 1, var.scale = 1,
         groups = (grp$Soil_Moisture), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp$Suction), color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()

  # STATS - PERMONVA ----

library(vegan)

adonis(RA_wide %>% dplyr::select(where(is.numeric)) ~
         Suction + Soil_Moisture + Suction:Soil_Moisture, 
       data = RA_wide)


# 5. Unique Peaks ----
  # Load files ----

library(tidyverse)

data2_long_trt = read.csv("fticr_test_data2/Processed_Data2/fticr_data2_long_trt.csv.gz")
meta2 = read.csv("fticr_test_data2/Processed_Data2/fticr_meta2.csv")
meta_hcoc = meta2 %>%
  dplyr::select(formula, HC, OC)

  # Grpahic Function ----

gg_vankrev <- function(data, mapping){
  ggplot(data, mapping) +
    geom_point(size=1, alpha=0.5) +
    ylab("H/C") +
    xlab("O/C") +
    ylim(0,2.5) +
    xlim(0,1.25) +
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5, color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4, color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51, color="black",linetype="longdash") +
    guides(colours = guide_legend(overrides.aes = list(alpha=1, size=2)))
  
}

  # give us unique peaks in each suction type ----
data2_unique =  
  data2_long_trt %>%
  group_by(formula, Soil_Moisture, Rewetting) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)# %>% filter(n==1)

  # compare drought vs. field moist ----
# calculate unique peaks in drought and fm for each suction

data2_unique_moisture=  
  data2_long_trt %>%
  group_by(formula, Suction, Rewetting) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)# %>% filter(n==1)


data2_common_moisture = data2_unique_moisture %>%
  filter(count == 2) %>%
  distinct(formula, Suction, Rewetting, count, HC, OC)

data2_uniquepeaks_moisture = data2_unique_moisture %>%
  filter(count == 1)

gg_unique = gg_vankrev(data2_uniquepeaks_moisture,
                       aes(x = OC, y = HC, color = Soil_Moisture))+
  facet_wrap(~Suction)+
  labs(title = "Unique peaks")+
  theme_classic()

gg_common = gg_vankrev(data2_common_moisture,
                       aes(x = OC, y = HC))+
  facet_wrap(~Suction)+
  labs(title = "Common peaks")+
  theme_classic()

# Combine Common and Unique Graphs into One

library(patchwork)

gg_common / gg_unique
