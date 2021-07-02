# 4. STATS - Multivariate of Suction and Rewetting Graphs ----

library(dplyr)
library(tidyr)

RA_cores = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_cores.csv")

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
         groups = as.character(grp$treatment), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = grp$depth, color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()

ggbiplot(pca_int,
         obs.scale = 1, var.scale = 1,
         groups = (grp$treatment), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp$Site), color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()


ggbiplot(pca_int,
         obs.scale = 1, var.scale = 1,
         groups = (grp$depth), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp$Site), color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()


  # STATS - PERMONVA ----

library(vegan)

adonis(RA_wide %>% dplyr::select(where(is.numeric)) ~
         treatment + Site + depth + treatment:Site + treatment:depth + Site:depth, 
       data = RA_wide)


# 5. Unique Peaks ----
  # Load files ----

library(tidyverse)

data_long_trt = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz")
meta = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv")
meta_hcoc = meta %>%
  dplyr::select(formula, HC, OC)

  # Graphic Function ----

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

data_counts =  
  data_long_trt %>%
  group_by(formula, depth, Site, treatment) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)

  # compare treatment ----

data_treatment=  
  data_long_trt %>%
  group_by(formula, depth, Site) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)


common_treatment = data_treatment %>%
  filter(count > 1) %>%
  distinct(formula, depth, Site, count, HC, OC)

gg_treatment = gg_vankrev(data_treatment,
                       aes(x = OC, y = HC, color = Site))+
  facet_wrap(~treatment)+
  labs(title = "Common Peaks")+
  theme_classic()

gg_treatment

  #compare Site ----

data_Site =  
  data_long_trt %>%
  group_by(formula, depth, treatment) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)# %>% filter(n==1)

  #compare depth

data_depth=  
  data_long_trt %>%
  group_by(formula, Site, treatment) %>% 
  dplyr::mutate(count = n()) %>% 
  left_join(meta_hcoc)# %>% filter(n==1)


# Combine Common and Unique Graphs into One

library(patchwork)

gg_common / gg_unique
