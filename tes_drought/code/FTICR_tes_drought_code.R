## TES Drought -- FTICR data analysis
## Summer 2021


# Step 0: LOAD THE PACKAGES -----------------------------------------------
## run this portion before doing anything else.
library(tidyverse)
library(ggbiplot)
library(vegan)
library(patchwork)

#
# 1. PRocessing TES Drought Data 

#####################################################################################

# 1. Processing the data --------------------------------------------------
## Load Files ----

fticr_data = read.csv("tes_drought/data/fticr_data/fticr_data.csv")

fticr_meta = read.csv("tes_drought/data/fticr_data/fticr_meta.csv")

core_keys = read.csv("tes_drought/data/corekey.csv") %>% filter(skip != "skip") %>% dplyr::select(-skip)

# create a vector of just the sample IDs, will be used later
SAMPLE_IDs = core_keys %>% pull(DOC_ID)


## Process meta ----

meta = fticr_meta %>%
  
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

## Process data ----

### a. make long-form ----

fticr_data_long = 
  fticr_data %>% 
  filter(Mass %in% mass_list) %>% 
  pivot_longer(-Mass,
               names_to = "FTICR_ID",
               values_to = "intensity") %>% 
  # clean the FTICR_ID column
  # extract strings that include "DOC" followed by a 3-digit number
  mutate(FTICR_ID = str_replace(FTICR_ID, "DOC_", "DOC"),
         DOC_ID = str_extract(FTICR_ID, "DOC[0-9]{3}"), 
         DOC_ID = str_replace(DOC_ID, "DOC", "DOC-")) %>% 
  dplyr::select(Mass, DOC_ID, intensity) %>% 
  mutate(presence = if_else(intensity>0, 1, 0)) %>% 
  filter(presence==1) %>% 
  # add the molecular formula column
  left_join(select(meta, Mass, formula), by = "Mass") %>% 
  # some formulae have multiple m/z. drop the multiples
  distinct(DOC_ID, formula, presence) %>% 
  filter(DOC_ID %in% SAMPLE_IDs)
  

### b. Merging fticr data and corekey ----

fticr_data_long_key =
  fticr_data_long %>%
  left_join(core_keys, by = "DOC_ID") %>%
  group_by(depth, Site, treatment, formula) %>%
  mutate(n = n())

### c. Rep Filter ----

reps = 
  fticr_data_long_key %>%
  ungroup() %>%
  distinct(DOC_ID, depth, Site, treatment) %>%
  group_by(depth, Site, treatment) %>%
  dplyr::summarise(reps = n())

fticr_data_long_key2 = 
  fticr_data_long_key %>%
  left_join(reps) %>%
  ungroup() %>%
  mutate(keep = n >= (2/3)*reps) %>%
  filter(keep)

### d. peaks present by treatment ----

# create a "summary dataframe" of formulas present per treatment type
# group by depth, Site, treatment
fticr_data_long_trt = 
  fticr_data_long_key2 %>%
  group_by(depth, Site, treatment) %>%
  distinct(formula, presence)

#
### e. Processed Data_Outputs ----

write.csv(meta, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv", row.names = F)

crunch::write.csv.gz(fticr_data_long, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_core.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_data_long_key2, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_data_long_trt, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz", row.names = F, na="")

#
#####################################################################################

# 2. Relative Abundance (RA) ----

## RA_Load files ----

fticr_data_key = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz")

fticr_meta = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv")

## RA Calculation ----

# RA by core
RA_cores = 
  fticr_data_key %>%
  left_join(dplyr::select(fticr_meta, formula, class), by = "formula") %>%
  group_by(DOC_ID, depth, Site, treatment, class) %>%
  dplyr::summarise(abund = sum(presence)) %>%
  filter(!is.na(class)) %>%
  ungroup %>%
  group_by(DOC_ID) %>%
  dplyr::mutate(total = sum(abund), relabund = round((abund/total)*100,2))

# RA summary (mean, se) per treatment
RA_trt = 
  RA_cores %>%
  group_by(depth, Site, treatment, class) %>%
  dplyr::summarize(relabund2 = mean(relabund),
                   se = sd(relabund/sqrt(n())),
                   relative_abundance = paste(relabund2, "\u00b1", se))

## RA BAR Plotting ----
RA_trt %>%
  # reorder treatments
  mutate(treatment = factor(treatment, levels = c("timezero", "drought"))) %>% 
  ggplot(aes(x = treatment, y = relabund2, fill = class))+
  geom_bar(stat = "identity")+
  labs(x = "",
       y = "relative abundance, %")+
  facet_grid(. ~ Site)+
  theme_classic()




## RA_Output ----
write.csv(RA_cores, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_core.csv", row.names=FALSE)
write.csv(RA_trt, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_trt.csv", row.names=FALSE)

##################################################################################  


# 3. Van Krevelen Plots ----

## gg_vankrev function for plots ----
gg_vankrev <- function(data,mapping){
  ggplot(data,mapping) +
    # plot points
    geom_point(size=1, alpha = 0.5) + # set size and transparency
    # axis labels
    ylab("H/C") +
    xlab("O/C") +
    # axis limits
    xlim(0,1.25) +
    ylim(0,2.5) +
    # add boundary lines for Van Krevelen regions
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
    guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))
  
}

## Loading Files ----

data_long_trt = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz")

meta = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv")

## Processing Files for Plotting ----
meta_hcoc = 
  meta %>%
  select(formula, HC, OC) %>% 
  # there are some duplicate formulas, because multiple masses could have the same formula
  # get rid of duplicates using distinct()
  distinct()

data_hcoc =
  data_long_trt %>%
  left_join(meta_hcoc) 

## VK plots ----

gg_vankrev(data_hcoc, aes(x = OC, y = HC, color = treatment))+
  facet_grid(. ~ Site)+
  theme_classic()


## 3b. Unique Peaks ----
### calculate common vs. unique peaks by treatment ----

## group by formula, depth, Site
# this will return counts of 1 or 2. 1 = unique, 2 = common
# for unique peaks, if unique to tzero = lost during drought, if unique to drought = gained during drought
data_counts =  
  data_long_trt %>%
  group_by(formula, depth, Site) %>% 
  dplyr::mutate(count = n()) %>% 
  mutate(loss_gain = case_when(count == 1 & treatment == "timezero" ~ "lost",
                               count == 1 & treatment == "drought" ~ "gained")) %>% 
  left_join(meta_hcoc)

### unique/common VK plots ----
# plot common points
data_counts %>% 
  filter(count == 2) %>% 
  #distinct(depth, Site, HC, OC) %>% 
  gg_vankrev(aes(x = OC, y = HC))+
  facet_grid(depth ~ Site)+
  labs(title = "common peaks")+
  theme_bw()

# plot unique points
data_counts %>% 
  filter(count == 1) %>% 
  gg_vankrev(aes(x = OC, y = HC, color = loss_gain))+
  facet_grid(. ~ Site)+
  labs(title = "peaks lost/gained after drought",
       color = "")+
  annotate("text", label = "aliphatic", x = 1.0, y = 1.8, fontface = "bold")+
  annotate("text", label = "lignin-like", x = 1.0, y = 1.2, fontface = "bold")+
  annotate("text", label = "aromatic", x = 1.0, y = 0.7, fontface = "bold")+
  annotate("curve", xend = 1.0, x = 0.9, yend = 0.65, y = 0.55)+
  annotate("text", label = "cond. aromatic", x = 1.0, y = 0.25, fontface = "bold")+
  theme_bw()

# Combine Common and Unique Graphs into One

# gg_common / gg_unique


###################################################################################

# 4. STATS - Multivariate Graphs ----
RA_cores = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_core.csv")

## STATS - Processing Data ----
## full dataset
RA_wide =
  RA_cores %>%
  ungroup %>% 
  dplyr::select(-c(abund, total)) %>% 
  mutate_all(as.character) %>% 
  mutate(relabund = as.numeric(relabund)) %>% 
  spread(class, relabund) %>% 
  replace(.,is.na(.),0)  %>% 
  dplyr::select(-1)

## a. PCA ----
### compute PCA ----
num = 
  RA_wide %>%
  dplyr::select(where(is.numeric))

grp = 
  RA_wide %>% 
  dplyr::select(-where(is.numeric)) %>%
  dplyr::mutate(row = row_number())

pca_int = prcomp(num, scale. = T)
plot(pca_int)

### plot PCA biplots ----
ggbiplot(pca_int,
         obs.scale = 1, var.scale = 1,
         groups = (grp$treatment), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp$Site), color = groups),
    size=4,stroke=1, alpha = 0.8)+
  labs(color = "Treatment", shape = "Site")+
  theme_classic()

#
## b. PERMANOVA ----
adonis(RA_wide %>% dplyr::select(where(is.numeric)) ~ treatment * Site, 
       data = RA_wide)
