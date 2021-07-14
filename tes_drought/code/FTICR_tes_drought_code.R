
library(tidyverse)

# 1. PRocessing TES Drought Data 

#Loading Files ----

fticr_data = read.csv("tes_drought/data/fticr_data/fticr_data.csv")

fticr_meta = read.csv("tes_drought/data/fticr_data/fticr_meta.csv")

core_keys = read.csv("tes_drought/data/corekey.csv")

#Renaming columns in the FTICR core_keys ----

fticr_data_renamed = rename(fticr_data,
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


#fticr_tes_drought_meta processing ----

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

#fticr_tes_drought_data_long ----

fticr_data_long = 
  fticr_data_renamed %>% 
  filter(Mass %in% mass_list) %>% 
  pivot_longer(-Mass,
               names_to = "DOC_ID",
               values_to = "intensity") %>% 
  mutate(presence = if_else(intensity>0, 1, 0)) %>% 
  filter(presence==1) %>% 
  # add the molecular formula column
  left_join(select(meta, Mass, formula), by = "Mass") %>% 
  # some formulae have multiple m/z. drop the multiples
  distinct(DOC_ID, formula, presence)

#Merging fticr data and corekey ----

fticr_data_long_key =
  fticr_data_long %>%
  left_join(core_keys, by = "DOC_ID") %>%
  group_by(depth, Site, treatment, formula) %>%
  mutate(n = n())

#Rep Filter ----

reps = fticr_data_long_key %>%
  ungroup() %>%
  distinct(DOC_ID, depth, Site, treatment) %>%
  group_by(depth, Site, treatment) %>%
  dplyr::summarise(reps = n())

fticr_data_long_key2 = fticr_data_long_key %>%
  left_join(reps) %>%
  ungroup() %>%
  mutate(keep = n >= (2/3)*reps) %>%
  filter(keep)

# create a "summary dataframe" of formulas present per treatment type
# group by depth, Site, treatment
fticr_data_long_trt = fticr_data_long_key2 %>%
  group_by(depth, Site, treatment) %>%
  distinct(formula, presence)

meta_HCOC = meta %>%
  select(formula, HC, OC)


# Processed Data_Outputs ----

write.csv(meta, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv", row.names = F)

crunch::write.csv.gz(fticr_data_long, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_core.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_data_long_key2, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_key.csv.gz", row.names = F, na="")

crunch::write.csv.gz(fticr_data_long_trt, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz", row.names = F, na="")


#####################################################################################

# 2. Relative Abundance (RA) 

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
  group_by(DOC_ID) %>%
  dplyr::mutate(total = sum(abund), relabund = round((abund/total)*100,2))

# Calculating Mean RA ----

RA_trt = RA_cores %>%
  group_by(depth, Site, treatment, class) %>%
  dplyr::summarize(relabund2 = mean(relabund),
                   
                   se = sd(relabund/sqrt(n())),
                   
                   relative_abundance = paste(relabund2, "\u00b1", se))

# RA_Output ----

write.csv(RA_cores, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_cores.csv", row.names=FALSE)

write.csv(RA_trt, "tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_trt.csv", row.names=FALSE)
  
##################################################################################  


# 3. Van Krevelen Plot ----

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


# Loading Files ----

data_long_trt = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_data_long_trt.csv.gz")

meta = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_meta.csv")

# Processing Files for Plotting ----

library(tidyverse)


meta_hcoc = meta %>%
  dplyr::select(formula, HC, OC)

data_hcoc =
  data_long_trt %>%
  left_join(meta_hcoc) %>%
  mutate(DOC_ID = as.character(DOC_ID))

gg_vankrev(data_hcoc, aes(x = OC, y = HC, color = depth))+
  facet_grid(Site ~ treatment)+
  theme_classic()

gg_vankrev(data_hcoc, aes(x = OC, y = HC, color = treatment))+
  facet_grid(Site ~ depth)+
  theme_classic()

gg_vankrev(data_hcoc, aes(x = OC, y = HC, color = Site))+
  facet_grid(depth ~ treatment)+
  theme_classic()


# RA BAR Plotting ----

library(ggplot2)

RA = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_trt.csv")

RA %>%
  ggplot(aes(x = Site, y = relabund2, fill = class))+
  geom_bar(stat = "identity")+
  facet_wrap(~treatment + depth)+
  theme_classic()

RA %>%
  # reorder treatments
  mutate(treatment = factor(treatment, levels = c("timezero", "drought"))) %>% 
  ggplot(aes(x = depth, y = relabund2, fill = class))+
  geom_bar(stat = "identity")+
  #facet_wrap(~treatment + Site)+
  facet_grid(Site ~ treatment)+
  theme_classic()


RA %>%
  # reorder treatments
  mutate(treatment = factor(treatment, levels = c("timezero", "drought"))) %>% 
  ggplot(aes(x = treatment, y = relabund2, fill = class))+
  geom_bar(stat = "identity")+
  #facet_wrap(~treatment + Site)+
  facet_grid(depth ~ Site)+
  theme_classic()


###################################################################################

# 4. STATS - Multivariate Graphs ----

library(dplyr)
library(tidyr)

RA_cores = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_cores.csv")

# STATS - Processing Data ----
## full dataset
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

## split by depth

RA_wide_0to5 = 
  RA_wide %>% 
  filter(depth == "0-5cm")

RA_wide_5toend = 
  RA_wide %>% 
  filter(depth == "5cm-end")


## and then run PCA for each set

num_0to5 = 
  RA_wide_0to5 %>%
  dplyr::select(where(is.numeric))

grp_0to5 = 
  RA_wide_0to5 %>% 
  dplyr::select(-where(is.numeric)) %>%
  dplyr::mutate(row = row_number())

pca_int_0to5 = prcomp(num_0to5, scale. = T)

num_5toend = 
  RA_wide_5toend %>%
  dplyr::select(where(is.numeric))

grp_5toend = 
  RA_wide_5toend %>% 
  dplyr::select(-where(is.numeric)) %>%
  dplyr::mutate(row = row_number())

pca_int_5toend = prcomp(num_5toend, scale. = T)

# STATS - Biplots ----

library(ggbiplot)

ggbiplot(pca_int,
         obs.scale = 1, var.scale = 1,
         groups = (grp$depth), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp$Site), color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()

ggbiplot(pca_int_0to5,
         obs.scale = 1, var.scale = 1,
         groups = (grp_0to5$treatment), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp_0to5$Site), color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()

ggbiplot(pca_int_5toend,
         obs.scale = 1, var.scale = 1,
         groups = (grp_5toend$treatment), 
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    aes(shape = as.character(grp_5toend$Site), color = groups),
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()


# STATS - PERMONVA ----

library(vegan)

adonis(RA_wide %>% dplyr::select(where(is.numeric)) ~
         treatment + Site + depth + treatment:Site + treatment:depth + Site:depth + treatment:Site:depth, 
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
  facet_grid(depth ~ Site)+
  labs(title = "unique peaks")+
  theme_bw()

# Combine Common and Unique Graphs into One

library(patchwork)

gg_common / gg_unique
