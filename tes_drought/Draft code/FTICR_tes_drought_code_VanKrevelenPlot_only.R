
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
  facet_wrap(~treatment)+
  facet_wrap(~Site)+
  theme_classic()


  # RA BAR Plotting ----

RA = read.csv("tes_drought/data/Processed Data/Processed_FTICR_DATA/fticr_tes_drought_RA_trt.csv")

RA %>%
  ggplot(aes(x = Site, y = relabund2, fill = class))+
  geom_bar(stat = "identity")+
  facet_wrap(~treatment)+
  theme_classic()

RA %>%
  ggplot(aes(x = depth, y = relabund2, fill = class))+
  geom_bar(stat = "identity")+
  facet_wrap(~treatment)+
  theme_classic()

