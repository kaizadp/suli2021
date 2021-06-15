## FTICR-VAN KREVELENS
## 2021-06-07: modify code for CC, SULI-2021


# PART I: VAN KREVELEN PLOTS ----------------------------------------------
# make function for Van Krevelen plot -------------------------------------
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


# load files --------------------------------------------------------------
data_long_trt = read.csv("fticr_test/data/processed/fticr_long_trt.csv.gz")
meta = read.csv("fticr_test/data/processed/fticr_meta.csv")


# process the files -------------------------------------------------------

## make a new file that only includes the formula, HC, OC columns
meta_hcoc = meta %>% 
  dplyr::select(formula, HC, OC)

## merge the hcoc file with the data file
data_hcoc =
  data_long_trt %>% 
  left_join(meta_hcoc) %>% 
  # Suction has two values, 1.5 and 50
  # currently coded as numeric, make character
  mutate(Suction = as.character(Suction))


# plot Van Krevelen -------------------------------------------------------
gg_vankrev(data_hcoc, aes(x = OC, y = HC, color = Suction))+
  theme_classic()


# PART II: RELATIVE ABUNDANCE ---------------------------------------------
# load files --------------------------------------------------------------
relabund = read.csv("fticr_test/data/processed/fticr_relabund_trt.csv")


# plot bar graph ----------------------------------------------------------
relabund %>% 
  ggplot(aes(x = Suction, y = rel_abund, fill = class))+
  geom_bar(stat = "identity")+
  theme_classic()


