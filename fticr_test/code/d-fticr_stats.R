## FTICR-STATISTICS
## 2021-06-11: modify code for CC, SULI-2021

# PART I: PCA -------------------------------------------------------------
## Principal Components Analysis allows us to analyze multivariate data
## by compressing the variables into "Principal Components",
## which are artificially generated pseudo-variables 

## we perform PCA on relative abundance data,
## where we have relative abundance of compound classes for each sample




# Step 1: load file -------------------------------------------------------
# we want the relative abundance for each sample/core
relabund_cores = read.csv("fticr_test/data/processed/fticr_relabund_cores.csv")


# Step 2: process the file ------------------------------------------------
## we need to convert the relabund file into long-form
## i.e. one column for each compound class
## then split the wide-form file into two pieces,
## a. the numeric variables (the relative abundances)
## b. the categorical variables (the grouping variables/treatments)

relabund_wide =
  relabund_cores %>% 
  ungroup %>% 
  dplyr::select(-c(abund, total)) %>% 
  # make all the variables character
  # and then convert `relabund` back to numerical
  mutate_all(as.character) %>% 
  mutate(relabund = as.numeric(relabund)) %>% 
  # make wide
  spread(class, relabund) %>% 
  # this introduces a lot of missing cells (NA)
  # replace all NA with 0 to run the analysis 
  replace(.,is.na(.),0)  %>% 
  # delete the first column, not needed
  dplyr::select(-1)

# subset only the numeric variables
num = 
  relabund_wide %>% 
  dplyr::select(where(is.numeric))

# subset variables that are not numeric
grp = 
  relabund_wide %>% 
  dplyr::select(-where(is.numeric)) %>% 
  # and then make a new column based on the row number
  dplyr::mutate(row = row_number())

# finally, perform the pca using the `prcomp` function
pca_int = prcomp(num, scale. = T)
plot(pca_int)


#
# Step 3: plot the PCA biplot ---------------------------------------------
# ggbiplot is a package that makes it easier to plot PCA biplots
# it uses the language and syntax of {ggplot2}
# this package is not available for download from CRAN (official R package network),
# but can be downloaded from GitHub
# install.packages("devtools")
# devtools::install_github("miraKlein/ggbiplot")

library(ggbiplot)

ggbiplot(pca_int, 
         # scaling sets the aspect ratio of the graph, scale = 1 means square 
         obs.scale = 1, var.scale = 1,
         # use groups from the grp file to color/group the data points
         groups = as.character(grp$Suction), 
         # ellipse = TRUE will generate confidence interval ellipses around the groups
         # make it easier to see clusters in the data
         ellipse = TRUE, 
         circle = FALSE, var.axes = TRUE, alpha = 0)+
  geom_point(
    # set color, shape by the grouping variable above
    aes(shape = groups, color = groups),
    # customize shape, size, etc.
    size=2,stroke=1, alpha = 0.5)+
  theme_classic()

#
# PART II: PERMANOVA ------------------------------------------------------
## PERMutational ANanlysis of VAriance (PERMANOVA) is a multivariate tool
## to determine if a treatment has a statistically significant effect on the data
## It is similar to the ANOVA, but for multivariate data

## just like PCA, we will use the wide-form of the relabund_core data

## we use the `adonis` function from the {vegan} package
library(vegan)

## the general form is adonis (y ~ x, data = DATA), similar form to ANOVA 
## here, our y-variable is a matrix of all the numeric variables

adonis(relabund_wide %>% dplyr::select(where(is.numeric)) ~ Suction, 
       data = relabund_wide)

## when you run this, the P-value (Pr(>F)) tells us 
## if a variable had a significant impact on the data,
## with p = 0.05 as the threshold
## if p < 0.05, that variable has a significant effect on the data

## since Suction had p = 0.025, we conclude that Suction had a significant influence on the 
## pore water chemistry 

