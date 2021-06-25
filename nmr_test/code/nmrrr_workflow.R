# NMRRR workflow
# KFP

## This script provides the workflow for basic NMR data processing and analysis.
## - importing and cleaning spectra files
## - plotting spectra
## - importing and cleaning peaks files
## - calculating relative abundances, based on NMR peaks (using bins from Clemente et al. 2012)
## - creating relative abundance summary tables
## - plotting relative abundance bar plots
## - computing statistics (PERMANOVA, PCA)



## The sample data used here are from Patel et al. 2021 Soil Biology and Biochemistry
## We investigate the effect of wetting vs. drying on soil C chemistry
  ## "Wetting" - soils were dried (drought) and then rewet to 50 % w/w moisture
  ## "Drying" - soils were saturated (flood) and then dried to 50 % w/w moisture



# STEP 0: load packages ---------------------------------------------------
library(tidyverse)
library(readxl)


# STEP 1b. set bins -------------------------------------------------------
bins_dat = 
  read.delim("nmr_test/Clemente2012.txt", header = TRUE) %>% 
  arrange(start) %>% 
  dplyr::mutate(number = row_number())


# STEP 2: set input directories -------------------------------------------
SPECTRA_FILES = "nmr_test/data/KFP_hysteresis/spectra/"
PEAKS_FILES = "nmr_test/data/KFP_hysteresis/peaks/"

# STEP 3: process spectra and peak data -----------------------------------

## import and combine SPECTRA files

  filePaths_spectra <- list.files(path = SPECTRA_FILES,pattern = "*.csv", full.names = TRUE)
  spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
    # the files are tab-delimited, so read.csv will not work. import using read.table
    # there is no header. so create new column names
    # then add a new column `source` to denote the file name
    df <- read.table(path, header=FALSE, col.names = c("ppm", "intensity"))
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
  spectra_processed = spectra_dat %>% 
      # retain only values 0-10ppm
      filter(ppm >= 0 & ppm <= 10) %>% 
      mutate(source = str_remove(source, paste0(SPECTRA_FILES, "/"))) %>% 
      mutate(source = str_remove(source, ".csv")) %>% 
      # mutate(source = paste0("DOC-",source)) %>% 
      # dplyr::rename(DOC_ID = source) %>% 
      # left_join(doc_key, by = "DOC_ID") %>% 
      force()




# STEP 4: spectra graphs ----------------------------------------------------------

  gg_spectra = function(dat, LABEL_POSITION, mapping, STAGGER){
    
    # create spectra-base plot ----
    
    spectra_base = 
      ggplot()+
      # stagger bracketing lines for odd vs. even rows  
      geom_segment(data=bins_dat %>% dplyr::filter(row_number() %% 2 == 0), 
                   aes(x=start, xend=stop, y=LABEL_POSITION, yend=LABEL_POSITION), color = "black")+
      geom_segment(data=bins_dat %>% dplyr::filter(row_number() %% 2 == 1), 
                   aes(x=start, xend=stop, y=LABEL_POSITION-0.2, yend=LABEL_POSITION-0.2), color = "black")+
      # stagger numbering like the lines
      geom_text(data=bins_dat %>% dplyr::filter(row_number() %% 2 == 0), 
                aes(x = (start+stop)/2, y = LABEL_POSITION+0.1, label = number))+
      geom_text(data=bins_dat %>% dplyr::filter(row_number() %% 2 == 1), 
                aes(x = (start+stop)/2, y = LABEL_POSITION-0.1, label = number))+
      scale_x_reverse(limits = c(10,0))+
      
      #geom_path(data = dat, aes(x = ppm, y = intensity, color = source))+
      xlab("shift, ppm")+
      ylab("intensity")+
      theme_classic()
    
    # add staggering factor ----
    
    stagger_factor = 1/STAGGER
    dat_y_stagger = 
      dat %>% 
      distinct(source) %>% 
      mutate(newsource = source != c(NA, head(source, -1))) %>% 
      drop_na() %>% 
      mutate(y_factor = cumsum(newsource)/stagger_factor) %>% 
      dplyr::select(source, y_factor)
    
    spectra_new = 
      dat %>% 
      left_join(dat_y_stagger) %>% 
      replace_na(list(y_factor = 0)) %>% 
      mutate(intensity_new = intensity + y_factor) %>% 
      dplyr::select(-intensity) %>% 
      rename(intensity = intensity_new)
    
    # combined plot ----
    
    spectra_base+
      geom_path(data = spectra_new, mapping)
    
  }
  
  
  
  gg_spectra(dat = spectra_processed, 
             LABEL_POSITION = 4, 
             aes(x = ppm, y = intensity, 
                 group = source, color = source),
             STAGGER = 1) + 
    labs(subtitle = "binset: Clemente 2012")+
    ylim(0, 5)


# STEP 5: relative abundance ---------------------------

  ## process peaks data
  
  ## import and combine all peaks data
  filePaths_peaks <- list.files(path = PEAKS_FILES,pattern = "*.csv", full.names = TRUE)
  peaks_rawdat <- do.call(bind_rows, lapply(filePaths_peaks, function(path) {
    # this function will import all the data files and combine for all samples
    # first, we run the function to clean a single file
    # the input data are spread across multiple columns, so use this function to align columns
    
    align_columns = function(path){
      # Step 1. import file. 
      # check.names=FALSE because columns have duplicate names, and we want to leave as is
      df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      
      # Step 2. confirm that the data are in 9-column groups
      noname_cols <- which(names(df) == "")
      if(!all(diff(noname_cols) == 9)) {
        stop("Formatting problem: data don't appear to be in 9-column groups")
      }
      names(df)[noname_cols] <- "Obs"  # give them a name
      
      # Step 3. Extract each group in turn and store temporarily in a list
      nmr_list <- lapply(noname_cols, function(x) df[x:(x + 8)])
      
      # Step 4. Finally, bind everything into a single data frame
      # This uses dplyr but we could also use base R: do.call("rbind", nmr_list)
      nmr_dat <- dplyr::bind_rows(nmr_list)
      
      # Step 5. Create a new column that includes source sample name
      nmr_dat[["source"]] <- rep(path, nrow(df))
      
      nmr_dat
    }
    
    # now create an object from the function
    align_columns(path)
    # this will be repeated for each file in the input folder
    
  }))
  
  # process the dataset
    # WATER_start = 3; WATER_stop = 4
    # DMSO_start = 2.25; DMSO_stop = 2.75
    
  ## process the peaks
    peaks_processed = 
      peaks_rawdat %>% 
      filter(ppm>=0&ppm<=10) %>% 
      filter(Intensity > 0) %>% 
      # remove solvent regions
      #filter(!(ppm>DMSO_start & ppm<DMSO_stop)) %>% 
      #filter(!(ppm>WATER_start & ppm<WATER_stop)) %>% 
      filter(!is.na(ppm)) %>% 
      # remove peaks with 0 intensity, and peaks flagged as weak 
      filter(!Flags=="Weak") %>% 
      mutate(source = str_remove(source, paste0(PEAKS_FILES, "/"))) %>% 
      mutate(source = str_remove(source, ".csv")) %>% 
      #mutate(DOC_ID = paste0("DOC-", DOC_ID)) %>% 
      #dplyr::select(-Obs, -source) %>% 
      dplyr::select(-Obs) %>% 
      force()
  
  
  
  
  ## i. calculate relative abundances ----
## 5a. load corekey
COREKEY = "nmr_test/data/KFP_hysteresis/corekey.csv"
corekey = read.csv(COREKEY) %>% mutate(Core = as.character(Core))

## 5c. compute and plot relabund

rel_abund_cores1 = 
  subset(merge(peaks_processed, bins_dat), start <= ppm & ppm <= stop) %>% 
  #dplyr::select(source,ppm, Area, group) %>% 
  #filter(!(ppm>DMSO_start&ppm<DMSO_stop)) %>% 
  rename(Core = source) %>% 
  group_by(Core, group) %>% 
  filter(group != "oalkyl") %>% 
  dplyr::summarize(area = sum(Area)) %>% 
  group_by(Core) %>% 
  dplyr::mutate(total = sum(area),
                relabund = round((area/total)*100,2)) %>% 
  dplyr::select(Core, group, relabund) %>% 
  replace(is.na(.), 0) %>% 
  left_join(corekey, by = "Core")

rel_abund_wide1 = 
  rel_abund_cores1 %>% 
  pivot_wider(names_from = "group", values_from = "relabund")

relabund_cores = 
    rel_abund_wide1 %>% 
    pivot_longer(where(is.numeric), values_to = "relabund", names_to = "group") %>% 
    replace_na(list(relabund = 0))


## make summary and summary table
relabund_summary = 
  relabund_cores %>%
  group_by(group, treatment) %>% 
  dplyr::summarize(relabund_mean = round(mean(relabund),2),
                   relabund_se = round(sd(relabund, na.rm = T)/sqrt(n()), 2))





relabund_summarytable = 
  relabund_summary %>% 
  mutate(relabund = paste(relabund_mean, "\u00b1", relabund_se),
         relabund = str_remove_all(relabund, " \u00b1 NA"))





## ii. plot relative abundances ----
(relabund_bar = 
   relabund_summary %>% 
   ggplot(aes(x = treatment, y = relabund_mean, fill = group))+
   geom_bar(stat = "identity")+
   #    facet_grid(~ treatment)+
   theme_classic()+
   NULL)

# STEP 6: statistics ------------------------------------------------------
## i. PERMANOVA ----
library(vegan)

relabund_wide = 
  relabund_cores %>% 
  ungroup() %>% 
  pivot_wider(names_from = group, values_from = relabund)

(permanova_tzero = 
    adonis(relabund_wide %>% 
             dplyr::select(where(is.numeric))  ~ 
             treatment,
           data = relabund_wide))

#
## ii. PCA ----
# devtools::install_github("miraKlein/ggbiplot")
library(ggbiplot)

relabund_pca=
  relabund_wide %>% 
  ungroup %>% 
  dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic1, aliphatic2, aromatic, alphah, amide))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic1, aliphatic2, aromatic, alphah, amide)) %>% 
  dplyr::mutate(row = row_number())

pca_int = prcomp(num, scale. = T)



## PCA plot
(gg_pca = 
    ggbiplot(pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(grp$treatment), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=2,stroke=1, alpha = 0.5,
               aes(shape = groups,
                   color = groups))+
    theme_classic()+
    NULL)






