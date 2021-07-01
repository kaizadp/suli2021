
library(tidyverse)
library(readxl)


  #Clemente 2012 Bins ----

bins_dat = 
  read.delim("tes_drought/data/Clemente2012.txt", header = TRUE) %>% 
  arrange(start) %>% 
  dplyr::mutate(number = row_number())


  #Input Directories ----

SPECTRA_FILES = "tes_drought/data/nmr_data/spectra/"
PEAKS_FILES = "tes_drought/data/nmr_data/peaks/"

  #processing spectra data ----

filePaths_spectra <- list.files(path = SPECTRA_FILES,pattern = "*.csv", full.names = TRUE)

spectra_data <- do.call(rbind, lapply(filePaths_spectra, function(path) {
  df <- read.table(path, header=FALSE, col.names = c("ppm", "intensity"))
  df[["source"]] <- rep(path, nrow(df))
  df}))

spectra_processed = spectra_data %>% 
  filter(ppm >= 0 & ppm <= 10) %>% 
  mutate(source = str_remove(source, paste0(SPECTRA_FILES, "/"))) %>% 
  mutate(source = str_remove(source, ".csv"))

  #Spectra Graph ----

gg_spectra = function(dat, LABEL_POSITION, mapping, STAGGER){
  
  # create spectra-base plot ----
  
  spectra_base = 
    ggplot()+
    geom_segment(data=bins_dat %>% dplyr::filter(row_number() %% 2 == 0), 
                 aes(x=start, xend=stop, y=LABEL_POSITION, yend=LABEL_POSITION), color = "black")+
    geom_segment(data=bins_dat %>% dplyr::filter(row_number() %% 2 == 1), 
                 aes(x=start, xend=stop, y=LABEL_POSITION-0.2, yend=LABEL_POSITION-0.2), color = "black")+
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

  #Relative Abundance ----

## import and combine all peaks data
filePaths_peaks <- list.files(path = PEAKS_FILES,pattern = "*.csv", full.names = TRUE)
peaks_rawdat <- do.call(bind_rows, lapply(filePaths_peaks, function(path) {
  
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

## process the dataset
# WATER_start = 3; WATER_stop = 4
# DMSO_start = 2.25; DMSO_stop = 2.75

## process the peaks
peaks_processed = 
  peaks_rawdat %>% 
  filter(ppm >=0 & ppm <= 10) %>% 
  filter(Intensity > 0) %>% 
  # remove solvent regions
  #filter(!(ppm>DMSO_start & ppm<DMSO_stop)) %>% 
  #filter(!(ppm>WATER_start & ppm<WATER_stop)) %>% 
  filter(!is.na(ppm)) %>% 
  # remove peaks with 0 intensity, and peaks flagged as weak 
  filter(!Flags=="Weak") %>% 
  mutate(source = str_remove(source, paste0(PEAKS_FILES, "/"))) %>% 
  mutate(source = str_remove(source, ".csv")) %>% 
  dplyr::select(-Obs)

  #Renaming processed peak ----


peaks_processed_renamed = rename(peaks_processed,
                                 "DOC-074" = "tes_drought/data/nmr_data/peaks/074",
                                 "DOC-079" = "tes_drought/data/nmr_data/peaks/079",
                                 "DOC-081" = "tes_drought/data/nmr_data/peaks/081",
                                 "DOC-178" = "tes_drought/data/nmr_data/peaks/178",
                                 "DOC-179" = "tes_drought/data/nmr_data/peaks/179",
                                 "DOC-180" = "tes_drought/data/nmr_data/peaks/180",
                                 "DOC-185" = "tes_drought/data/nmr_data/peaks/185",
                                 "DOC-186" = "tes_drought/data/nmr_data/peaks/186",
                                 "DOC-187" = "tes_drought/data/nmr_data/peaks/187",
                                 "DOC-181" = "tes_drought/data/nmr_data/peaks/181",
                                 "DOC-182" = "tes_drought/data/nmr_data/peaks/182",
                                 "DOC-183" = "tes_drought/data/nmr_data/peaks/183",
                                 "DOC-188" = "tes_drought/data/nmr_data/peaks/188",
                                 "DOC-189" = "tes_drought/data/nmr_data/peaks/189",
                                 "DOC-190" = "tes_drought/data/nmr_data/peaks/190")

  ## ii. calculate relative abundances ----

COREKEY = "tes_drought/data/corekey.csv"

corekey = read.csv(COREKEY) %>% mutate(DOC_ID = as.character(DOC_ID))


rel_abund_cores1 = 
  # match the peaks with the bins
  subset(merge(peaks_processed, bins_dat), start <= ppm & ppm <= stop) %>% 
  rename(DOC_ID = source) %>% 
  group_by(DOC_ID, group) %>% 
  # remove tthe oalkyl group because that's where the water peak lies
  filter(group != "oalkyl") %>% 
  dplyr::summarize(area = sum(Area)) %>% 
  group_by(DOC_ID) %>% 
  dplyr::mutate(total = sum(area),
                relabund = round((area/total)*100,2)) %>% 
  dplyr::select(DOC_ID, group, relabund) %>% 
  replace(is.na(.), 0) %>% 
  left_join(corekey, by = "DOC_ID")






RA_wide1 = 
  rel_abund_cores1 %>% 
  pivot_wider(names_from = "group", values_from = "relabund")

RA_cores = 
  RA_wide1 %>% 
  pivot_longer(where(is.numeric), values_to = "relabund", names_to = "group") %>% 
  replace_na(list(relabund = 0))


## iii. make summary and summary table ----
RA_TD_summary = 
  RA_TD_cores %>%
  group_by(group, treatment) %>% 
  dplyr::summarize(relabund_mean = round(mean(relabund),2),
                   relabund_se = round(sd(relabund, na.rm = T)/sqrt(n()), 2))

relabund_summarytable = 
  relabund_summary %>% 
  mutate(relabund = paste(relabund_mean, "\u00b1", relabund_se),
         relabund = str_remove_all(relabund, " \u00b1 NA"))

print(relabund_summarytable)

