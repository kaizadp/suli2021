library(tidyverse)
path = "tes_drought/data/nmr_data/peaks/074.csv"

# Step 1. import file. 
# check.names=FALSE because columns have duplicate names, and we want to leave as is
df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

colnames(df)[1] = ""

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