## R - INTRODUCTION
## 2021-06-02

## to run a line of code from an R script, 
## use cmd + return (Mac) or ctrl + enter (Windows)


# PART 1: some example commands in R ------------------------------------------

## using R for mathematical operations
1+1
1+2
1+3

## you can assign the values to an object

sum1 = 1+1
sum2 = 1+2
sum3 = 1+3

## printing sentences as output
print("this is a sentence")



# PART 2: reading/importing files -----------------------------------------
## use the read.csv() function to import a csv file
## enter the file path in the parentheses, ()
## the file path includes the folders/subfolders and the file name, including the extension
## assign the imported file to an object using =

penguins_data = read.csv("datasets/penguins.csv") # this command imports csv files

# this code below will not work 
# because the `penguins.csv` file is present in the `datasets` folder, not in the main parent folder
penguins_data = read.csv("penguins.csv")

# this code below will not work 
# because the file path includes an extra space in the file name
penguins_data = read.csv("datasets/penguins .csv")



# PART 3: exploring the dataset -------------------------------------------

## show the column names
names(penguins_data)

## show the structure of the dataset,
## including number of rows/columns, column names, and column types
str(penguins_data)

## for penguins_data, some columns are character, some are numerical, and some are integer


## exploratory plot of all the data
plot(penguins_data)
## this plot shows the different variables and how they relate to each other



# PART 4: packages --------------------------------------------------------
## packages are add-ons that help you add extra functionality in R

## install packages using the `Packages` tab
## you can also install packages using code:
install.packages("dplyr")

## once installed, you do not need to re-install unless you re-install R/RStudio
## after installing, you need to load the packages in order to use them
## load packages using the library() function

library(dplyr)

## you need to load packages each time you restart your R session



# PART 5: dplyr::rename example ------------------------------------------------

## dplyr is part of the Tidyverse, which is a group of packages used to clean, process, and visualize data 
## in a clean, tidy, and intuitive manner. 

library(dplyr)

## rename columns
## use the rename() function in dplyr to rename columns
## the syntax to use is: rename(FILENAME, "NEW_COLUMN_NAME" = "OLD_COLUMN_NAME")
penguins_data2 = rename(penguins_data, "penguinspecies" = "species")

