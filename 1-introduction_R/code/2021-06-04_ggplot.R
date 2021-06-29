## 2020-06-04
## ggplot2

library(ggplot2)
library(dplyr)

## we will use the buil-in starwars dataset for this tutorial

## first, get the column names and structure of the dataset
names(starwars)
str(starwars)

## make a basic scatterplot of mass ~ height
ggplot(starwars,
       aes(x = height,
           y = mass)) + #aesthetic
  geom_point()

## there is a strong linear relationship between mass and height,
## except for one point (Jabba), with mass of > 1000
## so to keep this exercise simple, we exclude that one data point

starwars_new = filter(starwars, mass < 500)
# use filter() to exclude or include rows, based on a condition

## 1. scatter plot
ggplot(starwars_new,
       aes(x = height,
           y = mass)) + #aesthetic
  geom_point()


## 2. regression plots
ggplot(starwars_new,
       aes(x = height,
           y = mass)) + #aesthetic
  geom_point()+
  geom_smooth() # default = LOESS (regional regression)

ggplot(starwars_new,
       aes(x = height,
           y = mass)) + #aesthetic
  geom_point()+
  geom_smooth(method = "lm") # lm = linear model

# remove confidence intervals
ggplot(starwars_new,
       aes(x = height,
           y = mass)) + #aesthetic
  geom_point()+
  geom_smooth(method = "lm",
              se = FALSE) # se = standard error

## ^^ for smooth and linear regression,
## both x and y are numerical

## 3. box plot

ggplot(starwars_new,
       aes(x = gender,
           y = mass))+
  geom_boxplot()

## 4. violin plot
ggplot(starwars_new,
       aes(x = gender,
           y = mass))+
  geom_violin()

## box and violin plots need x = categorical/character
## and y = numeric or continuous

## 5. bar plot
ggplot(starwars_new,
       aes(x = gender,
           y = mass))+
  geom_bar(stat = "identity")
## this does not work well for individual data points
## good only when you have a mean/average value
## come back to bar plots later


## 6. jitter plot

ggplot(starwars_new,
       aes(x = gender,
           y = mass))+
  geom_point()

ggplot(starwars_new,
       aes(x = gender,
           y = mass))+
  geom_jitter(width = 0.2)
## geom_jitter needs x = categorical/character


## 7. histogram
ggplot(starwars_new,
       aes(x = mass))+
  geom_histogram()

## you can adjust bins using bin number (bins = ...) or
## binwidth = ...

ggplot(starwars_new,
       aes(x = mass))+
  geom_histogram(bins = 50)

ggplot(starwars_new,
       aes(x = mass))+
  geom_histogram(binwidth = 50)


####
####

## formatting your geometries
ggplot(starwars_new,
       aes(x = height,
           y = mass)) + #aesthetic
  geom_point(color = "blue",
             shape = 1)

ggplot(starwars_new,
       aes(x = height,
           y = mass,
           color = gender,
           shape = gender)) + #aesthetic
  geom_point()


ggplot(starwars_new,
       aes(x = gender,
           y = mass,
           color = gender, # color will set the outline
           fill = gender # fill will set the fill color
           ))+ 
  geom_violin()

ggplot(starwars_new,
       aes(x = gender,
           y = mass,
           fill = gender # fill will set the fill color
       ))+ 
  geom_violin(color = "black", # outline is black
              size = 0.3, # outline thickness
              linetype = "dashed",
              alpha = 0.5 # transparency, from 0 to 1
              ) 

## adding scales for color

ggplot(starwars_new,
       aes(x = height,
           y = mass,
           color = gender)) + 
  geom_point()+
  # scale_color_manual(values = c("red", "blue", "yellow"))+
  scale_color_manual(values = c("#ff6666", "#7193ff", "yellow"))
  

## practice:
## make different types of plots using the starwars dataset, or any other dataset of your choosing
## - try different geoms, 
## - try different colors, shapes, fill, etc.