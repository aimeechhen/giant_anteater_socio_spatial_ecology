### FULL CODE (includes notes) ###

# Setting up R -----
## INSTALL PACKAGES
# install the R packages you be using for your analysis
# packages will change based on what you are doing
install.packages("readr")
install.packages("knitr")
install.packages("ctmm")
install.packages("tidyr")
install.packages("dplyr") # for %>% and other function, join functions
install.packages("ggplot2")
install.packages("lme4")
install.packages("tibble")
install.packages("lubridate") # for round_date() for corrMove

# Using github packages that are not on CRAN i.e. cannot be used via install.packages()
devtools::install_github("r-lib/devtools", force = TRUE)
devtools::install_github("ctmm-initiative/ctmm")
devtools::install_github("jmcalabrese/corrMove", force = TRUE)

### LOAD PACKAGES ###
# install the R packages you be using for your analysis
# packages will change based on what you are doing
library("devtools")
library("readr")
library("ctmm")
library("ggplot2")
library("dplyr") # for mutate
library("tidyr") # for pivot_longer()
library("lme4") # test to see if differences are significant using glmer()
library("corrMove")
library("tibble")
library("lubridate") # for round_date() for corrMove

### DATA PREPARATION ----

## WORKING DIRECTORY
# set working directory to where everything will be saved such as figures, RDS, etc.
# Only applicable to R files, not R Markdown files
setwd("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/R working directory")

## IMPORT DATASET
anteater.DATA <- read_csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/Anteaters_NoOutliers (original data).csv", col_types = cols(timestamp = "c", class = "c", identity = "c", id = "c", .default = "d"))

## TELEMETRY FORMAT
# Convert dataset to a telemetry object, assuming the data has been cleaned and containing no outliers
#DATA <- as.telemetry(anteater.DATA) # n=19
  #Error in strptime(xx, f, tz = tz) : input string is too long .-. used code below
DATA <- as.telemetry("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/Anteaters_NoOutliers (original data).csv")

### SUBSET DATA FOR INDIVIDUALS IN SPECIFIC SITES  ----
# Subset data to isolate individuals found only in specific sites
# general syntax: subsetname <- originaldataset[c(listofelements)] 
# to select multiple elements from a vector, add square brackets
# refer to https://campus.datacamp.com/courses/free-introduction-to-r/chapter-2-vectors-2?ex=11

## SUBSET DATA FOR SITE 1 (n=12)
site.1 <- DATA[c(1,3,9,10,12,15,21,24,27,29,36,38)] # male and female (n=12)
site.1.adult <- DATA[c(1,9,10,12,15,21,29,36,38)] # adult (n=9)
site.1.male <- DATA[c(1,3,12,21,24,27,38)] # male (n=7)
site.1.male.adult <- DATA[c(1,12,21,38)] # male adult (n=4)
site.1.female <- DATA[c(9,10,15,29,36)] # female adult (n=5)
# names() to check if individuals are correct and accounted for
## SUBSET DATA FOR SITE 2 (n=7)
site.2 <- DATA[c(2,8,22,25,30,37,42)] # male and female (n=7)
site.2.adult <- DATA[c(2,8,22,25,30,42)] # adult (n=6)
site.2.male <- DATA[c(8,25,37,42)]  # male (n=4)
site.2.male.adult <- DATA[c(8,25,42)] # male adult (n=3)
site.2.female <- DATA[c(2,22,30)]  # female adult (n=3)
# names() to check if individuals are correct and accounted for
## SUBSET DATA FOR SITE 3 (n=4)
site.3 <- DATA[c(19,28,31,41)] # male and female (n=4), contains adults only, no subadult
site.3.male <- DATA[28] # male (n=1)
site.3.female <- DATA[c(19,31,41)] # female (n=3)
# names() to check if individuals are correct and accounted for

## LOAD ALL SAVED FITTED MODELS (quick reference) ----
# Load all saved fitted models for SITE 1
FIT.1 <- readRDS("FIT.1.RDS")
FIT.1.male <- readRDS("FIT.1.male.RDS")
FIT.1.male.adult <- readRDS("FIT.1.male.adult.RDS")
FIT.1.female <- readRDS("FIT.1.female.RDS")
# Load all saved fitted models for SITE 2
FIT.2 <- readRDS("FIT.2.RDS")
FIT.2.male <- readRDS("FIT.2.male.RDS")
FIT.2.male.adult <- readRDS("FIT.2.male.adult.RDS")
FIT.2.female <- readRDS("FIT.2.female.RDS")
# Load all saved fitted models for SITE 3
FIT.3 <- readRDS("FIT.3.RDS")
FIT.3.male <- readRDS("FIT.3.male.RDS")
FIT.3.female <- readRDS("FIT.3.female.RDS")

## LOAD ALL AKDE ALIGNED UDS (quick reference) ----
# Load all saved AKDE aligned UDs for SITE 1
AKDE.1 <- readRDS("AKDE.1.RDS")
AKDE.1.male <- readRDS("AKDE.1.male.RDS")
AKDE.1.male.adult <- readRDS("AKDE.1.male.adult.RDS")
AKDE.1.female <- readRDS("AKDE.1.female.RDS")
# Load all saved AKDE aligned UDs for SITE 2
AKDE.2 <- readRDS("AKDE.2.RDS")
AKDE.2.male <- readRDS("AKDE.2.male.RDS")
AKDE.2.male.adult <- readRDS("AKDE.2.male.adult.RDS")
AKDE.2.female <- readRDS("AKDE.2.female.RDS")
# Load all saved AKDE aligned UDs for SITE 3
AKDE.3 <- readRDS("AKDE.3.RDS")
AKDE.3.male <- readRDS("AKDE.3.male.RDS")
AKDE.3.female <- readRDS("AKDE.3.female.RDS")

## META DATASET (quick reference) ----
# Adding a meta dataset from a supplementary dataset
METADATA <- read_csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/Anteater_Results_Final.csv")
# Must correct a mismatch entry for 'Larry 267' and 'Larry' between dataset and meta dataset
METADATA <- mutate(select(METADATA, 1:3), ID = if_else(condition = ID == 'Larry',
                                                       true = 'Larry 267',
                                                       false = ID))

## LOAD ALL PROXIMITY DATA ANALYSIS RESULTS (quick reference) ----
# Load Proximity Analysis results for SITE 1
DATA.proximity.1 <- read.csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.1.csv")
# Load Proximity Analysis results for SITE 2
DATA.proximity.2 <- read.csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.2.csv")
# Load Proximity Analysis results for SITE 3
DATA.proximity.3 <- read.csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.3.csv")

### FIT MOVEMENT MODELS ----

## FITTING MODELS FOR SITE 1 ----

# SITE 1 - MALE AND FEMALE (n=12)
GUESS.1 <- lapply(site.1[1:12], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1 <- lapply(1:12, function(i) ctmm.select(site.1[[i]],GUESS.1[[i]]) )
names(FIT.1) <- names(site.1[1:12])
saveRDS(object = FIT.1, file = "FIT.1.RDS")
# Load saved fitted model
FIT.1 <- readRDS("FIT.1.RDS")
overlap(FIT.1)

# SITE 1 - MALE (n=7)
GUESS.1.male <- lapply(site.1.male[1:7], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1.male <- lapply(1:7, function(i) ctmm.select(site.1.male[[i]],GUESS.1.male[[i]]) )
names(FIT.1.male) <- names(site.1.male[1:7])
saveRDS(object = FIT.1.male, file = "FIT.1.male.RDS")
# Load saved fitted model
FIT.1.male <- readRDS("FIT.1.male.RDS")
overlap(FIT.1.male)

# SITE 1 - MALE ADULT (n=4)
GUESS.1.male.adult <- lapply(site.1[1:4], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1.male.adult <- lapply(1:4, function(i) ctmm.select(site.1.male.adult[[i]],GUESS.1.male.adult[[i]]) )
names(FIT.1.male.adult) <- names(site.1.male.adult[1:4])
saveRDS(object = FIT.1.male.adult, file = "FIT.1.male.adult.RDS")
# Load saved fitted model
FIT.1.male.adult <- readRDS("FIT.1.male.adult.RDS")
overlap(FIT.1.male.adult)

# SITE 1 - FEMALE (n=5)
GUESS.1.female <- lapply(site.1.female[1:5], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1.female <- lapply(1:5, function(i) ctmm.select(site.1.female[[i]],GUESS.1.female[[i]]) )
names(FIT.1.female) <- names(site.1.female[1:5])
saveRDS(object = FIT.1.female, file = "FIT.1.female.RDS")
# Load saved fitted model
FIT.1.female <- readRDS("FIT.1.female.RDS")
overlap(FIT.1.female)

## FITTING MODELS FOR SITE 2 ----

# SITE 2 - MALE AND FEMALE (n=7)
GUESS.2 <- lapply(site.2[1:7], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2 <- lapply(1:7, function(i) ctmm.select(site.2[[i]],GUESS.2[[i]]) )
names(FIT.2) <- names(site.2[1:7])
saveRDS(object = FIT.2, file = "FIT.2.RDS")
# Load saved fitted model
FIT.2 <- readRDS("FIT.2.RDS")
overlap(FIT.2)

# SITE 2 - MALE (n=4)
GUESS.2.male <- lapply(site.2.male[1:4], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2.male <- lapply(1:4, function(i) ctmm.select(site.2.male[[i]],GUESS.2.male[[i]]) )
names(FIT.2.male) <- names(site.2.male[1:4])
saveRDS(object = FIT.2.male, file = "FIT.2.male.RDS")
# Load saved fitted model
FIT.2.male <- readRDS("FIT.2.male.RDS")
overlap(FIT.2.male)

# SITE 2 - MALE ADULT (n=3)
GUESS.2.male.adult <- lapply(site.2.male.adult[1:3], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2.male.adult <- lapply(1:3, function(i) ctmm.select(site.2.male.adult[[i]],GUESS.2.male.adult[[i]]) )
names(FIT.2.male.adult) <- names(site.2.male.adult[1:3])
saveRDS(object = FIT.2.male.adult, file = "FIT.2.male.adult.RDS")
# Load saved fitted model
FIT.2.male.adult <- readRDS("FIT.2.male.adult.RDS")
overlap(FIT.2.male.adult)

# SITE 2 - FEMALE (n=3)
GUESS.2.female <- lapply(site.2.female[1:3], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2.female <- lapply(1:3, function(i) ctmm.select(site.2.female[[i]],GUESS.2.female[[i]]) )
names(FIT.2.female) <- names(site.2.female[1:3])
saveRDS(object = FIT.2.female, file = "FIT.2.female.RDS")
# Load saved fitted model
FIT.2.female <- readRDS("FIT.2.female.RDS")
overlap(FIT.2.female)

## FITTING MODELS FOR SITE 3 ----

# SITE 3 - MALE AND FEMALE (n=4)
GUESS.3 <- lapply(site.3[1:4], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.3 <- lapply(1:4, function(i) ctmm.select(site.3[[i]],GUESS.3[[i]]) )
names(FIT.3) <- names(site.3[1:4])
saveRDS(object = FIT.3, file = "FIT.3.RDS")
# Load saved fitted model
FIT.3 <- readRDS("FIT.3.RDS")
overlap(FIT.3)

# SITE 3 - MALE (n=1)
GUESS.3.male <- lapply(site.3.male[1], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.3.male <- lapply(1, function(i) ctmm.select(site.3.male[[i]],GUESS.3.male[[i]]) )
names(FIT.3.male) <- names(site.3.male[1])
saveRDS(object = FIT.3.male, file = "FIT.3.male.RDS")
# Load saved fitted model
FIT.3.male <- readRDS("FIT.3.male.RDS")
overlap(FIT.3.male)

# SITE 3 - FEMALE (n=3)
GUESS.3.female <- lapply(site.3.female[1:3], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.3.female <- lapply(1:3, function(i) ctmm.select(site.3.female[[i]],GUESS.3.female[[i]]) )
names(FIT.3.female) <- names(site.3.female[1:3])
saveRDS(object = FIT.3.female, file = "FIT.3.female.RDS")
# Load saved fitted model
FIT.3.female <- readRDS("FIT.3.female.RDS")
overlap(FIT.3.female)

###################################################
#### ALTERNATE CODE FOR SAVING FIT MODELS RDS #####
# using if/else to fit models & to include save and load RDS file

# SITE 1 - ALL INDIVIDUALS (n=12)
GUESS.1 <- lapply(site.1[1:12], function(b) ctmm.guess(b,interactive=FALSE) ) #shouldn't take too long to run, may not be necessary to save as RDS
if(!file.exists("FIT.1.RDS"))  {# ! = not
  FIT.1 <- lapply(1:12, function(i) ctmm.select(site.1[[i]],GUESS.1[[i]]) )
  names(FIT.1) <- names(site.1[1:12])
  saveRDS(object = FIT.1, file = "FIT.1.RDS")
} else {# to import/load RDS file, it will appear in the console .-. need to load
  FIT.1 <- readRDS("FIT.1.RDS")
}
overlap(FIT.1)

# SITE 2 - ALL INDIVIDUALS (n=7)
GUESS.2 <- lapply(site.2[1:7], function(b) ctmm.guess(b,interactive=FALSE) ) #shouldn't take too long to run, may not be necessary to save as RDS
if(!file.exists("FIT.2.RDS"))  {# ! = not
  FIT.2 <- lapply(1:7, function(i) ctmm.select(site.2[[i]],GUESS.2[[i]]) )
  names(FIT.2) <- names(site.2[1:7])
  saveRDS(object = FIT.2, file = "FIT.2.RDS")
} else {# to import/load RDS file, it will appear in the console .-. need to load
  FIT.2 <- readRDS("FIT.2.RDS")
}
overlap(FIT.2)
###################################################

### AKDE OVERLAP ----
# create aligned UDs

# OVERLAP colour code individuals as male/female on the overlap plot
# Make a list of colours that matches the order of the HR estimates
# blue = male adult
# light blue = subadult male
# red = female adult
# ggplot has a different saving code/method -> ggsave()

## AKDE OVERLAP SITE 1 ----

# SITE 1 - MALE AND FEMALE (n=12)
AKDE.1 <- akde(site.1[1:12],FIT.1)
saveRDS(object = AKDE.1, file = "AKDE.1.RDS")
# Load saved AKDE aligned UDs
AKDE.1 <- readRDS("AKDE.1.RDS")
overlap(AKDE.1)
# colour coding sexes for plot
  # Make a list of colours that matches the order of the HR estimates
  #ggplot has a different saving code/method -> ggsave()
COL.1 <- c("blue3", "lightblue1", "red", "red", "blue3", "red", "blue3", "lightblue1", "lightblue1", "red",
           "red", "blue3")
png(file = "Overlap.1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1, col.DF = COL.1, col.level = COL.1, col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: male and female)")
dev.off()

# SITE 1 - MALE (n=7)
AKDE.1.male <- akde(site.1[1:7],FIT.1.male)
saveRDS(object = AKDE.1.male, file = "AKDE.1.male.RDS")
# Load saved AKDE aligned UDs
AKDE.1.male <- readRDS("AKDE.1.male.RDS")
overlap(AKDE.1.male)
png(file = "Overlap.1.male.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1.male, col.DF = "dodgerblue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: male)")
dev.off()

# SITE 1 - MALE ADULT (n=4)
AKDE.1.male.adult <- akde(site.1.male.adult[1:4],FIT.1.male.adult)
saveRDS(object = AKDE.1.male.adult, file = "AKDE.1.male.adult.RDS")
# Load saved AKDE aligned UDs
AKDE.1.male.adult <- readRDS("AKDE.1.male.adult.RDS")
overlap(AKDE.1.male.adult)
png(file = "Overlap.1.male.adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1.male.adult, col.DF = "blue3", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: male adult)")
dev.off()

# SITE 1 - FEMALE (n=5)
AKDE.1.female <- akde(site.1.female[1:5],FIT.1.female)
saveRDS(object = AKDE.1.female, file = "AKDE.1.female.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.1.female <- readRDS("AKDE.1.female.RDS")
overlap(AKDE.1.female)
png(file = "Overlap.1.female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1.female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: female)")
dev.off()

## AKDE OVERLAP SITE 2 ----

# SITE 2 - MALE AND FEMALE (n=7)
AKDE.2 <- akde(site.2[1:7],FIT.2)
saveRDS(object = AKDE.2, file = "AKDE.2.RDS")
# Load saved AKDE aligned UDs
AKDE.2 <- readRDS("AKDE.2.RDS")
overlap(AKDE.2)
# colour coding sexes for plot
  # Make a list of colours that matches the order of the HR estimates
  #ggplot has a different saving code/method -> ggsave()
COL.2 <- c("red", "blue3", "red", "blue3", "red", "lightblue1", "blue3") # blue = male; light blue = subadult male; red = female
png(file = "Overlap.2.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2, col.DF = COL.2, col.level = COL.2, col.grid = NA, level = NA) 
title("aKDE Overlap (Site 2: male and female)")
dev.off()

# SITE 2 - MALE (n=4)
AKDE.2.male <- akde(site.2[1:4],FIT.2.male)
saveRDS(object = AKDE.2.male, file = "AKDE.2.male.RDS")
# Load saved AKDE aligned UDs
AKDE.2.male <- readRDS("AKDE.2.male.RDS")
overlap(AKDE.2.male)
png(file = "Overlap.2.male.adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.male.adult, col.DF = "dodgerblue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: male)")
dev.off()

# SITE 2 - MALE ADULT (n=3)
AKDE.2.male.adult <- akde(site.2.male.adult[1:3],FIT.2.male.adult)
saveRDS(object = AKDE.2.male.adult, file = "AKDE.2.male.adult.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.2.male.adult <- readRDS("AKDE.2.male.adult.RDS")
overlap(AKDE.2.male.adult)
png(file = "Overlap.2.male.adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.male.adult, col.DF = "blue3", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: male adult)")
dev.off()

# SITE 2 - FEMALE (n=3)
AKDE.2.female <- akde(site.2.female[1:3],FIT.2.female)
saveRDS(object = AKDE.2.female, file = "AKDE.2.female.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.2.female <- readRDS("AKDE.2.female.RDS")
overlap(AKDE.2.female)
png(file = "Overlap.2.female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: female)")
dev.off()

## AKDE OVERLAP SITE 3 ----

# SITE 3 - MALE AND FEMALE (n=4)
AKDE.3 <- akde(site.3[1:4],FIT.3)
saveRDS(object = AKDE.3, file = "AKDE.3.RDS")
# Load saved AKDE aligned UDs
AKDE.3 <- readRDS("AKDE.3.RDS")
overlap(AKDE.3)
# colour coding sexes for plot
# Make a list of colours that matches the order of the HR estimates
#ggplot has a different saving code/method -> ggsave()
COL.3 <- c("red", "blue3", "red", "red") # blue = male; light blue = subadult male; red = female
png(file = "Overlap.3.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.3, col.DF = COL.3, col.level = COL.3, col.grid = NA, level = NA) 
title("aKDE Overlap (Site 3: male and female)")
dev.off()

# SITE 3 - MALE (n=1)
AKDE.3.male <- akde(site.3[1],FIT.3.male)
saveRDS(object = AKDE.3.male, file = "AKDE.3.male.RDS")
# Load saved AKDE aligned UDs
AKDE.3.male <- readRDS("AKDE.3.male.RDS")
overlap(AKDE.3.male)
png(file = "Overlap.3.male.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.male.adult, col.DF = "dodgerblue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 3: male)")
dev.off()

# SITE 3 - FEMALE (n=3)
AKDE.3.female <- akde(site.3.female[1:3],FIT.3.female)
saveRDS(object = AKDE.3.female, file = "AKDE.3.female.RDS")
# Load saved AKDE aligned UDs
AKDE.3.female <- readRDS("AKDE.3.female.RDS")
overlap(AKDE.3.female)
png(file = "Overlap.3.female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.3.female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 3: female)")
dev.off()

### META DATASET (for pairwise analysis) ## ----

# Adding a meta dataset from a supplementary dataset
METADATA <- read_csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/Anteater_Results_Final.csv")

# Must correct a mismatch entry for 'Larry 267' and 'Larry' between dataset and meta dataset
METADATA <- mutate(select(METADATA, 1:3), ID = if_else(condition = ID == 'Larry',
                                                       true = 'Larry 267',
                                                       false = ID))
#unique(METADATA$ID) # to check if the name has been corrected, (dataset$column)

### PAIRWISE ANALYSIS SEX COMPARISON ----
# creating the data frame from overlap values

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 1 ----
# SITE 1 - taking the overlap cube (array), extracting median layer, removing one portion of the triangle
overlap.1 <- overlap(object = AKDE.1, level = 0.95) # assigns the overlap results as an object
overlap.1.median <- overlap.1$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
  # [, , ,] = [row, column, layer] of the cube/array
  # values of layers: 1 = low, 2 = median (50%), 3 = high
overlap.1.median[upper.tri(overlap.1.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the matrix the triangle are duplicate values 
  # because its a symmetric matrix and removing the diagonal because it is the value of home range of itself, .-. we change upper and diag to NA, 
  # setting diag = TRUE, because 1. select the values you don't want then 2. remove the values by using "<- NA"
overlap.1.median # to check everything is correct (full-screen console for best results)

# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.1.matrix <- as.data.frame(overlap.1.median) # Convert matrix to data frame
pairwise.1.matrix$anteater_A <- rownames(pairwise.1.matrix) # add column of individual names
pairwise.1.pivot <- pivot_longer(pairwise.1.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
  # table is too wide .-. rotate it to make it long

# add columns to the dataframe matrix, general syntax -> join_type(firstTable, secondTable, by=columnTojoinOn)
pairwise.1.pivot.A <- left_join(pairwise.1.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
# adding anteater_A info to matrix
pairwise.1.pivot.B <- left_join(pairwise.1.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding anteater_B info to matrix with anteater_A info
DATA.pairwise.1 <- mutate(pairwise.1.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                       paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                       paste(Sex.A, Sex.B) == "Male Female" ~ "male-female",
                                                                       paste(Sex.A, Sex.B) == "Female Male" ~ "male-female"))
# adding column to indicate which sexes that are being compared
DATA.pairwise.1 # to check if matrix has all the correct columns, variables etc., with no NA values

# removing subadults from dataframe matrix
pairwise.1.df.A <- DATA.pairwise.1[which(DATA.pairwise.1$Age.A != "Subadult"),] # removing subadults from anteater_A from matrix
DATA.pairwise.1.adult <- pairwise.1.df.A[which(pairwise.1.df.A$Age.B != "Subadult"),] # removing subadults from anteater_B from matrix with anteater_A filtered

# Plot pairwise sex comparison for SITE 1 ----
## SITE 1 plot pairwise comparison (male and female)
ggplot(data = DATA.pairwise.1, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 1: male and female)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.1.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## SITE 1 plot pairwise comparison (adult only)
ggplot(data = DATA.pairwise.1.adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 1: adult only)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.1.adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 2 ----
# taking the overlap cube, extracting median layer, removing one portion of the triangle
overlap.2 <- overlap(object = AKDE.2, level = 0.95) # assigns the overlap results as an object
overlap.2.median <- overlap.2$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
overlap.2.median[upper.tri(overlap.2.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the matrix
#View(overlap.2.median) # to check everything is correct (full-screen console for best results)

# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.2.matrix <- as.data.frame(overlap.2.median) # Convert matrix to data frame
pairwise.2.matrix$anteater_A <- rownames(pairwise.2.matrix) # add column of individual names
pairwise.2.pivot <- pivot_longer(pairwise.2.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
  # table is too wide .-. rotate it to make it long

# add columns to the dataframe matrix, general syntax -> join_type(firstTable, secondTable, by=columnTojoinOn)
pairwise.2.pivot.A <- left_join(pairwise.2.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
# adding anteater_A info to matrix
pairwise.2.pivot.B <- left_join(pairwise.2.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding anteater_B info to matrix with anteater_A info
DATA.pairwise.2 <- mutate(pairwise.2.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                       paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                       paste(Sex.A, Sex.B) == "Male Female" ~ "male-female",
                                                                       paste(Sex.A, Sex.B) == "Female Male" ~ "male-female"))
# adding column to indicate which sexes that are being compared
DATA.pairwise.2 # to check if matrix is good, has all the correct columns, variables etc. ie. no NA values

# removing subadults from dataframe matrix
pairwise.2.df.A <- DATA.pairwise.2[which(DATA.pairwise.2$Age.A != "Subadult"),] # removing subadults from anteater_A from matrix
DATA.pairwise.2.adult <- pairwise.2.df.A[which(pairwise.2.df.A$Age.B != "Subadult"),] # removing subadults from anteater_B from matrix with anteater_A filtered

# Plot pairwise sex comparison for SITE 2
## SITE 2 plot pairwise comparison (male and female)
ggplot(data = DATA.pairwise.2, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 2: male and female)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.2.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## SITE 2 plot pairwise comparison (adults only)
ggplot(data = DATA.pairwise.2.adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 2: adult only)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.2.adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 3 ----
# taking the overlap cube, extracting median layer, removing one portion of the triangle
overlap.3 <- overlap(object = AKDE.3, level = 0.95) # assigns the overlap results as an object
overlap.3.median <- overlap.3$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
overlap.3.median[upper.tri(overlap.3.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the matrix
#View(overlap.3.median) # to check everything is correct (full-screen console for best results)

# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.3.matrix <- as.data.frame(overlap.3.median) # Convert matrix to data frame
pairwise.3.matrix$anteater_A <- rownames(pairwise.3.matrix) # add column of individual names
pairwise.3.pivot <- pivot_longer(pairwise.3.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
# table is too wide .-. rotate it to make it long

# add columns to the dataframe matrix, general syntax -> join_type(firstTable, secondTable, by=columnTojoinOn)
pairwise.3.pivot.A <- left_join(pairwise.3.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
# adding anteater_A info to matrix
pairwise.3.pivot.B <- left_join(pairwise.3.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding anteater_B info to matrix with anteater_A info
DATA.pairwise.3 <- mutate(pairwise.3.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                         paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                         paste(Sex.A, Sex.B) == "Male Female" ~ "male-female",
                                                                         paste(Sex.A, Sex.B) == "Female Male" ~ "male-female"))
# adding column to indicate which sexes that are being compared
DATA.pairwise.3 # to check if matrix is good, has all the correct columns, variables etc. ie. no NA values

# Plot pairwise sex comparison for SITE 3 ----

## SITE 3 plot pairwise comparison (male and female)
ggplot(data = DATA.pairwise.3, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 3: male and female)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.3.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

# PAIRWISE COMPARISON ANALYSIS OF SEX OVERALL COMPARISON ## ----
# use bind_rows() to join the 2 pairwise comparison and then plot it

# male and female
DATA.pairwise <- bind_rows(DATA.pairwise.1, DATA.pairwise.2, DATA.pairwise.3)
# Adult only
DATA.pairwise.adult <- bind_rows(DATA.pairwise.1.adult, DATA.pairwise.2.adult, DATA.pairwise.3)

# NOTE: pairwise coding comprehension from Stefano -> re-coded the pipe version

# Plot Pairwise Comparison SITE 1, 2, 3 ----
# Plot combined SITE 1, 2, 3 pairwise analysis (male and female)
ggplot(data = DATA.pairwise, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  ggtitle("Anteater overlap pairwise comparison of sexes (SITE 1-3: male and female)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.combined.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

# Plot combined SITE 1, 2, 3 pairwise analysis (Adult only)
ggplot(data = DATA.pairwise.adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  ggtitle("Anteater overlap pairwise comparison of sexes (SITE 1-3: adult only)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.combined.adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#ERROR ############# Quick test to see if differences are significant -------
test.sex <- glmer(overlap ~ sex_comparison + (1|Sex.A), family = "binomial", data = DATA.pairwise.adult)
summary(test.sex)
  # male-female, pvalue = 0.1716
  # male-male, pvalue = 1.00
  # results indicating there is no difference across the groups, therefore individuals are doing their own thing
  # if there was a difference across the groups, then individuals are not doing their own thing (and not independent of each other????)

### PROXIMITY PAIRWISE ANALYSIS BETWEEN SEX ## ----
# refer to help("proximity")

################## 

# combining the fitted models for both sites so you don't have to do the proximity looping test twice 
# FIT.ALL <- c(FIT.1, FIT.2)
# ***** splitting analysis -> doing each site one at a time ********

################

### PROXIMITY ANALYSIS BETWEEN SEX FOR SITE 1 ----

# create empty columns for where the result information will be added/filled into
DATA.pairwise.1$proximity_low <- NA
DATA.pairwise.1$proximity_est <- NA
DATA.pairwise.1$proximity_high <- NA

# this will take a while, days to loop, if R crashes, change the # for the loop number it was on. syntax: for(i in #:nrow(pairwise.df)) 
# Using tryCatch
for(i in 1:nrow(DATA.pairwise.1)){
  ANIMAL_A <- as.character(DATA.pairwise.1[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(DATA.pairwise.1[i, 'anteater_B'])
  TRACKING_DATA.1 <- DATA[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
    # line above is using as.character removes all the fluff because you just want the text string
  MODELS.1 <- list(FIT.1[ANIMAL_A][[1]], FIT.1[ANIMAL_B][[1]])
  PROXIMITY.1 <- tryCatch(
    {
      PROXIMITY.1 <- proximity(data = TRACKING_DATA.1, CTMM = MODELS.1, GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY.1 <- c(NA,NA,NA)
      return(PROXIMITY.1)
    }
  )
  DATA.pairwise.1[i, c("proximity_low")] <- PROXIMITY.1[1]
  DATA.pairwise.1[i, c("proximity_est")] <- PROXIMITY.1[2]
  DATA.pairwise.1[i, c("proximity_high")] <- PROXIMITY.1[3]
  write.csv(DATA.pairwise.1, "C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/R working directory/DATA.proximity.1.csv", row.names = FALSE)
}

# Load Proximity Analysis results for SITE 1
DATA.proximity.1 <- read.csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.1.csv")

# Plot Proximity Analysis between sex (SITE 1)
FIG.proximity.1 <- 
  ggplot(data = DATA.proximity.1, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), size = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#d1495b", "#009E73", "#0072B2"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home range overlap") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.3),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
  # original legend position = c(0.8,0.8)
ggsave(FIG.proximity.1,
       width = 3.23,height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="Proximity_site_1.png")

### PROXIMITY ANALYSIS BETWEEN SEX FOR SITE 2 ----
# create empty columns for where the result information will be added/filled into
DATA.pairwise.2$proximity_low <- NA
DATA.pairwise.2$proximity_est <- NA
DATA.pairwise.2$proximity_high <- NA

# this will take a while, days to loop, if R crashes, change the # for the loop number it was on. syntax: for(i in #:nrow(pairwise.df)) 
# Using tryCatch
for(i in 1:nrow(DATA.pairwise.2)){
  ANIMAL_A <- as.character(DATA.pairwise.2[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(DATA.pairwise.2[i, 'anteater_B'])
  TRACKING_DATA.2 <- DATA[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  # line above is using as.character removes all the fluff because you just want the text string
  MODELS.2 <- list(FIT.2[ANIMAL_A][[1]], FIT.2[ANIMAL_B][[1]])
  PROXIMITY.2 <- tryCatch(
    {
      PROXIMITY.2 <- proximity(data = TRACKING_DATA.2, CTMM = MODELS.2, GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY.2 <- c(NA,NA,NA)
      return(PROXIMITY.2)
    }
  )
  DATA.pairwise.2[i, c("proximity_low")] <- PROXIMITY.2[1]
  DATA.pairwise.2[i, c("proximity_est")] <- PROXIMITY.2[2]
  DATA.pairwise.2[i, c("proximity_high")] <- PROXIMITY.2[3]
  write.csv(DATA.pairwise.2, "C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.2.csv", row.names = FALSE)
}

# Load Proximity Analysis results for SITE 2
DATA.proximity.2 <- read.csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.2.csv")

# Plot Proximity Analysis between sex ----
FIG.proximity.2 <- 
  ggplot(data = DATA.proximity.2, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), size = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#d1495b", "#009E73", "#0072B2"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home range overlap") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.8),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
FIG.proximity.2
ggsave(FIG.proximity.2,
       width = 3.23, height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="Proximity_site_2.png")

### PROXIMITY ANALYSIS BETWEEN SEX FOR SITE 3 ----
# create empty columns for where the result information will be added/filled into
DATA.pairwise.3$proximity_low <- NA
DATA.pairwise.3$proximity_est <- NA
DATA.pairwise.3$proximity_high <- NA

# this will take a while, days to loop, if R crashes, change the # for the loop number it was on. syntax: for(i in #:nrow(pairwise.df)) 
# Using tryCatch
for(i in 1:nrow(DATA.pairwise.3)){
  ANIMAL_A <- as.character(DATA.pairwise.3[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(DATA.pairwise.3[i, 'anteater_B'])
  TRACKING_DATA.3 <- DATA[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  # line above is using as.character removes all the fluff because you just want the text string
  MODELS.3 <- list(FIT.3[ANIMAL_A][[1]], FIT.3[ANIMAL_B][[1]])
  PROXIMITY.3 <- tryCatch(
    {
      PROXIMITY.3 <- proximity(data = TRACKING_DATA.3, CTMM = MODELS.3, GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY.3 <- c(NA,NA,NA)
      return(PROXIMITY.3)
    }
  )
  DATA.pairwise.3[i, c("proximity_low")] <- PROXIMITY.3[1]
  DATA.pairwise.3[i, c("proximity_est")] <- PROXIMITY.3[2]
  DATA.pairwise.3[i, c("proximity_high")] <- PROXIMITY.3[3]
  write.csv(DATA.pairwise.3, "C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/R working directory/DATA.proximity.3.csv", row.names = FALSE)
}

# Load Proximity Analysis results for SITE 3
DATA.proximity.3 <- read.csv("C:/Users/achhen/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Github/giantanteater/data/DATA.proximity.3.csv")

# Plot Proximity Analysis between sex ----
FIG.proximity.3 <- 
  ggplot(data = DATA.pairwise.3, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), size = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#d1495b", "#009E73", "#0072B2"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home range overlap") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.8),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(FIG.proximity.3,
       width = 3.23, height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="Proximity_site_3.png")

# IDENTIFY PAIRS THAT WERE CLOSER/FURTHER TO EACH OTHER ----
# To do this, subset proximity analysis results for proximity values above and below 1
# These individuals displayed distances that were further or closer (.-. above or below 1)
  # general syntax for at or above 1: dataset[which(dataset$est_lower >= 1),]
  # general syntax for at or below 1: dataset[which(dataset$est_higher >= 1),]

# SITE 1
proximity.1.above1 <- DATA.proximity.1[which(DATA.proximity.1$proximity_low > 1),] # 1 pair
proximity.1.below1 <- DATA.proximity.1[which(DATA.proximity.1$proximity_high < 1),] # 6 pairs
proximity.1.above1
proximity.1.below1

# SITE 2
proximity.2.above1 <- DATA.proximity.2[which(DATA.proximity.2$proximity_low > 1),] # none
proximity.2.below1 <- DATA.proximity.2[which(DATA.proximity.2$proximity_high < 1),] # 4 pairs
proximity.2.above1
proximity.2.below1

# SITE 3
proximity.3.above1 <- DATA.proximity.3[which(DATA.proximity.3$proximity_low > 1),] # none
proximity.3.below1 <- DATA.proximity.3[which(DATA.proximity.3$proximity_high < 1),] # 1 pair
proximity.3.above1
proximity.3.below1

## SUBSET INDIVIDUALS FOR PROXIMITY IDENTIFIED PAIRS

# SITE 1 INDIVIDUALS
Kyle <- DATA$Kyle
Christoffer <- DATA$Christoffer
Elaine <- DATA$Elaine
Bumpus <- DATA$Bumpus
Little_rick <- DATA$`Little Rick`
Makao <- DATA$Makao
Puji <- DATA$Puji
Rodolfo <- DATA$Rodolfo

# SITE 2 INDIVIDUALS
Larry <- DATA$`Larry 267`
Annie <- DATA$Annie
Reid <- DATA$Reid
Thomas <- DATA$Thomas
Margaret <- DATA$Margaret

# SITE 3 INDIVIDUALS
Sheron <- DATA$Sheron
Maria <- DATA$Maria

#### PROXIMITY METRIC MEASUREMENTS OF IDENTIFIED PAIRS
# distances() function further estimates the instantaneous distances between individuals
# Use telemetry data
# 'DATA' needs to contain the pair, 2 individuals being compared
metric <- distances(DATA,FITS)
metric # distance measurement and time between the pair
names(metric)
plot(log(est)~timestamp, data=metric, type="l") # type="l" changes the plot from dots to a line


# SITE 1
# above 1 (further)
pair1 <- DATA[c(12,24)]
metric1 <- distances(pair1, FIT.1)

# below 1 (closer)


# SITE 2

# below 1 (closer)

# SITE 3

# below 1 (closer)



### PREP PROXIMITY RESULTS FOR CORRMOVE ANALYSIS ----
# clean up data because there are duplicate values
  # no more duplicated values
#DATA.proximity.1 <- DATA.proximity.1[!duplicated(DATA.proximity.1[,c(1,2)]),]
#DATA.proximity.2 <- DATA.proximity.2[!duplicated(DATA.proximity.2[,c(1,2)]),]
#DATA.proximity.3 <- DATA.proximity.3[!duplicated(DATA.proximity.3[,c(1,2)]),]

# Create table that has 3 columns to compare time and space ie. at Time x where was anteater_A and anteater_B
  # Column 1: Time that spans first GPS time point to last GPS time point
  # Column 2: anteater_A location ie. GPS coordinate
  # Column 3: anteater_B location ie. GPS coordinate
  # refer to whiteboard photo for this visualization

# FIND THE TIME STAMPS OF PAIR INDIVIDUALS (ie. Column 1: Time)
  # so they align/match up in time to figure out where they were in space
  # general syntax: seq(from, to, by, length.out, along.with)
  # seq(t1, t2, by = 20 minutes)
# Create time column
anteater.time <- seq(from=as.POSIXct("2017-07-05"), to=as.POSIXct("2019-09-05"), by='20 mins')

# PREDICT THE LOCATION OF THE PAIR INDIVIDUALS (filling in Column 2 and 3)
predict.Alexander <- predict(DATA[[1]], CTMM=FIT.1[[1]], t = anteater.time)
predict.Bumpus <- predict(DATA[[5]], CTMM=FIT.1[[3]], t = anteater.time)
predict.Christoffer <- predict(DATA[[7]], CTMM=FIT.[[7]], t = anteater.time)
predict.Elaine <- predict(DATA[[8]], CTMM=FIT.ALL[[8]], t = anteater.time)
predict.Kyle <- predict(DATA[[11]], CTMM=FIT.ALL[[11]], t = anteater.time)
predict.LittleRick <- predict(DATA[[13]], CTMM=FIT.ALL[[13]], t = anteater.time)
predict.Makao <- predict(DATA[[14]], CTMM=FIT.ALL[[14]], t = anteater.time)
predict.Puji <- predict(DATA[[16]], CTMM=FIT.ALL[[16]], t = anteater.time)
predict.Rodolfo <- predict(DATA[[18]], CTMM=FIT.ALL[[18]], t = anteater.time)
# [[#]] indicates the animal number

saveRDS(object = predict.Alexander, file = "predict.Alexander.RDS")
saveRDS(object = predict.Bumpus, file = "predict.Bumpus.RDS")
saveRDS(object = predict.Christoffer, file = "predict.Christoffer.RDS")
saveRDS(object = predict.Elaine, file = "predict.Elaine.RDS")
saveRDS(object = predict.Kyle, file = "predict.Kyle.RDS")
saveRDS(object = predict.LittleRick, file = "predict.LittleRick.RDS")
saveRDS(object = predict.Makao, file = "predict.Makao.RDS")
saveRDS(object = predict.Puji, file = "predict.Puji.RDS")
saveRDS(object = predict.Rodolfo, file = "predict.Rodolfo.RDS")

predict.Alexander <- readRDS("predict.Alexander.RDS")
predict.Bumpus <- readRDS("predict.Bumpus.RDS")
predict.Christoffer <- readRDS("predict.Christoffer.RDS")
predict.Elaine <- readRDS("predict.Elaine.RDS")
predict.Kyle <- readRDS("predict.Kyle.RDS")
predict.LittleRick <- readRDS("predict.LittleRick.RDS")
predict.Makao <- readRDS("predict.Makao.RDS")
predict.Puji <- readRDS("predict.Puji.RDS")
predict.Rodolfo <- readRDS("predict.Rodolfo.RDS")

### CORRELATIVE MOVEMENT ----
## DETERMINE IF THE MOVEMENTS ARE CORRELATED
#Help files available for main functions in corrMove
#?as.corrData
#?findPrts
#?corrMove
#?plot.corrMove

#findPrts() #Error with -> Estimate the partition points for the anteater data
#Error with dICold > dICnew
# getIC() function not working -> from CompR -> CompR package installed
# Error related to duplicate timestamps?

##################
#Extract some test individuals and do some data carpentry
Elaine <- DATA$Elaine #data$Elaine
Christoffer <- DATA$Christoffer # data$Christoffer
plot(list(Elaine, Christoffer), col = c("red", "blue"))
El <- data.frame(timestamp = round_date(Elaine$timestamp, "20 minutes") ,
                 E.x = Elaine$longitude,
                 E.y = Elaine$latitude)
Chris <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                    S.x = Christoffer$longitude,
                    S.y = Christoffer$latitude)
test <- merge(El, Chris)
test <- test[, c(1,2,4,3,5)]
test <- test[!duplicated(test$timestamp),]
#Create corrData object.
cdAnteater <- as.corrData(test)
#Estimate the partition points for the khulan data, with W=25
prtsAnteater <- findPrts(cdAnteater, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater <- corrMove(cdAnteater, prtsAnteater)
#3-panel plot of the MCIs over time
plot.corrMove(cmAnteater)
title("Elaine and Christoffer")

###########################














# ### CORRELATIVE MOVEMENT (above) -----
# pair 1a: elaine/alexander

Elaine <- DATA$Elaine #data$Elaine
Christoffer <- DATA$Christoffer # data$Christoffer
plot(list(Elaine, Christoffer), col = c("red", "blue"))
El <- data.frame(timestamp = round_date(Elaine$timestamp, "20 minutes") ,
                 E.x = Elaine$longitude,
                 E.y = Elaine$latitude)
Chris <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                    S.x = Christoffer$longitude,
                    S.y = Christoffer$latitude)
test <- merge(El, Chris)
test <- test[, c(1,2,4,3,5)]
test <- test[!duplicated(test$timestamp),]
#Create corrData object.
cdAnteater <- as.corrData(test)
#Estimate the partition points for the khulan data, with W=25
prtsAnteater <- findPrts(cdAnteater, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater <- corrMove(cdAnteater, prtsAnteater)
#3-panel plot of the MCIs over time
plot.corrMove(cmAnteater)
title("Elaine and Christoffer")



# pair 2a: kyle/christoffer
plot(list(Kyle, Christoffer), col = c("red", "blue"))
Ky <- data.frame(timestamp = round_date(Kyle$timestamp, "20 minutes") ,
                 E.x = Elaine$longitude,
                 E.y = Elaine$latitude)
Chris <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                    S.x = Christoffer$longitude,
                    S.y = Christoffer$latitude)
test.pair2a <- merge(Ky, Chris)
test.pair2a <- test[, c(1,2,4,3,5)]
test.pair2a <- test[!duplicated(test.pair2a$timestamp),]
#Create corrData object.
cdAnteater.pair2a <- as.corrData(test)
#Estimate the partition points for the khulan data, with W=25
prtsAnteater.pair2a <- findPrts(cdAnteater.pair2a, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater.pair2a <- corrMove(cdAnteater.pair2a, prtsAnteater.pair2a)
#3-panel plot of the MCIs over time
plot.corrMove(cmAnteater.pair2a)
title("Kyle and Christoffer")

# pair 3a: little rick/elaine

# pair 1b: elaine/christoffer
# pair 2b: kyle/bumpus
# pair 3b: kyle/elaine
# pair 4b: little rick/elaine
# pair 5b: makao/bumpus
# pair 6b: puji/bumpus
# pair 7b: puji/elaine
# pair 8b: rodolfo/elaine




















