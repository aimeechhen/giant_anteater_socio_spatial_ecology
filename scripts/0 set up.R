
# Set up

#............................................................
# Load packages ----
#............................................................

#data, visualization
library(ggplot2)
# #library(khroma)          #colour blind friendly colour palette
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
# library(tibble)          #data wrangling
library(lubridate)       #yday() for timestamp; round_date() for corrMove
# #library(parsedate)
library(gridExtra)       #arrangement of plots for multi-panel figures
library(egg)             #arrangement of plots for multi-panel figures
library(scales)          #scaling axis in plots
library(osmdata)         #open street map for base map
library(terra)           #raster data
library(tidyterra)       #raster data
library(sf)              #spatial data
# #library(geobr)           #shape files for Brazil
# #analysis
# library(devtools)
# #if the ctmm package needs to be updated
devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
remotes::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
library(glmmTMB)         #beta distribution
# library(mgcv)            #gam() for encounters
# #if installing the corrMove package for the first time
# #devtools::install_github("jmcalabrese/corrMove", force = TRUE) 
library(corrMove)        #correlative movement

#............................................................
# Data ----
#............................................................

#anteater data
load("data/anteater/anteater_data.rda")
load("data/anteater/bio_data.rda")
load("data/anteater/telemetry_data.rda")

#movement models and home range estimate
load("data/anteater_fit.rda")
load("data/anteater_akdes.rda")

#home range
load("data/home_range/HR_size.rda")
load("data/home_range/AKDE_1.rda")
load("data/home_range/AKDE_2.rda")
load("data/home_range/overlap_1_df.rda")
load("data/home_range/overlap_2_df.rda")
load("data/home_range/overlap_df.rda")

#encounters
load("data/encounter/proximity_df.rda")
load("data/encounter/distance_df.rda")
encounter_radius_df <- readRDS("data/encounter/encounter_radius_df.RDS")
load("data/encounter/proximity_identified_pairs_df.rda")
distance_pairs_df <- readRDS("data/encounter/distance_pairs_df.RDS")

#correlative movement data
cm_pair1 <- readRDS("data/correlative_movement/cm_pair1.RDS")
cm_pair2 <- readRDS("data/correlative_movement/cm_pair2.RDS")
cm_pair3 <- readRDS("data/correlative_movement/cm_pair3.RDS")
cm_pair4 <- readRDS("data/correlative_movement/cm_pair4.RDS")
cm_pair5 <- readRDS("data/correlative_movement/cm_pair5.RDS")
cm_pair6 <- readRDS("data/correlative_movement/cm_pair6.RDS")
cm_pair7 <- readRDS("data/correlative_movement/cm_pair7.RDS")
cm_pair8 <- readRDS("data/correlative_movement/cm_pair8.RDS")
cm_pair9 <- readRDS("data/correlative_movement/cm_pair9.RDS")
cm_pair10 <- readRDS("data/correlative_movement/cm_pair10.RDS")
cm_pair11 <- readRDS("data/correlative_movement/cm_pair11.RDS")
cm_pair12 <- readRDS("data/correlative_movement/cm_pair12.RDS")


#............................................................
# Adults only ----
#............................................................

bio_data <- bio_data[-c(3,12,14,20),]
DATA_TELEMETRY <- DATA_TELEMETRY[-c(3,12,14,20)]
FIT <- FIT[-c(3,12,14,20)]
AKDE <- AKDE[-c(3,12,14,20)]
load("data/home_range/HR_size_adult.rda")
overlap_df <- readRDS("data/home_range/overlap_data_adult.rds")


#............................................................
# End
#............................................................

#check versions
packageVersion()
sessionInfo()
