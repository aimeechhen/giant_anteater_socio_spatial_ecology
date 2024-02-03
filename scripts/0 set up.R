
# Set up

#............................................................
# Load packages ----
#............................................................

#data, visualization
library(readr)
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
# #devtools::install_github("ctmm-initiative/ctmm", force = TRUE) 
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

getwd()

#load data
GPS_df <- readRDS("data/rds/GPS_df.rds")
bio_df <- readRDS("data/rds/bio_df.rds")
DATA_TELEMETRY <- readRDS("data/rds/DATA_TELEMETRY.rds")

#load movement models
FIT <- readRDS("data/FIT.rds")

#load home range estimates
AKDE <- readRDS("data/AKDE.rds")
AKDE_1 <- readRDS("rds/AKDE_1.rds")
AKDE_2 <- readRDS("rds/AKDE_2.rds")
AKDE_male <- readRDS("rds/AKDE_male.rds")
AKDE_female <- readRDS("rds/AKDE_female.rds")

#load home range size
HR_size <- readRDS("rds/HR_size.rds")

#load overlap estimates
overlap_1_df <- readRDS("rds/overlap_1_df.rds")
overlap_2_df <- readRDS("rds/overlap_2_df.rds")
overlap_df <- readRDS("rds/overlap_df.rds")

#load proximity data
proximity_df <- readRDS("rds/proximity_df.rds")

#load distance data
distance_df <- readRDS("data/rds/distance_df.rds")

#load pair data
proximity_identified_pairs_df <- readRDS("RDS/proximity_identified_pairs_df.rds")
distance_pairs_df <- readRDS("data/rds/distance_pairs_df.RDS")

#load correlative movement data
cm_pair1 <- readRDS("RDS/cm_pair1.RDS")
cm_pair2 <- readRDS("RDS/cm_pair2.RDS")
cm_pair3 <- readRDS("RDS/cm_pair3.RDS")
cm_pair4 <- readRDS("RDS/cm_pair4.RDS")
cm_pair5 <- readRDS("RDS/cm_pair5.RDS")
cm_pair6 <- readRDS("RDS/cm_pair6.RDS")
cm_pair7 <- readRDS("RDS/cm_pair7.RDS")
cm_pair8 <- readRDS("RDS/cm_pair8.RDS")
cm_pair9 <- readRDS("RDS/cm_pair9.RDS")
cm_pair10 <- readRDS("RDS/cm_pair10.RDS")
cm_pair11 <- readRDS("RDS/cm_pair11.RDS")
cm_pair12 <- readRDS("RDS/cm_pair12.RDS")

#............................................................
# End
#............................................................

#check versions
packageVersion()
sessionInfo()
