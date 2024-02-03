
#............................................................
# Load packages ----
#............................................................
#data, visualization
library(readr)
library(ggplot2)
library(khroma)          #colour blind friendly colour palette
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
library(tibble)          #data wrangling
library(lubridate)       #round_date() for corrMove
library(geobr)           #shape files for Brazil
library(gridExtra)       #arrangement of plots for multi-panel figures
#analysis
library(devtools)
#devtools::install_github("ctmm-initiative/ctmm", force = TRUE) #if package needs to be updated
#devtools::install_github("jmcalabrese/corrMove", force = TRUE) #if installing for the first time
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
library(glmmTMB)         #beta distribution
library(corrMove)        #correlative movement

#............................................................
# Data ----
#............................................................

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

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
# Correlative movement of identified pairs----
#............................................................

png(file = "figures/correlative_movement/cm_pair1.png", width = 6.86, height = 6.86, units = "in", res = 600)
plot.corrMove(cm_pair1)
title("Pair 1: Kyle and Christoffer")
dev.off()

png(file = "figures/correlative_movement/cm_pair2.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair2)
title("Pair 2: Elaine and Christoffer")
dev.off()

png(file = "figures/correlative_movement/cm_pair3.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair3)
title("Pair 3: Kyle and Bumpus")
dev.off()

png(file = "figures/correlative_movement/cm_pair4.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair4)
title("Pair 4: Little Rick and Elaine")
dev.off()

png(file = "figures/correlative_movement/cm_pair5.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair5)
title("Pair 5: Makao and Bumpus")
dev.off()

png(file = "figures/correlative_movement/cm_pair6.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair6)
title("Pair 6: Puji and Bumpus")
dev.off()

png(file = "figures/correlative_movement/cm_pair8.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair8)
title("Pair 8: Larry and Annie")
dev.off()

png(file = "figures/correlative_movement/cm_pair9.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair9)
title("Pair 9: Reid and Larry")
dev.off()

png(file = "figures/correlative_movement/cm_pair10.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair10)
title("Pair 10: Sheron and Maria")
dev.off()

png(file = "figures/correlative_movement/cm_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair11)
title("Pair 11: Thomas and Margaret")
dev.off()

png(file = "figures/correlative_movement/cm_pair12.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair12)
title("Pair 12: Thomas and Reid")
dev.off()









