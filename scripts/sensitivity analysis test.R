

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
library(scales)          #scaling axis in plots
#analysis
library(devtools)
#devtools::install_github("ctmm-initiative/ctmm", force = TRUE) #if package needs to be updated
#devtools::install_github("jmcalabrese/corrMove", force = TRUE) #if installing for the first time
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
library(glmmTMB)         #beta distribution
library(mgcv)            #gam() for encounters
library(corrMove)        #correlative movement

#............................................................
# Data ----
#............................................................

# Set working directory
setwd("C:/Users/achhen/Documents/GitHub/Giant_Anteater")

#import data, cleaned GPS giant anteater data
DATA_GPS <- readRDS("RDS/DATA_GPS.RDS")
DATA_TELEMETRY <- readRDS("RDS/DATA_TELEMETRY.RDS")
DATA_META <- readRDS("RDS/DATA_META.RDS")
DATA_BIO <- readRDS("RDS/DATA_BIO.RDS")

#............................................................
# Load RDS ----
#............................................................

FIT <- readRDS("RDS/FIT.RDS")
AKDE <- readRDS("RDS/AKDE.RDS")
overlap_df <- readRDS("RDS/overlap_df.RDS")
proximity_df <- readRDS("RDS/proximity_df.RDS")
proximity_identified_pairs_df <- readRDS("RDS/proximity_identified_pairs_df.RDS")
#add proximity ratio data to home-range overlap dataframe
overlap_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                         "Sex.A", "Sex.B",
                                                         "Age.A", "Age.B",
                                                         "sex_comparison",
                                                         "site"))
distance_df <- readRDS("RDS/distance_df.RDS")

# #locate NA values within the dataframe
distance_df[!complete.cases(distance_df), ] #3,502,701 observations
#drop the 3 fixes that had no distance values 
distance_df <- na.omit(distance_df) #3,502,698 observations

#add supplementary info to distance data from the home range overlap dataframe
distance_df <- merge(distance_df, overlap_df, by = "pair_ID")
distance_df <- relocate(distance_df, c(distance_low, distance_est, distance_high,
                                       t, timestamp), .after = proximity_high)

#............................................................
# Sensitivity Analysis ----
#............................................................
### Sensitivity Analysis ----
# Calculate the distance threshold to be used as an encounter event

#set encounter radius
#larger the radius = more encounters can occur within that radius due to more individuals that can be within the radius (measurements are in meters)
enc_radius <- 0:1000
enc_count <- vector("numeric", length(enc_radius))

#calculate the number of encounters occurring within each radius size
for(i in 1:length(enc_radius)){
  enc_count[i] <- sum(distance_df$distance_est < enc_radius[i])
}

#to be ggplottified
plot(x = enc_radius, y = enc_count, type = "l")

#sensitivity analysis on male - female encounter significance
encounter_radius_pvalue <- vector("numeric", length(enc_radius))
identified_pairs <- unique(overlap_df$pair_ID)

#Loop over encounter radii
for(i in 1:length(enc_radius)){
  
  res <- list()
  
  for (j in identified_pairs){
    subset_A <- distance_df[distance_df$pair_ID == j,]
    
    # Count the number of times "distance_est" is below some threshold distance i 
    encounter_count <- sum(subset_A$distance_est < enc_radius[i])
    
    #save results
    res[[j]] <- data.frame(encounter_count = encounter_count,
                           overlap_est = subset_A$overlap_est[1],
                           sex_comparison = subset_A$sex_comparison[1],
                           site = subset_A$site[1])
    
  }
  
  res <- do.call(rbind, res)
  encounter_radius_test <- try(glmer(encounter_count ~ overlap_est + sex_comparison + (1|site),
                                     family = poisson(link = "log"), data = res, subset = res > 0))
  encounter_radius_test2 <- try(glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = res, subset = res > 0))
  encounter_radius_test_results <- try(anova(encounter_radius_test, encounter_radius_test2))
  p_val <- try(encounter_radius_test_results$`Pr(>Chisq)`[2])
  encounter_radius_pvalue[i] <- ifelse(class(p_val) == "try-error", NA, p_val)
  
  cat("finished index", i, "\n")
}

encounter_radius_df <- data.frame(x = enc_radius,
                                  y = encounter_radius_pvalue)
saveRDS(encounter_radius_df, file = "RDS/encounter_radius_df.RDS")

plot(y ~ x,
     data = encounter_radius_df,
     type = "l",
     xlab = "Encounter radius (m)",
     ylab = "p-value")
abline(0.05, 0)

