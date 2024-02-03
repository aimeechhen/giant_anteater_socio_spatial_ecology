
# Encounters

#............................................................
# Estimating distance ----
#............................................................

#Calculate the distance statistics
proximity_df$distance_low <- NA
proximity_df$distance_est <- NA
proximity_df$distance_high <- NA

RES <- list()

for (i in 1:nrow(overlap_df)) {
  ANIMAL_A <- as.character(overlap_df[i, 'anteater_A']) 
  ANIMAL_B <- as.character(overlap_df[i, 'anteater_B'])
  TRACKING_DATA <- DATA_TELEMETRY[c(ANIMAL_A, ANIMAL_B)]
  MODELS <- list(FIT[[ANIMAL_A]], FIT[[ANIMAL_B]])
  
  DISTANCES_RES <- tryCatch({
    distances_result <- distances(data = TRACKING_DATA, CTMM = MODELS, GUESS = ctmm(error = FALSE))
    data.frame(pair_ID = paste(ANIMAL_A, ANIMAL_B, sep = "_"),
               distance_low = distances_result$low, 
               distance_est = distances_result$est, 
               distance_high = distances_result$high,
               t = distances_result$t,
               timestamp = distances_result$timestamp)
  }, error = function(err) {
    data.frame(pair_ID = paste(ANIMAL_A, ANIMAL_B, sep = "_"),
               distance_low = NA,
               distance_est = NA,
               distance_high = NA,
               t = NA, 
               timestamp = NA)
  })
  
  RES[[i]] <- DISTANCES_RES
  
  #write.csv(RES, "data/DATA_distance.csv", row.names = FALSE)
  cat("finished index", i, "\n")
}

#Turn the list of list into a data frame
DATA_DISTANCE <- do.call(rbind, RES)

#save distance data
saveRDS(DATA_DISTANCE, file = "data/rds/DATA_DISTANCE.rds")

#............................................................

#import the distance data
DATA_DISTANCE <- readRDS("data/rds/DATA_DISTANCE.rds")

#locate NA values within the dataframe
DATA_DISTANCE[!complete.cases(DATA_DISTANCE), ] #3,502,701 observations
#drop the 3 fixes that had no distance values 
DATA_DISTANCE <- na.omit(DATA_DISTANCE) #3,502,698 observations

#add overlap and proximity information to the distance dataframe
distance_df <- merge(DATA_DISTANCE, proximity_df, by = "pair_ID")
distance_df <- relocate(distance_df, c(distance_low, distance_est, distance_high,
                                       t, timestamp), .after = proximity_high)

#save the distance dataframe
saveRDS(distance_df, file = "data/rds/distance_df.rds")

#............................................................
# Sensitivity analysis ----
#............................................................

#set encounter radius
#larger the radius = more encounters can occur within that radius due to more individuals that can be within the radius (measurements are in meters)
enc_radius <- 0:1000
enc_count <- vector("numeric", length(enc_radius))

#calculate the number of encounters occurring within each radius size
for(i in 1:length(enc_radius)){
  enc_count[i] <- sum(distance_df$distance_est < enc_radius[i])
}

#visualization
plot(x = enc_radius, y = enc_count, type = "l")

#.....

#sensitivity analysis on female-male encounter significance
encounter_radius_pvalue <- vector("numeric", length(enc_radius))
pair_ID <- unique(overlap_df$pair_ID)

#Loop over encounter radii
for(i in 1:length(enc_radius)){
  
  res <- list()
  
  for (j in pair_ID){
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

#save dataframe
saveRDS(encounter_radius_df, file = "RDS/encounter_radius_df.RDS")

#visualization
plot(y ~ x,
     data = encounter_radius_df,
     type = "l",
     xlab = "Encounter radius (m)",
     ylab = "p-value")
abline(0.05, 0)


#............................................................
# Estimating encounters ----
#............................................................

#calculate total encounters of all individuals based on sex comparison type
proximity_df$encounter_count <- NA
pair_ID <- unique(proximity_df$pair_ID)

for (i in pair_ID){
  subset_A <- distance_df[distance_df$pair_ID == i,]
  
  # Count the number of times "distance_est" is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  
  #save results
  proximity_df[proximity_df$pair_ID == i, "encounter_count"] <- encounter_count
  
}

#number of pairs that had 0 encounters
proximity_df[proximity_df$encounter_count == 0,] #78
#number of pairs that had at least 1 encounter
proximity_df[proximity_df$encounter_count != 0,] #43

#calculate the number of encounters based on threshold
sum(proximity_df$encounter_count)
sum(proximity_df$encounter_count[proximity_df$sex_comparison == "male-male"])
sum(proximity_df$encounter_count[proximity_df$sex_comparison == "female-female"])
sum(proximity_df$encounter_count[proximity_df$sex_comparison == "female-male"])

#............................................................
# Encounter results ----
#............................................................

#effect of sex and overlap on encounter rates (model that does not include 0 encounter counts)
encounter_test <- glmer(encounter_count ~ overlap_est + sex_comparison + (1|site), family = poisson(link = "log"), data = proximity_df, subset = encounter_count > 0)
encounter_test2 <- glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = proximity_df, subset = encounter_count > 0)
encounter_test_results <- anova(encounter_test, encounter_test2)
encounter_test_pvalue <- round(encounter_test_results$`Pr(>Chisq)`[2], 2)

# amount of home-range overlap and the number of observed encounters (beta (B) = 4.86 ± 0.148, p = 0.00)
summary(encounter_test)

