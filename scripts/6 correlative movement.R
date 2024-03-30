
# Script description: identify individuals of interest, deviated pairs analysis, caluculate distances between individuals in the deviated pairs, investigate correlative movement between those individuals, calculate mean correlative movement statistics

# Correlative movement

#............................................................
# Exploring deviations in proximity ratios
#............................................................

#identify pairs that did not have a proximity ratio of 1
proximity_above1 <- proximity_df[proximity_df$proximity_low > 1,]
proximity_below1 <- proximity_df[proximity_df$proximity_high < 1,]

#exclude pairs with a HR overlap of 0
proximity_below1[proximity_below1$overlap_est < 0.0001,]
proximity_below1 <- proximity_below1[!(proximity_below1$overlap_est < 0.0001),]

#create a dataframe of the deviated pairs
proximity_identified_pairs_df <- rbind(proximity_above1, proximity_below1)
proximity_identified_pairs_df$pair_ID_number <- seq(from = 1, to = 12, by = 1)
proximity_identified_pairs_df <- relocate(proximity_identified_pairs_df, pair_ID_number, .before = anteater_A)

#correct the sex_comparison output to female-male
proximity_identified_pairs_df <- mutate(proximity_identified_pairs_df,
                                        sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                   paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                   paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                                   paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

#clean up environment
rm(proximity_above1, proximity_below1)

#save identified pairs dataframe
# save(proximity_identified_pairs_df, file = "data/encounter/proximity_identified_pairs_df.rda")
load("data/encounter/proximity_identified_pairs_df.rda")

#............................................................
# Deviated pairs results ----
#............................................................

#number of pairs with a deviated proximity ratio based on sex comparison (ie. a proximity ratio value not equal to 1)
table(proximity_identified_pairs_df$sex_comparison)

# Proximity ratio sex analysis for identified pairs
#test for significance in sex, compare model with and without sex as a variable
proximity_test_pairs <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
proximity_test2_pairs <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
proximity_test_results_pairs <- anova(proximity_test_pairs, proximity_test2_pairs)
proximity_test_pvalue_pairs <- round(proximity_test_results_pairs$`Pr(>Chisq)`[2], 2) #0.16

# Proximity and overlap analysis for identified pairs
prox_overlap_test_pairs <- glmer(proximity_est ~ overlap_est + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
prox_overlap_test2_pairs <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
prox_overlap_test_results_pairs <- anova(prox_overlap_test_pairs, prox_overlap_test2_pairs)
prox_overlap_test_pvalue_pairs <- round(prox_overlap_test_results_pairs$`Pr(>Chisq)`[2], 2) #0.65

#............................................................
# Estimating distances of the deviated pairs ----
#............................................................

#subset telemetry data and fitted model for each pair
pair1 <- DATA_TELEMETRY[c("Kyle","Christoffer")]
FIT_pair1 <- FIT[c("Kyle","Christoffer")]

pair2 <- DATA_TELEMETRY[c("Elaine","Christoffer")]
FIT_pair2 <- FIT[c("Elaine","Christoffer")]

pair3 <- DATA_TELEMETRY[c("Kyle","Bumpus")]
FIT_pair3 <- FIT[c("Kyle","Bumpus")]

pair4 <- DATA_TELEMETRY[c("Little_Rick","Elaine")]
FIT_pair4 <- FIT[c("Little_Rick","Elaine")]

pair5 <- DATA_TELEMETRY[c("Makao","Bumpus")]
FIT_pair5 <- FIT[c("Makao","Bumpus")]

pair6 <- DATA_TELEMETRY[c("Puji","Bumpus")]
FIT_pair6 <- FIT[c("Puji","Bumpus")]

pair7 <- DATA_TELEMETRY[c("Rodolfo", "Elaine")]
FIT_pair7 <- FIT[c("Rodolfo", "Elaine")]

pair8 <- DATA_TELEMETRY[c("Larry","Annie")]
FIT_pair8 <- FIT[c("Larry","Annie")]

pair9 <- DATA_TELEMETRY[c("Reid","Larry")]
FIT_pair9 <- FIT[c("Reid","Larry")]

pair10 <- DATA_TELEMETRY[c("Sheron","Maria")]
FIT_pair10 <- FIT[c("Sheron","Maria")]

pair11 <- DATA_TELEMETRY[c("Thomas","Margaret")]
FIT_pair11 <- FIT[c("Thomas","Margaret")]

pair12 <- DATA_TELEMETRY[c("Thomas","Reid")]
FIT_pair12 <- FIT[c("Thomas","Reid")]

#calculate the instantaneous Euclidean distance between each deviated pairs
distance_pair1 <- distances(pair1, FIT_pair1) 
distance_pair2 <- distances(pair2, FIT_pair2)
distance_pair3 <- distances(pair3, FIT_pair3)
distance_pair4 <- distances(pair4, FIT_pair4)
distance_pair5 <- distances(pair5, FIT_pair5)
distance_pair6 <- distances(pair5, FIT_pair5) 
distance_pair7 <- distances(pair7, FIT_pair7)
distance_pair8 <- distances(pair8, FIT_pair8)
distance_pair9 <- distances(pair9, FIT_pair9) 
distance_pair10 <- distances(pair10, FIT_pair10)
distance_pair11 <- distances(pair11, FIT_pair11)
distance_pair12 <- distances(pair12, FIT_pair12)

#add columns and reorganize dataframe
distance_pair1$pair_ID_number <- 1
distance_pair1$anteater_A <- "Kyle"
distance_pair1$anteater_B <- "Christoffer"
distance_pair1$pair_ID <- paste(distance_pair1$anteater_A, distance_pair1$anteater_B, sep = "_")
distance_pair1 <- relocate(distance_pair1, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair2$pair_ID_number <- 2
distance_pair2$anteater_A <- "Elaine"
distance_pair2$anteater_B <- "Christoffer"
distance_pair2$pair_ID <- paste(distance_pair2$anteater_A, distance_pair2$anteater_B, sep = "_")
distance_pair2 <- relocate(distance_pair2, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair3$pair_ID_number <- 3
distance_pair3$anteater_A <- "Kyle"
distance_pair3$anteater_B <- "Bumpus"
distance_pair3$pair_ID <- paste(distance_pair3$anteater_A, distance_pair3$anteater_B, sep = "_")
distance_pair3 <- relocate(distance_pair3, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair4$pair_ID_number <- 4
distance_pair4$anteater_A <- "Little_Rick"
distance_pair4$anteater_B <- "Elaine"
distance_pair4$pair_ID <- paste(distance_pair4$anteater_A, distance_pair4$anteater_B, sep = "_")
distance_pair4 <- relocate(distance_pair4, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair5$pair_ID_number <- 5
distance_pair5$anteater_A <- "Makao"
distance_pair5$anteater_B <- "Bumpus"
distance_pair5$pair_ID <- paste(distance_pair5$anteater_A, distance_pair5$anteater_B, sep = "_")
distance_pair5 <- relocate(distance_pair5, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair6$pair_ID_number <- 6
distance_pair6$anteater_A <- "Puji"
distance_pair6$anteater_B <- "Bumpus"
distance_pair6$pair_ID <- paste(distance_pair6$anteater_A, distance_pair6$anteater_B, sep = "_")
distance_pair6 <- relocate(distance_pair6, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair7$pair_ID_number <- 7
distance_pair7$anteater_A <- "Rodolfo"
distance_pair7$anteater_B <- "Elaine"
distance_pair7$pair_ID <- paste(distance_pair7$anteater_A, distance_pair7$anteater_B, sep = "_")
distance_pair7 <- relocate(distance_pair7, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair8$pair_ID_number <- 8
distance_pair8$anteater_A <- "Larry"
distance_pair8$anteater_B <- "Annie"
distance_pair8$pair_ID <- paste(distance_pair8$anteater_A, distance_pair8$anteater_B, sep = "_")
distance_pair8 <- relocate(distance_pair8, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair9$pair_ID_number <- 9
distance_pair9$anteater_A <- "Reid"
distance_pair9$anteater_B <- "Larry"
distance_pair9$pair_ID <- paste(distance_pair9$anteater_A, distance_pair9$anteater_B, sep = "_")
distance_pair9 <- relocate(distance_pair9, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair10$pair_ID_number <- 10
distance_pair10$anteater_A <- "Sheron"
distance_pair10$anteater_B <- "Maria"
distance_pair10$pair_ID <- paste(distance_pair10$anteater_A, distance_pair10$anteater_B, sep = "_")
distance_pair10 <- relocate(distance_pair10, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair11$pair_ID_number <- 11
distance_pair11$anteater_A <- "Thomas"
distance_pair11$anteater_B <- "Margaret"
distance_pair11$pair_ID <- paste(distance_pair11$anteater_A, distance_pair11$anteater_B, sep = "_")
distance_pair11 <- relocate(distance_pair11, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

distance_pair12$pair_ID_number <- 12
distance_pair12$anteater_A <- "Thomas"
distance_pair12$anteater_B <- "Reid"
distance_pair12$pair_ID <- paste(distance_pair12$anteater_A, distance_pair12$anteater_B, sep = "_")
distance_pair12 <- relocate(distance_pair12, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

#combine into a dataframe
distance_pairs_df <- rbind(distance_pair1, distance_pair2, distance_pair3, distance_pair4,
                           distance_pair5, distance_pair6, distance_pair7, distance_pair8,
                           distance_pair9, distance_pair10, distance_pair11, distance_pair12)

#clean up environment
rm(distance_pair1, distance_pair2, distance_pair3, distance_pair4, distance_pair5,
   distance_pair6, distance_pair7, distance_pair8, distance_pair9, distance_pair10,
   distance_pair11, distance_pair12)

#save the distance dataframe of the deviated pairs
saveRDS(object = distance_pairs_df, file = "data/encounter/distance_pairs_df.rds")
distance_pairs_df <- readRDS("data/encounter/distance_pairs_df.rds")

#............................................................

#subset telemetry data for each individual with a deviated proximity ratio
Bumpus <- DATA_TELEMETRY$Bumpus
Christoffer <- DATA_TELEMETRY$Christoffer
Elaine <- DATA_TELEMETRY$Elaine
Kyle <- DATA_TELEMETRY$Kyle
Little_rick <- DATA_TELEMETRY$Little_Rick
Makao <- DATA_TELEMETRY$Makao
Puji <- DATA_TELEMETRY$Puji
Rodolfo <- DATA_TELEMETRY$Rodolfo
Annie <- DATA_TELEMETRY$Annie
Larry <- DATA_TELEMETRY$Larry
Margaret <- DATA_TELEMETRY$Margaret
Maria <- DATA_TELEMETRY$Maria
Sheron <- DATA_TELEMETRY$Sheron
Reid <- DATA_TELEMETRY$Reid
Thomas <- DATA_TELEMETRY$Thomas

#create a dataframe of an individual's GPS coordinates, format dataset for corrMove analysis
Bumpus_GPS <- data.frame(timestamp = round_date(Bumpus$timestamp, "20 minutes"),
                         Bumpus.x = Bumpus$longitude,
                         Bumpus.y = Bumpus$latitude)
Christoffer_GPS <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                              Christoffer.x = Christoffer$longitude,
                              Christoffer.y = Christoffer$latitude)
Elaine_GPS <- data.frame(timestamp = round_date(Elaine$timestamp, "20 minutes"),
                         Elaine.x = Elaine$longitude,
                         Elaine.y = Elaine$latitude)
Kyle_GPS <- data.frame(timestamp = round_date(Kyle$timestamp, "20 minutes"),
                       Kyle.x = Kyle$longitude,
                       Kyle.y = Kyle$latitude)
Little_rick_GPS <- data.frame(timestamp = round_date(Little_rick$timestamp, "20 minutes"),
                              Little_rick.x = Little_rick$longitude,
                              Little_rick.y = Little_rick$latitude)
Makao_GPS <- data.frame(timestamp = round_date(Makao$timestamp, "20 minutes"),
                        Makao.x = Makao$longitude,
                        Makao.y = Makao$latitude)
Puji_GPS <- data.frame(timestamp = round_date(Puji$timestamp, "20 minutes"),
                       Puji.x = Puji$longitude,
                       Puji.y = Puji$latitude)
Rodolfo_GPS <- data.frame(timestamp = round_date(Rodolfo$timestamp, "20 minutes"),
                          Rodolfo.x = Rodolfo$longitude,
                          Rodolfo.y = Rodolfo$latitude)
Annie_GPS <- data.frame(timestamp = round_date(Annie$timestamp, "20 minutes"),
                        Annie.x = Annie$longitude,
                        Annie.y = Annie$latitude)
Larry_GPS <- data.frame(timestamp = round_date(Larry$timestamp, "20 minutes"),
                        Larry.x = Larry$longitude,
                        Larry.y = Larry$latitude)
Margaret_GPS <- data.frame(timestamp = round_date(Margaret$timestamp, "20 minutes"),
                           Margaret.x = Margaret$longitude,
                           Margaret.y = Margaret$latitude)
Maria_GPS <- data.frame(timestamp = round_date(Maria$timestamp, "20 minutes"),
                        Maria.x = Maria$longitude,
                        Maria.y = Maria$latitude)
Sheron_GPS <- data.frame(timestamp = round_date(Sheron$timestamp, "20 minutes"),
                         Sheron.x = Sheron$longitude,
                         Sheron.y = Sheron$latitude)
Reid_GPS <- data.frame(timestamp = round_date(Reid$timestamp, "20 minutes"),
                       Reid.x = Reid$longitude,
                       Reid.y = Reid$latitude)
Thomas_GPS <- data.frame(timestamp = round_date(Thomas$timestamp, "20 minutes"),
                         Thomas.x = Thomas$longitude,
                         Thomas.y = Thomas$latitude)

#combine the GPS coordinates of pairs together, reorganize dataframe needed for corrMove into format required and remove duplicate timestamps
cd_pair1 <- merge(Kyle_GPS, Christoffer_GPS)
cd_pair1 <- cd_pair1[, c(1,2,4,3,5)]
cd_pair1 <- cd_pair1[!duplicated(cd_pair1$timestamp),]

cd_pair2 <- merge(Elaine_GPS, Christoffer_GPS)
cd_pair2 <- cd_pair2[, c(1,2,4,3,5)]
cd_pair2 <- cd_pair2[!duplicated(cd_pair2$timestamp),]

cd_pair3 <- merge(Kyle_GPS, Bumpus_GPS, )
cd_pair3 <- cd_pair3[, c(1,2,4,3,5)]
cd_pair3 <- cd_pair3[!duplicated(cd_pair3$timestamp),]

cd_pair4 <- merge(Little_rick_GPS, Elaine_GPS)
cd_pair4 <- cd_pair4[, c(1,2,4,3,5)]
cd_pair4 <- cd_pair4[!duplicated(cd_pair4$timestamp),]

cd_pair5 <- merge(Makao_GPS, Bumpus_GPS)
cd_pair5 <- cd_pair5[, c(1,2,4,3,5)]
cd_pair5 <- cd_pair5[!duplicated(cd_pair5$timestamp),]

cd_pair6 <- merge(Puji_GPS, Bumpus_GPS)
cd_pair6 <- cd_pair6[, c(1,2,4,3,5)]
cd_pair6 <- cd_pair6[!duplicated(cd_pair6$timestamp),]

cd_pair7 <- merge(Rodolfo_GPS, Elaine_GPS)
cd_pair7 <- cd_pair7[, c(1,2,4,3,5)]
cd_pair7 <- cd_pair7[!duplicated(cd_pair7$timestamp),]

cd_pair8 <- merge(Larry_GPS, Annie_GPS)
cd_pair8 <- cd_pair8[, c(1,2,4,3,5)]
cd_pair8 <- cd_pair8[!duplicated(cd_pair8$timestamp),]

cd_pair9 <- merge(Reid_GPS, Larry_GPS)
cd_pair9 <- cd_pair9[, c(1,2,4,3,5)]
cd_pair9 <- cd_pair9[!duplicated(cd_pair9$timestamp),]

cd_pair10 <- merge(Sheron_GPS, Maria_GPS)
cd_pair10 <- cd_pair10[, c(1,2,4,3,5)]
cd_pair10 <- cd_pair10[!duplicated(cd_pair10$timestamp),]

cd_pair11 <- merge(Thomas_GPS, Margaret_GPS)
cd_pair11 <- cd_pair11[, c(1,2,4,3,5)]
cd_pair11 <- cd_pair11[!duplicated(cd_pair11$timestamp),]

cd_pair12 <- merge(Thomas_GPS, Reid_GPS)
cd_pair12 <- cd_pair12[, c(1,2,4,3,5)]
cd_pair12 <- cd_pair12[!duplicated(cd_pair12$timestamp),]

#............................................................
# Estimating correlated movement ----
#............................................................

#Estimate the partition points
prts_pair1 <- findPrts(cd_pair1, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair1 <- corrMove(cd_pair1, prts_pair1)

prts_pair2 <- findPrts(cd_pair2, W=5, IC = 2)
cm_pair2 <- corrMove(cd_pair2, prts_pair2)

prts_pair3 <- findPrts(cd_pair3, W=5, IC = 2)
cm_pair3 <- corrMove(cd_pair3, prts_pair3)

prts_pair4 <- findPrts(cd_pair4, W=5, IC = 2)
cm_pair4 <- corrMove(cd_pair4, prts_pair4)

prts_pair5 <- findPrts(cd_pair5, W=5, IC = 2)
cm_pair5 <- corrMove(cd_pair5, prts_pair5)

prts_pair6 <- findPrts(cd_pair6, W=5, IC = 2)
cm_pair6 <- corrMove(cd_pair6, prts_pair6)

prts_pair7 <- findPrts(cd_pair7, W=5, IC = 2)
cm_pair7 <- corrMove(cd_pair7, prts_pair7)

prts_pair8 <- findPrts(cd_pair8, W=5, IC = 2)
cm_pair8 <- corrMove(cd_pair8, prts_pair8)

prts_pair9 <- findPrts(cd_pair9, W=5, IC = 2)
cm_pair9 <- corrMove(cd_pair9, prts_pair9)

prts_pair10 <- findPrts(cd_pair10, W=5, IC = 2)
cm_pair10 <- corrMove(cd_pair10, prts_pair10)

prts_pair11 <- findPrts(cd_pair11, W=5, IC = 2)
cm_pair11 <- corrMove(cd_pair11, prts_pair11)

prts_pair12 <- findPrts(cd_pair12, W=5, IC = 2)
cm_pair12 <- corrMove(cd_pair12, prts_pair12)

saveRDS(cm_pair1, file = "data/correlative_movement/cm_pair1.RDS")
saveRDS(cm_pair2, file = "data/correlative_movement/cm_pair2.RDS")
saveRDS(cm_pair3, file = "data/correlative_movement/cm_pair3.RDS")
saveRDS(cm_pair4, file = "data/correlative_movement/cm_pair4.RDS")
saveRDS(cm_pair5, file = "data/correlative_movement/cm_pair5.RDS")
saveRDS(cm_pair6, file = "data/correlative_movement/cm_pair6.RDS")
saveRDS(cm_pair7, file = "data/correlative_movement/cm_pair7.RDS")
saveRDS(cm_pair8, file = "data/correlative_movement/cm_pair8.RDS")
saveRDS(cm_pair9, file = "data/correlative_movement/cm_pair9.RDS")
saveRDS(cm_pair10, file = "data/correlative_movement/cm_pair10.RDS")
saveRDS(cm_pair11, file = "data/correlative_movement/cm_pair11.RDS")
saveRDS(cm_pair12, file = "data/correlative_movement/cm_pair12.RDS")

#............................................................
# Correlative movement results ----
#............................................................

#create a new dataframe for correlative movement data
corrmove_df <- proximity_identified_pairs_df

#create empty columns for the results to be saved to
corrmove_df$mean_etaTot.CI.Low <- NA
corrmove_df$mean_etaTot.MLE <- NA
corrmove_df$mean_etaTot.CI.Upp <- NA

#calculate mean total correlative movement
for (i in 1:12) {
  #get the corresponding cm_pair
  cm_pair <- get(paste0("cm_pair", i))
  
  # calculate means
  mean_etaTot_CI_Low <- mean(cm_pair$etaTot.CI.Low)
  mean_etaTot_MLE <- mean(cm_pair$etaTot.MLE)
  mean_etaTot_CI_Upp <- mean(cm_pair$etaTot.CI.Upp)
  
  #save results with mean values to the empty columns
  corrmove_df$mean_etaTot.CI.Low[corrmove_df$pair_ID_number == i] <- mean_etaTot_CI_Low
  corrmove_df$mean_etaTot.MLE[corrmove_df$pair_ID_number == i] <- mean_etaTot_MLE
  corrmove_df$mean_etaTot.CI.Upp[corrmove_df$pair_ID_number == i] <- mean_etaTot_CI_Upp
}

#mean amount of total correlation in all identified pairs movement
round(mean(corrmove_df$mean_etaTot.CI.Low[-1]), 2)
round(mean(corrmove_df$mean_etaTot.MLE[-1]), 2)
round(mean(corrmove_df$mean_etaTot.CI.Upp[-1]), 2)

#create empty columns for the results to be saved to
corrmove_df$mean_etaDif.MLE <- NA
corrmove_df$mean_etaDft.MLE <- NA

#calculate mean total drift and mean total diffusion correlative movement
for (i in 1:12) {
  # get the corresponding cm_pair
  cm_pair <- get(paste0("cm_pair", i))
  
  # calculate means
  mean_etaDif_MLE <- mean(cm_pair$etaDif.MLE)
  mean_etaDft_MLE <- mean(cm_pair$etaDft.MLE)
  
  #save results with mean values
  corrmove_df$mean_etaDif.MLE[corrmove_df$pair_ID_number == i] <- mean_etaDif_MLE
  corrmove_df$mean_etaDft.MLE[corrmove_df$pair_ID_number == i] <- mean_etaDft_MLE
}

#mean total drift and mean total diffusion correlative movement
round(mean(corrmove_df$mean_etaDif.MLE[-1]), 2)
round(mean(corrmove_df$mean_etaDft.MLE[-1]), 2)

