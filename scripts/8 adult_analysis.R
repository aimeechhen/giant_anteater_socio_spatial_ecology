
# Script description: investigating adults only to see if age is a factor

library(ctmm)
library(dplyr)
library(tidyverse)
library(glmmTMB)
library(lme4)

#____________________________________________________________
# import data

#import data, cleaned giant anteater GPS tracking data, containing no outliers
COLLAR_DATA <- read.csv("data/anteater/Anteaters_NoOutliers.csv")

#correct mismatch ID entries
COLLAR_DATA$ID[COLLAR_DATA$ID == "Larry 267"] <- "Larry"

COLLAR_DAT <- data.frame(timestamp = COLLAR_DATA$timestamp,
                         ID = COLLAR_DATA$ID,
                         GPS.Latitude = COLLAR_DATA$GPS.Latitude,
                         GPS.Longitude = COLLAR_DATA$GPS.Longitude,
                         GPS.Horizontal.Dilution = COLLAR_DATA$GPS.Horizontal.Dilution,
                         OUT = COLLAR_DATA$OUT,
                         Road = COLLAR_DATA$Road)

#subset to the 20 adult range-resident individuals
anteater_data <- COLLAR_DAT[which(COLLAR_DAT$ID 
                                  %in% c("Alexander", "Annie", "Beto", "Bumpus", "Cate", 
                                         "Christoffer","Elaine", "Hannah","Jackson", "Jane", 
                                         "Larry", "Luigi", "Makao", "Margaret", "Maria", 
                                         "Puji", "Rodolfo", "Sheron", "Thomas")),]

# save(anteater_data, file = "data/anteater/anteater_data_adults.rda")
load("data/anteater/anteater_data_adults.rda")



#subset to adults only
load("data/anteater/bio_data.rda")
bio_data <- bio_data[-c(3,12,14,20),]

# Convert to telemetry
DATA_TELEMETRY <- as.telemetry(anteater_data)
#subset to adults only
DATA_TELEMETRY <- DATA_TELEMETRY[-c(3,12,14,20)]



#____________________________________________________________
# Fit movement models

# # create guesstimate non-interactively
load("data/anteater_fit.rda")

#subset to adults only
FIT <- FIT[-c(3,12,14,20)]

#____________________________________________________________
# Estimate home-range areas

load("data/anteater_akdes.rda")


#subset to adults only
AKDE <- AKDE[-c(3,12,14,20)]

#Mean HR size...
meta(AKDE)

#____________________________________________________________
# Compare between sex

#subset each individual based on their sex
AKDE_male <- AKDE[c("Alexander", "Beto","Christoffer","Jackson", "Larry", 
                    "Luigi", "Rodolfo", "Thomas")]
AKDE_female <- AKDE[c("Annie", "Bumpus", "Cate", "Elaine", "Hannah",
                      "Jane","Makao", "Margaret", "Maria", "Puji",
                      "Sheron")]

#calculate mean home range sizes for male
meta(AKDE_male)

#calculate mean home range sizes for female
meta(AKDE_female)

#female/male ratio of mean home-range areas
AKDE_sex_compare <- list(male = AKDE_male,
                         female = AKDE_female)
meta(AKDE_sex_compare, col = c("#004488", "#A50026"), sort = TRUE)

#Mean HR size...
mean(overlap(AKDE_male)$CI[,,"est"][lower.tri(overlap(AKDE_male)$CI[,,"est"])])

mean(overlap(AKDE_female)$CI[,,"est"][lower.tri(overlap(AKDE_female)$CI[,,"est"])])


#create a dataframe to store home range area statistics from the AKDE
HR_size_adult <- data.frame()
for (i in 1:length(AKDE)) {
  #extract the home range area statistics summary
  summary <- summary(AKDE[[i]])$CI
  
  #bind the summary to the dataframe
  HR_size_adult <- rbind(HR_size_adult, as.data.frame(summary))
}

row.names(HR_size_adult) <- NULL

#add biological data to dataframe
HR_size_adult <- cbind(HR_size_adult, bio_data)
HR_size_adult <- relocate(HR_size_adult, c(low, est, high), .after = Site)
names(HR_size_adult)[6] <- "HR_low"
names(HR_size_adult)[7] <- "HR_est"
names(HR_size_adult)[8] <- "HR_high"

# save(HR_size_adult, file = "data/home_range/HR_size_adult.rda")
load("data/home_range/HR_size_adult.rda")

#............................................................
# Estimating home range overlap ----
#............................................................

#subset home range overlap based on site location
AKDE_1 <- AKDE[c("Alexander", "Bumpus", "Cate", "Christoffer",
                 "Elaine", "Jackson", "Makao",
                 "Puji", "Rodolfo")]

AKDE_2 <- AKDE[c("Annie", "Beto", "Hannah", "Jane", "Larry",
                 "Luigi", "Margaret", "Maria", "Sheron",
                 "Thomas")]

#calculate 95% AKDE home range overlap for a pairwise comparison for each site
overlap1 <- overlap(AKDE_1, level = 0.95)
overlap2 <- overlap(AKDE_2, level = 0.95)

#indicate the matrix layers to be extracted
matrix_level <- c('low', 'est', 'high')

num_matrices1 <- dim(overlap1$CI)[3]
overlap_result1 <- list()
for (i in 1:num_matrices1) {
  matrix_layer1 <- overlap1$CI[,,i]
  matrix_layer1[upper.tri(matrix_layer1, diag = TRUE)] <- NA
  matrix_layer1 <- as.data.frame(matrix_layer1)
  matrix_layer1$anteater_A <- rownames(matrix_layer1)
  overlap_col_name1 <- paste0('overlap_', matrix_level[i])
  matrix_layer1 <- pivot_longer(matrix_layer1, cols = -anteater_A,
                                names_to = 'anteater_B', values_to = overlap_col_name1,
                                values_drop_na = TRUE)
  overlap_result1[[i]] <- matrix_layer1
}

overlap_df1 <- overlap_result1[[1]]
for (i in 2:num_matrices1) {
  overlap_df1 <- left_join(overlap_df1, overlap_result1[[i]],
                           by = c("anteater_A", "anteater_B"))
}


# Add biological data to data frame
overlap_df1 <- left_join(overlap_df1, rename(bio_data, anteater_A = ID), by = "anteater_A")
overlap_df1 <- left_join(overlap_df1, rename(bio_data, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))

# Add column to indicate which sexes are being compared
overlap_df1 <- mutate(
  overlap_df1, sex_comparison =
    case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
              paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
              paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
              paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

num_matrices2 <- dim(overlap2$CI)[3]
overlap_result2 <- list()
for (i in 1:num_matrices2) {
  matrix_layer2 <- overlap2$CI[,,i]
  matrix_layer2[upper.tri(matrix_layer2, diag = TRUE)] <- NA
  matrix_layer2 <- as.data.frame(matrix_layer2)
  matrix_layer2$anteater_A <- rownames(matrix_layer2)
  overlap_col_name2 <- paste0('overlap_', matrix_level[i])
  matrix_layer2 <- pivot_longer(matrix_layer2, cols = -anteater_A,
                                names_to = 'anteater_B', values_to = overlap_col_name2,
                                values_drop_na = TRUE)
  overlap_result2[[i]] <- matrix_layer2
}

overlap_df2 <- overlap_result2[[1]]
for (i in 2:num_matrices2) {
  overlap_df2 <- left_join(overlap_df2, overlap_result2[[i]],
                           by = c("anteater_A", "anteater_B"))
}

# Add biological data to data frame
overlap_df2 <- left_join(overlap_df2, rename(bio_data, anteater_A = ID), by = "anteater_A")
overlap_df2 <- left_join(overlap_df2, rename(bio_data, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))

# Add column to indicate which sexes are being compared
overlap_df2 <- mutate(
  overlap_df2, sex_comparison =
    case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
              paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
              paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
              paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

#combine both sites into one dataframe
overlap_data <- rbind(overlap_df1, overlap_df2)
overlap_data$pair_ID <- paste(overlap_data$anteater_A, overlap_data$anteater_B, sep = "_")
overlap_data <- relocate(overlap_data, pair_ID, .before = anteater_A)
overlap_data <- relocate(overlap_data, c(overlap_low, overlap_est, overlap_high), .after = sex_comparison)

#clean up environment
rm(matrix_layer1, matrix_layer2, matrix_level, 
   overlap_col_name1, overlap_col_name2,
   overlap_result1, overlap_result2)

overlap_data$site <- overlap_data$Site.A


save(overlap_data, file = "data/home_range/overlap_data_adult.rda")
load("data/home_range/overlap_data_adult.rda")


#............................................................
#Is sex a factor?...

#rescale the values
min_val <- min(overlap_data$overlap_est)
max_val <- max(overlap_data$overlap_est)
squeeze_min <- 0.001
squeeze_max <- 0.999
overlap_data$overlap_est_squeezed <- ((overlap_data$overlap_est - min_val) / (max_val - min_val)) * (squeeze_max - squeeze_min) + squeeze_min

#compare model with and without sex as a variable
HRO_test <- glmmTMB(overlap_est_squeezed ~ sex_comparison + (1|site), 
                    family = beta_family(link = "logit"), data = overlap_data)
HRO_test2 <- glmmTMB(overlap_est_squeezed ~ 1 + (1|site), 
                     family = beta_family(link = "logit"), data = overlap_data)
HRO_test_results <- anova(HRO_test, HRO_test2)
HRO_test_results

#calculate sex impact via likelihood ratio test
HRO_test_pvalue <- round(HRO_test_results$`Pr(>Chisq)`[2], 2)
HRO_test_pvalue

#number of home range overlap in each sex comparison category
table(overlap_df$sex_comparison)


#Home range overlap results based on sex comparisons (i.e.. male-male, female-female, and female-male).

#number of home range overlap in each sex comparison category
table(overlap_data$sex_comparison)

#calculate mean home range overlap and the range based on sex comparisons
round(mean(overlap_data$overlap_est[overlap_data$sex_comparison == "male-male"]), 2)
round(min(overlap_data$overlap_est[overlap_data$sex_comparison == "male-male"]), 2)
round(max(overlap_data$overlap_est[overlap_data$sex_comparison == "male-male"]), 2)

round(mean(overlap_data$overlap_est[overlap_data$sex_comparison == "female-female"]), 2)
round(min(overlap_data$overlap_est[overlap_data$sex_comparison == "female-female"]), 2)
round(max(overlap_data$overlap_est[overlap_data$sex_comparison == "female-female"]), 2)

round(mean(overlap_data$overlap_est[overlap_data$sex_comparison == "female-male"]), 2)
round(min(overlap_data$overlap_est[overlap_data$sex_comparison == "female-male"]), 2)
round(max(overlap_data$overlap_est[overlap_data$sex_comparison == "female-male"]), 2)



#__________________________________________________________________
# Estimating proximity ratio ----

#import proximity data
load("data/encounter/proximity_df.rda")
proximity_df <- as.data.frame(proximity_df)

#remove subadults from proximity data
proximity_df <- proximity_df[!(proximity_df$anteater_A %in% "Anthony" | proximity_df$anteater_B %in% "Anthony"),]
proximity_df <- proximity_df[!(proximity_df$anteater_A %in% "Kyle" | proximity_df$anteater_B %in% "Kyle"),]
proximity_df <- proximity_df[!(proximity_df$anteater_A %in% "Little_Rick" | proximity_df$anteater_B %in% "Little_Rick"),]
proximity_df <- proximity_df[!(proximity_df$anteater_A %in% "Reid" | proximity_df$anteater_B %in% "Reid"),]

#combine overlap and proximity data
overlap_data <- merge(overlap_data, proximity_df, all = TRUE)


#...........................................................
# Proximity ratio results
#...........................................................

#test for significance in sex, compare model with and without sex as a variable
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), 
                        family = Gamma(link = "log"), data = overlap_data)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), 
                         family = Gamma(link = "log"), data = overlap_data)
proximity_test_results <- anova(proximity_test, proximity_test2)

#calculate sex impact on proximity ratio via likelihood ratio test
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2)
proximity_test_pvalue

#compare model with and without overlap as a variable
prox_overlap_test <- glmer(proximity_est ~ overlap_est + (1|site), 
                           family = Gamma(link = "log"), data = overlap_data)
prox_overlap_test2 <- glmer(proximity_est ~ 1 + (1|site), 
                            family = Gamma(link = "log"), data = overlap_data)
prox_overlap_test_results <- anova(prox_overlap_test, prox_overlap_test2)

#calculate home-range overlap impact via likelihood ratio test
prox_overlap_test_pvalue <- round(prox_overlap_test_results$`Pr(>Chisq)`[2], 2)
prox_overlap_test_pvalue



#________________________________________________________________
# Distance ----



#import the distance data
DATA_DISTANCE <- readRDS("data/encounter/DATA_DISTANCE.rds")

#locate NA values within the dataframe
DATA_DISTANCE[!complete.cases(DATA_DISTANCE), ] #3,502,701 observations
#drop the 3 fixes that had no distance values 
DATA_DISTANCE <- na.omit(DATA_DISTANCE) #3,502,698 observations

#add overlap and proximity information to the distance dataframe
distance_data <- merge(DATA_DISTANCE, proximity_df, by = "pair_ID")
distance_data <- relocate(distance_data, c(distance_low, distance_est, distance_high,
                                           t, timestamp), .after = proximity_high)

#save the distance dataframe
#save(distance_data, file = "data/encounter/distance_data_adults.rda")
load("data/encounter/distance_data_adults.rda")



#________________________________________________________________
# Encounter ----

overlap_data$encounter_count <- NA
pair_ID <- unique(overlap_data$pair_ID)

#calculate total encounters of all individuals based on sex comparison type
for (i in pair_ID){
  subset_A <- distance_data[distance_data$pair_ID == i,]
  
  #count the number of times distance is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  
  #save results
  overlap_data[overlap_data$pair_ID == i, "encounter_count"] <- encounter_count
  
}


#calculate the number of encounters based on threshold of 15m
sum(overlap_data$encounter_count)
sum(overlap_data$encounter_count[overlap_data$sex_comparison == "male-male"])
sum(overlap_data$encounter_count[overlap_data$sex_comparison == "female-female"])
sum(overlap_data$encounter_count[overlap_data$sex_comparison == "female-male"])

#effect of sex and overlap on encounter rates and does not include 0 encounter counts
encounter_test <- glmer(encounter_count ~ overlap_est + sex_comparison + (1|site), 
                        family = poisson(link = "log"), 
                        data = overlap_data, subset = encounter_count > 0)
encounter_test2 <- glmer(encounter_count ~ 1 + (1|site), 
                         family = poisson(link = "log"), 
                         data = overlap_data, subset = encounter_count > 0)
encounter_test_results <- anova(encounter_test, encounter_test2)
encounter_test_results

#calculate sex impact on encounters via likelihood ratio test
encounter_test_pvalue <- round(encounter_test_results$`Pr(>Chisq)`[2], 2)
encounter_test_pvalue

#amount of home range overlap and the number of observed encounters
summary(encounter_test)


#________________________________________________________________
# Deviated pairs ----

#identify pairs that did not have a proximity ratio of 1
proximity_above1 <- proximity_data[proximity_data$proximity_low > 1,]
proximity_below1 <- proximity_data[proximity_data$proximity_high < 1,]

#exclude pairs with a home range overlap of 0
proximity_below1[proximity_below1$overlap_est > 0.0001,]
proximity_below1 <- proximity_below1[proximity_below1$overlap_est > 0.0001,]

#create a dataframe of the deviated pairs
proximity_identified_pairs_df <- rbind(proximity_above1, proximity_below1)
proximity_identified_pairs_df$pair_ID_number <- seq(from = 1, to = 7, by = 1)
proximity_identified_pairs_df <- relocate(proximity_identified_pairs_df, pair_ID_number,
                                          .before = anteater_A)

#correct the sex_comparison output to female-male
proximity_identified_pairs_df <- mutate(proximity_identified_pairs_df, sex_comparison = 
                                          case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                    paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                    paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                    paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

#clean up environment
rm(proximity_above1, proximity_below1)

#save(proximity_identified_pairs_df, file = "data/encounter/proximity_identified_adult_pairs_df.rda")
load("data/encounter/proximity_identified_adult_pairs_df.rda")


#...........................................................
# Proximity analysis ----
#...........................................................

#number of pairs with a deviated proximity ratio based on sex comparison
table(proximity_identified_pairs_df$sex_comparison)

#test for significance in sex, compare model with and without sex as a variable
proximity_test_pairs <- glmer(proximity_est ~ sex_comparison + (1|site), 
                              family = Gamma(link = "log"), 
                              data = proximity_identified_pairs_df)
proximity_test2_pairs <- glmer(proximity_est ~ 1 + (1|site), 
                               family = Gamma(link = "log"), 
                               data = proximity_identified_pairs_df)
proximity_test_results_pairs <- anova(proximity_test_pairs, proximity_test2_pairs)
proximity_test_results_pairs

#calculate sex impact via likelihood ratio test
proximity_test_pvalue_pairs <- round(proximity_test_results_pairs$`Pr(>Chisq)`[2], 2)
proximity_test_pvalue_pairs

#test for significance in home range overlap, compare model with and without overlap as a variable
prox_overlap_test_pairs <- glmer(proximity_est ~ overlap_est + (1|site), 
                                 family = Gamma(link = "log"), 
                                 data = proximity_identified_pairs_df)
prox_overlap_test2_pairs <- glmer(proximity_est ~ 1 + (1|site), 
                                  family = Gamma(link = "log"), 
                                  data = proximity_identified_pairs_df)
prox_overlap_test_results_pairs <- anova(prox_overlap_test_pairs, prox_overlap_test2_pairs)
prox_overlap_test_results_pairs

#calculate home-range overlap impact via likelihood ratio test
prox_overlap_test_pvalue_pairs <- round(prox_overlap_test_results_pairs$`Pr(>Chisq)`[2], 2)
prox_overlap_test_pvalue_pairs