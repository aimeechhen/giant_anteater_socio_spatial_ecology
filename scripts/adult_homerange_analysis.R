
# Load packages
library(ctmm)
library(dplyr)
library(tidyverse)
library(glmmTMB)

anteater_data <- readRDS("data/anteater_data.rds")

#import supplementary data containing biological information
DATA_META <- read.csv("data/Anteater_Results_Final.csv")

#subset biological data from supplementary data
bio_data <- DATA_META[which(DATA_META$ID 
                            %in% c("Alexander", "Annie", "Beto", "Bumpus", "Cate", 
                                   "Christoffer","Elaine", "Hannah","Jackson", "Jane", 
                                   "Larry", "Luigi", "Makao", "Margaret", "Maria", 
                                   "Puji", "Rodolfo", "Sheron", "Thomas")),c(1:5)]

#add site location 
bio_data$Site[bio_data$Road == "MS-040"] <- 1
bio_data$Site[bio_data$Road == "BR_267"] <- 2


# Convert to telemetry
DATA_TELEMETRY <- as.telemetry(anteater_data)



#____________________________________________________________
# Fit movement models

# # create guesstimate non-interactively
# GUESS <- lapply(DATA_TELEMETRY[1:23], function(b) ctmm.guess(b,interactive=FALSE) )
# # fit models
# FIT <- lapply(1:23, function(i) ctmm.select(DATA_TELEMETRY[[i]],GUESS[[i]]) )
# #rename for convenience
# names(FIT) <- names(DATA_TELEMETRY[1:23])
# overlap(FIT)

#save movement models
#save(FIT, file = "data/anteater_fit.rda")
load("data/anteater_fit.rda")



#subset to adults only
FIT <- FIT[-c(3,12,14,20)]

#____________________________________________________________
# Estimate home-range areas

#calculate AKDE home range estimates based on the best fit model, create aligned UDs
# AKDE <- akde(DATA_TELEMETRY[1:23],FIT)
# save(AKDE, file = "data/anteater_AKDE.rda")
load("data/anteater_AKDE.rda")



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
names(HR_size_adult)[7] <- "HR_low"
names(HR_size_adult)[8] <- "HR_est"
names(HR_size_adult)[9] <- "HR_high"




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
  matrix_layer1 <- overlap_1$CI[,,i]
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
overlap_df <- rbind(overlap_df1, overlap_df2)
overlap_df$pair_ID <- paste(overlap_df$anteater_A, overlap_df$anteater_B, sep = "_")
overlap_df <- relocate(overlap_df, pair_ID, .before = anteater_A)

#clean up environment
rm(matrix_layer1, matrix_layer2, matrix_level, 
   overlap_col_name1, overlap_col_name2,
   overlap_result1, overlap_result2)


saveRDS(overlap_df, file = "data/overlap_df_adult.rds")


#............................................................
# Is sex a factor?

#rescale the values
min_val <- min(overlap_df$overlap_est)
max_val <- max(overlap_df$overlap_est)
squeeze_min <- 0.001
squeeze_max <- 0.999
overlap_df$overlap_est_squeezed <- ((overlap_df$overlap_est - min_val) / (max_val - min_val)) * (squeeze_max - squeeze_min) + squeeze_min
overlap_df <- relocate(overlap_df, overlap_est_squeezed, .after = overlap_high)

#test for significance in sex, compare model with and without sex as a variable
HRO_test <- glmmTMB(overlap_est_squeezed ~ sex_comparison + (1|Site.A), family = beta_family(link = "logit"), data = overlap_df)
HRO_test2 <- glmmTMB(overlap_est_squeezed ~ 1 + (1|Site.A), family = beta_family(link = "logit"), data = overlap_df)
HRO_test_results <- anova(HRO_test, HRO_test2)
HRO_test_pvalue <- round(HRO_test_results$`Pr(>Chisq)`[2], 2)
HRO_test_pvalue
#number of home range overlap in each sex comparison category
table(overlap_df$sex_comparison)




#calculate mean total home range overlap 
round(mean(overlap_df$overlap_est), 2)

#number of home range overlap in each sex comparison category
table(overlap_df$sex_comparison)

#calculate mean home range overlap & range based on sex comparison categories
round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "female-male"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "female-male"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "female-male"]), 2)
