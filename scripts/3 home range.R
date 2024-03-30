
# Script description: extract the home range

library(ctmm)
library(tidyverse)

calculate home range estimate using akde, extract home range estimates, create a pairwise overlap dataframe from their home range estimates for each individual, calculate their overlap values, home range and home range overlap analysis

load("data/anteater/bio_data.rda")
load("data/anteater_akdes.rda")

#............................................................
# Extract Home-range area estimates ----
#............................................................

#create a dataframe to store home range area statistics from the AKDE
HR_size <- data.frame()

#loop through each object in the AKDE list
for (i in 1:length(AKDE)) {
  #extract the home range area statistics summary
  summary <- summary(AKDE[[i]])$CI
  
  #bind the summary to the dataframe
  HR_size <- rbind(HR_size, as.data.frame(summary))
}

row.names(HR_size) <- NULL

#add biological data to dataframe
HR_size <- cbind(HR_size, bio_data)
HR_size <- relocate(HR_size, c(low, est, high), .after = Site)
names(HR_size)[7] <- "HR_low"
names(HR_size)[8] <- "HR_est"
names(HR_size)[9] <- "HR_high"

#save home range size results
# save(HR_size, file = "data/home_range/HR_size.rda")
load("data/home_range/HR_size.rda")

#............................................................

#calculate the mean total home range size
round(mean(HR_size$HR_est), 2)

#calculate CIs of the mean total home range size
round(mean(HR_size$HR_low), 2)
round(mean(HR_size$HR_high), 2)

#............................................................
# Does home range size differ between sexes?

#subset each individual based on their sex
AKDE_male <- AKDE[c("Alexander", "Anthony", "Beto","Christoffer","Jackson",
                    "Kyle", "Larry", "Little_Rick", "Luigi", "Reid", 
                    "Rodolfo", "Thomas")]
AKDE_female <- AKDE[c("Annie", "Bumpus", "Cate", "Elaine", "Hannah",
                      "Jane","Makao", "Margaret", "Maria", "Puji",
                      "Sheron")]

#calculate mean home range sizes for male
meta(AKDE_male)

#calculate mean home range sizes for female
meta(AKDE_female)

#test to see significance of sex on home range
AKDE_sex_compare <- list(male = AKDE_male,
                         female = AKDE_female)
COL_sex <- c("#004488", "#A50026")
meta(AKDE_sex_compare, col = COL_sex, sort = TRUE)


#............................................................
# Weight and home-range ----
#............................................................

# Is weight a factor in home-range size?

#without luigi
HR_size <- HR_size[HR_size$ID != "Luigi",]

#compare model with and without weight as a variable
HR_weight_test <- glmmTMB(HR_est ~ Weight + (1|Site), 
                          family = Gamma(link = "log"), data = HR_size)
HR_weight_test2 <- glmmTMB(HR_est ~ 1 + (1|Site), 
                           family = Gamma(link = "log"), data = HR_size)

HR_weight_test_results <- anova(HR_weight_test, HR_weight_test2)
HR_weight_test_results

#calculate weight impact via likelihood ratio test
HR_weight_test_pvalue <- round(HR_weight_test_results$`Pr(>Chisq)`[2], 2)
HR_weight_test_pvalue

summary(HR_weight_test)

plot(HR_size$HR_est ~ HR_size$Weight)



#............................................................
# Estimating home range overlap ----
#............................................................

#subset home range overlap based on site location
AKDE_1 <- AKDE[c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
                 "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
                 "Puji", "Rodolfo")]

AKDE_2 <- AKDE[c("Annie", "Beto", "Hannah", "Jane", "Larry",
                 "Luigi", "Margaret", "Maria", "Reid", "Sheron",
                 "Thomas")]

#save home range estimates for each site
save(AKDE_1, file = "data/home_range/AKDE_1.rda")
save(AKDE_2, file = "data/home_range/AKDE_2.rda")

#calculate 95% AKDE home range overlap for a pairwise comparison for each site
overlap_1 <- overlap(AKDE_1, level = 0.95)
overlap_2 <- overlap(AKDE_2, level = 0.95)

#............................................................
#create a pairwise dataframe by pairing up every individual at each site

#extract CI 'est' matrix from array
overlap_1_est <- overlap_1$CI[ , , 2]
#remove duplicate values of the matrix
overlap_1_est[upper.tri(overlap_1_est, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_1_df <- as.data.frame(overlap_1_est)
overlap_1_df$anteater_A <- rownames(overlap_1_df)
overlap_1_df <- pivot_longer(overlap_1_df, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_est', values_drop_na = TRUE)

#extract CI 'low' matrix from array
overlap_1_low <- overlap_1$CI[ , , 1]
#remove duplicate values of the matrix
overlap_1_low[upper.tri(overlap_1_low, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_1_low <- as.data.frame(overlap_1_low)
overlap_1_low$anteater_A <- rownames(overlap_1_low)
overlap_1_low <- pivot_longer(overlap_1_low, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_low', values_drop_na = TRUE)
overlap_1_df <- left_join(overlap_1_df, overlap_1_low, by = c("anteater_A", "anteater_B"))
overlap_1_df <- relocate(overlap_1_df, overlap_low, .before = overlap_est)

#extract CI 'high' matrix from array
overlap_1_high <- overlap_1$CI[ , , 3]
#remove duplicate values of the matrix
overlap_1_high[upper.tri(overlap_1_high, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_1_high <- as.data.frame(overlap_1_high)
overlap_1_high$anteater_A <- rownames(overlap_1_high)
overlap_1_high <- pivot_longer(overlap_1_high, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_high', values_drop_na = TRUE)
overlap_1_df <- left_join(overlap_1_df, overlap_1_high, by = c("anteater_A", "anteater_B"))

#add biological data to dataframe
overlap_1_df <- left_join(overlap_1_df, rename(bio_df, anteater_A = ID), by = "anteater_A")
overlap_1_df <- left_join(overlap_1_df, rename(bio_df, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
#add column to indicate which sexes that are being compared
overlap_1_df <- mutate(overlap_1_df,
                       sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                  paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                  paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                  paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))
#assign site
overlap_1_df$site <- 1
overlap_1_df <- relocate(overlap_1_df, c("overlap_low", "overlap_est", "overlap_high"), .after = site)




#extract CI 'est' matrix from array
overlap_2_est <- overlap_2$CI[ , , 2]
#remove duplicate values of the matrix
overlap_2_est[upper.tri(overlap_2_est, diag = TRUE)] <- NA
#create a new data frame based on the overlap values
overlap_2_df <- as.data.frame(overlap_2_est)
overlap_2_df$anteater_A <- rownames(overlap_2_df)
overlap_2_df <- pivot_longer(overlap_2_df, cols = -anteater_A, 
                             names_to = 'anteater_B', values_to = 'overlap_est', values_drop_na = TRUE)

#extract CI 'low' matrix from array
overlap_2_low <- overlap_2$CI[ , , 1]
#remove duplicate values of the matrix
overlap_2_low[upper.tri(overlap_2_low, diag = TRUE)] <- NA
#create a new data frame based on the overlap values
overlap_2_low <- as.data.frame(overlap_2_low)
overlap_2_low$anteater_A <- rownames(overlap_2_low)
overlap_2_low <- pivot_longer(overlap_2_low, cols = -anteater_A, 
                              names_to = 'anteater_B', values_to = 'overlap_low',
                              values_drop_na = TRUE)
overlap_2_df <- left_join(overlap_2_df, overlap_2_low, 
                          by = c("anteater_A", "anteater_B"))
overlap_2_df <- relocate(overlap_2_df, overlap_low, .before = overlap_est)

#.....

#extract CI 'est' matrix from array
overlap_2_est <- overlap_2$CI[ , , 2]
#remove duplicate values of the matrix
overlap_2_est[upper.tri(overlap_2_est, diag = TRUE)] <- NA
#create a new data frame based on the overlap values
overlap_2_df <- as.data.frame(overlap_2_est)
overlap_2_df$anteater_A <- rownames(overlap_2_df)
overlap_2_df <- pivot_longer(overlap_2_df, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_est', values_drop_na = TRUE)

#extract CI 'low' matrix from array
overlap_2_low <- overlap_2$CI[ , , 1]
#remove duplicate values of the matrix
overlap_2_low[upper.tri(overlap_2_low, diag = TRUE)] <- NA
#create a new data frame based on the overlap values
overlap_2_low <- as.data.frame(overlap_2_low)
overlap_2_low$anteater_A <- rownames(overlap_2_low)
overlap_2_low <- pivot_longer(overlap_2_low, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_low', values_drop_na = TRUE)
overlap_2_df <- left_join(overlap_2_df, overlap_2_low, by = c("anteater_A", "anteater_B"))
overlap_2_df <- relocate(overlap_2_df, overlap_low, .before = overlap_est)

#extract CI 'high' matrix from array
overlap_2_high <- overlap_2$CI[ , , 3]
#remove duplicate values of the matrix
overlap_2_high[upper.tri(overlap_2_high, diag = TRUE)] <- NA
#create a new data frame based on the overlap values
overlap_2_high <- as.data.frame(overlap_2_high)
overlap_2_high$anteater_A <- rownames(overlap_2_high)
overlap_2_high <- pivot_longer(overlap_2_high, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_high', values_drop_na = TRUE)
overlap_2_df <- left_join(overlap_2_df, overlap_2_high, by = c("anteater_A", "anteater_B"))

#add biological data to dataframe
overlap_2_df <- left_join(overlap_2_df, rename(bio_df, anteater_A = ID), by = "anteater_A")
overlap_2_df <- left_join(overlap_2_df, rename(bio_df, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
#add column to indicate which sexes that are being compared
overlap_2_df <- mutate(overlap_2_df,
                       sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                  paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                  paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                  paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))
#assign site
overlap_2_df$site <- 2
overlap_2_df <- relocate(overlap_2_df, c("overlap_low", "overlap_est", "overlap_high"), .after = site)

#combine both sites into one dataframe
overlap_df <- rbind(overlap_1_df, overlap_2_df)
overlap_df$pair_ID <- paste(overlap_df$anteater_A, overlap_df$anteater_B, sep = "_")
overlap_df <- relocate(overlap_df, pair_ID, .before = anteater_A)

#clean up environment
rm(overlap_1_low, overlap_1_est, overlap_1_high,
   overlap_2_low, overlap_2_est, overlap_2_high)


#save home range overlap dataframes
save(object = overlap_1_df, file = "data/home_range/overlap_1_df.rda")
save(object = overlap_2_df, file = "data/home_range/overlap_2_df.rda")
save(object = overlap_df, file = "data/home_range/overlap_df.rda")

#............................................................
# Home range overlap results ----
#............................................................

#calculate mean total home range overlap 
round(mean(overlap_df$overlap_est), 2)

#calculate range of total home range overlap 
round(min(overlap_df$overlap_est), 2)
round(max(overlap_df$overlap_est), 2)

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
HRO_test <- glmmTMB(overlap_est_squeezed ~ sex_comparison + (1|site), family = beta_family(link = "logit"), data = overlap_df)
HRO_test2 <- glmmTMB(overlap_est_squeezed ~ 1 + (1|site), family = beta_family(link = "logit"), data = overlap_df)
HRO_test_results <- anova(HRO_test, HRO_test2)
HRO_test_pvalue <- round(HRO_test_results$`Pr(>Chisq)`[2], 2)

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
