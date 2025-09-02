
# Script description: calculate the proximity of the individuals, proximity analysis

#............................................................
# Estimating proximity ratio ----
#............................................................

#subset individual movement models based on their site location
FIT_1 <- FIT[c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
               "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
               "Puji", "Rodolfo")]
FIT_2 <- FIT[c("Annie", "Beto", "Hannah", "Jane", "Larry",
               "Luigi", "Margaret", "Maria", "Reid", "Sheron",
               "Thomas")]

#create empty columns for results to be saved to
overlap_1_df$proximity_low <- NA
overlap_1_df$proximity_est <- NA
overlap_1_df$proximity_high <- NA

# this will take a while, days to loop, if R crashes, change the # for the loop number it was on. syntax: for(i in #:nrow(pairwise.df)) 

#Calculate the proximity statistics
for(i in 1:nrow(overlap_1_df$CI)){
  
  # Extract animal indices from columns 'anteater_A' and 'anteater_B'
  ANIMAL_A <- as.character(overlap_1_df[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(overlap_1_df[i, 'anteater_B'])
  
  # Extract tracking data for the pair of animals
  TRACKING_DATA.1 <- DATA_TELEMETRY[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  
  # Extract models for the pair of animals
  MODELS.1 <- list(FIT_1[ANIMAL_A][[1]], FIT_1[ANIMAL_B][[1]])   # line above is using as.character removes all the fluff because you just want the text string
  
  # Use tryCatch to handle potential errors during the calculation of the proximity statistic
  PROXIMITY1 <- tryCatch(
    {
      # Attempt to calculate the proximity statistic using the proximity function
      PROXIMITY_1 <- proximity(data = TRACKING_DATA.1, CTMM = MODELS.1, GUESS=ctmm(error=FALSE))},
    
    # If an error occurs during the try block, execute the error block
    error=function(err){
      
      # Print an error message indicating that an error occurred (added this line, issues running this code post pre-print version)
      cat("Error occurred at index", i, ": ", conditionMessage(err), "\n")
      
      # If an error occurs, set the proximity values to NA
      PROXIMITY_1 <- c(NA,NA,NA)
      
      # Return the NA values
      return(PROXIMITY_1)
    }
  )
  
  # Assign the proximity values to the corresponding columns 
  overlap_1_df[i, c("proximity_low")] <- PROXIMITY_1[1]
  overlap_1_df[i, c("proximity_est")] <- PROXIMITY_1[2]
  overlap_1_df[i, c("proximity_high")] <- PROXIMITY_1[3]
  
  #save results to a csv file
  # write.csv(overlap_1_df, "data/encounter/DATA_proximity_1.csv", row.names = FALSE)
  
  cat("finished index", i, "\n") # see the loop happening in real time
}

#.....

#create empty columns for results to be saved to
overlap_2_df$proximity_low <- NA
overlap_2_df$proximity_est <- NA
overlap_2_df$proximity_high <- NA

#Calculate the proximity statistics
for(i in 1:nrow(overlap_2_df)){
  ANIMAL_A <- as.character(overlap_2_df[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(overlap_2_df[i, 'anteater_B'])
  
  TRACKING_DATA_2 <- DATA_TELEMETRY[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  
  # line above is using as.character removes all the fluff because you just want the text string
  MODELS_2 <- list(FIT_2[ANIMAL_A][[1]], FIT_2[ANIMAL_B][[1]])
  PROXIMITY_2 <- tryCatch(
    {
      #calculate the proximity statistic
      PROXIMITY_2 <- proximity(data = TRACKING_DATA_2, 
                               CTMM = MODELS_2, 
                               GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY_2 <- c(NA,NA,NA)
      return(PROXIMITY_2)
    }
  )
  overlap_2_df[i, c("proximity_low")] <- PROXIMITY_2[1]
  overlap_2_df[i, c("proximity_est")] <- PROXIMITY_2[2]
  overlap_2_df[i, c("proximity_high")] <- PROXIMITY_2[3]
  
  #save results in a csv file
  write.csv(overlap_2_df, "data/enocunter/DATA_proximity_2.csv", row.names = FALSE)
  cat("finished index", i, "\n") # see the loop happening in real time
}

#............................................................

#import proximity data
DATA_proximity_1 <- read.csv("data/encounter/DATA_proximity_1.csv")
DATA_proximity_2 <- read.csv("data/encounter/DATA_proximity_2.csv")

#correct mismatch entry
DATA_proximity_1$anteater_A[DATA_proximity_1$anteater_A == "Little Rick"] <- "Little_Rick"
DATA_proximity_1$anteater_B[DATA_proximity_1$anteater_B == "Little Rick"] <- "Little_Rick"
DATA_proximity_2$anteater_A[DATA_proximity_2$anteater_A == "Larry 267"] <- "Larry"
DATA_proximity_2$anteater_B[DATA_proximity_2$anteater_B == "Larry 267"] <- "Larry"

#add missing site column to dataframe for site 2
DATA_proximity_2$site <- 2
DATA_proximity_2 <- relocate(DATA_proximity_2, site, .before = proximity_low)

proximity_df <- bind_rows(DATA_proximity_1, DATA_proximity_2)
proximity_df <- proximity_df[,-3]
proximity_df <- mutate(proximity_df,
                       sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                  paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                  paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                  paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

#add home range overlap data to proximity dataframe
proximity_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                           "Sex.A", "Sex.B",
                                                           "Age.A", "Age.B",
                                                           "sex_comparison",
                                                           "site"))

#save proximity dataframe
# save(proximity_df, file = "data/encounter/proximity_df.rda")
load("data/encounter/proximity_df.rda")

#............................................................
# Proximity ratio results ----
#............................................................

#test for significance in sex, compare model with and without sex as a variable across all 121 dyads
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "log"), data = proximity_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2) #p = 0.13

prox_overlap_test <- glmer(proximity_est ~ overlap_est + (1|site), family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test_results <- anova(prox_overlap_test, prox_overlap_test2)
prox_overlap_test_pvalue <- round(prox_overlap_test_results$`Pr(>Chisq)`[2], 2) #p = 0.03

#test for significance in sex, compare model with and without sex as a variable
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), 
                        family = Gamma(link = "log"), data = proximity_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), 
                         family = Gamma(link = "log"), data = proximity_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_results
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2) #p = 0.13
proximity_test_pvalue

#test for significance in home-range overlap, compare model with and without overlap as a variable
prox_overlap_test <- glmer(proximity_est ~ overlap_est + (1|site), 
                           family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test2 <- glmer(proximity_est ~ 1 + (1|site), 
                            family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test_results <- anova(prox_overlap_test, prox_overlap_test2)
prox_overlap_test_results
prox_overlap_test_pvalue <- round(prox_overlap_test_results$`Pr(>Chisq)`[2], 2) #p = 0.03
prox_overlap_test_pvalue