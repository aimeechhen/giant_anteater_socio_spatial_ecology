
library(ctmm)
library(dplyr)
library(lme4)

#import home-range estimate
dat.hr <- readRDS("data/home_range/dat_hr.rds")

#............................................................
# Is weight a factor in home-range size?

#compare model with and without weight as a variable
HR_weight_test <- glmmTMB(HR_est ~ Weight + (1|Site), 
                          family = Gamma(link = "log"), data = dat.hr)
HR_weight_test2 <- glmmTMB(HR_est ~ 1 + (1|Site), 
                           family = Gamma(link = "log"), data = dat.hr)

HR_weight_test_results <- anova(HR_weight_test, HR_weight_test2)
HR_weight_test_results

#calculate weight impact via likelihood ratio test
HR_weight_test_pvalue <- round(HR_weight_test_results$`Pr(>Chisq)`[2], 2)
HR_weight_test_pvalue
