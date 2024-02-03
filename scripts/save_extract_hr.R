
library(ctmm)
library(sf)
library(glmmTMB)

#import data, cleaned giant anteater GPS tracking data, containing no outliers
COLLAR_DATA <- read.csv("data/Anteaters_NoOutliers.csv")

#correct mismatch ID entries
COLLAR_DATA$ID[COLLAR_DATA$ID == "Larry 267"] <- "Larry"
COLLAR_DATA$ID[COLLAR_DATA$ID == "Little Rick"] <- "Little_Rick"

COLLAR_DAT <- data.frame(timestamp = COLLAR_DATA$timestamp,
                         ID = COLLAR_DATA$ID,
                         GPS.Latitude = COLLAR_DATA$GPS.Latitude,
                         GPS.Longitude = COLLAR_DATA$GPS.Longitude,
                         GPS.Horizontal.Dilution = COLLAR_DATA$GPS.Horizontal.Dilution,
                         OUT = COLLAR_DATA$OUT,
                         Road = COLLAR_DATA$Road)

#subset to the 23 adult range-resident individuals
anteater_data <- COLLAR_DAT[which(COLLAR_DAT$ID %in%
                                    c("Alexander", "Annie", "Anthony", "Beto", "Bumpus",
                                      "Cate", "Christoffer","Elaine", "Hannah","Jackson",
                                      "Jane","Kyle", "Larry", "Little_Rick", "Luigi",
                                      "Makao", "Margaret", "Maria", "Puji", "Reid", 
                                      "Rodolfo", "Sheron", "Thomas")),]

#saveRDS(anteater_data, file = "data/anteater_data.rds")
anteater_data <- readRDS("data/anteater_data.rds")




#import supplementary data containing biological information
DATA_META <- read.csv("data/Anteater_Results_Final.csv")
DATA_META$ID[DATA_META$ID == "Little Rick"] <- "Little_Rick"

#subset biological data from supplementary data for 23 individuals
bio_data <- DATA_META[which(DATA_META$ID%in%
                              c("Alexander", "Annie", "Anthony", "Beto", "Bumpus",
                                "Cate", "Christoffer","Elaine", "Hannah","Jackson",
                                "Jane","Kyle", "Larry", "Little_Rick", "Luigi",
                                "Makao", "Margaret", "Maria", "Puji", "Reid", 
                                "Rodolfo", "Sheron", "Thomas")),c(1:5)]
#add site location 
bio_data$Site[bio_data$Road == "MS-040"] <- 1
bio_data$Site[bio_data$Road == "BR_267"] <- 2

#add bio data to anteater data
anteater_data <- merge(anteater_data, bio_data, all = TRUE)

#convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry(anteater_data)

#import movement models
load("data/anteater_fit.rda")

#import home-range estimates
load("data/anteater_AKDE.rda")





#............................................................

output_directory <- "data/home_range/shp"
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

#save 95% range estimate as shapefile:
for (name in names(AKDE)) {
  shp_file <- file.path(output_directory, paste0(name))
  writeVector(AKDE[[name]], filename = shp_file, filetype="ESRI Shapefile", 
              level.UD=0.95, level=0.95)
}




#...................................................................

# Import 95% home range estimate shapefiles

# Set folder path containing the exported subfolders
folder_path <- "data/home_range/shp"

# Load .shp files from subfolders
shp.dir <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

#Read each shapefile into a list
shp.files <- lapply(shp.dir, st_read)
names(shp.files) <- names(AKDE)

# Combine all the shapefiles into a single sf object
#hr.shp = dplyr::bind_rows(shp.files)
hr.shp <- do.call(rbind, shp.files)

# Extract/subset 95% est shapefiles only based on the text "95% est"
library(stringr)
hr.shp <- hr.shp[str_detect(hr.shp$name, "est"),]
# hr.shp <- hr.shp[grepl("95% est", hr.shp$name), ]
rownames(hr.shp) <- NULL

# #save extracted 95% hr shapefile
# st_write(hr.shp, dsn = 'data/home_range/95_HR_estimate', 
#           driver = 'ESRI Shapefile', append=FALSE)




#.......................................
#import 95% hr shapefile
hr.shp <- st_read('data/home_range/95_HR_estimate/95_HR_estimate.shp')

#convert sf dataframe object to dataframe object
dat.hr <- as.data.frame(hr.shp)

#Get HR results (units = square km)
HR_size <- data.frame()
for (i in 1:length(AKDE)) {
  summary <- summary(AKDE[[i]])$CI
  #  summary_df <- data.frame(individual.local.identifier = names(AKDES)[i], summary)
  HR_size <- rbind(HR_size, summary)
}
names(HR_size)[1] <- "HR_low"
names(HR_size)[2] <- "HR_est"
names(HR_size)[3] <- "HR_high"
rownames(HR_size) <- NULL

dat.hr <- cbind(dat.hr, HR_size)

#correct mismatch column name entries
names(dat.hr)[1] <- "ID"

#correct mismatch ID entries dropping 95% est from the name, extract text before 9 including the space in front of the 9
dat.hr$ID <- sub("^(.*?)\\s*9.*", "\\1", dat.hr$ID)

#correct mismatch ID entries 
dat.hr$ID[dat.hr$ID == "Larry 267"] <- "Larry"
dat.hr$ID[dat.hr$ID == "Little Rick"] <- "Little_Rick"

#add biological information to home range dataframe
dat.hr <- merge(dat.hr, bio_data, all = TRUE)

#saveRDS(dat.hr, file = "data/home_range/dat_hr.rds")
dat.hr <- ("data/home_range/dat_hr.rds")




#............................................................
# Is weight a factor in home-range size?

#compare model with and without weight as a variable
HR_test <- glmmTMB(HR_est ~ Weight + (1|Site), 
                    family = Gamma(link = "logit"), data = dat.hr)
HR_test2 <- glmmTMB(HR_est ~ 1 + (1|Site), 
                     family = Gamma(link = "logit"), data = dat.hr)

HR_test_results <- anova(HR_test, HR_test2)
HR_test_results

#calculate weight impact via likelihood ratio test
HR_test_pvalue <- round(HR_test_results$`Pr(>Chisq)`[2], 2)
HR_test_pvalue


