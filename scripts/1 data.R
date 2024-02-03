

#............................................................
# Data ----
#............................................................

#import data, cleaned giant anteater GPS tracking data, containing no outliers
DATA_GPS <- read_csv("data/Anteaters_NoOutliers.csv")

#import supplementary data containing biological information
DATA_META <- read_csv("data/Anteater_Results_Final.csv")

#correct mismatch ID entries
DATA_GPS$ID[DATA_GPS$ID == "Larry 267"] <- "Larry"
DATA_GPS$ID[DATA_GPS$ID == "Little Rick"] <- "Little_Rick"

#correct mismatch ID entries
DATA_META$ID[DATA_META$ID == "Little Rick"] <- "Little_Rick"

#subset to the 23 range-resident individuals
GPS_df <- DATA_GPS[which(DATA_GPS$ID %in% c("Alexander", "Annie", "Anthony", "Beto", "Bumpus",
                                            "Cate", "Christoffer","Elaine", "Hannah","Jackson",
                                            "Jane","Kyle", "Larry", "Little_Rick", "Luigi",
                                            "Makao", "Margaret", "Maria", "Puji", "Reid", 
                                            "Rodolfo", "Sheron", "Thomas")),]

#subset to the 23 range-resident individuals
bio_df <- DATA_META[c(1:3,8:10,12,14,17,19,20,22,23,25:29,33:35,37,38),]
#subset the biological data
bio_df <- bio_df[,c(1:3,5)]

#add site location to the dataframe
bio_df$Site <- NA
bio_df$Site[bio_df$Road == "MS-040"] <- 1
bio_df$Site[bio_df$Road == "BR_267"] <- 2
bio_df

#convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry(GPS_df)

#summary of the dataset
summary(DATA_TELEMETRY)

#visualisation of the data
plot(DATA_TELEMETRY)

#save GPS dataframe
saveRDS(GPS_df, file = "data/rds/GPS_df.rds")
#save biological information dataframe
saveRDS(bio_df, file = "data/rds/bio_df.rds")
#save telemetry data
saveRDS(DATA_TELEMETRY, file = "data/rds/DATA_TELEMETRY.rds")
