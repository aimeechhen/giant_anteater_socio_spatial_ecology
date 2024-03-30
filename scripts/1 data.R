
#............................................................
# Data ----
#............................................................

#import data, cleaned giant anteater GPS tracking data, containing no outliers
COLLAR_DATA <- read.csv("data/anteater/Anteaters_NoOutliers.csv")

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

#............................................................
# Bio data ----

#import supplementary data containing biological information
DATA_META <- read.csv("data/anteater/Anteater_Results_Final.csv")
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


#save GPS dataframe
# save(anteater_data, file = "data/anteater/anteater_data.rda")
load("data/anteater/anteater_data.rda")
#save biological information dataframe
# save(bio_data, file = "data/anteater/bio_data.rda")
load("data/anteater/bio_data.rda")



