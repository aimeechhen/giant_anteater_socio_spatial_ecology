

# Script description: calculate population autocorrelated kernel density home-range estimates for 3 sites

#Prior to estimating population home range areas, need to get UD of the animals, i.e. estimate their individual home range via akde() which creates the UDs that you need.

#Then, to estimate the population level based on site, group the individuals into their sites

#Now, you can fit the pkdes for each site


library(ctmm)
library(tidyverse)

#............................................................
# Data ----
#............................................................

#import data, cleaned giant anteater GPS tracking data, containing no outliers
COLLAR_DATA <- read.csv("data/anteater/Anteaters_NoOutliers.csv")

#correct mismatch ID entries
COLLAR_DATA$ID[COLLAR_DATA$ID == "Larry 267"] <- "Larry"
COLLAR_DATA$ID[COLLAR_DATA$ID == "Little Rick"] <- "Little_Rick"
COLLAR_DATA$ID[COLLAR_DATA$ID == "Nancy 05"] <- "Nancy"
COLLAR_DATA$ID[COLLAR_DATA$ID == "Jennifer 262"] <- "Jennifer"
COLLAR_DATA$ID[COLLAR_DATA$ID == "Phoenix 2"] <- "Phoenix"


COLLAR_DAT <- data.frame(timestamp = COLLAR_DATA$timestamp,
                         ID = COLLAR_DATA$ID,
                         GPS.Latitude = COLLAR_DATA$GPS.Latitude,
                         GPS.Longitude = COLLAR_DATA$GPS.Longitude,
                         GPS.Horizontal.Dilution = COLLAR_DATA$GPS.Horizontal.Dilution,
                         OUT = COLLAR_DATA$OUT,
                         Road = COLLAR_DATA$Road)

#............................................................
# Bio data ----

#import supplementary data containing biological information
DATA_META <- read.csv("data/anteater/Anteater_Results_Final.csv")
DATA_META$ID[DATA_META$ID == "Little Rick"] <- "Little_Rick"

#subset biological data from supplementary data for 23 individuals
bio_data <- DATA_META[c(1:5)]
#add site location 
bio_data$Site[bio_data$Road == "BR262"] <- "A"
bio_data$Site[bio_data$Road == "BR_267"] <- "B"
bio_data$Site[bio_data$Road == "MS-040"] <- "C"

#subset to based on the 38 individuals
ids <- bio_data$ID
anteater_data <- COLLAR_DAT[which(COLLAR_DAT$ID %in% ids),]

#add bio data to anteater data
anteater_data <- merge(anteater_data, bio_data, all = TRUE)
anteater_data <- relocate(anteater_data, Road, .after = Weight)

# save(anteater_data, file = "data/anteater/anteater_data_all.rda")
load("data/anteater/anteater_data_all.rda")

#............................................................
# Movement models ----
#............................................................

#convert dataset to a telemetry object to fit movement models
DATA_TELEMETRY <- as.telemetry(anteater_data)



#create guesstimate non-interactively
GUESS <- lapply(DATA_TELEMETRY[1:38], function(b) ctmm.guess(b,interactive=FALSE) )

START <- Sys.time()

#fit movement models
FIT <- lapply(1:38, function(i) ctmm.select(DATA_TELEMETRY[[i]],GUESS[[i]]) )
names(FIT) <- names(DATA_TELEMETRY[1:38])

END <- Sys.time()

# save(FIT, file = "data/home_range/anteater_fit_all.rda")
load("data/home_range/anteater_fit_all.rda")

#............................................................
# Estimating home range areas (akde UD) ----
#............................................................

#calculate AKDE home range estimates based on the best fit model, creates in UD (aligned) format/object
START2 <- Sys.time()
AKDES <- akde(DATA_TELEMETRY[1:38],FIT)
overlap(AKDES)

#save AKDE home range estimations
save(AKDES, file = "data/home_range/anteater_akde_all.rda")
load("data/home_range/anteater_akde_all.rda")

END <- Sys.time()

#............................................................
# Estimating population home range areas ----
#............................................................

# pkde(data,UD,kernel="individual",weights=FALSE,ref="Gaussian",...)

#Prior to estimating population home range areas, need to get UD of the animals, i.e. estimate their individual home range via akde() which creates the UDs that you need.

#............................................
#Then, to estimate the population level based on site, group the individuals into their sites

#subset based on site location
site_a <- anteater_data[anteater_data$Road == "BR262",] #7
site_b <- anteater_data[anteater_data$Road == "BR_267",] #14
site_c <- anteater_data[anteater_data$Road == "MS-040",] #17
#convert dataset to a telemetry object
SITE_A <- as.telemetry(site_a)
SITE_B <- as.telemetry(site_b)
SITE_C <- as.telemetry(site_c)

#Extract individuals from the AKDES list based on the list of individuals at each site
AKDES_A <- AKDES[names(AKDES) %in% names(SITE_A)]
AKDES_B <- AKDES[names(AKDES) %in% names(SITE_B)]
AKDES_C <- AKDES[names(AKDES) %in% names(SITE_C)]

#............................................
#Now, you can fit the pkdes for each site

START3 <- Sys.time()
PKDE_A <- pkde(SITE_A[1:7],AKDES_A)
END3 <- Sys.time()

PKDE_B <- pkde(SITE_B[1:14],AKDES_B)
names(PKDE_A) <- names(SITE_A[1:14])

PKDE_C <- pkde(SITE_C[1:17],AKDES_C)
names(PKDE_A) <- names(SITE_A[1:17])

save(PKDE_A, file = "data/home_range/pkde/anteater_pkde_A.rda")
save(PKDE_B, file = "data/home_range/pkde/anteater_pkde_B.rda")
save(PKDE_C, file = "data/home_range/pkde/anteater_pkde_C.rda")
load("data/home_range/pkde/anteater_pkde_A.rda")
load("data/home_range/pkde/anteater_pkde_B.rda")
load("data/home_range/pkde/anteater_pkde_C.rda")

#............................................................
# Save home range estimates: shp files ----
#............................................................

# Save 95% home range estimate as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: if you run into an error, update the ctmm package. writeShapefile() is no longer in use. Use writeVector(), package switched from the depreciated rgdal package to the terra package

dir.create("data/home_range/shp", recursive = TRUE)

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp.path, filetype="ESRI Shapefile",
              level.UD=0.95, level=0.95, overwrite = TRUE)
}



#............................................................
# Save population home range estimates: shp files ----
#............................................................

# Save 95% population home range estimate as shapefile:

dir.create("data/home_range/shp_pkde", recursive = TRUE)
dir.create("data/home_range/shp_pkde/pkde_a", recursive = TRUE)
dir.create("data/home_range/shp_pkde/pkde_b", recursive = TRUE)
dir.create("data/home_range/shp_pkde/pkde_c", recursive = TRUE)

writeVector(PKDE_A, "data/home_range/shp_pkde/pkde_a", 
            filetype="ESRI Shapefile", level.UD=0.95, level=0.95, overwrite = TRUE)

writeVector(PKDE_B, "data/home_range/shp_pkde/pkde_b",
            filetype="ESRI Shapefile", level.UD=0.95, level=0.95, overwrite = TRUE)

writeVector(PKDE_C, "data/home_range/shp_pkde/pkde_c",
            filetype="ESRI Shapefile", level.UD=0.95, level=0.95, overwrite = TRUE)



library(sf)


# Plot to check

png(file = "figures/pkde/pkde_a.png", width = 6.86, height = 6, units = "in", res = 600)
plot(SITE_A, UD = PKDE_A)
title("Site A (BR262)", adj = 0)
dev.off()

png(file = "figures/pkde/pkde_b.png", width = 6.86, height = 6, units = "in", res = 600)
plot(SITE_B, UD = PKDE_B)
title("Site B (BR267)", adj = 0)
dev.off()

png(file = "figures/pkde/pkde_c.png", width = 6.86, height = 6, units = "in", res = 600)
plot(SITE_C, UD = PKDE_C)
title("Site C (MS040)", adj = 0)
dev.off()