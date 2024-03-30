
# Script description: fit movement models and akdes, save akde home range estimates as UD and shp files

library(ctmm)

#convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry(anteater_data)

#summary of the dataset
summary(DATA_TELEMETRY)

#visualisation of the data
plot(DATA_TELEMETRY)

#save(DATA_TELEMETRY, file = "data/anteater/telemetry_data.rda")
load("data/anteater/telemetry_data.rda")

#............................................................
# Movement models ----
#............................................................

#fit movement models
GUESS <- lapply(DATA_TELEMETRY[1:23], function(b) ctmm.guess(b,interactive=FALSE) )
FIT <- lapply(1:23, function(i) ctmm.select(DATA_TELEMETRY[[i]],GUESS[[i]]) )
names(FIT) <- names(DATA_TELEMETRY[1:23])
overlap(FIT)

#summary of the fitted model
summary(FIT)

#save movement models
#save(FIT, file = "data/anteater_fit.rda")
load("data/anteater_fit.rda")


#............................................................
# Estimating home range areas ----
#............................................................

#calculate AKDE home range estimates based on the best fit model, create aligned UDs
AKDE <- akde(DATA_TELEMETRY[1:23],FIT)
overlap(AKDE)

#save AKDE home range estimations
#save(AKDE, file = "data/anteater_akdes.rda")
load("data/anteater_akdes.rda")



#............................................................
# Save home range estimates: UD and shp files ----
#............................................................

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile


# 1. save UD (with PMF) as raster :
dir.create("data/home_range/UD", recursive = TRUE)
#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDE)) {
  UD_file <- file.path("data/home_range/UD", paste0(names(AKDE)[i], ".tif"))
  writeRaster(AKDE[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}



# 2. Save 95% range estimate as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: if you run into an error, update the ctmm package. writeShapefile() is no longer in use. Use writeVector(), package switched from the depreciated rgdal package to the terra package
dir.create("data/home_range/shp", recursive = TRUE)

for (name in names(AKDE)) {
  shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
  writeVector(AKDE[[name]], shp.path,
                 level.UD=0.95, level=0.95, overwrite = TRUE)
}
