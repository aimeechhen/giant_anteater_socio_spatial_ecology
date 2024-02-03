
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
save(FIT, file = "data/anteater_fit.rda")
load("data/anteater_fit.rda")


