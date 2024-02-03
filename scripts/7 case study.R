
# Case study 
# Margaret and Thomas

#............................................................
# Home range overlap ----
#............................................................

#home range size
AKDE_thomas <- AKDE["Thomas"]
AKDE_margaret <- AKDE["Margaret"]

#calculate mean home-range sizes for Thomas
meta(AKDE_thomas)

#calculate mean home-range sizes for Margaret
meta(AKDE_margaret)

#home range overlap
round(proximity_identified_pairs_df[11,]$overlap_low, 2)
round(proximity_identified_pairs_df[11,]$overlap_est, 2)
round(proximity_identified_pairs_df[11,]$overlap_high, 2)

#............................................................
# Proximity, encounters, correlated movement
#............................................................

#calculate proximity ratio
round(proximity_identified_pairs_df$proximity_low[proximity_identified_pairs_df$pair_ID_number == 11], 2)
round(proximity_identified_pairs_df$proximity_est[proximity_identified_pairs_df$pair_ID_number == 11], 2)
round(proximity_identified_pairs_df$proximity_high[proximity_identified_pairs_df$pair_ID_number == 11], 2)

#calculate distances (measurements are in meters, convert them to km)
round(mean(distance_pairs_df$est[distance_pairs_df$pair_ID_number == 11])/1000, 2)
round(min(distance_pairs_df$est[distance_pairs_df$pair_ID_number == 11])/1000, 2)
round(max(distance_pairs_df$est[distance_pairs_df$pair_ID_number == 11])/1000, 2)

#.....

#investigate when encounter events occurred

#subset distance data
distance_pair11 <- distance_pairs_df[distance_pairs_df$pair_ID_number == 11,]

#visualization of the distances between the two over time
ggplot() +
  geom_line(data = distance_pair11,
            aes(y = est, x = timestamp), 
            size = 0.15) +
  xlab("") +
  ylab("Distance (m)") +
  scale_x_datetime(labels = scales::date_format("%b %Y"))

#clustering in late October is observed
distance_pair11$month <- format(distance_pair11$timestamp, "%m")
distance_pair11$day <- format(distance_pair11$timestamp, "%d")
distance_pair11$date <- format(distance_pair11$timestamp, "%Y-%m-%d")

#subset October data with a threshold of 15m to take a closer look
oct <- distance_pair11[distance_pair11$month == 10,]
oct <- oct[oct$est < 15,]
oct$month <- format(oct$timestamp, "%b")
oct$date <- format(oct$timestamp, "%b %d")

#when did the encounter events occur in October?
table(oct$day)

#visualization of events
ggplot() +
  geom_line(data = oct,
            aes(y = est, x = timestamp, 
            ), size = 0.15) +
  xlab("") +
  ylab("Distance (m)")

#calculate the mean 95% CI correlated movement
round(mean(cm_pair11$etaTot.CI.Low), 2)
round(mean(cm_pair11$etaTot.MLE), 2)
round(mean(cm_pair11$etaTot.CI.Upp), 2)

#.....

#Investigate December 2018 encounters for Arnaud. 
#Margaret was seen with a pup 13,14,15/05/2019

#subset December data with a threshold of 15m to take a closer look
dec <- distance_pair11[distance_pair11$month == 12,]
dec <- dec[dec$est < 15,]
dec$month <- format(dec$timestamp, "%b")
dec$date <- format(dec$timestamp, "%b %d")

#when did the encounter events occur in December?
table(dec$day)