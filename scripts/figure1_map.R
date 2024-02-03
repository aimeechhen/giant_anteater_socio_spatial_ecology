

# Figure 1: Map

#............................................................
# Base map ----
#............................................................

#get bounding box for the geographic location of a city 
#bbox <- getbb("Campo Grande Brazil")
bbox <- getbb("Nova Alvorada do Sul Brazil")

#add OSM features for roads
roads <- add_osm_feature(opq(bbox), key = "highway", 
                         value = c("trunk", "motorway", 
                                   "primary", "secondary", "tertiary"))
#retrieve OSM data as an sf object
roads <- osmdata_sf(roads)

#create an overpass query for medium/small streets
secondary_roads <- add_osm_feature(opq(bbox), key = "highway", 
                                   value = c("residential",
                                             "living_street",
                                             "unclassified",
                                             "service",
                                             "footway",
                                             "unpaved",
                                             "motorway_link",
                                             "trunk_link",
                                             "primary_link",
                                             "secondary_link",
                                             "tertiary_link"))
#retrieve OSM data as an sf object
secondary_roads <- osmdata_sf(secondary_roads)

#create an overpass query for medium/small streets
dirt_roads <- add_osm_feature(opq(bbox), key = "highway", 
                              value = c("track"))
#retrieve OSM data as an sf object
dirt_roads <- osmdata_sf(dirt_roads)

#create an overpass query for rivers
rivers <- add_osm_feature(opq(bbox), key = "waterway", 
                          value = c("river"))
#retrieve OSM data as an sf object
rivers <- osmdata_sf(rivers)

#create an overpass query for streams
streams <- add_osm_feature(opq(bbox), key = "waterway", 
                           value = c("stream"))
#retrieve OSM data as an sf object
streams <- osmdata_sf(streams)

#............................................................
# Raster data ----
#............................................................

#import raster data
pasture <- terra::rast("data/map/pasture.TIF")
pasture[pasture == 0] <- NA
pasture[pasture == 1] <- 1

planted_forest <- terra::rast("data/map/planted_forest.TIF")
planted_forest[planted_forest == 0] <- NA
planted_forest[planted_forest == 1] <- 3

native_forest <- terra::rast("data/map/native_forest.TIF")
native_forest[native_forest == 0] <- NA
native_forest[native_forest == 1] <- 2

#............................................................
# Map ----
#............................................................

#add biological data to GPS dataframe
GPS_df <- left_join(GPS_df, bio_df, by = "ID")

#convert the GPS data into a sf object
gps_sf <- st_as_sf(GPS_df, coords = c("GPS.Longitude", "GPS.Latitude"), 
                   crs = 4326)

#find the extent of the GPS coordinates
gps_sf_ext <- ext(gps_sf)

#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
-53.79925 - 0.01
-53.474271 + 0.01
-21.772054 - 0.01
-21.08363 + 0.01

#............................................................
# Plot map ----
#............................................................

figure1a_map <-
  ggplot() +
  geom_spatraster(data = native_forest, aes(fill = native_forest), na.rm = T) +
  geom_spatraster(data = planted_forest, aes(fill = planted_forest), na.rm = T) +
  geom_spatraster(data = pasture, aes(fill = pasture), na.rm = T) +
  scale_fill_gradientn(colours = alpha(c("#eddea4", "#84a98c", "#52796f"), 0.7),
                       na.value = "transparent") +
  geom_sf(data = rivers$osm_lines,
          inherit.aes = FALSE,
          color = "#2c7fb8",
          lwd = 0.3) +
  geom_sf(data = streams$osm_lines,
          inherit.aes = FALSE,
          color = "#2c7fb8",
          lwd = 0.2) +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "#212529",
          lwd = 0.3) +
  geom_sf(data = secondary_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#6c757d",
          lwd = 0.2) +
  geom_sf(data = dirt_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#a68a64",
          lwd = 0.2) +
  geom_sf(data = gps_sf, aes(color = Sex),
          size = 0.05, alpha = 0.5, pch = 16) +
  ggtitle("A") +
  scale_color_manual(values = c('#004488', '#A50026'), breaks = c('Male', 'Female')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=5, family = "sans"),
        legend.position="none",
        panel.background = element_rect(fill = "#e9ecef"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  coord_sf(xlim = c(-53.80925, -53.46427),
           ylim = c(-21.78205, -21.07363))

ggsave(figure1a_map, 
       file="figures/individual figures/figure1a_map.png",
       width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent")


#............................................................
# Site 1 ----
#............................................................

#subset site 1 GPS data and create a new dataframe 
GPS_site1 <- GPS_df[GPS_df$Site == 1,]

#convert the GPS data into a sf object
gps_sf1 <- st_as_sf(GPS_site1, coords = c("GPS.Longitude", "GPS.Latitude"), 
                    crs = 4326)

#find the extent of the GPS coordinates
gps_sf_ext1 <- ext(gps_sf1)

#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
-53.79925 - 0.01
-53.67522 + 0.01
-21.19662 - 0.01
-21.08363 + 0.01

#............................................................
#find the extent of the raster
pasture_ext1 <- ext(pasture)
#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
pasture_ext1[1] <- -53.79925 - 0.1
pasture_ext1[2] <- -53.67522 + 0.1
pasture_ext1[3] <- -21.19662 - 0.1
pasture_ext1[4] <- -21.08363 + 0.1
#crop the raster to the extent of the data
pasture_crop1 <- crop(pasture, pasture_ext1)

#.....
#find the extent of the raster
planted_forest_ext1 <- ext(planted_forest)
#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
planted_forest_ext1[1] <- -53.79925 - 0.1
planted_forest_ext1[2] <- -53.67522 + 0.1
planted_forest_ext1[3] <- -21.19662 - 0.1
planted_forest_ext1[4] <- -21.08363 + 0.1
#crop the raster to the extent of the data
planted_forest_crop1 <- crop(planted_forest, planted_forest_ext1)

#.....
#find the extent of the raster
native_forest_ext1 <- ext(native_forest)
#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
native_forest_ext1[1] <- -53.79925 - 0.1
native_forest_ext1[2] <- -53.67522 + 0.1
native_forest_ext1[3] <- -21.19662 - 0.1
native_forest_ext1[4] <- -21.08363 + 0.1
#crop the raster to the extent of the data
native_forest_crop1 <- crop(native_forest, native_forest_ext1)

#............................................................
# Site 2 ----
#............................................................

#subset site 2 GPS data and create a new dataframe 
GPS_site2 <- GPS_df[GPS_df$Site == 2,]

#convert the GPS data into a sf object
gps_sf2 <- st_as_sf(GPS_site2, coords = c("GPS.Longitude", "GPS.Latitude"), 
                    crs = 4326)

#find the extent of the GPS coordinates
gps_sf_ext2 <- ext(gps_sf2)

#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
-53.633241 - 0.01
-53.474271 + 0.01
-21.772054 - 0.01
-21.613568 + 0.01

#.....
#find the extent of the raster
pasture_ext2 <- ext(pasture)
#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
pasture_ext2[1] <- -53.633241 - 0.1
pasture_ext2[2] <- -53.474271 + 0.1
pasture_ext2[3] <- -21.772054 - 0.1
pasture_ext2[4] <- -21.613568 + 0.1
#crop the raster to the extent of the data
pasture_crop2 <- crop(pasture, pasture_ext2)

#.....
#find the extent of the raster
planted_forest_ext2 <- ext(planted_forest)
#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
planted_forest_ext2[1] <- -53.633241 - 0.1
planted_forest_ext2[2] <- -53.474271 + 0.1
planted_forest_ext2[3] <- -21.772054 - 0.1
planted_forest_ext2[4] <- -21.613568 + 0.1
#crop the raster to the extent of the data
planted_forest_crop2 <- crop(planted_forest, planted_forest_ext2)

#.....
#find the extent of the raster
native_forest_ext2 <- ext(native_forest)
#calculate values for the bounding box that are slightly larger than the extent values of the GPS data
native_forest_ext2[1] <- -53.633241 - 0.1
native_forest_ext2[2] <- -53.474271 + 0.1
native_forest_ext2[3] <- -21.772054 - 0.1
native_forest_ext2[4] <- -21.613568 + 0.1
#crop the raster to the extent of the data
native_forest_crop2 <- crop(native_forest, native_forest_ext2)

#............................................................
# Plot site 1 ----
#............................................................

figure1b_map_site1 <-
  ggplot() +
  geom_spatraster(data = native_forest_crop1, aes(fill = native_forest), na.rm = T) +
  geom_spatraster(data = planted_forest_crop1, aes(fill = planted_forest), na.rm = T) +
  geom_spatraster(data = pasture_crop1, aes(fill = pasture), na.rm = T) +
  scale_fill_gradientn(colours = alpha(c("#eddea4", "#84a98c", "#52796f"), 0.7),
                       na.value = "transparent") +
  geom_sf(data = rivers$osm_lines,
          inherit.aes = FALSE,
          color = "#2c7fb8",
          lwd = 0.3) +
  geom_sf(data = streams$osm_lines,
          inherit.aes = FALSE,
          color = "#2c7fb8",
          lwd = 0.2) +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "#212529",
          lwd = 0.3) +
  geom_sf(data = secondary_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#6c757d",
          lwd = 0.2) +
  geom_sf(data = dirt_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#a68a64",
          lwd = 0.2) +
  geom_sf(data = gps_sf1, aes(color = Sex),
          size = 0.05, alpha = 0.5, pch = 16) +
  ggtitle("B") +
  scale_color_manual(values = c('#004488', '#A50026'), breaks = c('Male', 'Female')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=5, family = "sans"),
        legend.position="none",
        panel.background = element_rect(fill = "#e9ecef"),
        plot.background = element_rect(fill = "transparent", color = NA), 
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) + #top, right, bot, left
  coord_sf(xlim = c(-53.80925, -53.66522),
           ylim = c(-21.20662, -21.07363))

ggsave(figure1b_map_site1, 
       file="figures/individual figures/figure1b_map_site1.png",
       width = 6, height = 6, units = "in", dpi = 600, bg = "transparent")

#............................................................
# Plot site 2 ----
#............................................................

figure1c_map_site2 <-
  ggplot() +
  geom_spatraster(data = native_forest_crop2, aes(fill = native_forest), na.rm = T) +
  geom_spatraster(data = planted_forest_crop2, aes(fill = planted_forest), na.rm = T) +
  geom_spatraster(data = pasture_crop2, aes(fill = pasture), na.rm = T) +
  scale_fill_gradientn(colours = alpha(c("#eddea4", "#84a98c", "#52796f"), 0.7),
                       na.value = "transparent") +
  geom_sf(data = rivers$osm_lines,
          inherit.aes = FALSE,
          color = "#2c7fb8",
          lwd = 0.3) +
  geom_sf(data = streams$osm_lines,
          inherit.aes = FALSE,
          color = "#2c7fb8",
          lwd = 0.2) +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "#212529",
          lwd = 0.3) +
  geom_sf(data = secondary_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#6c757d",
          lwd = 0.2) +
  geom_sf(data = dirt_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#a68a64",
          lwd = 0.2) +
  geom_sf(data = gps_sf2, aes(color = Sex),
          size = 0.05, alpha = 0.5, pch = 16) +
  ggtitle("C") +
  scale_color_manual(values = c('#004488', '#A50026'), breaks = c('Male', 'Female')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=5, family = "sans"),
        legend.position="none",
        panel.background = element_rect(fill = "#e9ecef"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) + #top, right, bot, left
  coord_sf(xlim = c(-53.64324, -53.46427),
           ylim = c(-21.78205, -21.60357))

ggsave(figure1c_map_site2, 
       file="figures/individual figures/figure1c_map_site2.png",
       width = 6, height = 6, units = "in", dpi = 600, bg = "transparent")

#............................................................
# multi-panel ----
#............................................................

figure1_map_sites <- ggarrange(figure1b_map_site1, figure1c_map_site2, 
                               ncol=1)

figure1 <- grid.arrange(figure1a_map,
                        figure1_map_sites,
                        ncol = 2)

ggsave(figure1_map_sites, 
       file="figures/individual figures/figure1_map_sites.png",
       width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent")

ggsave(figure1, 
       file="figures/figure1.png",
       width = 6, height = 6, units = "in", dpi = 600, bg = "transparent")
