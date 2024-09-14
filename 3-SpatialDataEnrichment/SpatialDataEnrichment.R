################################# LIBRARIES ####################################
library(osmdata)    
library(sf)         
library(geosphere)  
library(dplyr)      
library(ggplot2)   
library(readxl)     

################################# DATA IMPORT ##################################
data <- read_xlsx("/Users/michellehajduk/idealista_properties_cleaned.xlsx")


############################ QUERY TRAIN STATIONS ##############################
# Define bounding boxes for various regions in Spain for querying
bbox_spain <- c(-9.5, 35.5, 3.5, 43.5)               
bbox_canary_islands <- c(-18.2, 27.6, -13.3, 29.5) 
bbox_balearic_islands <- c(1.3, 38.5, 4.3, 40.5)   

# Query for train stations in Spain using the bounding box
query <- opq(bbox = bbox_spain) %>%
  add_osm_feature(key = 'railway', value = 'station') %>%
  osmdata_sf()

# Query for bus stops in the Canary Islands
query_canary_island <- opq(bbox = bbox_canary_islands) %>%
  add_osm_feature(key = 'highway', value = 'bus_stop') %>%
  osmdata_sf()

# Query for bus stops in the Balearic Islands
query_balearic_island <- opq(bbox = bbox_balearic_islands) %>%
  add_osm_feature(key = 'highway', value = 'bus_stop') %>%
  osmdata_sf()

# Extract points for train stations and bus stops
stations_spain_sf <- query$osm_points
stations_ci_sf <- query_canary_island$osm_points
stations_bi_sf <- query_balearic_island$osm_points

# Combine all station data into a single dataframe
stations_sf <- bind_rows(stations_spain_sf, stations_ci_sf, stations_bi_sf)

# Plot train stations on a map
ggplot(data = stations_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Train/Bus Stations in Spain", 
       x = "Longitude", 
       y = "Latitude")

# Convert OSM data to data frame and add coordinates
stations_df <- as.data.frame(stations_sf) %>%
  dplyr::mutate(lon = st_coordinates(stations_sf)[,1],
                lat = st_coordinates(stations_sf)[,2])

# Save train station data to an Excel file
writexl::write_xlsx(stations_df, "data_TrainStations_spain.xlsx")

# Add a column for distances to the nearest train station, initialize with NA
if (!"distanceTrainStation" %in% names(data)) {
  data$distanceTrainStation <- NA
}

# Calculate distance to the nearest train station for each property
for (i in 1:nrow(data)) {
  your_coords <- c(lat = data$lat[i], lon = data$lon[i])
  coordinates <- cbind(stations_df$lon, stations_df$lat)
  
  if (nrow(stations_df) > 0 && nrow(coordinates) > 0) {
    your_point <- c(your_coords["lon"], your_coords["lat"])
    distances <- distHaversine(your_point, coordinates)
    
    if (length(distances) > 0) {
      nearest_station_index <- which.min(distances)
      data$distanceTrainStation[i] <- distances[nearest_station_index]
    } else {
      data$distanceTrainStation[i] <- NA
    }
  } else {
    data$distanceTrainStation[i] <- NA
  }
}

############################# QUERY HOSPITALS ###############################
# Define bounding boxes for different regions to find hospitals
bbox_north <- c(-9.5, 42.2, 3.5, 43.5)
bbox_south <- c(-9.5, 36.9, 3.5, 37.5)
bbox_east <- c(-6.2, 35.5, 3.5, 43.5)
bbox_west <- c(-9.5, 35.5, -6.2, 43.5)
bbox_center <- c(-6.2, 39.0, -3.0, 41.5)

# Query for hospitals in each region
query_hospitals_north <- opq(bbox = bbox_north) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf()

query_hospitals_south <- opq(bbox = bbox_south) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf()

query_hospitals_east <- opq(bbox = bbox_east) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf()

query_hospitals_west <- opq(bbox = bbox_west) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf()

query_hospitals_center <- opq(bbox = bbox_center) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf()

query_ci_hospitals <- opq(bbox = bbox_canary_islands) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf()

# Extract hospital points and combine into a single dataframe
hospitals_sf <- bind_rows(
  query_hospitals_north$osm_points,
  query_hospitals_south$osm_points,
  query_hospitals_east$osm_points,
  query_hospitals_west$osm_points,
  query_hospitals_center$osm_points,
  query_ci_hospitals$osm_points
)

# Convert OSM data to data frame and add coordinates
hospitals_df <- as.data.frame(hospitals_sf) %>%
  dplyr::mutate(lon = st_coordinates(hospitals_sf)[,1],
                lat = st_coordinates(hospitals_sf)[,2])

# Save hospital data to an Excel file
writexl::write_xlsx(hospitals_df, "data_Hospitals_spain.xlsx")

# Plot hospitals on a map
ggplot(data = hospitals_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Hospitals in Spain", 
       x = "Longitude", 
       y = "Latitude")

# Add a column for distances to the nearest hospital, initialize with NA
if (!"distanceHospitals" %in% names(data)) {
  data$distanceHospitals <- NA
}

# Calculate distance to the nearest hospital for each property
for (i in 1:nrow(data)) {
  your_coords <- c(lat = data$lat[i], lon = data$lon[i])
  coordinates <- cbind(hospitals_df$lon, hospitals_df$lat)
  
  if (nrow(hospitals_df) > 0 && nrow(coordinates) > 0) {
    your_point <- c(your_coords["lon"], your_coords["lat"])
    distances_hospitals <- distHaversine(your_point, coordinates)
    
    if (length(distances_hospitals) > 0) {
      nearest_hospital_index <- which.min(distances_hospitals)
      data$distanceHospitals[i] <- distances_hospitals[nearest_hospital_index]
    } else {
      data$distanceHospitals[i] <- NA
    }
  } else {
    data$distanceHospitals[i] <- NA
  }
}

############################# QUERY SCHOOLS ###############################
# Query for schools in different regions
query_schools_in_bbox <- function(bbox) {
  opq(bbox = bbox) %>%
    add_osm_feature(key = 'amenity', value = 'school') %>%
    osmdata_sf()
}

# Define bounding boxes including the Canary Islands
query_north <- query_schools_in_bbox(bbox_north)
query_south <- query_schools_in_bbox(bbox_south)
query_east <- query_schools_in_bbox(bbox_east)
query_west <- query_schools_in_bbox(bbox_west)
query_center <- query_schools_in_bbox(bbox_center)
query_canary <- query_schools_in_bbox(bbox_canary_islands)

# Extract school points and combine into a single dataframe
schools_sf <- bind_rows(
  query_north$osm_points,
  query_south$osm_points,
  query_east$osm_points,
  query_west$osm_points,
  query_center$osm_points,
  query_canary$osm_points
)

# Convert OSM data to data frame and add coordinates
schools_df <- as.data.frame(schools_sf) %>%
  dplyr::mutate(lon = st_coordinates(schools_sf)[,1],
                lat = st_coordinates(schools_sf)[,2])

# Save school data to an Excel file
writexl::write_xlsx(schools_df, "data_Schools_spain.xlsx")

# Plot schools on a map
ggplot(data = schools_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Schools in Spain", 
       x = "Longitude", 
       y = "Latitude")

# Add a column for distances to the nearest school, initialize with NA
if (!"distanceSchools" %in% names(data)) {
  data$distanceSchools <- NA
}

# Calculate distance to the nearest school for each property
for (i in 1:nrow(data)) {
  your_coords <- c(lat = data$lat[i], lon = data$lon[i])
  coordinates <- cbind(schools_df$lon, schools_df$lat)
  
  if (nrow(schools_df) > 0 && nrow(coordinates) > 0) {
    your_point <- c(your_coords["lon"], your_coords["lat"])
    distances_schools <- distHaversine(your_point, coordinates)
    
    if (length(distances_schools) > 0) {
      nearest_school_index <- which.min(distances_schools)
      data$distanceSchools[i] <- distances_schools[nearest_school_index]
    } else {
      data$distanceSchools[i] <- NA
    }
  } else {
    data$distanceSchools[i] <- NA
  }
}

# Round distances to nearest integer
data$distanceTrainStation <- ceiling(data$distanceTrainStation)
data$distanceHospitals <- ceiling(data$distanceHospitals)
data$distanceSchools <- ceiling(data$distanceSchools)

# Display the first few rows of the updated data
head(data)

# Save the final data with distances to a CSV file
write_xlsx(data, "idealista_properties_cleaned.xlsx")
