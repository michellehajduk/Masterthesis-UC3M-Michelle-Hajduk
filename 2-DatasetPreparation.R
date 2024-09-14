################################# LIBRARIES ####################################
library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)
library(jsonlite)
library(CatastRo)
library(caret)
library(VIM)
library(readxl)

############################# COMBINING DATASETS ###############################
# Load all scraped CSV files from the specified directory into a single dataframe

file_paths <- list.files(
  path = "/Users/michellehajduk/Documents/UC3M/Masterthesis/Data/Properties_new", 
  pattern = "*.csv", 
  full.names = TRUE
)

# Read and combine all CSV files into one dataframe
combined_data <- file_paths %>%
  map_dfr(~ read.csv(.))

# Remove duplicate rows based on the 'url' column, keeping the first occurrence
combined_data <- combined_data %>% distinct(url, .keep_all = TRUE)
combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)

# Extract additional information from the 'title' and 'location' columns
combined_data <- combined_data %>%
  mutate(street = str_extract(title, "(?<=in\\s).+$"),
         city = ifelse(str_detect(location, ","), trimws(str_extract(location, "(?<=, ).*$")), trimws(location)),
         area = ifelse(str_detect(location, ","), trimws(str_extract(location, "^[^,]+")), NA)
  )

write_xlsx(combined_data, "combined_data.xlsx")

######################### ADDING GEOCODES ######################################
# Adding geocodes in a separately processed file now to be able to handle missing values for 'built_in'
# Filename "AddingGeocodes"

# Read the processed data with geocodes from an Excel file
property_data <- read_xlsx("/Users/michellehajduk/Documents/UC3M/Masterthesis/Data/combined_data_with_coords.xlsx")
property_data <- as.data.frame(property_data)
str(property_data)

############################### DATA PARSING ###################################
# Function to clean and parse JSON-like strings
parse_features <- function(features_string) {
  json_string <- str_replace_all(features_string, "'", "\"")
  feature_list <- fromJSON(json_string, flatten = TRUE)
  return(feature_list)
}

# Function to extract numerical data
extract_number <- function(text, pattern) {
  combined_pattern <- paste0("\\d{1,3}(?:,\\d{3})*\\s*", pattern, "(s)?")

  if (length(text) > 1) {
    result <- sapply(text, function(txt) {
      result <- str_extract(txt, combined_pattern)
      if (is.na(result)) return(NA)
      number_str <- str_extract(result, "\\d{1,3}(?:,\\d{3})*")
      if (is.na(number_str)) return(NA)
      number_str <- str_replace_all(number_str, ",", "")
      as.numeric(number_str)
    })
  } else {
    result <- str_extract(text, combined_pattern)
    if (is.na(result)) return(NA)
    number_str <- str_extract(result, "\\d{1,3}(?:,\\d{3})*")
    if (is.na(number_str)) return(NA)
    number_str <- str_replace_all(number_str, ",", "")
    as.numeric(number_str)
  }
  
  return(result)
}

# Function to extract 'Built in #' and 'Orientation xyz'
extract_built_in <- function(text) {
  str_extract(text, "(?<=Built in )\\d+")
}

# Function's to check for presence of a specific feature
extract_presence <- function(text, pattern) {
  ifelse(str_detect(text, pattern), 1, 0)
}

extract_two_presences <- function(text, pattern1, pattern2) {
  case_when(
    str_detect(text, pattern1) ~ pattern1,
    str_detect(text, pattern2) ~ pattern2,
    TRUE ~ NA
  )
}

# Function to extract specific conditions
extract_condition <- function(text) {
  condition_patterns <- c(
    "Second hand/good condition",
    "Second hand/needs renovating",
    "New housing development"
  )
  result <- str_extract(text, paste(condition_patterns, collapse = "|"))
  return(result)
}

# Function to extract orientation 
extract_orientation <- function(text) {
  pattern <- "(?<=Orientation )((?:North|South|East|West)(?:,\\s*(?:North|South|East|West))*)"
  result <- str_extract(text, pattern)
  if (is.na(result)) {
    return(NA)
  }
  return(result)
}

# Function to extract heating type and details
extract_heating_info <- function(text) {
  if (str_detect(text, "Individual heating")) {
    heating_type <- "Individual heating"
    heating_details <- str_extract(text, "(?<=Individual heating: )[^,]*")
    heating_details <- str_trim(gsub("']$", "", heating_details))
  } else if (str_detect(text, "Central heating")) {
    heating_type <- "Central heating"
    heating_details <- str_extract(text, "(?<=Central heating:  )[^,]*")
    # Remove any trailing unwanted characters like ']'
    heating_details <- str_trim(gsub("']$", "", heating_details))
    
  } else {
    heating_type <- NA
    heating_details <- NA
  }
  return(list(heating_type = heating_type, heating_details = heating_details))
}

# Function to extract the land plot size
extract_land_plot <- function(text) {
  combined_pattern <- "(?<=Land plot of )\\d{1,3}(?:,\\d{3})*"
  if (length(text) > 1) {
    result <- sapply(text, function(txt) {
      result <- str_extract(txt, combined_pattern)
      if (is.na(result)) return(NA)
      number_str <- str_replace_all(result, ",", "")
      as.numeric(number_str)
    })
  } else {
    result <- str_extract(text, combined_pattern)
    if (is.na(result)) return(NA)
    number_str <- str_replace_all(result, ",", "")
    as.numeric(number_str)
  }
  
  return(result)
}


# Function to extract kitchen status
extract_kitchen_status <- function(text) {
  ifelse(
    str_detect(text, "Furnished and with equipped kitchen") | 
      str_detect(text, "Fully equipped kitchen and unfurnished house"),
    "1",
    ifelse(
      str_detect(text, "Non equipped kitchen and unfurnished house"),
      "0",
      NA
    )
  )
}

# Function to extract the type of rental
extract_rental_type <- function(title) {
  rental_types <- c(
    "Flat / apartment", "Studio flat", "Village house", "Penthouse", 
    "Terraced house", "Duplex", "House", "Semi-detached house", 
    "Detached house", "Country home", "Estate", "Rural manor", 
    "country house", "Cortijo", "Masía", "Castle", "Rustic house",
    "Tower", "Palace"
  )
  pattern <- paste(rental_types, collapse = "|")
  result <- str_extract(title, regex(pattern, ignore_case = TRUE))
  return(result)
}

# Function to extract energy performance certificate information
extract_energy_certificate <- function(feature_list) {
  if ("Energy performance certificate" %in% names(feature_list)) {
    certificate_info <- feature_list[["Energy performance certificate"]]
    
    if (length(certificate_info) == 1) {
      return(certificate_info)
    } else if (length(certificate_info) == 2) {
      consumption <- str_trim(certificate_info[1])
      emissions <- str_trim(certificate_info[2])
      return(paste(consumption, emissions, sep = "; "))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Function to extract consumption from energy performance certificate
extract_consumption <- function(text) {
  match <- str_extract(text, "Consumption:\\s*\\n*[0-9]+[,]?[0-9]*")
  if (!is.na(match)) {
    numeric_part <- str_extract(match, "[0-9]+[,]?[0-9]*")
    consumption <- as.numeric(gsub(",", ".", numeric_part))
    return(consumption)
  } else {
    return(NA)
  }
}

# Function to classify energy efficiency consumption based on spanish government 
classify_certification <- function(consumption) {
  if (is.na(consumption)) {
    return("In process")
  } else if (consumption <= 34.1) {
    return("A")
  } else if (consumption <= 55.1) {
    return("B")
  } else if (consumption <= 85.4) {
    return("C")
  } else if (consumption <= 111) {
    return("D")
  } else if (consumption <= 136.6) {
    return("E")
  } else if (consumption <= 170.7) {
    return("F")
  } else {
    return("G")
  }
}

# Function to extract floor level
extract_floor <- function(feature) {
  floor <- str_extract(feature, "(\\d+)(?=th floor|st floor|nd floor|rd floor)")
  if (is.na(floor)) {
    floor <- ifelse(str_detect(feature, "Ground floor"), "Ground floor", floor)
    floor <- ifelse(str_detect(feature, "Basement"), "Basement", floor)
    floor <- ifelse(str_detect(feature, "Mezzanine"), "Mezzanine", floor)
  }
  return(floor)
}

# Function to extract exterior/interior
extract_exterior <- function(feature) {
  if (str_detect(feature, "interior")) {
    return(0)
  } else if (str_detect(feature, "exterior")) {
    return(1)
  } else {
    return(NA)  
  }
}

# Apply parsing and extraction functions
property_data <- property_data %>%
  mutate(
    parsed_features = lapply(features, parse_features),
    propertyType = sapply(title, extract_rental_type),
    m2_built = extract_number(features, "m² built"),
    m2_floor_area = extract_number(features, "m² floor area"),
    landPlot = extract_land_plot(features),
    bedrooms = extract_number(features, "bedroom"),
    bedrooms = ifelse(str_detect(features, "No bedroom"), 0, bedrooms),
    bathrooms = extract_number(features, "bathroom"),
    bathrooms = ifelse(str_detect(features, "No bathroom"), 0, bathrooms),
    floorLevel = sapply(features, extract_floor),
    numFloors = extract_number(features, "floors"),
    exterior = sapply(features, extract_exterior),
    built_in = sapply(features, extract_built_in),
    condition = extract_condition(features),
    orientation = sapply(features, extract_orientation),
    energy_certificate = sapply(parsed_features, extract_energy_certificate),
    consumption = sapply(energy_certificate, extract_consumption),
    energy_certification = sapply(consumption, classify_certification),
    heating_info = lapply(features, extract_heating_info),
    heatingType = sapply(heating_info, `[[`, "heating_type"),
    heatingDetails = sapply(heating_info, `[[`, "heating_details"),
    hasBalcony = extract_presence(features, "Balcony"),
    hasBalcony = ifelse(str_detect(features, "Terrace and balcony"), 1, hasBalcony),
    hasTerrace = extract_presence(features, "Terrace"),
    hasTerrace = ifelse(str_detect(features, "Terrace and balcony"), 1, hasTerrace),
    hasStoreroom = extract_presence(features, "Storeroom"),
    hasFireplace = extract_presence(features, "Fireplace"),
    hasFittedWardrobes = extract_presence(features, "Fitted wardrobes"),
    hasEquippedKitchen = sapply(features, extract_kitchen_status),
    isFurnished = extract_two_presences(features, "Furnished", "unfurnished"),
    hasGarden = extract_presence(features, "Garden"),
    hasAirConditioning = extract_presence(features, "Air conditioning"),
    hasGreenAreas = extract_presence(features, "Green areas"),
    hasParkingSpace = str_extract(features, "(?<=Parking space )[^,]*(?=\\,|$)"),
    hasParkingSpace = ifelse(is.na(hasParkingSpace), "no parking space", hasParkingSpace),
    hasParkingSpace = ifelse(str_detect(hasParkingSpace, "included in the price'"), 1, 0),
    hasLift = ifelse(str_detect(features, "lift"), "With lift", "No lift"),
    hasLift = ifelse(str_detect(hasLift, "With lift"), 1, 0),
    hasSwimmingPool = extract_presence(features, "Swimming pool")
  ) %>%
  select(-parsed_features, -heating_info, -energy_certificate, -consumption)  # Remove temporary columns

# Handling some special cases further
property_data <- property_data %>%
  mutate(
    isFurnished = ifelse(str_detect(isFurnished, "Furnished"), 1, ifelse(str_detect(isFurnished, "unfurnished"), 0, isFurnished)),
    exterior = ifelse(str_detect(propertyType, "House"), 1, exterior),
    exterior = ifelse(str_detect(propertyType, "Luxury/Historic"), 1, exterior)
  )

########################### HANDLING MISSING DATA ##############################
colSums(sapply(property_data, is.na))

# Deleting unuseful columns due to missing data
property_data <- property_data[, -which(names(property_data) %in% c("m2_floor_area", "orientation"))]

# Filling NA's logically for landPlot, numFloors, isFurnished, hasEquippedKitchen
property_data <- property_data %>%
  mutate(
    landPlot = ifelse(is.na(landPlot), 0, landPlot), 
    numFloors = ifelse(is.na(numFloors), 1, numFloors),
    isFurnished = ifelse(is.na(isFurnished), 0, isFurnished),
    hasEquippedKitchen = ifelse(is.na(hasEquippedKitchen), 0, hasEquippedKitchen)
  )

# Filling NA's by mode for floor level and exterior
property_data <- property_data %>%
  mutate(floorLevel = ifelse(numFloors > 1, "multiple", floorLevel))
property_data$floorLevel <- ifelse(property_data$propertyType %in% c("House", "Country House"), "Ground floor", property_data$floorLevel)
mode_floorLevel <- names(which.max(table(property_data$floorLevel)))
property_data$floorLevel[is.na(property_data$floorLevel)] <- mode_floorLevel

mode_exterior <- names(which.max(table(property_data$exterior)))
property_data$exterior[is.na(property_data$exterior)] <- mode_exterior

# Filling NA's with CatastRo for built in
## Geocodes need to be already included to the dataset first for this!! 
calculate_bounding_box <- function(lat, lon, buffer = 0.0005) {
  lat_min <- lat - buffer
  lat_max <- lat + buffer
  lon_min <- lon - buffer
  lon_max <- lon + buffer
  
  return(list(lat_min = lat_min, lat_max = lat_max, lon_min = lon_min, lon_max = lon_max))
}

preprocess <- function(text) {
  text <- tolower(text)  # Convert to lowercase
  text <- gsub("[^a-z0-9 ]", "", text)  # Remove punctuation
  return(text)
}

property_data$cat_reference <- NA

for (i in 1:nrow(property_data)) {
  if (is.na(property_data$built_in[i])){
    Sys.sleep(5)
    tryCatch({
      # Fetch property details near the given coordinates
      cat_ref_list <- catr_ovc_get_rccoor_distancia(
        lat = property_data$lat[i],
        lon = property_data$lon[i],
        srs = 4326
      )
      
      # Check if the result is not NULL and has the required columns
      if (is.null(cat_ref_list) || !all(c("address", "refcat") %in% names(cat_ref_list))) {
        print(paste("Query did not produce results or returned an unexpected format for coordinates:", data$lat[i], data$lon[i]))
        next 
      }
      
      # Calculate the bounding box for the given latitude and longitude
      boundingbox <- calculate_bounding_box(property_data$lat[i], property_data$lon[i])
      
      # Fetch building information within the bounding box
      wfs_get_buildings <- catr_wfs_get_buildings_bbox(
        c(boundingbox$lon_min, boundingbox$lat_min, boundingbox$lon_max, boundingbox$lat_max), 
        what = "building", 
        srs = 4326, 
        verbose = FALSE
      )
      
      # Check if wfs_get_buildings is not NULL and has data
      if (is.null(wfs_get_buildings) || nrow(wfs_get_buildings) == 0) {
        print("No spatial layers found or building data returned.")
        next
      }
      
      # Preprocess the addresses for matching
      addresses_clean <- preprocess(cat_ref_list$address)
      address_clean <- preprocess(data$street[i])
      
      if (length(addresses_clean) > 0) {
        distances <- stringdist(address_clean, addresses_clean, method = "lv")
        closest_index <- which.min(distances)
        cat_ref <- cat_ref_list$refcat[closest_index]
      } else {
        print("No addresses found for cleaning.")
        next
      }
      
      # Filter the building data based on the closest cadastral reference
      highlight_building <- wfs_get_buildings %>%
        filter(reference == !!cat_ref)
      
      if (!is.null(highlight_building) && nrow(highlight_building) > 0) {
        property_data$cat_reference[i] <- cat_ref
        property_data$built_in[i] <- substr(highlight_building$beginning, 1, 4)
      } else {
        print(paste("No matching building found for reference:", cat_ref))
      }
      
    }, error = function(e) {
      if (grepl("403 Forbidden", e$message)) {
        stop("HTTP 403 Forbidden error encountered. Stopping the loop.")
      } else {
        print(paste("Error occurred:", e$message))
      }
    })
  }
}


# Filling NA's with kNN for correlated features
property_data$energy_certification <- ifelse(property_data$energy_certification %in% c("In process"),
                                             NA, property_data$energy_certification) # To be able to fill "In process" with certification level A-G

## Check correlation
contingency_table1 <- table(property_data$energy_certification, property_data$heatingDetails)
chisq.test(contingency_table1)

contingency_table2 <- table(property_data$energy_certification, property_data$built_in)
chisq.test(contingency_table2)

## Remove rows where all specified columns are NA
columns_to_check <- c("built_in", "energy_certification", "heatingDetails")
property_data <- property_data[!apply(property_data[columns_to_check], 1, function(x) all(is.na(x))), ]

## Apply kNN
property_data <- kNN(property_data, variable = c("heatingType", "heatingDetails", "energy_certification", "built_in"), k = 5, imp_var = FALSE)

colSums(sapply(property_data, is.na))

########################### CATEGORIZING FEATURES ##############################
# Categorizing property types
categorize_property <- function(property_type) {
  if (property_type %in% c("Detached house", "Semi-detached house", "Terraced house", "House")) {
    return("House")
  } else if (property_type %in% c("country house", "Country home", "Rustic house", "Village house", "Cortijo", "Masía")) {
    return("Country House")
  } else if (property_type %in% c("Flat / apartment", "Duplex", "Penthouse", "Studio flat")) {
    return("Apartment")
  } else if (property_type %in% c("Castle", "Estate", "Palace", "Rural manor", "Tower")) {
    return("Luxury/Historic")
  } else {
    return("Unknown") # for any property types not covered
  }
}

property_data <- property_data %>%
  mutate(propertyType = sapply(propertyType, categorize_property))

## Cleaning since categorization identified country houses as unknown
property_data$propertyType[which(property_data$propertyType == "Unknown")] <- "Country House"

# Categorizing floor level
property_data$floorLevel<- ifelse(property_data$floorLevel %in% c("multiple"),
                                  mode_floorLevel, property_data$floorLevel)

property_data$floorLevel <- ifelse(property_data$floorLevel %in% c("Basement", "Ground floor"),
                                   "Ground floor",
                                   ifelse(property_data$floorLevel %in% c("Mezzanine"), 
                                          "1 or 2",
                                          ifelse(property_data$floorLevel %in% c("1","2"),
                                                 "1 or 2",
                                                 ifelse(property_data$floorLevel %in% c("3","4"),
                                                        "3 or 4",
                                                        ifelse(property_data$floorLevel %in% c("5","6"),
                                                               "5 or 6", 
                                                               ifelse(as.numeric(property_data$floorLevel) >= 7,
                                                                      "7 or higher", property_data$floorLevel))))))
# Categorizing built in 
property_data$built_in <- ifelse(property_data$built_in <= 1917,
                                 "1917 or earlier",
                                 ifelse(property_data$built_in %in% 1918:1944, 
                                        "Between 1918 and 1944",
                                        ifelse(as.numeric(property_data$built_in) %in% 1945:1978,
                                               "Between 1945 and 1978",
                                               ifelse(as.numeric(property_data$built_in) %in% 1979:2007,
                                                      "Between 1979 and 2007",
                                                      ifelse(as.numeric(property_data$built_in) %in% 2008:2019,
                                                             "Between 2008 and 2019", 
                                                             ifelse(as.numeric(property_data$built_in) >= 2020,
                                                                    "2020 or later", NA))))))

# Rename categories of condition                                      
property_data <- property_data %>%
  mutate(
   condition = str_replace(condition, "Second hand/good condition", "Good"),
   condition = str_replace(condition, "New housing development", "New development"),
   condition = str_replace(condition, "Second hand/needs renovating", "Needs renovating")
  )
########################### SAVING CLEANED DATASET #############################
# Removing unnecessary/processed variables
property_data_final <- property_data %>%
  select(-location,  -title, -features, -cat_reference)


# Saving dataset in excel
write_xlsx(property_data_final, 'idealista_properties_cleaned.xlsx')



