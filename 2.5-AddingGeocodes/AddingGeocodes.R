library(xml2)
library(readxl)
library(writexl)
library(dplyr)

# Read and parse the KML file
kml_file1 <- "Spain_properties_part1.kml"
kml_file2 <- "Spain_properties_part2.kml"  

kml_data1 <- read_xml(kml_file1)
kml_data2 <- read_xml(kml_file2)

placemarks1 <- xml_find_all(kml_data1, ".//d1:Placemark", ns = c(d1 = "http://www.opengis.net/kml/2.2"))
placemarks2 <- xml_find_all(kml_data2, ".//d1:Placemark", ns = c(d1 = "http://www.opengis.net/kml/2.2"))


# Function to extract address and coordinates
extract_data <- function(placemark) {
  address_node <- xml_find_first(placemark, ".//d1:address", ns = c(d1 = "http://www.opengis.net/kml/2.2"))
  address <- ifelse(length(address_node) > 0, xml_text(address_node), NA)
  
  # Extract coordinates
  coord_node <- xml_find_first(placemark, ".//d1:Point/d1:coordinates", ns = c(d1 = "http://www.opengis.net/kml/2.2"))
  coord_text <- ifelse(length(coord_node) > 0, xml_text(coord_node), NA)
  
  # Process coordinates
  if (!is.na(coord_text) && nchar(coord_text) > 0) {
    coords <- strsplit(trimws(coord_text), ",")[[1]]
    if (length(coords) >= 2) {
      lat <- as.numeric(coords[2])
      lon <- as.numeric(coords[1])
      return(data.frame(address = address, lat = lat, lon = lon, stringsAsFactors = FALSE))
    }
  }
  
  return(data.frame(address = address, lat = NA, lon = NA, stringsAsFactors = FALSE))
}

# Apply the function to all placemarks
kml_data_list1 <- lapply(placemarks1, extract_data)
kml_data_list2 <- lapply(placemarks2, extract_data)

spain_properties_part1<- do.call(rbind, kml_data_list1)
spain_properties_part2<- do.call(rbind, kml_data_list2)


spain_properties_coordinates <- rbind(spain_properties_part1, spain_properties_part2)
#write_xlsx(spain_properties_coordinates, "/Users/michellehajduk/addresses_spain_with_coords.xlsx" )


# Merge coordinates to dataset
addresses_data <- as.data.frame(read_xlsx("combined_data.xlsx"))
spain_properties_coordinates <- as.data.frame(spain_properties_coordinates)
addresses_data$address <- paste(addresses_data$street, addresses_data$city, addresses_data$autonomous_community, sep = " ")
addresses_data$address <- trimws(addresses_data$address)
spain_properties_coordinates$address <- trimws(spain_properties_coordinates$address)
spain_properties_coordinates_unique <- spain_properties_coordinates %>%
  distinct(address, .keep_all = TRUE)

# Apply left join
join <- left_join(addresses_data, spain_properties_coordinates_unique, by="address", multiple= "any")

# Save data in file
write_xlsx(join, "combined_data_with_coords.xlsx")


