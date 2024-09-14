library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyjs)
library(httr)
library(httr2)
library(jsonlite)
library(mapSpain)
library(sf)
library(CatastRo)
library(stringdist)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(shinycssloaders)
library(geosphere)
library(readxl)
library(data.table)
library(caret)
library(randomForest)
library(xgboost)
library(glmnet)

#################################### DATA ######################################
#options(download.file.method="curl", download.file.extra="-k -L")

data <- read_xlsx("Data/property_data_final.xlsx")
data_hospitals <-  fread("Data/data_Hospitals_spain.csv")
data_schools <- fread("Data/data_Schools_spain.csv") 
data_trainStations <- fread("Data/data_TrainStations_spain.csv")

################################# FUNCTIONS ####################################


# Define the function to calculate the shortest distance to a train station
calculate_shortest_distance <- function(lat, lon, df) {
  if (nrow(df) > 0) {
    your_point <- c(lon, lat)
    df_coords <- cbind(df$lon, df$lat)
    distances <- distHaversine(your_point, df_coords)
    shortest_distance <- min(distances, na.rm = TRUE)
    return(ceiling(shortest_distance))
  } else {
    return(NA)
  }
}

calculate_bounding_box <- function(lat, lon, buffer = 0.001) {
  # Calculate the bounding box around the central point
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

get_lat_lon_from_address <- function(address) {
  address_url <- URLencode(address)
  
  # Make the API request
  response <- GET(paste0("https://nominatim.openstreetmap.org/search?q=", address_url, "&format=json"))
  
  # Check if response is successful
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (length(data) > 0 && !is.null(data$lat[1]) && !is.null(data$lon[1])) {
      lat <- as.numeric(data$lat[1])
      lon <- as.numeric(data$lon[1])
      
      return(list(lat = lat, lon = lon))
    } else {
      return(NULL)
    }
  } else {
    showNotification("API request failed. Please try again.", type = "error")
    return(NULL)
  }
}

# Function to reverse geocode using Nominatim API
reverse_geocode_nominatim <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", lat, "&lon=", lon, "&zoom=18&addressdetails=1")
  response <- GET(url)
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  return(data)
}

# Function to forward geocode using Nominatim API
forward_geocode_nominatim <- function(street, city) {
  url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", URLencode(paste(street, city)))
  response <- GET(url)
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  if (length(data) > 0 && !is.null(data[[1]]$lat) && !is.null(data[[1]]$lon)) {
    return(list(lat = as.numeric(data[[1]]$lat), lon = as.numeric(data[[1]]$lon)))
  } else {
    return(NULL)
  }
}

# Load geographic data for autonomous communities and provinces
autonomous_communities <- esp_get_ccaa()
provinces <- esp_get_prov()

target_crs <- 25830  

# Ensure autonomous communities and provinces are in the target CRS
autonomous_communities <- st_transform(autonomous_communities, crs = target_crs)
provinces <- st_transform(provinces, crs = target_crs)


# Function to find province and autonomous community based on lat/lon
find_location_details <- function(lat, lon, target_crs, communities, provinces) {
  if (lat >= 27 && lat <= 29.5 && lon >= -18 && lon <= -13) {
    # Define bounding boxes for the Canary Islands provinces
    canary_provinces <- list(
      "Santa Cruz de Tenerife" = list(lat_min = 27, lat_max = 29, lon_min = -18.5, lon_max = -16),
      "Las Palmas" = list(lat_min = 27, lat_max = 29.5, lon_min = -16, lon_max = -13)
    )
    # Check which province the point falls into
    province_name <- "No province found"
    for (prov_name in names(canary_provinces)) {
      bbox <- canary_provinces[[prov_name]]
      if (lat >= bbox$lat_min && lat <= bbox$lat_max && lon >= bbox$lon_min && lon <= bbox$lon_max) {
        province_name <- prov_name
        break
      }
    }
    
    community_name <- "Canary Islands"
    
    return(list(province = province_name, community = community_name))
  } else {
    
    location_point <- st_sfc(st_point(c(lon, lat)), crs = 4326)
    location_point_transformed <- st_transform(location_point, crs = target_crs)
    
    province_name <- "No province found"
    community_name <- "No autonomous community found"
    # municipality_name <- "No municipality found"
    
    for (i in seq_len(nrow(provinces))) {
      if (st_within(location_point_transformed, provinces$geometry[i], sparse = FALSE)) {
        province_name <- provinces$cldr.prov.name.en[i]
        break
      }
    }
    
    
    for (i in seq_len(nrow(communities))) {
      if (st_within(location_point_transformed, communities$geometry[i], sparse = FALSE)) {
        community_name <- communities$cldr.ccaa.name.en[i]
        break
      }
    }
  }
  
  return(list(province = province_name, community = community_name))
}

################################# SHINY APP ####################################

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Real Estate Investment Platform"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model Selection", tabName = "model", icon = icon("cogs")),
      selectInput("model", "Choose a predictive model:",
                  choices = list("Random Forest" = "rf",
                                 "Linear Regression" = "lm",
                                 "Glmnet" ="glmnet",
                                 "XGBoost" = "xgb"))
    )
  ),
  
  dashboardBody(
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(HTML("
          .title-container {
             display: flex;
             align-items: center;
             justify-content: center; 
          }

    
        .title-line {
            display: none; 
          }

        .title-text {
            white-space: nowrap;
           font-weight: bold;
           color: #333; 
          }


        .clear {
             clear: both;
        }

        @media (max-width: 767px) {
         .title-container {
         flex-direction: column;
         align-items: center; 
        }
        .title-line {
         display: none; 
         width: 100%;
         margin: 5px 0;
         }
        }
          .blue-box {
            background-color: lightblue;
            padding: 20px;
            border-radius: 5px;
            margin-bottom: 15px;
          }
          .error-box {
            background-color: lightcoral;
            padding: 20px;
            border-radius: 5px;
            margin-bottom: 15px;
          }
          .info-btn-custom {
            background-color: lightgrey;
            color: white;
            border-radius: 50%;
            padding: 2px 6px;
            font-size: 10px;
            margin-left: 5px;
            border: none;
            width: 20px;
            height: 20px;
            align: right;
          }
          .nav-tabs > li > a {
          pointer-events: none; /* Disable clicks on the tabs */
          cursor: default; /* Show default cursor */
        }
        .nav-tabs > li {
          pointer-events: auto; /* Allow interaction with the tab container */
        }
        summary-container {
            display: flex;
            flex-wrap: wrap;
            gap: 15px;
            justify-content: space-between; /* Distribute space between items */
            width: 100%; /* Ensure the container takes full width */
          }
          .summary-item {
            flex: 1 1 calc(20% - 15px); /* Adjust for up to 5 items per row */
            background-color: #f9f9f9;
            padding: 10px;
            border: 1px solid #ddd;
            border-radius: 5px;
            margin-bottom: 15px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            box-sizing: border-box; /* Ensure padding and border are included in the width */
          }
          .summary-item b {
            display: block;
            margin-bottom: 5px;
          }
          .navbar-nav > li > a {
            pointer-events: none; /* Disable clicks on navbar tabs */
            cursor: default;
          }
          
      .custom-disabled-look .selectize-input {
        background-color: #E4E4E4; 
        color: #888; 
        border: 1px solid #ccc; 
        cursor: not-allowed; 
      }

      .custom-disabled-look .selectize-dropdown {
         background-color: #E4E4E4; 
         color: #888; 
         border: 1px solid #ccc;
      }
       .custom-disabled-look .form-control {
        background-color: #E4E4E4;
        color: #888;
        border: 1px solid #ccc; 
        cursor: not-allowed; 
       }
      .button-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .button-container .btn {
      }
      .summary-box {
  border-radius: 5px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      }
      .box-title-custom {
        font-size: 22px; /* Make the title bigger */
        font-weight: 300; /* Thinner font weight */
        color: #333; /* Adjust color if needed */
        margin: 0; /* Remove default margin */
        display: inline-block; /* Ensure title width is used for centering */
      }
        "))
      ),
      
      navbarPage(
        title = "Follow the steps",
        id = "nav",
        tabPanel("1 Address Details", 
                 value = "address",
                 div(
                   class = "title-container",
                   h4("ENTER ADDRESS DETAILS", class = "title-text")
                 ),
                 box(width = 12,
                     p("Choose your address by typing it in or by placing a marker on the map."),
                     br(), 
                     fluidRow(" "), 
                     fluidRow(
                       column(6,
                              textInput("street", "Street", ""),
                              textInput("city", "City", ""),
                              selectInput("province", "Province", choices = c("Select a province" = "", provinces$cldr.prov.name.en)),
                              selectInput("community", "Autonomous Community", choices = c("Select a community" = "", autonomous_communities$cldr.ccaa.name.en)),
                              actionButton("removeEntries", "Remove Entries")
                       ),
                       column(6,
                              leafletOutput("map", height = 400),
                              textOutput("coordinates")
                       )
                     ),
                     br(),
                     fluidRow(
                       div(
                         style = "text-align: right; padding-right: 30px;", 
                         actionButton("nextButton", "Next", style = "background-color: #3c8dbc; color: white; border: none;")
                       )
                     )
                 )
        ),
        
        tabPanel("2 Property Details",
                 value = "property",
                 div(
                   class = "title-container",
                   h4("ENTER PROPERTY DETAILS", class = "title-text")
                 ),
                 box(
                   width = 12, 
                   solidHeader = TRUE,
                   fluidRow(align = "center", 
                            box(width = 12, align = "center", textOutput("addressDetails"), background = "light-blue")
                   ),
                   column(6,
                          fluidRow(
                            column(4, 
                                   tags$label("Square Meters*", actionButton("info_btn_squareMeters", "ℹ", class = "info-btn-custom")),
                                   numericInput("squareMeters", NULL, value = NULL, min = 1)
                            ),
                            column(4,
                                   tags$label("Type*", actionButton("info_btn_type", "ℹ", class = "info-btn-custom")),
                                   selectInput("propertyType", NULL, choices = c("Select" = "", sort(unique(data$propertyType))))
                            ),
                            column(4,
                                   tags$label("Condition*", actionButton("info_btn_condition", "ℹ", class = "info-btn-custom")),
                                   selectInput("propertyCondition", NULL, choices = c("Select" = "", sort(unique(data$condition))))
                            )
                          ),
                          
                          fluidRow(
                            column(4, 
                                   tags$label("Bedrooms*", actionButton("info_btn_bedrooms", "ℹ", class = "info-btn-custom")),
                                   numericInput("bedrooms", NULL, value = NULL, min = 0, max = 25)
                            ),
                            column(4,
                                   tags$label("Bathrooms*", actionButton("info_btn_bathrooms", "ℹ", class = "info-btn-custom")),
                                   numericInput("bathrooms", NULL, value = NULL, min = 0, max = 25)
                            ),
                            column(4,
                                   tags$label("Floor level*", actionButton("info_btn_floorLevel", "ℹ", class = "info-btn-custom")),
                                   selectInput("floorLevel", NULL, choices = c("Select" = "",  "Ground floor", "1 or 2", "3 or 4", "5 or 6", "7 or higher"))
                            )
                          ),
                          
                          fluidRow(
                            column(4, 
                                   tags$label("Energy Certificate*", actionButton("info_btn_energyCertificate", "ℹ", class = "info-btn-custom")),
                                   selectInput("energyCert", NULL, choices = c("Select" = "", "A", "B", "C", "D", "E", "F", "G", "None"))
                            ),
                            column(4, 
                                   tags$label("Heating type*", actionButton("info_btn_heatingType", "ℹ", class = "info-btn-custom")),
                                   selectInput("heatingType", NULL, choices = c("Select" = "", sort(unique(data$heatingType))))
                            ),
                            column(4, 
                                   tags$label("Heating details*", actionButton("info_btn_heatingDetails", "ℹ", class = "info-btn-custom")),
                                   selectInput("heatingDetails", NULL, choices = c("Select" = "", sort(unique(data$heatingDetails))))
                            )
                          ),
                          
                          
                          fluidRow(
                            column(4, 
                                   tags$label("Year Built*", actionButton("info_btn_built_in", "ℹ", class = "info-btn-custom")),
                                   tags$div(
                                     class = "custom-disabled-look", 
                                     selectInput("built_in", NULL, choices = c("Select" = "", "1917 or earlier", "Between 1918 and 1944", "Between 1945 and 1978", "Between 1979 and 2007", "Between 2008 and 2019", "2020 or later"))
                                   )
                            ),
                            column(4, 
                                   tags$label("Land plot (in m²)", actionButton("info_btn_landPlot", "ℹ", class = "info-btn-custom")),
                                   tags$div(
                                     class = "custom-disabled-look", 
                                     numericInput("landPlot", NULL, value = 0, min = 0)
                                   )
                                   
                            ),
                            column(4, 
                                   tags$label("Number of floors", actionButton("info_btn_numFloors", "ℹ", class = "info-btn-custom")),
                                   tags$div(
                                     class = "custom-disabled-look", 
                                     numericInput("numFloors", NULL, value = 1, min = 1)
                                   )
                            )
                          ),
                          
                          fluidRow(
                            column(4, 
                                   prettySwitch("hasAirConditioning", "AC", value = FALSE)
                            ),
                            column(4,
                                   prettySwitch("hasBalcony", "Balcony", value = FALSE)
                            ),
                            column(4,
                                   prettySwitch("hasSwimmingPool", "Pool", value = FALSE)
                            )
                          ),
                          fluidRow(
                            column(4, 
                                   prettySwitch("hasParkingSpace", "Parking", value = FALSE)
                            ),
                            column(4,
                                   prettySwitch("hasTerrace", "Terrace", value = FALSE)
                            ),
                            column(4,
                                   prettySwitch("isFurnished", "Furnished", value = FALSE)
                            )
                          ),
                          fluidRow(
                            column(4, 
                                   prettySwitch("hasGarden", "Garden", value = FALSE)
                            ),
                            column(4,
                                   prettySwitch("hasGreenAreas", "Green Areas", value = FALSE)
                            ),
                            column(4,
                                   prettySwitch("hasStoreroom", "Storeroom", value = FALSE)
                            )
                          ),
                          fluidRow(
                            column(4, 
                                   prettySwitch("hasFittedWardrobes", "Fitted Wardrobes", value = FALSE)
                            ),
                            column(4,
                                   " "
                            ),
                            column(4,
                                   " "
                            )
                          )
                   ),
                   column(6,  
                          title = "Building Image", 
                          withSpinner(plotOutput("buildingImage"), type = 8, color = "#3c8dbc")
                   ),
                   br(),
                   fluidRow(
                     column(12, 
                            div(class = "button-container",
                                actionButton("backButton", "Back", style = "background-color: #E4E4E4; color: #888; border: none;"),
                                actionButton("predictButton", "Predict", style = "background-color: #3c8dbc; color: white; border: none;")
                            )
                     )
                   )
                 )
        ),
        
        tabPanel("3 Prediction Results",
                 value = "prediction",
                 div(
                   class = "title-container",
                   h4("PREDICTION RESULTS", class = "title-text"),
                 ),
                 fluidRow(column(6, uiOutput("squareMeterPrice")),
                          column(6, uiOutput("totalPrice"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          box(
                            title = h4("SUMMARY", class="box-title-custom", align="center"),
                            status = NULL,
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            class = "box-body-custom",
                            uiOutput("summary")
                          )
                   )
                 ),
                 br(),
                 fluidRow(
                 column(12, align = "left",
                        actionButton("backButton2", "Back", style = "background-color: #E4E4E4; color: #888; border: none;")
                        )
                   )

        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -3.7038, lat = 40.4168, zoom = 6) 
  })
  
  reactive_coords <- reactiveValues(lat = NULL, lng = NULL)
  
  load("Models/lm_model2.RData")
  load("Models/glmnet_model2.RData")
  load("Models/rf_model2.RData")
  load("Models/xgb_model2.RData")
  
  models <- reactiveVal(list())
  
  # Function to update map marker and address details
  updateMapAndAddress <- function(lat, lon) {
    if (!is.null(lat) && !is.null(lon)) {
      location_details <- find_location_details(lat, lon, target_crs, autonomous_communities, provinces)
      
      updateSelectInput(session, "province", selected = location_details$province)
      updateSelectInput(session, "community", selected = location_details$community)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(lng = lon, lat = lat, popup = "Selected Location") %>%
        setView(lng = lon, lat = lat, zoom = 18)
      
      reactive_coords$lat <- lat
      reactive_coords$lng <- lon
    }
  }
  
  # Function to get lat/lon from address
  get_lat_lon_from_address <- function(address) {
    address_url <- URLencode(address)
    response <- GET(paste0("https://nominatim.openstreetmap.org/search?q=", address_url, "&format=json"))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (length(data) > 0 && !is.null(data$lat[1]) && !is.null(data$lon[1])) {
        lat <- as.numeric(data$lat[1])
        lon <- as.numeric(data$lon[1])
        
        return(list(lat = lat, lon = lon))
      } else {
        return(NULL)
      }
    } else {
      showNotification("API request failed. Please try again.", type = "error")
      return(NULL)
    }
  }
  
  # Observe event for manual address input
  observeEvent({
    input$street
    input$city
  }, {
    if (input$street != "" && input$city != "") {
      address <- paste(input$street, input$city)
      geocode_result <- get_lat_lon_from_address(address)
      
      if (!is.null(geocode_result)) {
        updateMapAndAddress(geocode_result$lat, geocode_result$lon)
      } else {
        showNotification("No geocode result found. Please check the address and try again.", type = "error")
      }
    }
  })
  
  # Observe map clicks to update coordinates and address fields
  observeEvent(input$map_click, {
    click <- input$map_click
    reactive_coords$lat <- click$lat
    reactive_coords$lng <- click$lng
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat, popup = "Selected Location") %>%
      setView(lng = click$lng, lat = click$lat, zoom = 18)
    
    address <- reverse_geocode_nominatim(click$lat, click$lng)
    
    if (!is.null(address) && !is.null(address$address)) {
      updateTextInput(session, "street", value = paste(address$address$road, address$address$house_number))
      updateTextInput(session, "city", value = address$address$city %||% address$address$town %||% address$address$village)
      
      location_details <- find_location_details(click$lat, click$lng, target_crs, autonomous_communities, provinces)
      updateSelectInput(session, "province", selected = location_details$province)
      updateSelectInput(session, "community", selected = location_details$community)
    }
  })
  
  # Display selected coordinates
  output$coordinates <- renderText({
    if (!is.null(reactive_coords$lat) && !is.null(reactive_coords$lng)) {
      paste("Latitude: ", reactive_coords$lat, "Longitude: ", reactive_coords$lng)
    } 
  })
  
  
  # Remove entries button
  observeEvent(input$removeEntries, {
    updateTextInput(session, "street", value = "")
    updateTextInput(session, "city", value = "")
    updateSelectInput(session, "province", selected = "")
    updateSelectInput(session, "community", selected = "")
    leafletProxy("map") %>%
      clearMarkers()
  })
  
  # Info buttons
  observeEvent(input$info_btn_squareMeters, {
    showModal(modalDialog(
      title = "Info about Square Meters",
      "Enter the built area of the property in square meters. This measures the total floor space within the property.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_numFloors, {
    showModal(modalDialog(
      title = "Info about Number of Floors",
      "Enter the number of floors in the property. This indicates how many levels the property has, including all stories above and below ground. For example, a mezzanine apartment typically has 2 floors.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_landPlot, {
    showModal(modalDialog(
      title = "Info about Land Plot",
      "Enter the size of the land plot in square meters. This refers to the area of land that belongs to the property.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_energyCertificate, {
    showModal(modalDialog(
      title = "Info about Energy Certificate",
      "Select property's energy certificate rating. This rating indicates the property's energy efficiency and helps assess its environmental impact. If the certificate is not available, choose 'None'.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_heatingType, {
    showModal(modalDialog(
      title = "Info about Heating Type",
      "Select the type of heating for the property. Individual heating refers to systems serving only one unit, while central heating covers the entire property.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_heatingDetails, {
    showModal(modalDialog(
      title = "Info about Heating Details",
      "Select the heating details for the property. This describes the type of heating system used in the property.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_bedrooms, {
    showModal(modalDialog(
      title = "Info about Bedrooms",
      "Enter the number of bedrooms in the property.  This indicates how many rooms are designated as bedrooms.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_bathrooms, {
    showModal(modalDialog(
      title = "Info about Bathrooms",
      "Enter the number of bathrooms in the property. This indicates how many rooms are equipped with a toilet and bathing facilities.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_built_in, {
    showModal(modalDialog(
      title = "Info about Year Built",
      "Select the year the building was constructed, categorized by ranges. If a cadastral reference is available, this field will auto-fill and be locked.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_type, {
    showModal(modalDialog(
      title = "Info about Type",
      "Select the type of property. This indicates the category of the property.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_condition, {
    showModal(modalDialog(
      title = "Info about Condition",
      "Select the condition of the property. This describes the overall state of the property.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn_floorLevel, {
    showModal(modalDialog(
      title = "Info about Floor Level",
      "Select the floor level of the property. This indicates the floor on which the property is located.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$backButton, {
    updateTabsetPanel(session, "nav", selected = "address")
  })
  
  observeEvent(input$backButton2, {
    updateTabsetPanel(session, "nav", selected = "property")
  })
  
  
  # Observe "Next" button click
  observeEvent(input$nextButton, {
    if (!is.null(reactive_coords$lat) && !is.null(reactive_coords$lng)) {
      
      updateTabsetPanel(session, "nav", selected = "property")
      
      boundingbox <- calculate_bounding_box(reactive_coords$lat, reactive_coords$lng)
      
      # Retrieve cadastre reference data
      cat_ref_list <- catr_ovc_get_rccoor_distancia(
        lat = reactive_coords$lat,
        lon = reactive_coords$lng,
        srs = 4326
      )
      
      if (is.null(cat_ref_list) || nrow(cat_ref_list) == 0) {
        cat_ref <- NULL
        highlight_building <- NULL
      } else {
        # Retrieve building data
        wfs_get_buildings <- tryCatch({
          catr_wfs_get_buildings_bbox(
            c(boundingbox$lon_min, boundingbox$lat_min, boundingbox$lon_max, boundingbox$lat_max),
            what = "building", 
            srs = 4326, 
            verbose = FALSE
          )
        }, error = function(e) {
          # Handle errors and return NULL if no spatial layers found
          NULL
        })
        
        if (is.null(wfs_get_buildings) || nrow(wfs_get_buildings) == 0) {
          highlight_building <- NULL
          cat_ref <- NULL
        } else {
          # Extract and clean the address
          address <- paste(input$street, input$city)
          addresses_clean <- preprocess(cat_ref_list$address)
          address_clean <- preprocess(address)
          
          # Compute distances and find the closest building
          distances <- stringdist(address_clean, addresses_clean, method = "lv")
          closest_index <- which.min(distances)
          cat_ref <- cat_ref_list$refcat[closest_index]
          
          # Highlight the building on the map
          highlight_building <- wfs_get_buildings %>%
            dplyr::filter(reference == !!cat_ref)
          
          if (is.null(highlight_building) || nrow(highlight_building) == 0) {
            highlight_building <- NULL
          }
        }
      }
      
        if (!is.null(highlight_building) && nrow(highlight_building) > 0) {
         year_built <- substr(highlight_building$beginning, 1, 4)
        
          # Convert year to numeric
          year_built_numeric <- as.numeric(year_built)
          year_built_category <- ifelse(year_built_numeric <= 1917,
                                      "1917 or earlier",
                                      ifelse(year_built_numeric %in% 1918:1944, 
                                             "Between 1918 and 1944",
                                             ifelse(year_built_numeric %in% 1945:1978,
                                                    "Between 1945 and 1978",
                                                    ifelse(year_built_numeric %in% 1979:2007,
                                                           "Between 1979 and 2007",
                                                           ifelse(year_built_numeric %in% 2008:2019,
                                                                  "Between 2008 and 2019", 
                                                                  ifelse(year_built_numeric >= 2020,
                                                                         "2020 or later", NA))))))
        
          # Update year built input field
          updateSelectInput(session, "built_in", selected = year_built_category)
          }
      
      
      output$addressDetails <- renderText({
        paste0(toupper(input$street), ", ", toupper(input$city), " (", toupper(input$province), ")")
      })
      
      output$buildingImage <- renderPlot({
        if (!is.null(highlight_building) && nrow(highlight_building) > 0) {
          ggplot2::ggplot() +
            geom_sf(data = wfs_get_buildings, fill = "lightgrey") +
            geom_sf(data = highlight_building, fill = "red") +
            ggtitle(paste("Cadastre reference:", cat_ref)) +
            theme_minimal()
        } else {
          plot.new()
          text(0.5, 0.5, "No building found with the specified reference ID.")
        }
      })
    }
  })
  
  # Define a function to get the appropriate model
  getModel <- function(model_name) {
    switch(model_name,
           "lm" = lm_model,
           "glmnet" = glmnet_model,
           "rf" = rf_model,
           "xgb" = xgb_model,
           stop("Invalid model name")
    )
  }
  reactive_inputs <- reactive({
    req(input$predictButton)  
    
    # Construct the test dataframe using user inputs
    data.frame(
      province = input$province,
      propertyType = input$propertyType,
      m2_built = log(input$squareMeters),    
      landPlot = log(input$landPlot + 1),     
      bedrooms = log(input$bedrooms + 1),     
      bathrooms = log(input$bathrooms + 1),    
      floorLevel = input$floorLevel, 
      numFloors = log(input$numFloors),      
      built_in = input$built_in, 
      condition = input$propertyCondition, 
      energy_certification = input$energyCert,
      heatingType = input$heatingType,
      heatingDetails = input$heatingDetails, 
      hasBalcony = ifelse(input$hasBalcony, "1", "0"), 
      hasTerrace = ifelse(input$hasTerrace, "1", "0"), 
      hasStoreroom = ifelse(input$hasStoreroom, "1", "0"), 
      hasFittedWardrobes = ifelse(input$hasFittedWardrobes, "1", "0"), 
      isFurnished = ifelse(input$isFurnished, "1", "0"), 
      hasGarden = ifelse(input$hasGarden, "1", "0"), 
      hasAirConditioning = ifelse(input$hasAirConditioning, "1", "0"),
      hasGreenAreas = ifelse(input$hasGreenAreas, "1", "0"), 
      hasParkingSpace = ifelse(input$hasParkingSpace, "1", "0"), 
      hasSwimmingPool = ifelse(input$hasSwimmingPool, "1", "0"), 
      distanceTrainStation = log(calculate_shortest_distance(reactive_coords$lat, reactive_coords$lng, data_trainStations)),  
      distanceHospitals = log(calculate_shortest_distance(reactive_coords$lat, reactive_coords$lng, data_hospitals)),    
      distanceSchools = log(calculate_shortest_distance(reactive_coords$lat, reactive_coords$lng, data_schools))        
    )
  })
  
  reactive_inputs2 <- reactive({
    req(input$predictButton)  
    
    data.frame(
      Province = input$province,
      Type = input$propertyType,
      Square_Meters = input$squareMeters,    
      Land_plot_in_square_meters = input$landPlot,     
      Bedrooms = input$bedrooms,     
      Bathrooms = input$bathrooms,    
      Floor_level = input$floorLevel, 
      Number_of_floors = input$numFloors,      
      Year_built = input$built_in, 
      Condition = input$propertyCondition, 
      Energy_Certificate = input$energyCert,
      Heating_type = input$heatingType,
      Heating_details = input$heatingDetails, 
      Balcony = ifelse(input$hasBalcony, "yes", "no"), 
      Terrace = ifelse(input$hasTerrace, "yes", "no"), 
      Storeroom = ifelse(input$hasStoreroom, "yes", "no"), 
      Fitted_Wardrobes = ifelse(input$hasFittedWardrobes, "yes", "no"), 
      Furnished = ifelse(input$isFurnished, "yes", "no"), 
      Garden = ifelse(input$hasGarden, "yes", "no"), 
      Air_Conditioning = ifelse(input$hasAirConditioning, "yes", "no"),
      Green_Areas = ifelse(input$hasGreenAreas, "yes", "no"), 
      Parking_Space = ifelse(input$hasParkingSpace, "yes", "no"), 
      Swimming_Pool = ifelse(input$hasSwimmingPool, "yes", "no"), 
      Distance_to_train_station_in_meter = calculate_shortest_distance(reactive_coords$lat, reactive_coords$lng, data_trainStations),  
      Distance_to_hospital_in_meter = calculate_shortest_distance(reactive_coords$lat, reactive_coords$lng, data_hospitals),    
      Distance_to_school_in_meter = calculate_shortest_distance(reactive_coords$lat, reactive_coords$lng, data_schools)        
    )
  })
  
  
  # Observe the "Predict" button click and handle the prediction
  observeEvent(input$predictButton, {
    shinyjs::hide("predictionResults")
    # Check if any required inputs are missing
    if (!isTruthy(input$propertyType) || !isTruthy(input$squareMeters) ||
        !isTruthy(input$bedrooms) || !isTruthy(input$bathrooms) ||
        !isTruthy(input$built_in) || !isTruthy(input$floorLevel) ||
        !isTruthy(input$propertyCondition) || !isTruthy(input$energyCert) ||
        !isTruthy(input$heatingType) || !isTruthy(input$heatingDetails)) {
      
      # Show message if any required input is missing
      showModal(modalDialog(
        title = NULL,
        div(
          class = "modal-content",
          h4("Please complete all required fields marked with an asterisk (*) before proceeding.", style = "margin: 0;")
        ),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
      
      # Hide the prediction results area
      shinyjs::hide("predictionResults")
      
    } else {
      # Clear any previous notification
      output$notification <- renderUI(NULL)
      updateTabsetPanel(session, "nav", selected = "prediction")
      # Construct the test dataframe using user inputs
      test_df <- reactive_inputs()
      
      if (input$energyCert == "None") {
        mode_energyCert_data <- data %>%
          dplyr::filter(built_in == input$built_in,
                 heatingDetails == input$heatingDetails)
        mode_energyCert<-names(which.max(table(mode_energyCert_data$energy_certification)))
        test_df$energy_certification <- mode_energyCert
      }
      test_df_input <- reactive_inputs2()
      
      output$summary <- renderUI({
        # Create a list to hold the UI elements
        summary_items <- list()
        
        # Number of variables to display per row
        vars_per_row <- 5
        
        # Calculate the number of rows needed
        num_vars <- ncol(test_df)
        num_rows <- ceiling(num_vars / vars_per_row)
        
        for (i in 1:num_rows) {
          start_var <- (i - 1) * vars_per_row + 1
          end_var <- min(i * vars_per_row, num_vars)

          # Extract the variables for the current row
          vars <- test_df_input[, start_var:end_var, drop = FALSE]
          
          # Create summary items for the current set of variables
          summary_items[[i]] <- div(
            class = "summary-container",
            lapply(1:ncol(vars), function(j) {
              div(class = "summary-item",
                  tags$b(names(vars)[j]),  # Variable name
                  tags$p(paste(vars[, j], collapse = ", "))  # Variable values
              )
            })
          )
        }
        
        # Return the list of summary items
        do.call(tagList, summary_items)
      })
      
      # Get the selected model
      model <- getModel(input$model)
      
      if (!is.null(model)) {
        tryCatch({
          # Make predictions
          prediction <- predict(model, newdata = test_df)
          predicted_per_sqm <- round(exp(prediction), 2)
          predicted_total <- round(exp(prediction) * input$squareMeters, 2)
          
          # Show prediction results
          output$squareMeterPrice <- renderUI({
            div(
              style = "background-color: lightblue; padding: 20px; border-radius: 5px; text-align: center;",
              h1(paste0(format(predicted_per_sqm, big.mark = ",", scientific = FALSE), " €"), 
                 style = "font-size: 40px; font-weight: bold;"),
              p("Monthly rent price per square meter", 
                style = "font-size: 16px; margin-top: 10px;")
            )
          })
          
          output$totalPrice <- renderUI({
            div(
              style = "background-color: lightblue; padding: 20px; border-radius: 5px; text-align: center;",
              h1(paste0(format(predicted_total, big.mark = ",", scientific = FALSE), " €"), 
                 style = "font-size: 40px; font-weight: bold;"),
              p("Total monthly rent price", 
                style = "font-size: 16px; margin-top: 10px;")
            )
          })
          
          # Show the prediction results area
          shinyjs::show("predictionResults")
          
        }, error = function(e) {
          # Handle prediction errors
          output$squareMeterPrice <- renderUI({
            div(
              style = "background-color: lightcoral; padding: 20px; border-radius: 5px;",
              h4(paste("Error in prediction:", e$message))
            )
          })
          output$totalPrice <- NULL
          
          # Hide the prediction results area
          shinyjs::hide("predictionResults")
        })
      } else {
        # Handle case where model is not found
        output$squareMeterPrice <- renderUI({
          div(
            style = "background-color: lightcoral; padding: 20px; border-radius: 5px;",
            h4("Model not found. Please select a valid model.")
          )
        })
        output$totalPrice <- NULL
        
        # Hide the prediction results area
        shinyjs::hide("predictionResults")
      }
    }
  })
  
}

shinyApp(ui = ui, server = server)

