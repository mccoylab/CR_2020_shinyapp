library(shiny)
library(leaflet)

school_dist_data = readr::read_rds(here::here("Exports", "school_sites_distances_meters.rda"))

# Loads in data of schools to omit for now 
source(here::here("Data", "Cleaned_Data", "files_rda", "schools_remove.R"))

####### Pre-Cleaning of Data
school_dist_data = dplyr::filter(school_dist_data, is.na(total_student_size) == FALSE)

# Schools requested to have removed 
remove_schools_index = stringr::str_which(school_dist_data$school_name, remove_schools)

school_dist_data = school_dist_data[-remove_schools_index, ]

# Converting to numeric 
school_dist_data$total_student_size = as.numeric(school_dist_data$total_student_size)
school_dist_data$school_mdist = as.numeric(school_dist_data$school_mdist) / 1000
school_dist_data$pharm_dist = as.numeric(school_dist_data$pharm_dist) / 1000
school_dist_data$healthfac_dist = as.numeric(school_dist_data$healthfac_dist) / 1000
school_dist_data$youth_center_dist = as.numeric(school_dist_data$youth_center_dist) / 1000

# Updating school type
school_dist_data = dplyr::mutate(school_dist_data, 
                                 school_type_new = dplyr::case_when(
  school_type %in% c("Boarding", "Bording", "BOARDING", "BOADING", "boarding") ~ "Boarding",
  school_type %in% c("Day", "DAY", "day") ~ "Day",
  school_type %in% c("MIXED", "Boarding/ DAY", "Mixed", "BOARD&DAY", "DAY&BOARD") ~ "Mixed"
))

# Updating levels
school_dist_data = dplyr::mutate(school_dist_data,
                                 levels_new = dplyr::case_when(
  stringr::str_sub(levels, start = -2, end = -1) %in% c("S2", "S3", "L3", "L1", "ar") ~ 3,
  stringr::str_sub(levels, start = -2, end = -1) %in% c("S4") ~ 4,
  stringr::str_sub(levels, start = -2, end = -1) %in% c("S5") ~ 5,
  stringr::str_sub(levels, start = -2, end = -1) %in% c("S6") ~ 6))

# Creating own palette
palette_districts = rcartocolor::carto_pal(name = "Prism")
palette_districts = palette_districts[-c(10:12)]
##### Code for R Shiny 

# User Input
ui = fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  titlePanel("CR School Selection 2020"),
  
  sidebarLayout(
    sidebarPanel(
           sliderInput("student_size", "Total Student Size",
                       min = floor(min(school_dist_data$total_student_size, na.rm = TRUE)),
                       max = ceiling(max(school_dist_data$total_student_size, na.rm = TRUE)),
                       value = c(round(quantile(school_dist_data$total_student_size, na.rm = TRUE)[[2]], 2),
                                 round(quantile(school_dist_data$total_student_size, na.rm = TRUE)[[4]], 2)),
                       step = 50),
             sliderInput("school_dist", "Distance to Nearest School (km)",
                       min = floor(min(school_dist_data$school_mdist)), 
                       max = ceiling(max(school_dist_data$school_mdist)),
                       value = c(round(quantile(school_dist_data$school_mdist)[[2]], 2),
                                 round(quantile(school_dist_data$school_mdist)[[4]], 2)),
                       step = 0.5),
           sliderInput("pharm_dist", "Distance to Nearest Pharmacy (km)",
                       min = floor(min(school_dist_data$pharm_dist)),
                       max = ceiling(max(school_dist_data$pharm_dist)),
                       value = c(round(quantile(school_dist_data$pharm_dist)[[2]], 2),
                                 round(quantile(school_dist_data$pharm_dist)[[4]], 2)),
                       step = 0.5),
           sliderInput("healthfac_dist", "Distance to Nearest Health Facility (km)",
                       min = floor(min(school_dist_data$healthfac_dist)),
                       max = ceiling(max(school_dist_data$healthfac_dist)),
                       value = c(round(quantile(school_dist_data$healthfac_dist)[[2]], 2),
                                 round(quantile(school_dist_data$healthfac_dist)[[4]], 2)),
                       step = 0.5),
           sliderInput("youthcenter_dist", "Distance to Nearest Youth Center (km)",
                       min = floor(min(school_dist_data$youth_center_dist)),
                       max = ceiling(max(school_dist_data$youth_center_dist)),
                       value = c(round(quantile(school_dist_data$youth_center_dist)[[2]], 2),
                                 round(quantile(school_dist_data$youth_center_dist)[[4]], 2)),
                       step = 0.5),
           selectInput("levels", "School Levels",
                       choices = list("Up to S3" = 3, "Up to S4" = 4, 
                                      "Up to S5" = 5, "Up to S6" = 6),
                       selected = 3, multiple = FALSE),
           checkboxGroupInput("school_type", "School Type",
                       choices = c("Boarding", "Day", "Mixed"), 
                       selected = "Day")),
    mainPanel(
      tabsetPanel(
        tabPanel("OpenStreetMap", leafletOutput(outputId = "map", height = 850)),
        tabPanel("Filtered Data", DT::dataTableOutput("mytable"))
      )
      )
    
  )
)


# Server Function
server = function(input, output, session) {
  color_pal = colorFactor(palette = palette_districts, domain = unique(school_dist_data$district))
  
  # Reactive expression - filters to data based on what user selected
  filtered_data = reactive({
    school_dist_data[school_dist_data$total_student_size >= input$student_size[1] &
                       school_dist_data$total_student_size <= input$student_size[2] &
                       school_dist_data$school_mdist >= input$school_dist[1] & 
                       school_dist_data$school_mdist <= input$school_dist[2] & 
                       school_dist_data$pharm_dist >= input$pharm_dist[1] & 
                       school_dist_data$pharm_dist <= input$pharm_dist[2] &
                       school_dist_data$healthfac_dist >= input$healthfac_dist[1] & 
                       school_dist_data$healthfac_dist <= input$healthfac_dist[2] &
                       school_dist_data$youth_center_dist >= input$youthcenter_dist[1] &
                       school_dist_data$youth_center_dist <= input$youthcenter_dist[2] &
                       school_dist_data$school_type_new %in% input$school_type &
                       school_dist_data$levels_new <= input$levels, ]
  })
  
  # Only include aspects that do not need to change dynamically 
  output$map = renderLeaflet({
    leaflet(school_dist_data) %>%
      addProviderTiles("OpenStreetMap") %>%
      fitBounds(29.6024409, -2.6611626, 30.7128972, -1.0767259) %>%
      addLegend("topright", pal = color_pal, values = ~district,
                title = "Districts of Eligible Schools",
                opacity = 1)
  })
  
  output$mytable = DT::renderDataTable({
    filtered_data() %>%
      dplyr::select(district, sector, school_name, levels, mixed_sex, school_type_new, support_type,
                    religious, total_student_size) %>%
      dplyr::rename(District = district, Sector = sector, "School Name" = school_name,
                    Levels = levels, "Mixed Sex" = mixed_sex, "School Type" = school_type_new,
                    "Support Type" = support_type, "Religious Affiliation" = religious,
                    "Student Size" = total_student_size)
  })
  
  # Incremental changes to the map via an observer
  observe({
    
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      addCircles(color = ~color_pal(district), radius = 100, weight = 30,
                 stroke = TRUE, fill = TRUE, fillColor = ~color_pal(district), fillOpacity = 1,
                 popup = ~paste0("<i>", school_name, "</i>", "<br/>", 
                                 "Sector: ", sector, "<br/>",
                                 "Student Size: ", total_student_size, "<br/>",
                                 "School Type: ", school_type_new, "<br/>",
                                 "Levels: ", levels, "<br/>"))
  })
  
}

# Deploying the app 
shinyApp(ui = ui, server = server)

#library(rsconnect)
#deployApp()
