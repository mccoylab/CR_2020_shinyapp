# Libraries needed
library(dplyr)
library(sf)
library(leaflet)

####################### Loading Data  ################################
school_data = readr::read_rds(here::here("Data", "Cleaned_Data", 
                                         "files_rda","school_data.rda"))
pharmacy_data = readr::read_rds(here::here("Data", "Cleaned_Data", 
                                           "files_rda","pharmacy_data.rda"))
health_fac_data = readr::read_rds(here::here("Data", "Cleaned_Data", 
                                             "files_rda","health_fac_data.rda"))
youth_center_data = readr::read_rds(here::here("Data", "Cleaned_Data", 
                                             "files_rda","youth_centers.rda"))

################### Preparing Data for Mapping ###########################

# Editing the coordinates into latitude, longitude 
coordinates = stringr::str_split_fixed(school_data$gps_coordinates, ",", 2)
colnames(coordinates) = c("latitude", "longitude")
coordinates = as.data.frame(coordinates)

school_data$latitude = stringr::str_remove_all(coordinates$latitude, "\\(")
school_data$latitude = as.numeric(school_data$latitude)

school_data$longitude = stringr::str_remove_all(coordinates$longitude, "\\)")
school_data$longitude = as.numeric(school_data$longitude)

coordinates = stringr::str_split_fixed(health_fac_data$gps_coordinates, ",", 2)
colnames(coordinates) = c("latitude", "longitude")
coordinates = as.data.frame(coordinates)

health_fac_data$latitude = stringr::str_remove_all(coordinates$latitude, "\\(")
health_fac_data$latitude = as.numeric(health_fac_data$latitude)

health_fac_data$longitude = stringr::str_remove_all(coordinates$longitude, "\\)")
health_fac_data$longitude = as.numeric(health_fac_data$longitude)

coordinates = stringr::str_split_fixed(pharmacy_data$gps_coordinates, ",", 2)
colnames(coordinates) = c("latitude", "longitude")
coordinates = as.data.frame(coordinates)

pharmacy_data$latitude = stringr::str_remove_all(coordinates$latitude, "\\(")
pharmacy_data$latitude = as.numeric(pharmacy_data$latitude)

pharmacy_data$longitude = stringr::str_remove_all(coordinates$longitude, "\\)")
pharmacy_data$longitude = as.numeric(pharmacy_data$longitude)

coordinates = stringr::str_split_fixed(youth_center_data$gps_coordinates, ",", 2)
colnames(coordinates) = c("latitude", "longitude")
coordinates = as.data.frame(coordinates)

youth_center_data$latitude = stringr::str_remove_all(coordinates$latitude, "\\(")
youth_center_data$latitude = as.numeric(youth_center_data$latitude)

youth_center_data$longitude = stringr::str_remove_all(coordinates$longitude, "\\)")
youth_center_data$longitude = as.numeric(youth_center_data$longitude)

rm(coordinates)

################### Goals #############################
## Distance from other schools                       ##
## Distance from health center/health post/hospital  ##
## Distance from pharmacy                            ##
## Total student size                                ##
#######################################################

# Creating SF object
sf_school = st_as_sf(school_data, coords = c("latitude", "longitude"), crs = 4326)
sf_healthfac = st_as_sf(health_fac_data, coords = c("latitude", "longitude"), crs = 4326)
sf_pharm = st_as_sf(pharmacy_data, coords = c("latitude", "longitude"), crs = 4326)
sf_youth_center = st_as_sf(youth_center_data, coords = c("latitude", "longitude"), crs = 4326)
  
# Confirming CRS matches
st_crs(sf_school) == st_crs(sf_pharm)
st_crs(sf_school) == st_crs(sf_healthfac)
st_crs(sf_school) == st_crs(sf_youth_center)

# Computing distance matrix
dSchool = st_distance(sf_school, sf_school)
colnames(dSchool) = school_data$school_name
rownames(dSchool) = school_data$school_name

dPharm = st_distance(sf_school, sf_pharm)
rownames(dPharm) = school_data$school_name
colnames(dPharm) = sf_pharm$pharmacy_name

dHealthfac = st_distance(sf_school, sf_healthfac)
rownames(dHealthfac) = school_data$school_name
colnames(dHealthfac) = sf_healthfac$name

dYouthCenter = st_distance(sf_school, sf_youth_center)
rownames(dYouthCenter) = school_data$school_name
colnames(dYouthCenter) = sf_youth_center$youth_center


# Find the minimum distance
distance_school = data.frame(school=rownames(dSchool)[row(dSchool)], 
                         school_2=colnames(dSchool)[col(dSchool)], 
                         dist=c(dSchool))

distance_school = distance_school %>%
  filter(school != school_2) %>%
  group_by(school) %>%
  summarise(school_mdist = min(dist)) %>%
  ungroup()

distance_pharm = data.frame(school = rownames(dPharm)[row(dPharm)],
                            pharm = colnames(dPharm)[col(dPharm)],
                            dist = c(dPharm))

distance_pharm = distance_pharm %>%
  group_by(school) %>%
  summarise(pharm_dist = min(dist)) %>%
  ungroup()

distance_healthfac = data.frame(school=rownames(dHealthfac)[row(dHealthfac)], 
                                health_fac=colnames(dHealthfac)[col(dHealthfac)], 
                                dist=c(dHealthfac))

distance_healthfac = distance_healthfac %>%
  group_by(school) %>%
  summarise(healthfac_dist = min(dist)) %>%
  ungroup()

distance_youthcenter = data.frame(school = rownames(dYouthCenter)[row(dYouthCenter)],
                                  youth_center = colnames(dYouthCenter)[col(dYouthCenter)],
                                  dist = c(dYouthCenter))

distance_youthcenter = distance_youthcenter %>%
  group_by(school) %>%
  summarise(youth_center_dist = min(dist)) %>%
  ungroup()

total_distance = left_join(distance_school, distance_pharm, by = c("school" = "school")) %>%
  left_join(., distance_healthfac, by = c("school" = "school")) %>%
  left_join(., distance_youthcenter, by = c("school" = "school"))

# Removing files no longer needed
rm(distance_school, distance_pharm, distance_healthfac, distance_youthcenter)

# Adding distance features to the original school dataset
school_dist_data = left_join(school_data, total_distance, by = c("school_name" = "school"))

# Exporting School + Distance Data
readr::write_rds(x = school_dist_data, 
                 path = here::here("Exports", "school_sites_distances_meters.rda"))


################### Initial Mapping ###########################
district = unique(school_dist_data$district)
color_pal = colorFactor(palette = "Set1", domain = district)

init_map = leaflet(school_dist_data) %>%
  addProviderTiles("OpenStreetMap") %>%
  setView(lat = -2.622571, lng = 29.70141, zoom = 9) %>%
  addCircleMarkers(lng = school_dist_data$longitude, lat = school_dist_data$latitude,
                   color = ~color_pal(district),
                   radius = 6,
                   stroke = TRUE,
                   fillOpacity = 0.75,
                   popup = ~paste0("<i>", school_name, "</i>", "<br/>", 
                                   "Sector: ", sector, "<br/>",
                                   "Student Size: ", total_student_size, "<br/>")) %>%
  addLegend("bottomright", pal = color_pal, values = ~district,
            title = "Districts of Eligible Schools",
            opacity = 1)






