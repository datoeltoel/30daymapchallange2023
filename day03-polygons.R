# -------------------------------------------------------------------------
# 30 DAYS MAP CHALLENGE 2023
# day03-polygons
# title: Urban Area in Japan
# data: https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_urban_areas.zip 
# script by Ziyadatul Hikmah | 03 Nov 2023
# -------------------------------------------------------------------------

# install and load libraries ----------------------------------------------
# libraries we need
libs <- c("sf","tidyverse","giscoR")

installed_libs <- libs %in% row.names(
  install.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# set working directory ---------------------------------------------------
setwd("D:/Ziy/30 days map challange/data/day03")

# get country border ------------------------------------------------------
get_country_border <- function(){
  country_border <- giscoR::gisco_get_countries(
    resolution = "60",
    country = "JP"
  )
  
  return(country_border)
}

japan_border <- get_country_border()

st_write(japan_border, "japan_boundaries.shp", layer = NULL)

# get urban areas data from natural earth  --------------------------------
# download data 
get_urban_areas_data <- function(){
  url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_urban_areas.zip"
  file_name <- "ne_50m_urban_areas.zip"
  urban_areas <- download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

urban_areas <- get_urban_areas_data()

list.files()

# load urban data
load_urban_area_data <- function(){
  file_names <- list.files(
    pattern = "areas.shp$",
    full.names = T
  )
  urban_area_data <- sf::st_read(
    file_names
  )
  
  return(urban_area_data)
}

urban_area_data <- load_urban_area_data()
sf::sf_use_s2(F)

# urban area data that intersects with country borders --------------------
urban_area_japan <- urban_area_data |>
  sf::st_intersection(
    japan_border
  )

# visualization -----------------------------------------------------------
p <- ggplot() +
  geom_sf(data = urban_area_japan,
          size = 1,
          fill = "#ff003f",
          color = "#ff003f") +
  geom_sf(data = japan_border,
          size = 3,
          fill = "transparent",
          color = "#ec5800"
          )+
  labs(title = "Urban Area in Japan",
       subtitle = "Area of Dense Human Habitation",
       caption = "©2023 Ziyadatul Hikmah | #30daymapchallange | Data:https://www.naturalearthdata.com/") +
  
  theme_void() +
  
  theme(plot.background = element_rect(fill = "#ffddca",
                                       color = NA),
        panel.background = element_rect(fill = "#ffddca",
                                        color = NA),
        plot.title = element_text(family = "clim",
                                  hjust = 0.5,
                                  size = 30,
                                  color = "#ff003f",
                                  face = "bold"),
        plot.subtitle = element_text(family = "mont",
                                     hjust = 0.5,
                                     size = 20,
                                     color = "#ff003f"),
        plot.caption = element_text(family = "mont",
                                    size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    color = "#ff003f"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# save plot into directory
ggsave(
  filename = "day03-polygons1.png",
  dpi = 600,
  width = 8,
  height = 11,
  plot = p,
  device = "png"
)

# done ! ------------------------------------------------------------------

