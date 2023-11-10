# -------------------------------------------------------------------------
# 30 DAYS MAP CHALLENGE 2023
# day06-asia
# title: Topographic map of Philippines
# data: the Open Topography Global Datasets, API <https://opentopography.org/developers/>, and 
#       the USGS Elevation Point Query Service <https://apps.nationalmap.gov/epqs/>
# script by Ziyadatul Hikmah | 06 Nov 2023
# -------------------------------------------------------------------------

# install and load libraries ----------------------------------------------
# libraries we need
libs <- c("elevatr", "terra", "tidyverse", 
          "sf", "giscoR", "marmap")

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
setwd("D:/Ziy/30 days map challange/data/day06")

# get country data --------------------------------------------------------
# define crs longitude latitude (WGS84)
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# get country border
get_border <- function() {
  country_sf <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10",
    country = "Philippines")
  
  country_transformed <- sf::st_transform(country_sf, 
                                      crs = crsLONGLAT)
  
  return(country_transformed)
}

country_transformed <- get_border() 

# get elevation data ------------------------------------------------------
get_elevation_data <- function() {
  country_elevation <- elevatr::get_elev_raster(
    locations = country_transformed,
    z = 9, 
    clip = "locations") 
  
  country_elevation_df <- as.data.frame(country_elevation, 
                                        xy = T) %>%
    na.omit()
  
  colnames(country_elevation_df)[3] <- "elevation"
  
  return(country_elevation_df)
}

phil_elevation_df <- get_elevation_data()

# visualization -----------------------------------------------------------
phil_map <- ggplot() +
  geom_tile(data = phil_elevation_df,
            mapping = aes(x = x, y = y, fill = elevation)) +
  scale_fill_etopo() +
  coord_sf(crs = crsLONGLAT)+
  theme_minimal() +
  theme(text = element_text(family = "georg", 
                            color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "#ffddca", 
                                        size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, 
                                  color = "grey10", 
                                  hjust = .1, 
                                  vjust = .1),
        plot.caption = element_text(size = 6, 
                                    color = "grey60", 
                                    hjust =.5, 
                                    vjust = 15),
        plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), 
        plot.background = element_rect(fill = "#ffddca", 
                                       color = NA), 
        panel.background = element_rect(fill = "#ffddca", 
                                        color = NA),
        panel.border = element_blank()) +
  labs(x = "",
       y = NULL,
       title = "Topographic map of Philippines",
       subtitle = "", 
       caption = "©2023 Ziyadatul Hikmah | #30daymapchallange | Data:the Open Topography Global Datasets, API <https://opentopography.org/developers/>, and the USGS 
             Elevation Point Query Service <https://apps.nationalmap.gov/epqs/>")

ggsave(filename="day06-asia2.png", 
       width=7, 
       height=8.5, 
       dpi = 600, 
       device='png',
       plot = phil_map)



