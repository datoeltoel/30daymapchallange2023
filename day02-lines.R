# -------------------------------------------------------------------------
# 30 DAYS MAP CHALLENGE 2023
# day02-lines
# title: Rivers of Australia
# data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org"
# script by Ziyadatul Hikmah | 02 Nov 2023
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
setwd("D:/Ziy/30 days map challange/data/day02")

# get country borders -----------------------------------------------------
get_country_borders <- function(){
  country_border <- giscoR::gisco_get_countries(
    resolution = 3,
    country = "AU"
  )
  
  return(country_border)
}

australia <- get_country_borders()

# export australia border
st_write(australia, "au_border.shp", layer = NULL)

# get basin data ----------------------------------------------------------
# download basin data 
get_basins <- function(){
  url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_au_lev03_v1c.zip"
  file_name <- "hybas_au_lev03_v1c.zip"
  
  basins <- download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_basins()

list.files()

# load basin data
load_basins <- function(){
  filenames <- list.files(
    pattern = "v1c.shp$",
    full.names = T
  )
  
  au_basins <- sf::st_read(filenames)
  
  return(au_basins)
}

au_basins <- load_basins()
sf::sf_use_s2(F)

# basin intersection with border
au_basin <- au_basins |> 
  sf::st_intersection(
    australia
  ) |>
  dplyr::select(
    HYBAS_ID
  )

# get river data ----------------------------------------------------------
# download river data
get_rivers <- function(){
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_au_shp.zip"
  file_name <- "HydroRIVERS_v10_au_shp.zip"
  
  au_rivers <- download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_rivers()

list.files()

# load river data
load_rivers <- function(){
  filenames <- list.files(
    path = "HydroRIVERS_v10_au_shp",
    pattern = ".shp$",
    full.names = T
  )
  
  au_rivers <- sf::st_read(filenames)
  
  return(au_rivers)
}

au_rivers <- load_rivers()

# river intersection with border
au_river <- au_rivers |> 
  sf::st_intersection(
    australia
  ) |>
  dplyr::select(
    ORD_FLOW
  )

# determine basin for every river -----------------------------------------
au_river_basin <- sf::st_intersection(
  au_river,
  au_basin
)

st_write(au_river_basin, "au-river-basins.shp", layer = NULL)

# river width -------------------------------------------------------------
unique(au_river_basin$ORD_FLOW)

au_river_basin_width <- au_river_basin |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 3 ~ .6,
      width == 4 ~ .45,
      width == 5 ~ .35,
      width == 6 ~ .25,
      width == 7 ~ .2,
      width == 8 ~ .15,
      width == 9 ~ .1,
      width == 10 ~ .05,
      TRUE ~ 0
    )
  ) |> 
  sf::st_as_sf()

st_write(au_river_basin_width, "au-river-basin_width.shp", layer = NULL)
# visualization -----------------------------------------------------------
unique(au_river_basin_width$HYBAS_ID)

hcl.pals("qualitative")

p <- ggplot() +
  geom_sf(
    data = au_river_basin_width,
    mapping = aes(color = factor(HYBAS_ID),
                  size = width,
                  alpha = width)
  ) +
  scale_color_manual(
    name = "",
    values = hcl.colors(10, "Harmonic", alpha = 1)
  ) +
  scale_size(
    range = c(.1, .7)
  ) +
  scale_alpha(
    range = c(.01, .7)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 20, color = "grey60",
      hjust = 0.5, vjust = .2),
    plot.caption = element_text(
      size = 8, color = "grey60",
      hjust = .1, vjust = 10),
    plot.margin = unit(
      c(t = 0, r = 0,
        b = 0, l = 0), "lines"),
    plot.background = element_rect(
      fill = "black",
      color = NA),
    panel.background = element_rect(
      fill = "black",
      color = NA)
  ) +
  labs(
    title = "Rivers of Australia",
    x = "",
    y = "",
    caption = "©2023 Ziyadatul Hikmah | #30daymapchallage | Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org"
    )

ggsave(
  filename = "day02-lines.png",
  width = 10, height = 9, dpi = 600,
  bg = "black", device = "png", p
)

# done ! ------------------------------------------------------------------


