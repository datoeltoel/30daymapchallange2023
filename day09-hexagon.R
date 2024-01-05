# -------------------------------------------------------------------------
# 30 DAYS MAP CHALLENGE 2023
# day09-hexagons
# title: CO2 Emissions for Indonesia in 2021
# data: https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/CO2_excl_short-cycle_org_C/TOTALS/v7.0_FT2021_CO2_excl_short-cycle_org_C_2021_TOTALS.zip
# script by Ziyadatul Hikmah | 09 Nov 2023
# -------------------------------------------------------------------------

# install and load libraries ----------------------------------------------
# libraries we need
libs <- c("tidyverse", "readr", "janitor",
          "classInt", "rayshader", "terra",
          "giscoR", "sf","maps")

# install missing libraries
installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# set working directory ---------------------------------------------------
setwd("D:/Ziy/30 days map challange/data/day09")

# get CO2 data ------------------------------------------------------------
# download CO2 data
get_co2_data <- function() {
  url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/CO2_excl_short-cycle_org_C/TOTALS/v7.0_FT2021_CO2_excl_short-cycle_org_C_2021_TOTALS.zip"
  file_name <- "co2_emission_2021.zip"
  co2_data <- download.file(url = url,
                            destfile = file_name, 
                            mode = "wb")
  unzip(file_name)
  
}

co2_data <- get_co2_data()

# load and clean the data -------------------------------------------------
file <- list.files(
  path = "v7.0_FT2021_CO2_excl_short-cycle_org_C_2021_TOTALS",
  pattern = "*.txt",
  full.names = T
)

read_df <- function() {
  main_df <- readr::read_delim(
    file,
    delim = ";",
    col_names = T
  ) |> janitor::row_to_names(row_number = 2)
  
  names(main_df) <- "lat;long;emission"
  
  df <- main_df |>
    tidyr::separate(
      "lat;long;emission",
      into = c(
        "lat", "long", "emission"
      ),
      sep = ";"
    )
  
  final_df <- df |>
    dplyr::mutate_if(
      is.character, as.numeric
    )
  
  return(final_df)
}

co2_emission_df <- read_df()

# get border country ------------------------------------------------------
indonesia_sf <- giscoR::gisco_get_countries(
  country = "IDN",
  resolution = 3
)

indonesia_transformed <- indonesia_sf |>
  sf::st_transform(3857)

# make hexagons -----------------------------------------------------------
indonesia_hex <- sf::st_make_grid(
  indonesia_transformed,
  cellsize = units::as_units(
    2500, "km^2"
  ),
  what = "polygons",
  square = F
) |>
  sf::st_intersection(
    sf::st_buffer(
      indonesia_transformed, 0
    )
  ) |>
  sf::st_as_sf() |>
  dplyr::mutate(
    id = row_number()
  ) |>
  sf::st_make_valid()

sf::st_geometry(indonesia_hex) <- "geometry"

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

indonesia_final <- indonesia_hex |>
  dplyr::filter(
    !grepl(
      "POINT",
      sf::st_geometry_type(geometry)
    )
  ) |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(indonesia_hex))

# point within hex polygon ------------------------------------------------
co2_sf <- co2_emission_df |>
  sf::st_as_sf(
    coords = c(
      "long", "lat"
    )
  ) |>
  sf::st_set_crs(crsLONGLAT)

co2_indonesia_sf <- sf::st_join(
  co2_sf, indonesia_final,
  join = sf::st_within
) |>
  drop_na()

# aggregate and join ------------------------------------------------------
get_aggregated_co2 <- function() {
  co2_indonesia_sum <- co2_indonesia_sf |>
    dplyr::group_by(id) |>
    dplyr::summarise_at(
      vars(emission),
      list(sum_co2 = sum)
    ) |>
    sf::st_set_geometry(
      NULL
    )
  
  co2_indonesia_hex <- dplyr::left_join(
    indonesia_final, co2_indonesia_sum,
    by = "id"
  )
  
  co2_indonesia_hex$sum_co2 <- round(
    co2_indonesia_hex$sum_co2, 0
  )
  
  co2_indonesia_hex$sum_co2[
    is.na(co2_indonesia_hex$sum_co2)
  ] <- 0
  
  co2_indonesia_hex$sum_co2 <- co2_indonesia_hex$sum_co2 / 2500
  
  return(co2_indonesia_hex)
}

co2_indonesia_hex <- get_aggregated_co2()
summary(co2_indonesia_hex$sum_co2)


# breaks and palette color ------------------------------------------------
breaks <- classInt::classIntervals(
  co2_indonesia_hex$sum_co2,
  n = 8,
  style = "pretty"
)$brks

cols <- colorRampPalette(rev(c(
  "#91003f", "#ce1256", "#e7298a",
  "#df65b0", "#c994c7","#d4b9da",
  "#e7e1ef")))

# annotate hex map --------------------------------------------------------
data(world.cities)

indonesia_cities <- world.cities |>
  dplyr::filter(
    country.etc == "Indonesia"
  ) |>
  dplyr::slice_max(
    pop,
    n = 10
  )

# hex map visualization ---------------------------------------------------
p <- ggplot(data = co2_indonesia_hex) +
  geom_sf(aes(fill = sum_co2), color = NA) +
  ggrepel::geom_text_repel(
    data = indonesia_cities,
    aes(x = long, y = lat, label = name),
    color = "grey40",
    segment.size = .25,
    force = .75,
    segment.curvature = -.3
  ) +
  scale_fill_gradientn(
    name = "tons (thousands)",
    colors = cols(11),
    breaks = breaks
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, "mm"),
      keywidth = unit(15, "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = 0,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18, color = "#451a40",
      hjust = .5, vjust = 3
    ),
    legend.position = "top",
    legend.title = element_text(
      size = 10, color = "grey10"
    ),
    legend.text = element_text(
      size = 9, color = "grey10"
    ),
    plot.margin = unit(
      c(t = 1, r = 0, b = 0, l = 0), "lines"
    )
  ) +
  labs(
    title = "CO2 Emissions for Indonesia in 2021"
  )

ggsave(
  filename = "day09-hexagons-indonesia.png",
  width = 15, height = 7, dpi = 600, bg = "white", p
)

# 3D visualization of hexagon map -----------------------------------------
# make raster matrix
make_raster_matrix <- function() {
  co2_rast <- terra::rasterize(
    co2_indonesia_hex,
    terra::rast(
      co2_indonesia_hex,
      resolution = .01
    ),
    co2_indonesia_hex$sum_co2
  ) |> terra::na.omit()
  
  co2_mat <- rayshader::raster_to_matrix(
    co2_rast
  )
  
  return(co2_mat)
}

co2_mat <- make_raster_matrix()

# create the initial 3D object
texture <- colorRampPalette("#b74952")(256)

co2_mat |>
  rayshader::height_shade(texture) |>
  rayshader::plot_3d(
    heightmap = co2_mat,
    solid = F,
    soliddepth = 0,
    zscale = 20,
    shadowdepth = 0,
    shadow_darkness = .99,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# use render_camera to adjust the view
rayshader::render_camera(
  phi = 85, zoom = .5, theta = 0
)

# annotate
for (i in 1:max(nrow(indonesia_cities))){
  rayshader::render_label(
    co2_mat,
    lat = indonesia_cities$lat[i],
    long = indonesia_cities$long[i],
    extent = co2_indonesia_hex,
    altitude = (150000 - i * 10000),
    zscale = 50,
    text = indonesia_cities$name[i],
    textcolor = "navyblue",
    linecolor = "navyblue",
    linewidth = 1,
    dashed = F,
    family = "mono"
  )
}

# render
rayshader::render_highquality(
  filename = "day09-hexagons_3d.png",
  preview = T,
  interactive = F,
  light = T,
  lightdirection = c(
    225, 215, 225, 215
  ),
  lightaltitude = c(
    15, 15, 75, 75
  ),
  lightintensity = c(
    750, 1000, 150, 150
  ),
  parallel = T,
  width = 4000,
  height = 4000,
  size = 30
)

# done !!! ----------------------------------------------------------------


