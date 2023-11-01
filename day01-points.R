# -------------------------------------------------------------------------
# 30 DAYS MAP CHALLENGE 2023
# day01- points
# title: A history of nautical piracy in Indonesia
# data: https://msi.nga.mil/Piracy 
# script by Ziyadatul Hikmah | 01 Nov 2023
# -------------------------------------------------------------------------

# install and load libraries ----------------------------------------------
# libraries we need
libs <- c("ggplot2","sf","ggtext","showtext","sysfonts","dplyr")

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
setwd("D:/Ziy/30 days map challange")

# customized parameter of visualization -----------------------------------
# add fonts
sysfonts::font_add_google("Righteous", family = "clim")
sysfonts::font_add_google("Montserrat", family = "mont")

showtext_auto()

# read shapefile and point (maritim savety information) coord data --------
indonesia_boundary <- st_read("data/day01/indo.shp")
pirate_assault <- st_read("data/day01/pirate_assault_indo_5km.shp")

# visualization -----------------------------------------------------------
# map
g <- ggplot() +
  geom_sf(
    data = indonesia_boundary,
    size = 3,
    fill = "transparent",
    color = "#07CFF7"
  ) +
  geom_sf(
    data = subset(pirate_assault),
    size = 4,
    color = "#FFB115"
  ) +
  labs(
    title = "A history of nautical piracy in Indonesia (1986 - 2020)",
    subtitle = "Thirty four years of nautical piracy through slices of the calendar",
    caption = "©2023 Ziyadatul Hikmah | #30daymapchallage | Data: https://msi.nga.mil/Piracy"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#032326",
                                   color = NA),
    panel.background = element_rect(fill = "#032326",
                                    color = NA),
    plot.title = element_text(family = "clim",
                              hjust = 0.5,
                              size = 100,
                              color = "grey90",
                              face = "bold"),
    plot.subtitle = element_text(family = "mont",
                                 hjust = 0.5,
                                 size = 65,
                                 color = "grey90"),
    plot.caption = element_text(family = "mont",
                                size = 30,
                                hjust = 0.5,
                                color = "grey90")
  ) + 
  ggsave(
    filename = "day01-points.png",
    device = "png",
    dpi = 300,
    width = 15,
    height = 7,
    path = "output"
  )

# done !  -----------------------------------------------------------------


  