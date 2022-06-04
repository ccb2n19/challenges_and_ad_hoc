library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)
library(magrittr)
library(forcats)
library(sf)
library(tmap)
library(traveltime)
library(ggmap)
library(osmdata)

# Set up -----------------------------------------------------------------------
path <- "stanmore_accessibility"
source(paste0(path, "/setup_api.r"))
extrafont::loadfonts(device = "win", quiet = TRUE)

# Get start location -----------------------------------------------------------
station <- lst()

station$string <- "Stanmore Underground Station"

station$point <- station$string %>% 
  geocode() %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  ) %>%
  st_transform(
    27700
  )

# Get 5 kilometre radius for filtering the data --------------------------------
buffer_amount <- 5000

station$buffer <- station$point %>%
  st_buffer(
    buffer_amount
  )

station$grid <- station$buffer %>%
  st_make_grid(
    n = 10
  ) %>%
  st_as_sf() %>%
  rename(
    geometry = x
  )

station$map_area <- station$grid %>% 
  slice(41:100) %>%
  summarise()

station$immediate_area <- station$point %>%
  st_buffer(
    1000
  )

# Get OS data ------------------------------------------------------------------
os_data <- lst()

os_data$orig <- "london_vmd" %>%
  list.files(
    recursive = TRUE,
    full.names = TRUE
  ) %>%
  tibble(
    path = .
  ) %>%
  filter(
    path %>% str_detect(".shp$"),
    path %>% str_detect("Building|Woodland|Water_Area")
  ) %>%
  mutate(
    name = path %>% str_extract("(?<=\\/[:upper:][:upper:]_).+(?=.shp)" ),
    data = path %>% map(
      ~.x %>% read_sf()
    )
  )

# Filter data using the buffer and split ---------------------------------------
os_data$subset <- os_data$orig %>%
  mutate(
    data = data %>% map(
      ~.x %>% st_intersection(station$map_area)
    )
  ) %>%
  unnest(
    data
  ) %>%
  st_as_sf() %>%
  select(
    ID, name
  ) %>%
  group_by(
    name
  ) %>%
  group_split()

# Get the public transport network ---------------------------------------------
osm <- lst()

osm$bb <- station$map_area %>% st_transform(4326)

osm$opq <- opq(
  bbox = osm$bb
)

osm$bus$response <- osm$opq %>%
  add_osm_feature(
    key = "route", value = "bus"
  ) %>%
  osmdata_sf()

osm$bus$subset <- osm$bus$response %>%
  extract2(
    "osm_multilines"
  ) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(
    27700
  ) %>%
  select(
    ref,
    operator,
    route,
    from,
    to
  ) %>%
  st_filter(
    station$immediate_area
  ) %>%
  distinct(
    ref, .keep_all = TRUE
  ) %>%
  st_intersection(
    station$map_area
  ) %>%
  mutate(
    label = "Bus from\n" %>% paste0(station$string %>% word(start = 1L))
  )

osm$rail$response <- osm$opq %>%
  add_osm_feature(
    key = "railway", value = "station"
  ) %>%
  osmdata_sf()

osm$rail$subset <- osm$rail$response %>%
  extract2(
    "osm_points"
  ) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(
    27700
  ) %>%
  select(
    name,
    operator,
    line
  ) %>%
  st_filter(
    station$map_area
  ) %>%
  mutate(
    label = case_when(
      is.na(line) ~ "National Rail",
      TRUE ~ "Underground"
    )
  )

# Get place names --------------------------------------------------------------
osm$places$response <- osm$opq %>%
  add_osm_feature(
    key = "place", value = c("town", "village", "hamlet", "suburb")
  ) %>%
  osmdata_sf()

osm$places$subset <- osm$places$response %>%
  extract2(
    "osm_points"
  ) %>%
  select(
    name,
    place
  ) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(
    27700
  ) %>%
  filter(
    place != "hamlet",
    !name %in% c("Stanmore", "Mill Hill", "Arkley") 
  )

# Get the travel time data -----------------------------------------------------
isos <- lst()

isos$input <- station$point %>%
  expand_grid(
    minutes = c(5, 10, 15, 20, 30, 45, 60),
    mode = "public_transport",
    time = c("2022-07-01T18:00:00+01:00", "2022-07-01T18:10:00+01:00")
  ) %>%
  st_as_sf()

source("stanmore_accessibility/mutate_traveltime.R")

isos$output$one <- isos$input[1:4,] %>%
  mutate_isochrone()

Sys.sleep(90)

isos$output$two <- isos$input[5:8,] %>%
  mutate_isochrone()

Sys.sleep(90)

isos$output$three <- isos$input[9:12,] %>%
  mutate_isochrone()

Sys.sleep(90)

isos$output$four <- isos$input[13:14,] %>%
  mutate_isochrone()

isos$subset <- isos$output %>%
  bind_rows() %>%
  select(
    minutes,
    mode,
    time
  )

# Bounce the isochrone down to the buildings data ------------------------------
housing <- lst()

housing$grid <- station$map_area %>%
  st_make_grid(
    cellsize = 50
  ) %>%
  st_as_sf() %>%
  rename(
    geometry = x
  )

housing$split <- os_data$subset[[1]] %>%
  st_intersection(
    housing$grid
  ) %>%
  select(
    ID
  ) %>%
  mutate(
    uid = row_number()
  ) %>%
  st_join(
    isos$subset,
    largest = FALSE
  ) %>%
  arrange(
    minutes
  ) %>%
  distinct(
    uid,
    .keep_all = TRUE
  )

housing$data <- housing$split %>%
  arrange(
    minutes
  ) %>%
  mutate(
    minutes = case_when(
      is.na(minutes) ~ "Longer",
      minutes == 60 ~ "Longer",
      TRUE ~ minutes %>% as.character() %>% paste0(" minutes")
    ),
    minutes = minutes %>% fct_inorder()
  )

# Make map ---------------------------------------------------------------------
pal <- lst(
  bg = "grey20",
  bus = "grey70",
  border = "grey30",
  fill = "grey90",
  water = "grey25",
  woodland = "grey22",
  font = "Bahnschrift"
)

scale <- lst()

scale$line <- tibble(
  lon = c(station$point %>% st_coordinates %>% extract(1), station$point %>% st_coordinates %>% extract(1) - 5000),
  lat = c(station$point %>% st_coordinates %>% extract(2) - 1000, station$point %>% st_coordinates %>% extract(2) - 1000)
) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 27700
  ) %>%
  summarise() %>%
  st_cast("LINESTRING")

scale$label <- scale$line %>%
  mutate(
    length = st_length(.) / 1000,
    label = length %>% paste0("km")
  ) %>%
  st_centroid()

plot <- ggplot() +
  geom_sf(
    data = os_data$subset[[2]] %>% bind_rows(),
    fill = pal$water,
    colour = pal$water
  ) +
  geom_sf(
    data = os_data$subset[[3]] %>% bind_rows(),
    fill = pal$water,
    colour = pal$woodland
  ) +
  geom_sf(
    data = housing$data,
    aes(
      fill = minutes
    ),
    colour = alpha("white", 0),
    show.legend = "point"
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    direction = -1
  ) +
  geom_sf(
    data = osm$bus$subset,
    aes(
      colour = label
    ),
    lwd = 1.5
  ) +
  scale_colour_manual(
    values = pal$bus
  ) +
  geom_sf(
    data = osm$rail$subset,
    aes(
      shape = label
    ),
    colour = pal$border,
    fill = pal$fill,
    size = 4,
    stroke = 3
  ) +
  scale_shape_manual(
    values = c("National Rail" = 22, "Underground" = 21)
  ) +
  geom_sf_label(
    data = osm$places$subset,
    aes(
      label = name
    ),
    label.size  = NA,
    col = pal$fill,
    fill = pal$bg,
    family = pal$font,
    alpha = 0.5
  ) +
  geom_sf_label(
    data = osm$rail$subset %>% filter(name %>% str_detect(station$string %>% word())),
    aes(
      label = name
    ),
    nudge_y = 350,
    family = pal$font,
    fill = pal$bg,
    col = pal$fill
  ) +
  geom_sf(
    data = scale$line,
    colour = pal$fill,
    lwd = 1.5
  ) +
  geom_sf_label(
    data = scale$label,
    aes(
      label = label
    ),
    family = pal$font,
    fill = pal$bg,
    col = pal$fill
  ) +
  labs(
    title = "Public transport travel times from " %>% paste0(station$string),
    caption = c(
      "Made by Chris Brown @ChrisB_Go using R | Based on departure at either 18:00 or 18:10 on 1 July 2022 | Travel times from © 2021 TravelTime",
      "Buildings are from Ordnance Survey data © Crown copyright and database right 2022 | Everything else is Open Street Map data © OpenStreetMap contributors"
     ) %>% paste(collapse = "\n"),
    fill = "",
    shape = "",
    colour = ""
  ) +
  theme_void(
    base_size = 15,
    base_family = pal$font
  ) +
  theme(
    legend.position = "right",
    legend.justification = "top",
    plot.background = element_rect(fill = pal$bg, colour = pal$bg),
    text = element_text(colour = pal$fill),
    plot.caption = element_text(size = 8, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 0.5, r = 0, b = 0.5, l = 0, "cm"),
    panel.border = element_blank()
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(shape = 21, size = 5)),
    shape = guide_legend(),
    colour = guide_legend()
  )

dim <- lst(
  h = 512,
  w = 1024
)

ggsave(
  filename = path %>% paste0("/plot/public_transport_accessibility.png"),
  plot = plot,
  height = dim$h * 4,
  width = dim$w * 4,
  units = "px"
)
