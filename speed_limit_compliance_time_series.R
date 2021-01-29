library(tidyverse)
library(janitor)
library(readODS)
library(RColorBrewer)
library(scales)
library(cowplot)
library(extrafont)
extrafont::font_import(path="C:/Users/brown/Desktop/fonts/", prompt =  FALSE)
loadfonts(device = "win")
library(showtext)
font_add(family = "Transport Heavy",  regular = "C:/Users/brown/Desktop/fonts/Transport Heavy.ttf")
font_add(family = "Transport Gr",  regular = "C:/Users/brown/Desktop/fonts/Transport Medium Greek.ttf")
font_add(family = "Transport", regular = "C:/Users/brown/Desktop/fonts/Transport Medium.ttf")

### speed_limit_complience_distribuition.R must be run first ###

# Get annual data for 2011 to 2018 ------------------------------------------------------------------------
vehicle_types <- "Cars|LCVs|Short bus|Long bus|All Rigid HGVs|All Articulated HGVs|Motorcycles"
ts_file <- "spe0112.ods"

time_series <- read_ods(ts_file, skip = 5) %>%
  as_tibble(.name_repair = "unique") %>%
  rename(road_type = ...1,
         measure   = ...2) %>%
  fill(road_type, .direction = "down") %>%
  mutate(vehicle_type = case_when(measure %>% str_detect(vehicle_types) ~ measure %>% str_extract(vehicle_types),
                                                                   TRUE ~ as.character(NA))) %>%
  fill(vehicle_type, .direction = "down") %>%
  filter(vehicle_type == "Cars",
         measure      == "Exceeding speed limit (%)") %>%
  select(-...11, -vehicle_type) %>%
  pivot_longer(cols      = 3:11,
               names_to  = "year",
               values_to = "share_over_speed_limit") %>%
  mutate(year = year %>% str_extract("[:digit:][:digit:][:digit:][:digit:]") %>% as.numeric(),
         share_over_speed_limit = case_when(share_over_speed_limit == ".." ~ as.numeric(NA),
                                                                      TRUE ~ share_over_speed_limit %>% as.numeric()),
         road_type         = road_type %>% str_replace_all("\\s*\\([^\\)]+\\) |\\s*\\([^\\)]+\\)|", ""),
         road_type         = road_type %>% str_to_lower(),
         nice_road_type    = case_when(road_type == "motorways" ~ "Motorways",
                                       road_type == "national speed limit single carriageways" ~ "Single carriageways",
                                       road_type == "30 mph speed limit roads" ~ "Built-up areas",
                                       road_type == "20 mph speed limit roads" ~ "20 mph speed limit"),
         speed_limit = case_when(road_type == "motorways" ~ 70,
                                 road_type == "national speed limit single carriageways" ~ 60,
                                 road_type == "30 mph speed limit roads" ~ 30,
                                 road_type == "20 mph speed limit roads" ~ 20)) %>%
  arrange(speed_limit) %>%
  filter(year < 2019)

# Summarise quarterly figures for 2019 to 2020----------------------------------------------------------------------
quarterly <- long_data %>% 
  left_join(limits, by = "road_type") %>%
  filter(year %in% c(2019, 2020),
         speed_limit <= speed_lower_bound) %>%
  group_by(period, year, road_type, nice_road_type) %>%
  summarise(share_over_speed_limit = sum(share)) %>%
  mutate(year = case_when(period == "january_to_march" ~ year,
                          period == "april_to_june" ~ year + 0.25,
                          period == "july_to_september" ~ year + 0.5,
                          period == "october_to_december" ~ year + 0.75)) %>%
  select(-period)

# Join datasets together -------------------------------------------------------------------------------------------
full_data <- bind_rows(
  time_series,
  quarterly
) %>%
  mutate(nice_road_type = nice_road_type %>% str_wrap(9) %>% fct_inorder())


# Set colour palette and styles -------------------------------------------------------------------------------------
palette <- colorRampPalette(brewer.pal(4, "Spectral"))(4)

breaks <- seq(min(full_data$year) + 1, max(full_data$year), by = 2)

font <- "Transport Heavy"

# Set speed limits, titles and their positions ----------------------------------------------------------------------
speed_limits <- full_data %>%
  select(nice_road_type, speed_limit) %>%
  distinct(nice_road_type, .keep_all = TRUE) %>%
  mutate(x = 2019,
         title_x = 2010,
         y = 105)

# Create faceted plot ----------------------------------------------------------------------
ggplot(
  data = full_data,
  aes(x = year, 
      y = share_over_speed_limit)
      ) +
  geom_line(
    aes(colour = nice_road_type),
    lwd = 2,
    show.legend = FALSE
    ) +
  scale_colour_manual(
    values = palette
    ) +
  scale_x_continuous(
    breaks = breaks
    ) +
  scale_y_continuous(labels = c("25%", "50%", "75%"),
                     breaks = c(25, 50, 75),
                     limits = c(0, 110)) +
# Add speed limit signs ---------------------------------------------------------
  geom_point(data   = speed_limits, aes(x = x, y = y - 5),
             shape  = 21,
             size   = 20,
             stroke = 6,
             fill   = "white",
             colour = "#FF1945") +
  geom_text(
    data   = speed_limits, aes(x = x, y = y - 5, label = speed_limit),
    family = font,
    size   = 8) +
# Add facet labels as text object, rather than using the strip -------------------
  geom_text(
    data = speed_limits, 
    aes(x     = title_x, 
        y     = y, 
    label = nice_road_type),
    family = font,
    size   = 8,
    hjust  = 0,
    vjust  = 1
    ) +
  labs(
    x = "",
    y = "",
    title = "The share of drivers exceeding the speed limit, by road type",
    subtitle = "Under free-flowing conditions in Great Britain.\nAnnually from 2011 to 2018, and quarterly from January 2019 to September 2020.",
    caption = "Source: DfT, 2020, Vehicle speed compliance statistics for Great Britain, spe0112 and spe2501\nAuthor: Christopher C Brown | Twitter: chrisb_go\nSource code: github.com/ccb2n19\nFont: Transport Heavy, Open Government Licence v1.0, www.roads.org.uk/fonts"
  ) +
  facet_wrap(
    ~nice_road_type,
    nrow = 1
    ) +
  theme_minimal(
    base_family = "Transport Heavy"
    ) +
  theme(
   axis.text = element_text(size = 15, colour = "black"),
   strip.text = element_blank(),
   plot.caption = element_text(hjust = 0, size = 10,
                               margin = margin(2, 0, 0, 0, unit = "cm")),
   plot.title = element_text(hjust = 0, size = 30),
   plot.subtitle = element_text(hjust = 0, 
                                size = 15,
                                margin = margin(0, 0, 2, 0, unit = "cm")),
   plot.background = element_rect(fill = "grey98",
                                  colour = "transparent"),
   plot.margin = unit(c(3,1.5,3,1), "cm")
  )
