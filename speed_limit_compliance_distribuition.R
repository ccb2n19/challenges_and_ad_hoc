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


# Get file -------------------------------------------------------------------------------------------------------------
file <- "spe2501.ods"
orig <- read_ods(file, skip = 7) %>%
  as_tibble(.name_repair = "unique") %>%
  rename(road_type = ...1,
         speed     = ...2) %>%
  clean_names() %>%
  fill(road_type, .direction = "down")

# Clean data and make long---------------------------------------------------------------------------------------------- 
long_data <- orig %>%
  filter(speed %>% str_detect("mph$|or over$")) %>%
  select(-october_to_december_10) %>%
  pivot_longer(cols = 3:ncol(.),
               names_to  = "period",
               values_to = "share") %>%
  mutate(year              = period %>% str_extract("[:digit:]") %>% as.numeric(),
         year              = case_when(year >= 7 ~ 2020,
                                            TRUE ~ 2019),
         period            = period %>% str_remove("_[:digit:]"),
         speed_upper_bound = speed %>% str_extract("(?<=-)[:digit:][:digit:]") %>% as.numeric(),
         speed_upper_bound = case_when(is.na(speed_upper_bound) ~ speed %>% str_extract("[:digit:][:digit:]") %>% as.numeric() - 1,
                                                           TRUE ~ speed_upper_bound),
         speed_lower_bound = speed %>% str_extract("^[:digit:][:digit:]") %>% as.numeric(),
         speed_lower_bound = case_when(is.na(speed_lower_bound) ~ speed_upper_bound - 9,
                                                           TRUE ~ speed_lower_bound),
         road_type         = road_type %>% str_replace_all("\\s*\\([^\\)]+\\) |\\s*\\([^\\)]+\\)|", ""),
         road_type         = road_type %>% str_to_lower(),
         share             = share %>% str_replace("-", "0") %>% as.numeric(),
         nice_road_type    = case_when(road_type == "motorways" ~ "Motorways",
                                       road_type == "national speed limit single carriageways" ~ "Single carriageways",
                                       road_type == "30 mph speed limit roads" ~ "Built-up areas")) %>%
  select(road_type, nice_road_type, period, year, speed_lower_bound, speed_upper_bound, share)

# Set speed limits ---------------------------------------------------------------------------------------------------
limits <- long_data %>% distinct(road_type) %>% tibble(speed_limit = c(70, 60, 30))

# Create summary of 2020 compliance ----------------------------------------------------------------------------------
summary <- long_data %>%
  group_by(road_type, nice_road_type, year, speed_lower_bound, speed_upper_bound) %>%
  summarise(average_share = share %>% mean()) %>%
  ungroup() %>%
  group_by(year, road_type, nice_road_type) %>%
  mutate(multiplier = 100 / sum(average_share)) %>%
  ungroup() %>%
  mutate(average_share = average_share * multiplier) %>%
  select(-multiplier)

# Get smart rounding function (https://biostatmatt.com/archives/2902) ------------------------------------------------
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# Convert this into 1,000 observations for each road_type ----------------------------------------------------------
rounded <- summary %>%
  mutate(base_1000 = average_share * 10) %>%
  group_by(year, road_type, nice_road_type) %>%
  mutate(base_1000 = base_1000 %>% round_preserve_sum()) %>%
  ungroup()

# Disaggregate the data, so that each count is represented as a row ------------------------------------------------
disagg <- rounded %>%
  uncount(weights = base_1000) %>%
  left_join(limits, by = "road_type")

# Set an X and a Y coordinate for each observation -----------------------------------------------------------------
for_plot <- disagg %>%
  filter(year == 2020) %>%
  group_by(road_type, speed_upper_bound) %>%
  mutate(num_in_group    = row_number() %>% as.numeric(),
         squares_to_fill = (speed_upper_bound - speed_lower_bound) + 1,
         y = ((num_in_group / squares_to_fill) - 0.0001) %>% trunc()) %>%
  group_by(road_type, speed_upper_bound, y) %>%
  mutate(x = row_number() + speed_lower_bound) %>%
  ungroup() %>%
  mutate(x = x - speed_limit) %>%
  arrange(x) %>%
# Rejig the coordinates so that each plot is centrered around 0 (the speed limit) rather than the speed itself ------
  mutate(lower_from_limit   = speed_lower_bound - speed_limit + 1,
         upper_from_limit   = speed_upper_bound - speed_limit + 2,
         speed_category     = case_when(speed_upper_bound < speed_limit ~ lower_from_limit,
                                                                   TRUE ~ upper_from_limit),
         speed_category_fac = speed_category %>% as.character() %>% fct_inorder()) %>%
  arrange(speed_limit) %>%
  mutate(road_type = road_type %>% fct_inorder())

# Set axis labels --------------------------------------------------------------------------------------------------
labels <- for_plot %>%
  select(road_type, speed_lower_bound, speed_upper_bound, speed_category, speed_limit) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(x = speed_category %>% as.character() %>% as.numeric(),
         y = 0,
         label = case_when(speed_upper_bound < speed_limit ~ paste0(speed_lower_bound, "\nmph"),
                                                      TRUE ~ paste0(speed_upper_bound + 1, "\nmph")))
# Set titles -------------------------------------------------------------------------------------------------------
titles <- for_plot %>%
  select(nice_road_type, road_type, x, y, speed_limit) %>%
  mutate(label = nice_road_type %>% str_to_sentence(),
         label = label %>% str_wrap(9),
         y = max(for_plot %>% filter(road_type == "motorways") %>% pull(y) %>% max())) %>%
  group_by(road_type) %>%
  mutate(x = min(x)) %>%
  distinct(road_type, .keep_all = TRUE)

limits_labs <- limits %>%
  mutate(x = 0 + 0.5,
         y = -3)

# Set palette and styles ---------------------------------------------------------------------------------------------------
font <- "Transport Heavy"
palette <- colorRampPalette(brewer.pal(11, "Spectral") %>% rev())(length(for_plot$speed_category_fac %>% unique()) + 2)[1:10]
names(palette) <- for_plot %>% arrange(x) %>% distinct(speed_category_fac) %>% pull(speed_category_fac)

# Create main plot ---------------------------------------------------------------------------------------------------------
main_plot <- ggplot(
  data = for_plot, 
  aes(x = x, y = y)
  ) +
  geom_tile(
    aes(fill = speed_category_fac), 
    col = "white", 
    lwd = 0.5, 
    width = 1, 
    height = 1, 
    show.legend = FALSE
    ) +
  scale_fill_manual(
    values = palette
    ) +
# Add dashed lines for speed limits ---------------------------------------------------------
  geom_segment(
    x    = 0.5,
    xend = 0.5,
    y    = 0,
    yend = for_plot$y %>% max() + 5,
    lty = "dashed", 
    lwd = 1, 
    colour = "grey80"
    ) +
  geom_text(
    data = labels, 
    aes(x = x, y = y, label = label),
    hjust = 0.2, 
    vjust = 1.5,
    family = font
    ) +
  geom_text(
    data = titles,
    aes(x = x, y = y, label = label),
    family = font,
    fontface = "bold",
    size = 8,
    hjust = 0,
    vjust = 1
  ) +
# Add speed limit signs ---------------------------------------------------------
  geom_point(
    data = titles,
    aes(x = x + 3,
        y = y - 15),
    shape = 21,
    fill = "white",
    colour = "#FF1945",
    size = 20,
    stroke = 6) +
  geom_text(
    data = titles,
    aes(label = speed_limit,
    x = x + 3,
    y = y - 16.3),
    vjust = -0,
    size = 8,
    family = font,
    fontface = "bold") +
# Make vertical limits wider to accommodate other plot elements ----------------
  lims(
    y = c(-20, (for_plot$y %>% max()) + 20)
    ) +
  facet_wrap(
    ~road_type,
    nrow = 1,
    scales = "free_x"
    ) +
  theme_void(
    base_family = font
    ) +
  theme(strip.text = element_blank(),
        plot.background = element_rect(fill = "grey98",
                                       colour = "transparent"),
        plot.margin = unit(c(0,0.5,0,0), "cm"))

# Create a legend, by making just one square of the grid ---------------------------------------------------------------
leg_1 <- ggplot() +
  geom_tile(aes(
    x = 1,
    y = 1),
    colour = "grey10",
    lwd = 1, 
    width = 1, 
    height = 1, 
    show.legend = FALSE
  ) +
  theme_void(
    base_family = font
  ) +
  coord_equal(
  )

# Bring together and add additional annotations ---------------------------------------------------------------------------
ggdraw(
  main_plot
  ) +
  draw_label(
    "Speed limit",
    x = 0.578,
    y = 0.8,
    fontfamily = font
    ) +
  draw_plot(
    leg_1,
    height = 0.01,
    width = 0.01,
    x = 0.5 + 0.38,
    y = 0.1 + 0.8
    ) +
  draw_label(
    "One car\nin a thousand\non each road type.",
    x = 0.515 + 0.38,
    y = 0.89,
    fontfamily = font,
    hjust = 0,
    size = 14
    ) +
  draw_label(
    "The distribution of people's driving speeds around different speed limits",
    x = 0.014,
    y = 0.95,
    fontfamily = font,
    hjust = 0,
    vjust = 1,
    size = 20
    ) +
  draw_label(
    "Measured under free-flowing conditions on Great British roads between January and September 2020",
    x = 0.014,
    y = 0.915,
    fontfamily = font,
    hjust = 0,
    vjust = 1,
    size = 14
    ) +
  draw_label(
    "Source: DfT, 2020, Vehicle speed compliance statistics for Great Britain, spe2501\nAuthor: Christopher C Brown | Twitter: chrisb_go\nSource code: github.com/ccb2n19\nFont: Transport Heavy, Open Government Licence v1.0, www.roads.org.uk/fonts",
    x = 0.014,
    y = 0.125,
    fontfamily = font,
    hjust = 0,
    vjust = 1,
    size = 14
    )
