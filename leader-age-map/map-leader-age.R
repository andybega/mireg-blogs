
library("tidyverse")
library("sf")
library("googlesheets")
library("lubridate")
library("rnaturalearth")
library("rgeos")
library("raster")
library("cshapes")
library("hrbrthemes")
library("cowplot")

key <- "1mrtORyhXw9TJMBYLAGPrikA4VDpla_Eq7L-NsEQ5VXg"
binsize <- 4

leaders <- key %>%
  gs_key() %>%
  gs_read(ws = "Leader List")

leaders <- leaders %>%
  mutate(
    gwcode = case_when(
      CCODE==255 ~ 260L,
      CCODE==679 ~ 678L,
      CCODE==345 ~ 340L,  # Serbia, not panel safe
      CCODE==970 ~ 971L,  # Nauru
      CCODE==946 ~ 970L,  # Kiribati
      CCODE==955 ~ 972L,  # Tonga
      CCODE==947 ~ 973L,  # Tuvalu
      TRUE ~ CCODE
    ),
    start_date = as.Date(sprintf("%s-%s-%s", `START YEAR`, `START MONTH`, `START DAY`)),
    end_date = as.Date(sprintf("%s-%s-%s", `END YEAR`, `END MONTH`, 15))
    ) %>%
  filter(is.na(end_date)) %>%
  mutate(leader_age = today() - as.Date(paste0(`BIRTH YEAR`, "-06-15"), "%Y-%m-%d")) %>%
  mutate(leader_age = as.numeric(leader_age / 365.25))

map <- cshp(date = as.Date("2012-12-31")) %>%
  # the map data has some points outside 180 degrees, this causes plotting 
  # issues with projected data
  raster::crop(., extent(-180, 180, -90, 90)) %>%
  st_as_sf() %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform("+proj=robin") %>%
  st_simplify(dTolerance = 10000, preserveTopology = TRUE) %>%
  dplyr::select("GWCODE", "geometry")

# Cut age into bins
cutter <- function(x, bs = binsize) {
  cut(x, 
      breaks = seq(30, 94, by = bs), 
      include.lowest = TRUE)
}

map <- left_join(map, leaders, by = c("GWCODE" = "gwcode"))

# I had an issue here initially getting mutate to work with y_name...the problem
# was using sf::mutate by default; hopefully using dplyr::mutate does not 
# break anything. 
map <- map %>%
  dplyr::mutate(age_bins = cutter(leader_age, binsize))

# Cshapes maps are missing Greenland and some other places, get a background
# map to show the correct world
bg_map <- ne_countries(scale = 110, type = 'countries') %>%
  st_as_sf() %>%
  st_transform("+proj=robin")

# Main map
p1 <- ggplot() + 
  geom_sf(data = bg_map, size = .2, color = "white") +
  geom_sf(data = map, aes(fill = age_bins), size = .1, color = "white") +
  theme_ipsum() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank()) +
  scale_fill_viridis_d(guide = FALSE, direction = -1) +
  labs(x = "", y = "")

# Histogram for map legend
bks <- c(min(map$leader_age, na.rm = T), 
         c(40, 60, 80), 
         max(map$leader_age, na.rm = T)) %>%
  round(1)
p2 <- ggplot(map) +
  geom_histogram(aes(x = leader_age, fill = age_bins), binwidth = binsize) +
  theme_ipsum() +
  scale_fill_viridis_d(guide = FALSE, direction = -1) +
  coord_flip() + 
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(linetype = 3, color = "gray50", size = .2),
        axis.title.y = element_text(angle = 0, hjust = 1),
        axis.title.x = element_text(size = 8),
        axis.ticks.y = element_line(color = "gray50"),
        axis.text.y = element_text(margin = margin(r = 1)),
        plot.title = element_text(size = 10, face = "plain",
                                  margin=margin(b = 1, unit = "pt"))) +
  scale_y_continuous(position = "left", breaks = c(0, 15, 30), 
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = bks, labels = gsub("\\.0", "", bks), 
                     expand = c(0, 0)) +
  labs(x = "", y = "#", title = "Age in years")
p2

# Overlap histogram on main map
g <- ggplotGrob(p2)
pp <- p1 + 
  annotation_custom(grob = g, xmin = -1.9e7, xmax = -9e6, ymin = -1.05e7, ymax = 2e6) +
  labs(title = "Age of state leaders",
       subtitle = sprintf("On %s", today())) +
  annotate("text", x = 16e6, y = -8e6, label = "Data: Archigos, REIGN",
           fontface = "italic", hjust = 1, size = 3, family = "Arial Narrow")
ggsave(pp, file = "map-leader-age.png", height = 5.5, width = 8)

