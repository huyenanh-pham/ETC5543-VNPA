# Check Fire data 

# Load raw data 
tar_load(fire_history)

# Subset 
fire_history <- fire_history |>
  filter(FIRE_SEASON == 2020 & 
         between(START_DATE,
                 lubridate::ymd("2019-11-01"),
                 lubridate::ymd("2020-05-31"))) 
  # filter(FIRE_SEASON == 2009)

# Subset only Bunyip State Park area 
# Parks boundary
tar_load(parkres)
bbox_bunyip <- st_bbox(parkres |> filter(NAME == "Bunyip State Park")) 
fire_history_bunyip <- st_crop(fire_history, bbox_bunyip)

# Convert VNPA site list into sf object 
vnpa_bunyip_site <- vnpa_bunyip_site |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(fire_history))

# ---------------------------------------------------------------
# Get FIRE_HISTORY records contains VNPA sites 
# tar_load(vnpa_bunyip_site)
site_fire_history <- st_join(vnpa_bunyip_site, fire_history_bunyip, join = st_within)

site_fire_history |> group_by(site_id) |> 
  summarise(max_fire_season = max(SEASON)) |> 
  mutate(time_since_fire = 2021 - max_fire_season, 
         time_since_fire_cate = ) |> 
  arrange(site_id) |> 
  View()
         

selected_key <- unique(c(site_fire_history |> 
                           filter(year(START_DATE) > 2008 & !is.na(FIREKEY)) |> 
                           select(FIREKEY))$FIREKEY) 

fire_history |> filter(FIREKEY %in% selected_key) |> 
  st_drop_geometry() |> 
  group_by(FIREKEY) |> 
  summarise(n=n())
 

# ---------------------------------------------------------------
# Select columns
fire_history_subset |> 
  select("FIRETYPE", "SEASON", "FIRE_NO", "NAME", "STRTDATIT", "FIRE_SVRTY", "FIREKEY", "ACCURACY", "DSE_ID", "CFA_ID", "geometry") |> 
  rename(START_DATE = STRTDATIT, 
         FIRE_NAME = NAME,
         FIRE_SEASON = SEASON) |>
  mutate(START_DATE = lubridate::ymd(START_DATE)) 
  # |> View()

# Count number of fire records per year (FIRE_SEASON)
fire_history |> 
  st_drop_geometry() |>
  group_by(FIRE_SEASON) |> 
  summarise(n = n()) |> 
  ggplot() + 
  geom_line(aes(x = FIRE_SEASON, y = n))

fire_history |> 
  st_drop_geometry() |>
  group_by(FIRE_NO) |>   
  summarise(n = n()) |>
  filter(n > 10) |> 
  arrange(desc(n))

# Plot 
ggplot() +
  geom_sf(data = vic_map) +
  geom_sf(data = fire_history,
          fill = alpha("darkred",0.7)) +
  coord_sf(xlim = c(146, 150.5), ylim = c(-39.2,-35.0)) # Zoom
  theme_bw()
  
ggplot() + 
  geom_sf(data = sf::st_crop(vic_map, 
                             xmin = 146, xmax = 150.5, ymin = -39.2, ymax = -35.0)) +
  theme_bw()


fire_history |> 
  st_drop_geometry() |> 
  ggplot() + 
  geom_histogram(aes(x = START_DATE)) 

# Megafire 2019-20 -------------------------------------------------------------
# Union all fire_1920 record into one multi-polygon
poly_fire_1920 <- sf::st_union(fire_history_1920$geometry, by_feature = FALSE)
# Define bounding box
bbox_fire_1920 <-
  st_bbox(
    c(
      xmin = 146.3000,
      xmax = sf::st_bbox(poly_fire_1920)[["xmax"]],
      ymin = sf::st_bbox(poly_fire_1920)[["ymin"]],
      ymax = -35.3
    ),
    crs = st_crs(vic_map)
  )
bbox_laf <-
  st_bbox(
    c(
      xmin = 147.4,
      xmax = 148.9,
      ymin = -37.8,
      ymax = -37.2
    ),
    crs = st_crs(vic_map)
  )
# p1 = st_point(c(147.4, -37.8))
# p2 = st_point(c(148.9, -37.8))
# p3 = st_point(c(148.9, -37.2))
# p4 = st_point(c(147.4, -37.2))
poly_laf <- st_multipoint(c(
  st_point(c(147.4, -37.8)), # p1
  st_point(c(148.9, -37.8)), # p2 
  st_point(c(148.9, -37.2)), # p3 
  st_point(c(147.4, -37.2))  # p4
  )) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(vic_map))

# Plot megafires 2019-20 extension
ggplot() + 
  geom_sf(data = vic_map) +
  geom_sf(data = poly_fire_1920, fill = alpha("darkred",0.5)) +
  geom_sf(data = parkres, fill = alpha("limegreen",0.5)) + 
  geom_sf(data = poly_laf, col = "orange", fill = "transparent", linewidth = 1) + 
  coord_sf(xlim = bbox_fire_1920[c("xmin", "xmax")], ylim = bbox_fire_1920[c("ymin", "ymax")]) + # Zoom
  theme_bw()


