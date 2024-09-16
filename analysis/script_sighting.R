# ALA galah --------------------------------------------------------------------
# Get Data from ALA 
# 0. Dictionary 
# request_metadata(type = "fields") |>
#   collect() |>
#   dplyr::filter(grepl("record", id)) # filter column name

# Search for fields that include the word "date"
# galah::search_all(fields, "name")

## Count of records
# (https://galah.ala.org.au/R/reference/atlas_counts.html?q=atlas_counts#null)

# credential
galah_config(email = "hpha0042@student.monash.edu",
             download_reason_id = 10, verbose = TRUE)
  
bbox_bunyip <- st_bbox(parkres |> filter(NAME == "Bunyip State Park")) 
poly_bunyip <- st_as_sfc(bbox_bunyip)

galah_call() |> 
  galah_geolocate(poly_bunyip, type = "polygon") |> 
  # galah_filter(
  #     speciesGroup == "Mammals",
  #     year == 2016) |> 
  atlas_counts()

# Get all ALA mammals records within Bunyip State Park
ala_bunyip <- 
  galah_call() |> 
  galah_geolocate(poly_bunyip) |>
  galah_filter(speciesGroup == "Mammals", 
               !is.na(eventDate)) |> 
  galah_select(speciesGroup, basisOfRecord, recordedBy, cl22, BASIS_OF_RECORD_INVALID,
               group = c("taxonomy","basic","event")) |>
  atlas_occurrences(mint_doi = TRUE)
  
ala_bunyip |> 
  group_by(year(eventDate)) |>
  summarise(n=n()) |> View()

ala_bunyip |> 
  filter(genus == "Dama") |>
  View()

# Convert to sf object
ala_bunyip <- ala_bunyip |> 
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"))
st_crs(ala_bunyip) <- st_crs(vic_map)
st_crs(ala_sbb_raw) <- st_crs(vic_map)
             
# Plot
ggplot() +
  geom_sf(data = vic_map) + 
  geom_sf(data = parkres, fill=alpha("limegreen",0.1)) +
  geom_sf(data = ala_bunyip |> filter(genus == "Vulpes"), 
          color = "orange", size = 1) +
  geom_sf(data = ala_sbb_raw, 
          color = "black", size = 1) +
  coord_sf(xlim = bbox_bunyip[c("xmin", "xmax")], ylim = bbox_bunyip[c("ymin", "ymax")]) # Zoom
  

ala_sbb_raw$scientificName |> head(1)
ala_bunyip |> filter(scientificName == "Isoodon obesulus obesulus") |> 
  group_by(lubridate::year(eventDate)) |> 
  summarise(n = n())

## Goal: Get ALA data for extended area (East Gippsland) ----------------(start)
### Function: fucntion.R / get_ala()
### Target output: ala_sighting

# Define boundary of ALA sighting (sf polygon)
poly_filter_ala <- sf::st_multipoint(c(
  st_point(c(146.3, -38)), # p1
  st_point(c(sf::st_bbox(poly_fire_1920)[["xmax"]], -38)), # p2 
  st_point(c(sf::st_bbox(poly_fire_1920)[["xmax"]], -35.9)), # p3 
  st_point(c(146.3, -35.9))  # p4
)) |>
  st_cast("POLYGON") |>
  st_sfc(crs = st_crs(vic_map)) # |> sf::st_intersection(sf::st_union(vic_map))

ggplot() +
  geom_sf(data = vic_map) +
  geom_sf(data = poly_filter_ala, fill = "red", alpha = 0.3) +
  coord_sf(xlim = c(146.3, sf::st_bbox(poly_fire_1920)[["xmax"]]),
           ylim = c(-38, -35.5))

input_poly <- poly_filter_ala
input_start_date <- "2017-01-01T00:00:00Z"
input_end_date <- "2021-07-23T00:00:00Z"

ala_sighting <- get_ala(
  input_poly = poly_filter_ala,
  input_start_date = "2017-01-01T00:00:00Z",
  input_end_date = "2021-07-23T00:00:00Z"
)
# -------------------------------------------------------------------------(end)



# VBA --------------------------------------------------------------------------

# tar_load(vba_fauna)
# VBA sighting within PARKRES areas 
# vba_parkres <- st_join(vba_fauna |> dplyr::filter(START_YEAR > 1999), 
#                        parkres[,"NAME"], 
#                        join = st_within, left = FALSE) |> # inner join 
#   dplyr::rename(PARKRES_NAME = NAME)
# rm(vba_fauna)


# vba_parkres |> filter(PARKRES_NAME == "Bunyip State Park") |> 
#   group_by(START_YEAR) |>
#   summarise(n = n()) |> View()

# Records contributed by VNPA
vba_fauna_sbb |> 
  filter(COLLECTOR == "Sera Blair") |> 
  ggplot() +
  geom_histogram(aes(x = START_YEAR))

unique(vba_fauna_sbb$COLLECTOR) |> sort()

# PARKRES ----------------------------------------------------------------------
parkres |> filter(NAME == "Bunyip State Park") |> View()


# EVC --------------------------------------------------------------------------
# Plot
ggplot() +
  geom_sf(data = vic_map) + 
  geom_sf(data = evc, fill = "XGROUPNAME")
