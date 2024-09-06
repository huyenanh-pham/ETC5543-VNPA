# ALA galah --------------------------------------------------------------------
# Get Data from ALA 
# 0. Dictionary 
# request_metadata(type = "fields") |>
#   collect() |>
#   dplyr::filter(grepl("order", id)) # filter column name

# Search for fields that include the word "date"
# galah::search_all(fields, "order")

## COUNT OF RECORDS -----

# (https://galah.ala.org.au/R/reference/atlas_counts.html?q=atlas_counts#null)

# credential
galah_config(email = "hpha0042@student.monash.edu",
             download_reason_id = 10, verbose = TRUE)
  
bbox_bunyip <- st_bbox(parkres |> filter(NAME == "Bunyip State Park")) 
poly_bunyip <- st_as_sfc(bbox_bunyip)

galah_call() |> 
  galah_geolocate(
    # poly_bunyip, type = "polygon"
    bbox_laf, type = "bbox"
    ) |>
  galah_filter(
  #     speciesGroup == "Mammals",
  #     year == 2016
    eventDate >= "2017-01-01T00:00:00Z", 
    eventDate <= "2021-07-23T00:00:00Z"
       ) |> 
  atlas_counts()

## COUNT group_by -----
galah_call() |> 
  galah_geolocate(
    # poly_bunyip, type = "polygon"
    bbox_laf, type = "bbox"
  ) |>
  galah_filter(
    # speciesGroup == "Mammals",
    eventDate >= "2017-01-01T00:00:00Z", 
    eventDate <= "2021-07-23T00:00:00Z",
  ) |> 
  galah_group_by(year, phylum) |>
  atlas_counts() |> 
  View()


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

# Get all ALA records within LAF project area
galah_config(email = "hpha0042@student.monash.edu",
             download_reason_id = 10, verbose = TRUE)
ala_laf <- 
  galah_call() |> 
  galah_geolocate(poly_laf) |> # within LAF project area 
  galah_filter(!is.na(eventDate),
               !is,na(speciesGroup)) |> 
  galah_select(speciesGroup, basisOfRecord, recordedBy, cl22, BASIS_OF_RECORD_INVALID,
               group = c("taxonomy","basic","event")) |>
  atlas_occurrences(mint_doi = TRUE) |> 
  filter(BASIS_OF_RECORD_INVALID == FALSE) # Remove invalid records


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

# TAXON_TYPE
vba_fauna |> 
  sf::st_drop_geometry() |>
  group_by(TAXON_TYPE) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |> View()

vba_fauna |> 
  sf::st_drop_geometry() |>
  filter(START_YEAR < 2025) |> # Clean data
  filter(START_YEAR > 1999) |>
  ggplot() + 
  geom_histogram(aes(x = START_YEAR), fill = "lightblue") 



# PARKRES ----------------------------------------------------------------------
parkres |> filter(NAME == "Bunyip State Park") |> View()


# EVC --------------------------------------------------------------------------
# Plot
ggplot() +
  geom_sf(data = vic_map)  
  # geom_sf(data = evc, fill = "XGROUPNAME")

