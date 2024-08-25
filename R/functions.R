

# Get data from ALA by species name --------------------------------------------
get_ala_data <- function(species_comm_name = NULL) {
  # Query Occurrences (Basic + Event) data from ALA
  galah_config(email = "hpha0042@student.monash.edu",
               download_reason_id = 10, verbose = TRUE)
  
  
  df <- galah_call() |>
  galah_identify(species_comm_name) |>
  galah_filter(cl22 == "Victoria") |>
  galah_select(basisOfRecord, recordedBy, cl22, BASIS_OF_RECORD_INVALID,
               group = c("basic","event")) |>
  atlas_occurrences(mint_doi = TRUE) |> 
  # Process data
    filter(BASIS_OF_RECORD_INVALID == FALSE # Filter valid records 
           & !is.na(eventDate)) |>
    mutate(Year = lubridate::year(eventDate)) |> 
  # Convert to sf object
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude")
                 # crs = "GDA2020" # st_crs(ala_sbb_raw) <- st_crs(vic_map)
    )
  
  return(df)
}
