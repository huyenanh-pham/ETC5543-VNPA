

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


get_ala_data_speciesGroup <- function(){
  # Credentials
  galah_config(email = "hpha0042@student.monash.edu",
               download_reason_id = 10, verbose = TRUE)
  
  # Define polygon boundary
  poly_laf <- st_multipoint(c(
    st_point(c(147.4, -37.8)), # p1
    st_point(c(148.9, -37.8)), # p2 
    st_point(c(148.9, -37.2)), # p3 
    st_point(c(147.4, -37.2))  # p4
  )) %>%
    st_cast("POLYGON") |> # st_sfc(crs = st_crs(vic_map))
    st_sfc(crs = 4326)
  
  df <- galah_call() |> 
    galah_geolocate(poly_laf) |> # within LAF project area 
    galah_filter(!is.na(eventDate),
                 !is.na(speciesGroup),
                 eventDate >= "2017-01-01T00:00:00Z", 
                 eventDate <= "2021-07-23T00:00:00Z") |> 
    galah_select(speciesGroup, basisOfRecord, recordedBy, cl22, BASIS_OF_RECORD_INVALID,
                 group = c("taxonomy","basic","event")) |>
    atlas_occurrences(mint_doi = TRUE) |> 
    # Process data 
    filter(BASIS_OF_RECORD_INVALID == FALSE) |> # Remove invalid records
    mutate(Year = lubridate::year(eventDate)) 
  
  return(df)
}




# Get data from ALA by polygon boundary and Date -------------------------------

get_ala <- function(input_poly = NULL,
                    input_start_date = NULL,
                    input_end_date = NULL) {
  # Credentials
  galah_config(email = "hpha0042@student.monash.edu",
               download_reason_id = 10,
               verbose = TRUE)
  
  df <- galah_call() |>
    # Filter by boundary sf polygon
    galah_geolocate(input_poly, type = "polygon") |>
    # Filter Timeframe
    galah_filter(
      !is.na(eventDate),
      !is.na(speciesGroup),
      cl22 == "Victoria",
      eventDate >= input_start_date,
      eventDate <= input_end_date
    ) |>
    # Select columns
    galah_select(
      speciesGroup, 
      basisOfRecord, dataResourceName, recordedBy,
      BASIS_OF_RECORD_INVALID,
      group = c("taxonomy","basic","event")) |> 
    atlas_occurrences(mint_doi = TRUE) |> 
    # Remove invalid records
    filter(BASIS_OF_RECORD_INVALID == FALSE) |>
    # Remove columns
    dplyr::select(-c(BASIS_OF_RECORD_INVALID))
  
  return(df)
}