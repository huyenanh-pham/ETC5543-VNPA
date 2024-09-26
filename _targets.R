library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(data_summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
tar_source("R/functions.R")

# Set target-specific options such as packages:
tar_option_set(packages = c("utils","tidyverse","sf","galah", "terra"))

# End this file with a list of target objects.
list(
  # Load data ------------------------------------------------------------------
  tar_target(vic_map, sf::read_sf(
    here::here("data/SA2_2021_AUST_SHP_GDA2020/SA2_2021_AUST_GDA2020.shp")) |>
    filter(STE_CODE21 == 2 & !is.na(AREASQKM21)) # STE_NAME21 == "Victoria" 
    ),
  tar_target(crs_GDA2020, sf::st_crs(vic_map)),
  
  # VNPA's Bunyip sites
  tar_target(vnpa_bunyip_site,
             read_csv(here::here("data/vnpa_bunyip_site.csv")) |> 
               janitor::clean_names()),
  
  # Load data from ALA
  tar_target(ala_sbb_raw, 
             get_ala_data(species_comm_name = "Southern Brown Bandicoot")),
  
  # Load data from VBA
  tar_target(
    vba_fauna_filepaths,
    c("C:/Users/huyen/OneDrive/home/1_School/B6022/1_ETC5543_Project/ETC5543-VNPA/raw-data/Order_7JR9NQ/ll_gda2020/esrishape/whole_of_dataset/victoria/FLORAFAUNA1/VBA_FAUNA25.shp",
      "C:/Users/huyen/OneDrive/home/1_School/B6022/1_ETC5543_Project/ETC5543-VNPA/raw-data/Order_7JR9NQ/ll_gda2020/esrishape/whole_of_dataset/victoria/FLORAFAUNA1/VBA_FAUNA25_1.shp"
    )
  ),
  # Read each shapefile and return a list of sf objects
  tar_target(vba_fauna_listOfShp, lapply(vba_fauna_filepaths, sf::read_sf)),
  # Bind 1 and 2
  tar_target(vba_fauna, do.call(what = sf:::rbind.sf, args=vba_fauna_listOfShp)),
  
  # Filepath
  tar_target(filepath,
    c(
      "fire_history" = here::here("raw-data/FIRE_HISTORY_GDA2020/FIRE_HISTORY.shp"), 
      "parkres" = here::here("raw-data/PARKRES_GDA2020/PARKRES.shp"),
      "evc" = here::here("raw-data/EVC_GDA2020/NV2005_EVCBCS.shp"),
      "vba_fauna" = here::here("raw-data/Order_7JR9NQ/ll_gda2020/esrishape/whole_of_dataset/victoria/FLORAFAUNA1/VBA_FAUNA25.shp"),
      "vba_fauna" = here::here("raw-data/Order_7JR9NQ/ll_gda2020/esrishape/whole_of_dataset/victoria/FLORAFAUNA1/VBA_FAUNA25_1.shp")
     ),
    format = "file"        
  ),
  
  # [1] Fire History
  tar_target(fire_history,
    sf::read_sf(filepath[1]) |>
      select("FIRETYPE", "SEASON", "FIRE_NO", "NAME", "STRTDATIT", "FIRE_SVRTY", "FIREKEY", "ACCURACY", "DSE_ID", "CFA_ID", "geometry") |> 
      filter(SEASON > 1969) |> 
      rename(START_DATE = STRTDATIT, 
             FIRE_NAME = NAME,
             FIRE_SEASON = SEASON) |> 
      mutate(START_DATE = lubridate::ymd(START_DATE))
  ),
  
  # [2] Park boundaries
  tar_target(parkres,
    sf::read_sf(filepath[2])
  ),
  # [3] EVC
  tar_target(evc,
    sf::read_sf(filepath[3]) |>
    select("EVC", "EVCBCSDESC", "BIOREGION", "EVC_CODE", "X_EVCNAME", "XGROUPNAME", "XSUBGGROUP", "geometry")
  ),
  
  # Subset data ----------------------------------------------------------------
  tar_target(vba_fauna_sbb,
             vba_fauna |> filter(TAXON_ID == 61092)),
  
  ### LAF -----
  tar_target(fire_history_1920, 
             fire_history |>
               filter(between(
                 START_DATE,
                 lubridate::ymd("2019-11-21"), # Fire Start Date
                 lubridate::ymd("2020-02-29")  # Fire End Date
               ))
  ),
  # Victorian Fires 2019-2020 (November 2019 until February 2020)
  # [FFM VIC - Past bushfires](https://www.ffm.vic.gov.au/history-and-incidents/past-bushfires)
  # [Victorian Fires 2019-2020 Map](https://www.ffm.vic.gov.au/__data/assets/pdf_file/0022/500728/Victorian_fires_fire_area_end1920_Victorian-Fires.pdf)
  
  # Fire 2019/2020 extend (polygon)
  tar_target(poly_fire_1920,
             sf::st_union(fire_history_1920$geometry, by_feature = FALSE)
             ),
  tar_target(poly_laf ,
             sf::st_multipoint(c(
               st_point(c(147.4, -37.8)), # p1
               st_point(c(148.9, -37.8)), # p2 
               st_point(c(148.9, -37.2)), # p3 
               st_point(c(147.4, -37.2))  # p4
             )) %>%
               st_cast("POLYGON") %>%
               st_sfc(crs = st_crs(vic_map))
             ),
  tar_target(ala_laf,
             get_ala_data_speciesGroup()
             ),
  tar_target(fire_history_laf, 
             sf::st_intersection(fire_history, poly_laf) |> 
               filter(FIRE_SEASON >= 1970)
             ),
  tar_target(evc_laf,
             sf::st_intersection(evc, poly_laf)
             ),
  
  
  
  
  
  # [2.1] Define boundary of ALA sighting (sf polygon) = eas (East Gippsland Shire)
  tar_target(poly_filter_ala,
             sf::st_multipoint(c(
               st_point(c(146.3, -38)), # p1
               st_point(c(sf::st_bbox(poly_fire_1920)[["xmax"]], -38)), # p2 
               st_point(c(sf::st_bbox(poly_fire_1920)[["xmax"]], -35.9)), # p3 
               st_point(c(146.3, -35.9))  # p4
             )) |>
               st_cast("POLYGON") |>
               st_sfc(crs = st_crs(vic_map)) 
             # sf::st_intersection(sf::st_union(vic_map))
             ),
  
  # [2.2] Get raw data from ALA
  tar_target(ala_sighting_raw,
             get_ala(
               input_poly = poly_filter_ala,
               input_start_date = "2017-01-01T00:00:00Z",
               input_end_date = "2021-07-23T00:00:00Z")
             ),
  
  # [2.3] Data Wrangling ALA
  tar_target(ala_sighting,
             ala_sighting_raw |> 
             # Convert into sf object
             sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
                          crs = st_crs(vic_map))
            ),
  
    # Grid bounding box of ala_sighting
  tar_target(bbox_filter_ala,
             st_bbox(ala_sighting, crs = sf::st_crs(7844)) # GDA2020 
             ),
  
  # [2.4] Data Wrangling EVC
  tar_target(
    # Crop raw evc down to project extend
    evc_cropped,
    sf::st_intersection(evc, poly_filter_ala)
    ),
  tar_target(
    # Convert sf EVC polygons to terra::SpatVector
    evc_cropped_v, 
    terra::vect(evc_cropped[c("XGROUPNAME", "geometry")])
    ),
  
  # [2.5] Data Wrangling Fire
  tar_target(
    # Fire history in the East Gippsland Shire (eas)
    fire_history_eas,
    fire_history |>
      dplyr::filter(FIRE_SEASON >= 1970) |>
      dplyr::select(FIRE_SEASON, FIRE_NO, START_DATE, FIRE_SVRTY, geometry) |> 
      sf::st_intersection(poly_filter_ala)
  )
  # tar_target(
  #   
  # ),
)
