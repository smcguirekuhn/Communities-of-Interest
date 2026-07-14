
# Script 05: Prepare Location Match Shapefiles for Each Administrative Level

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(tigris)
library(sf)
library(geomander)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia"
usGeoNamesFilename <- "./Data/US.txt"
gaPrecinctsFilename <- "/ga_2024_gen_prec/ga_2024_gen_all_prec/ga_2024_gen_all_prec.shp"
gaGeoNamesFilename <- "GAGeoNames.rds"
gaLocationMatchesFilename <- "GALocationMatches.rds"

# import georgia precincts shapefile ----
gaPrecincts <- sf::st_read(dsn = file.path(dataPath, gaPrecinctsFilename)) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(UNIQUE_ID)

# import georgia geonames (source data too large for github) ----
# gaGeoNames <- utils::read.delim(file = file.path(usGeoNamesFilename), header = FALSE) |>
#   stats::setNames(
#     nm = c(
#       "Geonameid", "Name", "Asciiname", "AlternateNames",
#       "Latitude", "Longitude",
#       "FeatureClass", "FeatureCode",
#       "CountryCode", "CC2",
#       "Admin1Code", "Admin2Code", "Admin3Code", "Admin4Code",
#       "Population", "Elevation", "Dem", "Timezone",
#       "ModificationDate"
#     )
#   ) |>
#   dplyr::filter(CountryCode == "US", Admin1Code == "GA") |>
#   dplyr::select(-c(CC2, Admin4Code, Dem, Timezone, ModificationDate))
# 
# # save georgia geonames ----
# saveRDS(object = gaGeoNames, file = file.path(dataPath, gaGeoNamesFilename))

# import georgia geonames ----
gaGeoNames <- readRDS(file = file.path(dataPath, gaGeoNamesFilename))

# expand georgia geonames to include alternate names ----
gaGeoNames <- gaGeoNames |>
  dplyr::mutate(Name = paste(Name, Asciiname, AlternateNames, sep = ",")) |>
  tidyr::separate_longer_delim(cols = Name, delim = ",") |>
  dplyr::filter(Name != "") |>
  dplyr::distinct()

# clean shapefile data for each administrative level ----

## landmark matches ----
gaLandmarks <- tigris::landmarks(state = "GA", type = "area") |>
  dplyr::select(Name = FULLNAME) |>
  tidyr::drop_na() |>
  dplyr::mutate(AdminLevel = "Landmark")

## school matches ----
gaSchools <- gaGeoNames |>
  dplyr::filter(FeatureCode == "SCH") |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name) |>
  dplyr::mutate(AdminLevel = "School")

## neighborhood matches ----
gaNeighborhoods <- gaGeoNames |>
  dplyr::filter(FeatureCode == "PPL", Population == 0) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name) |>
  dplyr::mutate(AdminLevel = "Neighborhood")

## municipality matches ----

### import shapefile ----
gaMunicipalities <- tigris::places(state = "GA", cb = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAME) |>
  dplyr::mutate(
    Name = stringr::str_replace(
      string = Name,
      pattern = "-.*?(?i)county.*",
      replacement = ""
    ),
    AdminLevel = "Municipality"
  )

### match precincts ----
gaMunicipalityPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaMunicipalities[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaMunicipalities,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "Municipality",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID)

## school district matches ----

### import shapefile ----
gaSchoolDistricts <- tigris::school_districts(state = "GA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAME) |>
  dplyr::mutate(AdminLevel = "School District")

### match precincts ----
gaSchoolDistrictPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaSchoolDistricts[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaSchoolDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "School District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID)

## county matches ----

### import shapefile ----
gaCounties <- tigris::counties(state = "GA") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "County")

### match precincts ----
gaCountyPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaCounties[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaCounties,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "County",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID)

## legislative district matches ----

## region matches ----
gaRegions <- gaGeoNames |>
  dplyr::filter(FeatureCode %in% c("RGN", "RGNH", "RGNE", "RGNL")) |>
  dplyr::distinct(Geonameid, .keep_all = TRUE) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name) |>
  dplyr::mutate(AdminLevel = "Region")

## other matches ----

# combine location matches into singular data frame ----
gaLocationMatches <- dplyr::bind_rows(
  gaMunicipalityPrecincts,
  gaSchoolDistrictPrecincts,
  gaCountyPrecincts
)

# save location matches data ----
saveRDS(object = gaLocationMatches, file = file.path(dataPath, gaLocationMatchesFilename))
