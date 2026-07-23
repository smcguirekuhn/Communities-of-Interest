
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
library(alarmdata)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia"
usGeoNamesFilename <- "./Data/US.txt"
gaPrecinctsIDColumn <- "GEOID20"
gaGeoNamesFilename <- "GAGeoNames.rds"
gaCityNeighborhoodsPath <- "/CityNeighborhoods/"
gaLocationMatchesFilename <- "GALocationMatches.rds"

# import county fips codes from tigris ----
gaFIPSCodes <- tigris::fips_codes |>
  dplyr::filter(state == "GA") |>
  dplyr::select(Code = county_code, County = county) |>
  dplyr::mutate(Code = as.numeric(Code))

# import georgia precincts shapefile ----
gaPrecincts <- alarmdata::alarm_census_vest(state = "GA", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(
    PrecinctID = dplyr::all_of(x = gaPrecinctsIDColumn),
    County = dplyr::all_of(x = "county")
  )

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

# add county names to georgia geonames ----
gaGeoNames <- gaGeoNames |> dplyr::left_join(y = gaFIPSCodes, by = c("Admin2Code" = "Code"))

# clean shapefile data for each administrative level ----

## landmark matches ----
gaLandmarks <- tigris::landmarks(state = "GA", type = "area") |>
  dplyr::select(Name = FULLNAME) |>
  tidyr::drop_na() |>
  dplyr::mutate(AdminLevel = "Landmark")

## school matches ----

### filter geonames ----
gaSchools <- gaGeoNames |>
  dplyr::filter(FeatureCode == "SCH") |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name, County) |>
  dplyr::mutate(AdminLevel = "School")

### match precincts ----
gaSchoolPrecincts <- gaSchools |>
  dplyr::mutate(
    PrecinctID = gaPrecincts[["PrecinctID"]][
      geomander::geo_match(
        from = gaSchools,
        to = gaPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(PrecinctID) |>
  tidyr::nest(Precincts = PrecinctID, Counties = County)

## neighborhood matches ----

### assemble city neighborhoods ----
gaCityNeighborhoods <- file.path(dataPath, gaCityNeighborhoodsPath) |>
  list.files(full.names = TRUE) |>
  purrr::map(.f = \(cityNeighborhoodData) readRDS(cityNeighborhoodData)) |>
  purrr::list_rbind() |>
  dplyr::select(Name = nbhd_name, Code = county) |>
  dplyr::mutate(Code = as.numeric(Code)) |>
  dplyr::group_by(Name, Code) |>
  dplyr::summarise(Name = unique(Name), Code = unique(Code)) |>
  dplyr::left_join(gaFIPSCodes, by = "Code", keep = FALSE)

### match precincts ----
gaCityNeighborhoodPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaCityNeighborhoods[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaCityNeighborhoods,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "Neighborhood",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = PrecinctID, Counties = County)

### filter geonames ----
gaPointNeighborhoods <- gaGeoNames |>
  dplyr::filter(FeatureCode == "PPL", Population == 0) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name, County) |>
  dplyr::mutate(AdminLevel = "Neighborhood")

### match precincts ----
gaPointNeighborhoodPrecincts <- gaPointNeighborhoods |>
  dplyr::mutate(
    PrecinctID = gaPrecincts[["PrecinctID"]][
      geomander::geo_match(
        from = gaPointNeighborhoods,
        to = gaPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(PrecinctID) |>
  tidyr::nest(Precincts = PrecinctID, Counties = County)

# combine city neighborhood and geonames neighborhood precincts ----
gaNeighborhoodPrecincts <- dplyr::bind_rows(
  gaCityNeighborhoodPrecincts,
  gaPointNeighborhoodPrecincts
) |> dplyr::distinct(Name, AdminLevel, Counties, .keep_all = TRUE)

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
  tidyr::nest(Precincts = PrecinctID, Counties = County)

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
  tidyr::nest(Precincts = PrecinctID, Counties = County)

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
  tidyr::nest(Precincts = PrecinctID, Counties = County)

## legislative district matches ----

### import state house district shapefile ----
gaStateHouseDistricts <- tigris::state_legislative_districts(state = "GA", year = 2020, house = "lower") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "State House District")

### match precincts ----
gaStateHouseDistrictPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaStateHouseDistricts[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaStateHouseDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "State House District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = PrecinctID, Counties = County)

### import state senate district shapefile ----
gaStateSenateDistricts <- tigris::state_legislative_districts(state = "GA", year = 2020, house = "upper") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "State Senate District")

### match precincts ----
gaStateSenateDistrictPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaStateSenateDistricts[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaStateSenateDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "State Senate District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = PrecinctID, Counties = County)

### import congressional district shapefile ----
gaCongressionalDistricts <- tigris::congressional_districts(state = "GA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "Congressional District")

### match precincts ----
gaCongressionalDistrictPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaCongressionalDistricts[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaCongressionalDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "Congressional District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = PrecinctID, Counties = County)

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
  gaSchoolPrecincts,
  gaNeighborhoodPrecincts,
  gaMunicipalityPrecincts,
  gaSchoolDistrictPrecincts,
  gaCountyPrecincts,
  gaStateHouseDistrictPrecincts,
  gaStateSenateDistrictPrecincts,
  gaCongressionalDistrictPrecincts
)

# save location matches data ----
saveRDS(object = gaLocationMatches, file = file.path(dataPath, gaLocationMatchesFilename))
