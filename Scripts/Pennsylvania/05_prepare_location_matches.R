
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
dataPath <- "./Data/Pennsylvania"
usGeoNamesFilename <- "./Data/US.txt"
paPrecinctsFilename <- ""
paGeoNamesFilename <- "PAGeoNames.rds"
paCityNeighborhoodsPath <- "/CityNeighborhoods/"
paLocationMatchesFilename <- "PALocationMatches.rds"

# import county fips codes from tigris ----
paFIPSCodes <- tigris::fips_codes |>
  dplyr::filter(state == "PA") |>
  dplyr::select(Code = county_code, County = county) |>
  dplyr::mutate(Code = as.numeric(Code)) |>
  dplyr::mutate(County = stringr::str_remove(string = County, pattern = " County$"))

# import georgia precincts shapefile ----
paPrecincts <- sf::st_read(dsn = file.path(dataPath, paPrecinctsFilename)) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(UNIQUE_ID, County)

# import georgia geonames (source data too large for github) ----
# paGeoNames <- utils::read.delim(file = file.path(usGeoNamesFilename), header = FALSE) |>
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
#   dplyr::filter(CountryCode == "US", Admin1Code == "PA") |>
#   dplyr::select(-c(CC2, Admin4Code, Dem, Timezone, ModificationDate))
# 
# # save georgia geonames ----
# saveRDS(object = paGeoNames, file = file.path(dataPath, paGeoNamesFilename))

# import georgia geonames ----
paGeoNames <- readRDS(file = file.path(dataPath, paGeoNamesFilename))

# expand georgia geonames to include alternate names ----
paGeoNames <- paGeoNames |>
  dplyr::mutate(Name = paste(Name, Asciiname, AlternateNames, sep = ",")) |>
  tidyr::separate_longer_delim(cols = Name, delim = ",") |>
  dplyr::filter(Name != "") |>
  dplyr::distinct()

# add county names to georgia geonames ----
paGeoNames <- paGeoNames |> dplyr::left_join(y = paFIPSCodes, by = c("Admin2Code" = "Code"))

# clean shapefile data for each administrative level ----

## landmark matches ----
paLandmarks <- tigris::landmarks(state = "PA", type = "area") |>
  dplyr::select(Name = FULLNAME) |>
  tidyr::drop_na() |>
  dplyr::mutate(AdminLevel = "Landmark")

## school matches ----

### filter geonames ----
paSchools <- paGeoNames |>
  dplyr::filter(FeatureCode == "SCH") |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name, County) |>
  dplyr::mutate(AdminLevel = "School")

### match precincts ----
paSchoolPrecincts <- paSchools |>
  dplyr::mutate(
    UNIQUE_ID = paPrecincts[["UNIQUE_ID"]][
      geomander::geo_match(
        from = paSchools,
        to = paPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(UNIQUE_ID) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

## neighborhood matches ----

### assemble city neighborhoods ----
paCityNeighborhoods <- file.path(dataPath, paCityNeighborhoodsPath) |>
  list.files(full.names = TRUE) |>
  purrr::map(.f = \(cityNeighborhoodData) readRDS(cityNeighborhoodData)) |>
  purrr::list_rbind() |>
  dplyr::select(Name = nbhd_name, Code = county) |>
  dplyr::mutate(Code = as.numeric(Code)) |>
  dplyr::group_by(Name, Code) |>
  dplyr::summarise(Name = unique(Name), Code = unique(Code)) |>
  dplyr::left_join(paFIPSCodes, by = "Code", keep = FALSE)

### match precincts ----
paCityNeighborhoodPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paCityNeighborhoods[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paCityNeighborhoods,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "Neighborhood",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

### filter geonames ----
paPointNeighborhoods <- paGeoNames |>
  dplyr::filter(FeatureCode == "PPL", Population == 0) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name, County) |>
  dplyr::mutate(AdminLevel = "Neighborhood")

### match precincts ----
paPointNeighborhoodPrecincts <- paPointNeighborhoods |>
  dplyr::mutate(
    UNIQUE_ID = paPrecincts[["UNIQUE_ID"]][
      geomander::geo_match(
        from = paPointNeighborhoods,
        to = paPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(UNIQUE_ID) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

# combine city neighborhood and geonames neighborhood precincts ----
paNeighborhoodPrecincts <- dplyr::bind_rows(
  paCityNeighborhoodPrecincts,
  paPointNeighborhoodPrecincts
) |> dplyr::distinct(Name, AdminLevel, Counties, .keep_all = TRUE)

## municipality matches ----

### import shapefile ----
paMunicipalities <- tigris::places(state = "PA", cb = TRUE) |>
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
paMunicipalityPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paMunicipalities[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paMunicipalities,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "Municipality",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

## school district matches ----

### import shapefile ----
paSchoolDistricts <- tigris::school_districts(state = "PA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAME) |>
  dplyr::mutate(AdminLevel = "School District")

### match precincts ----
paSchoolDistrictPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paSchoolDistricts[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paSchoolDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "School District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

## county matches ----

### import shapefile ----
paCounties <- tigris::counties(state = "PA") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "County")

### match precincts ----
paCountyPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paCounties[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paCounties,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "County",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

## legislative district matches ----

### import state house district shapefile ----
paStateHouseDistricts <- tigris::state_legislative_districts(state = "PA", year = 2020, house = "lower") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "State House District")

### match precincts ----
paStateHouseDistrictPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paStateHouseDistricts[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paStateHouseDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "State House District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

### import state senate district shapefile ----
paStateSenateDistricts <- tigris::state_legislative_districts(state = "PA", year = 2020, house = "upper") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "State Senate District")

### match precincts ----
paStateSenateDistrictPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paStateSenateDistricts[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paStateSenateDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "State Senate District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

### import congressional district shapefile ----
paCongressionalDistricts <- tigris::congressional_districts(state = "PA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(Name = NAMELSAD) |>
  dplyr::mutate(AdminLevel = "Congressional District")

### match precincts ----
paCongressionalDistrictPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paCongressionalDistricts[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paCongressionalDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ],
    AdminLevel = "Congressional District",
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  tidyr::nest(Precincts = UNIQUE_ID, Counties = County)

## region matches ----
paRegions <- paGeoNames |>
  dplyr::filter(FeatureCode %in% c("RGN", "RGNH", "RGNE", "RGNL")) |>
  dplyr::distinct(Geonameid, .keep_all = TRUE) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "NAD83") |>
  dplyr::select(Name) |>
  dplyr::mutate(AdminLevel = "Region")

## other matches ----

# combine location matches into singular data frame ----
paLocationMatches <- dplyr::bind_rows(
  paSchoolPrecincts,
  paNeighborhoodPrecincts,
  paMunicipalityPrecincts,
  paSchoolDistrictPrecincts,
  paCountyPrecincts,
  paStateHouseDistrictPrecincts,
  paStateSenateDistrictPrecincts,
  paCongressionalDistrictPrecincts
)

# save location matches data ----
saveRDS(object = paLocationMatches, file = file.path(dataPath, paLocationMatchesFilename))
