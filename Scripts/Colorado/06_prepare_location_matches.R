
# Script 06: Prepare Location Match Shapefiles for Each Administrative Level in Colorado

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(tigris)
library(sf)
library(ellmer)
library(geomander)
library(alarmdata)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado"
coPrecinctsIDColumn <- "GEOID20"
coLocationMatchesFilename <- "COLocationMatches.rds"

# import colorado precincts shapefile ----
coPrecincts <- alarmdata::alarm_census_vest(state = "CO", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(PrecinctID = dplyr::all_of(x = coPrecinctsIDColumn))

# clean shapefile data for each administrative level ----


## municipality matches ----

### import shapefile ----
coMunicipalities <- tigris::places(state = "CO", cb = TRUE, year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(
    AdminLevel = sub(
      pattern = "^.*\\s(\\w+)$",
      replacement = "\\1",
      x = NAMELSAD
    )
  ) |>
  dplyr::mutate(
    AdminLevel = dplyr::case_when(
      AdminLevel == "CDP" ~ "Neighborhood",
      AdminLevel == "town" ~ "Town",
      AdminLevel == "city" ~ "City"
    )
  ) |>
  dplyr::select(GEOID20 = GEOID, Name = NAME, AdminLevel) |>
  dplyr::mutate(Name = paste(Name, AdminLevel))

### match municipality precincts by area (larger municipalities) ----
coMunicipalityAreaPrecincts <- coPrecincts |>
  dplyr::mutate(
    Name = coMunicipalities[["Name"]][
      geomander::geo_match(
        from = coPrecincts,
        to = coMunicipalities,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct area matches to municipalities data ----
coMunicipalities <- coMunicipalities |>
  dplyr::left_join(coMunicipalityAreaPrecincts, by = "Name")

### isolate remaining smaller municipalities without matched precincts ----
coMunicipalityPointPrecincts <- coMunicipalities |>
  dplyr::slice(which(unlist(lapply(X = coMunicipalities[["Precincts"]], FUN = is.null))))

### match municipality precincts by points (smaller municipalities) ----
coMunicipalityPointPrecincts <- coMunicipalityPointPrecincts |>
  dplyr::select(-Precincts) |>
  dplyr::mutate(
    PrecinctID = coPrecincts[["PrecinctID"]][
      geomander::geo_match(
        from = coMunicipalityPointPrecincts,
        to = coPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to municipalities data ----
coMunicipalities <- coMunicipalities |>
  dplyr::slice(which(!unlist(lapply(X = coMunicipalities[["Precincts"]], FUN = is.null)))) |>
  dplyr::bind_rows(coMunicipalityPointPrecincts) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(Name) |>
  dplyr::mutate(Name = stringr::str_remove(string = Name, pattern = "\\s+\\w+$"))


## school district matches ----

### import shapefile ----
coSchoolDistricts <- tigris::school_districts(state = "CO", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "School District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAME, AdminLevel)

### match precincts ----
coSchoolDistrictPrecincts <- coPrecincts |>
  dplyr::mutate(
    Name = coSchoolDistricts[["Name"]][
      geomander::geo_match(
        from = coPrecincts,
        to = coSchoolDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to school districts data ----
coSchoolDistricts <- coSchoolDistricts |>
  dplyr::left_join(coSchoolDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## county matches ----

### import shapefile ----
coCounties <- tigris::counties(state = "CO", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "County") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

### match precincts ----
coCountyPrecincts <- coPrecincts |>
  dplyr::mutate(
    Name = coCounties[["Name"]][
      geomander::geo_match(
        from = coPrecincts,
        to = coCounties,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to counties data ----
coCounties <- coCounties |>
  dplyr::left_join(coCountyPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## legislative district matches ----

### import state house district shapefile ----
coStateHouseDistricts <- tigris::state_legislative_districts(state = "CO", year = 2020, house = "lower") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "State House District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

### match precincts ----
coStateHouseDistrictPrecincts <- coPrecincts |>
  dplyr::mutate(
    Name = coStateHouseDistricts[["Name"]][
      geomander::geo_match(
        from = coPrecincts,
        to = coStateHouseDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to state house districts data ----
coStateHouseDistricts <- coStateHouseDistricts |>
  dplyr::left_join(coStateHouseDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()

### import state senate district shapefile ----
coStateSenateDistricts <- tigris::state_legislative_districts(state = "CO", year = 2020, house = "upper") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "State Senate District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

### match precincts ----
coStateSenateDistrictPrecincts <- coPrecincts |>
  dplyr::mutate(
    Name = coStateSenateDistricts[["Name"]][
      geomander::geo_match(
        from = coPrecincts,
        to = coStateSenateDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to state senate districts data ----
coStateSenateDistricts <- coStateSenateDistricts |>
  dplyr::left_join(coStateSenateDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()

### import congressional district shapefile ----
coCongressionalDistricts <- tigris::congressional_districts(state = "CO", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "Congressional District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

### match precincts ----
coCongressionalDistrictPrecincts <- coPrecincts |>
  dplyr::mutate(
    Name = coCongressionalDistricts[["Name"]][
      geomander::geo_match(
        from = coPrecincts,
        to = coCongressionalDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to congressional districts data ----
coCongressionalDistricts <- coCongressionalDistricts |>
  dplyr::left_join(coCongressionalDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## region matches ----

### assign relevant state regions ----
stateRegions <- c(
  "Northern Colorado",
  "Northeastern Colorado",
  "Eastern Colorado",
  "Southeastern Colorado",
  "Southern Colorado",
  "Southwestern Colorado",
  "Western Colorado",
  "Northwestern Colorado",
  "Western Slope",
  "Front Range",
  "Eastern Plains",
  "San Luis Valley",
  "Arkansas Valley",
  "Denver Metro Area",
  "Rural Colorado"
)

### assign region boundaries ----
coRegions <- stateRegions |>
  purrr::map_dfr(
    .progress = "Assigning Vernacular Region Boundaries",
    .f = \(stateRegion) {
      
      #### use ellmer to assign a region boundary ----
      regionBoundary <- assignVernacularRegionBoundary(
        regionName = stateRegion,
        state = "Colorado",
        countyBoundaries = coCounties
      )
      
      #### reformat precinct assignments of region boundary ----
      regionBoundary <- regionBoundary |>
        dplyr::rename(RegionWeight = Weight) |>
        tidyr::unnest(cols = Precincts) |>
        dplyr::mutate(GEOID20 = NA, Name = stateRegion, AdminLevel = "Region") |>
        dplyr::select(-c(Count, Weight)) |>
        dplyr::rename(Weight = RegionWeight) |>
        tidyr::nest(Precincts = c(PrecinctID, Weight))
      
      #### return region boundary ----
      Sys.sleep(time = 30)
      return(regionBoundary)
    }
  )

# combine location matches into singular data frame ----
coLocationMatches <- dplyr::bind_rows(
  coMunicipalities,
  coSchoolDistricts,
  coCounties,
  coStateHouseDistricts,
  coStateSenateDistricts,
  coCongressionalDistricts,
  coRegions
)

# check that no locations have null precinct assignments ----
sum(unlist(lapply(X = coLocationMatches[["Precincts"]], FUN = is.null)))

# save location matches data ----
saveRDS(object = coLocationMatches, file = file.path(dataPath, coLocationMatchesFilename))
