
# Script 06: Prepare Location Match Shapefiles for Each Administrative Level in Georgia

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
dataPath <- "./Data/Georgia"
gaPrecinctsIDColumn <- "GEOID20"
gaLocationMatchesFilename <- "GALocationMatches.rds"

# import georgia precincts shapefile ----
gaPrecincts <- alarmdata::alarm_census_vest(state = "GA", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(PrecinctID = dplyr::all_of(x = gaPrecinctsIDColumn))

# clean shapefile data for each administrative level ----


## municipality matches ----

### import shapefile ----
gaMunicipalities <- tigris::places(state = "GA", cb = TRUE, year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(
    AdminLevel = sub(
      pattern = "^.*\\s(\\w+)$",
      replacement = "\\1",
      x = NAMELSAD
    )
  ) |>
  dplyr::filter(!NAME %in% c("Webster County", "Echols County", "Georgetown-Quitman County")) |>
  dplyr::mutate(
    NAME = dplyr::case_when(
      AdminLevel %in% c("CDP", "town", "city") ~ NAME,
      .default = stringr::str_extract(NAME, pattern = "^\\w+")
    )
  ) |>
  dplyr::mutate(
    AdminLevel = dplyr::case_when(
      AdminLevel == "CDP" ~ "Neighborhood",
      AdminLevel == "town" ~ "Town",
      AdminLevel == "city" ~ "City",
      .default = "City"
    )
  ) |>
  dplyr::select(GEOID20 = GEOID, Name = NAME, AdminLevel) |>
  dplyr::mutate(Name = paste(Name, AdminLevel))

### match municipality precincts by area (larger municipalities) ----
gaMunicipalityAreaPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaMunicipalities[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaMunicipalities,
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
gaMunicipalities <- gaMunicipalities |>
  dplyr::left_join(gaMunicipalityAreaPrecincts, by = "Name")

### isolate remaining smaller municipalities without matched precincts ----
gaMunicipalityPointPrecincts <- gaMunicipalities |>
  dplyr::slice(which(unlist(lapply(X = gaMunicipalities[["Precincts"]], FUN = is.null))))

### match municipality precincts by points (smaller municipalities) ----
gaMunicipalityPointPrecincts <- gaMunicipalityPointPrecincts |>
  dplyr::select(-Precincts) |>
  dplyr::mutate(
    PrecinctID = gaPrecincts[["PrecinctID"]][
      geomander::geo_match(
        from = gaMunicipalityPointPrecincts,
        to = gaPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to municipalities data ----
gaMunicipalities <- gaMunicipalities |>
  dplyr::slice(which(!unlist(lapply(X = gaMunicipalities[["Precincts"]], FUN = is.null)))) |>
  dplyr::bind_rows(gaMunicipalityPointPrecincts) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(Name) |>
  dplyr::mutate(Name = stringr::str_remove(string = Name, pattern = "\\s+\\w+$"))


## school district matches ----

### import shapefile ----
gaSchoolDistricts <- tigris::school_districts(state = "GA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "School District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAME, AdminLevel)

### match school district precincts by area (larger school districts) ----
gaSchoolDistrictAreaPrecincts <- gaPrecincts |>
  dplyr::mutate(
    Name = gaSchoolDistricts[["Name"]][
      geomander::geo_match(
        from = gaPrecincts,
        to = gaSchoolDistricts,
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
gaSchoolDistricts <- gaSchoolDistricts |>
  dplyr::left_join(gaSchoolDistrictAreaPrecincts, by = "Name")

### isolate remaining smaller school districts without matched precincts ----
gaSchoolDistrictPointPrecincts <- gaSchoolDistricts |>
  dplyr::slice(which(unlist(lapply(X = gaSchoolDistricts[["Precincts"]], FUN = is.null))))

### match school district precincts by points (smaller school districts) ----
gaSchoolDistrictPointPrecincts <- gaSchoolDistrictPointPrecincts |>
  dplyr::select(-Precincts) |>
  dplyr::mutate(
    PrecinctID = gaPrecincts[["PrecinctID"]][
      geomander::geo_match(
        from = gaSchoolDistrictPointPrecincts,
        to = gaPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to school districts data ----
gaSchoolDistricts <- gaSchoolDistricts |>
  dplyr::slice(which(!unlist(lapply(X = gaSchoolDistricts[["Precincts"]], FUN = is.null)))) |>
  dplyr::bind_rows(gaSchoolDistrictPointPrecincts) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(Name)


## county matches ----

### import shapefile ----
gaCounties <- tigris::counties(state = "GA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "County") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to counties data ----
gaCounties <- gaCounties |>
  dplyr::left_join(gaCountyPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## legislative district matches ----

### import state house district shapefile ----
gaStateHouseDistricts <- tigris::state_legislative_districts(state = "GA", year = 2020, house = "lower") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "State House District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to state house districts data ----
gaStateHouseDistricts <- gaStateHouseDistricts |>
  dplyr::left_join(gaStateHouseDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()

### import state senate district shapefile ----
gaStateSenateDistricts <- tigris::state_legislative_districts(state = "GA", year = 2020, house = "upper") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "State Senate District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to state senate districts data ----
gaStateSenateDistricts <- gaStateSenateDistricts |>
  dplyr::left_join(gaStateSenateDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()

### import congressional district shapefile ----
gaCongressionalDistricts <- tigris::congressional_districts(state = "GA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "Congressional District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = c(PrecinctID, Weight))

### add precinct matches to congressional districts data ----
gaCongressionalDistricts <- gaCongressionalDistricts |>
  dplyr::left_join(gaCongressionalDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## region matches ----

### assign relevant state regions ----
stateRegions <- c(
  "Northern Georgia",
  "Northeastern Georgia",
  "Eastern Georgia",
  "Southeastern Georgia",
  "Southern Georgia",
  "Southwestern Georgia",
  "Western Georgia",
  "Northwestern Georgia",
  "Atlanta Metro Area",
  "Rural Georgia",
  "Coastal Georgia"
)

### assign region boundaries ----
gaRegions <- stateRegions |>
  purrr::map_dfr(
    .progress = "Assigning Vernacular Region Boundaries",
    .f = \(stateRegion) {
      
      #### use ellmer to assign a region boundary ----
      regionBoundary <- assignVernacularRegionBoundary(
        regionName = stateRegion,
        state = "Georgia",
        countyBoundaries = gaCounties
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
gaLocationMatches <- dplyr::bind_rows(
  gaMunicipalities,
  gaSchoolDistricts,
  gaCounties,
  gaStateHouseDistricts,
  gaStateSenateDistricts,
  gaCongressionalDistricts,
  gaRegions
)

# check that no locations have null precinct assignments ----
sum(unlist(lapply(X = gaLocationMatches[["Precincts"]], FUN = is.null)))

# save location matches data ----
saveRDS(object = gaLocationMatches, file = file.path(dataPath, gaLocationMatchesFilename))
