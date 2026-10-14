
# Script 06: Prepare Location Match Shapefiles for Each Administrative Level in Pennsylvania

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
dataPath <- "./Data/Pennsylvania"
paPrecinctsIDColumn <- "GEOID20"
paLocationMatchesFilename <- "PALocationMatches.rds"

# import pennsylvania precincts shapefile ----
paPrecincts <- alarmdata::alarm_census_vest(state = "PA", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(
    PrecinctID = .data[[paPrecinctsIDColumn]],
    Population = pop,
    PercentWhite = pop_white/pop,
    RepublicanVoteShare = pre_20_rep_tru/(pre_20_rep_tru + pre_20_dem_bid),
    PopulationDensity = log1p(as.numeric(pop/(sf::st_area(geometry) |> units::set_units("km^2"))))
  ) |>
  dplyr::select(PrecinctID, Population, PercentWhite, RepublicanVoteShare, PopulationDensity)


# clean shapefile data for each administrative level ----


## municipality matches ----

### import shapefile ----
paMunicipalities <- tigris::places(state = "PA", cb = TRUE, year = 2020) |>
  dplyr::bind_rows(tigris::county_subdivisions(state = "PA", cb = TRUE, year = 2020)) |>
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
      AdminLevel == "township" ~ "Township",
      AdminLevel == "borough" ~ "Borough",
      AdminLevel == "town" ~ "Town",
      AdminLevel == "city" ~ "City",
      NAME == "Bethel Park" ~ "Borough",
      NAME == "Murrysville" ~ "City",
      NAME == "Monroeville" ~ "Borough"
    )
  ) |>
  dplyr::select(GEOID20 = GEOID, Name = NAME, AdminLevel) |>
  dplyr::distinct(Name, AdminLevel, .keep_all = TRUE) |>
  dplyr::mutate(Name = paste(Name, AdminLevel))

### match municipality precincts by area (larger municipalities) ----
paMunicipalityAreaPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paMunicipalities[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paMunicipalities,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::relocate(Name, .before = 1) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct area matches to municipalities data ----
paMunicipalities <- paMunicipalities |>
  dplyr::left_join(paMunicipalityAreaPrecincts, by = "Name")

### isolate remaining smaller municipalities without matched precincts ----
paMunicipalityPointPrecincts <- paMunicipalities |>
  dplyr::slice(which(unlist(lapply(X = paMunicipalities[["Precincts"]], FUN = is.null))))

### match municipality precincts by points (smaller municipalities) ----
paMunicipalityPointPrecincts <- paMunicipalityPointPrecincts |>
  dplyr::select(-Precincts) |>
  dplyr::mutate(
    PrecinctID = paPrecincts[["PrecinctID"]][
      geomander::geo_match(
        from = paMunicipalityPointPrecincts,
        to = paPrecincts,
        method = "point",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::left_join(paPrecincts |> sf::st_drop_geometry(), by = "PrecinctID") |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct matches to municipalities data ----
paMunicipalities <- paMunicipalities |>
  dplyr::slice(which(!unlist(lapply(X = paMunicipalities[["Precincts"]], FUN = is.null)))) |>
  dplyr::bind_rows(paMunicipalityPointPrecincts) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(Name) |>
  dplyr::mutate(Name = stringr::str_remove(string = Name, pattern = "\\s+\\w+$"))

## school district matches ----

### import shapefile ----
paSchoolDistricts <- tigris::school_districts(state = "PA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "School District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAME, AdminLevel)

### match school district precincts by area (larger school districts) ----
paSchoolDistrictPrecincts <- paPrecincts |>
  dplyr::mutate(
    Name = paSchoolDistricts[["Name"]][
      geomander::geo_match(
        from = paPrecincts,
        to = paSchoolDistricts,
        method = "area",
        tiebreaker = FALSE
      ) |> purrr::modify_if(~.x < 0, ~NA)
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::relocate(Name, .before = 1) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct matches to school districts data ----
paSchoolDistricts <- paSchoolDistricts |>
  dplyr::left_join(paSchoolDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry() |>
  dplyr::arrange(Name)

## county matches ----

### import shapefile ----
paCounties <- tigris::counties(state = "PA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "County") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::relocate(Name, .before = 1) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct matches to counties data ----
paCounties <- paCounties |>
  dplyr::left_join(paCountyPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## legislative district matches ----

### import state house district shapefile ----
paStateHouseDistricts <- tigris::state_legislative_districts(state = "PA", year = 2020, house = "lower") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "State House District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::relocate(Name, .before = 1) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct matches to state house districts data ----
paStateHouseDistricts <- paStateHouseDistricts |>
  dplyr::left_join(paStateHouseDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()

### import state senate district shapefile ----
paStateSenateDistricts <- tigris::state_legislative_districts(state = "PA", year = 2020, house = "upper") |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "State Senate District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::relocate(Name, .before = 1) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct matches to state senate districts data ----
paStateSenateDistricts <- paStateSenateDistricts |>
  dplyr::left_join(paStateSenateDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()

### import congressional district shapefile ----
paCongressionalDistricts <- tigris::congressional_districts(state = "PA", year = 2020) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::mutate(AdminLevel = "Congressional District") |>
  dplyr::select(GEOID20 = GEOID, Name = NAMELSAD, AdminLevel)

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
    ]
  ) |>
  sf::st_drop_geometry() |>
  tidyr::drop_na(Name) |>
  dplyr::relocate(Name, .before = 1) |>
  dplyr::mutate(Weight = 1) |>
  tidyr::nest(Precincts = PrecinctID:Weight)

### add precinct matches to congressional districts data ----
paCongressionalDistricts <- paCongressionalDistricts |>
  dplyr::left_join(paCongressionalDistrictPrecincts, by = "Name") |>
  sf::st_drop_geometry()


## region matches ----

### assign relevant state regions ----
stateRegions <- c(
  "Northern Pennsylvania",
  "Northeastern Pennsylvania",
  "Eastern Pennsylvania",
  "Southeastern Pennsylvania",
  "Southern Pennsylvania",
  "Southwestern Pennsylvania",
  "Western Pennsylvania",
  "Northwestern Pennsylvania",
  "Central Pennsylvania",
  "Pennsylvania Wilds",
  "The Poconos",
  "Happy Valley",
  "Lehigh Valley",
  "Coal Region",
  "Mon Valley",
  "Philadelphia Metro Area",
  "Pittsburgh Metro Area"
)

### assign region boundaries ----
paRegions <- stateRegions[1] |>
  purrr::map_dfr(
    .progress = "Assigning Vernacular Region Boundaries",
    .f = \(stateRegion) {
      
      #### use ellmer to assign a region boundary ----
      regionBoundary <- assignVernacularRegionBoundary(
        regionName = stateRegion,
        state = "Pennsylvania",
        countyBoundaries = paCounties
      )
      
      #### reformat precinct assignments of region boundary ----
      regionBoundary <- regionBoundary |>
        dplyr::rename(RegionWeight = Weight) |>
        tidyr::unnest(cols = Precincts) |>
        dplyr::mutate(GEOID20 = NA, Name = stateRegion, AdminLevel = "Region") |>
        dplyr::select(-c(Count, Weight)) |>
        dplyr::rename(Weight = RegionWeight) |>
        tidyr::nest(Precincts = PrecinctID:Weight)
      
      #### return region boundary ----
      Sys.sleep(time = 45)
      return(regionBoundary)
    }
  )

# combine location matches into singular data frame ----
paLocationMatches <- dplyr::bind_rows(
  paMunicipalities,
  paSchoolDistricts,
  paCounties,
  paStateHouseDistricts,
  paStateSenateDistricts,
  paCongressionalDistricts,
  paRegions
)

# check that no locations have null precinct assignments ----
sum(unlist(lapply(X = paLocationMatches[["Precincts"]], FUN = is.null)))

# save location matches data ----
saveRDS(object = paLocationMatches, file = file.path(dataPath, paLocationMatchesFilename))
