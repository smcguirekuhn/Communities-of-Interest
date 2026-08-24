
# Script 08: Match Georgia Web Comment Data Location Mentions to Precincts

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(reclin2)
library(stringdist)
library(ggrepel)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia"
gaPrecinctsIDColumn <- "GEOID20"
gaLocationMatchesFilename <- "GALocationMatches.rds"
gaWebCommentDataFilename <- "GAWebCommentData.rds"

# import georgia precincts shapefile ----
gaPrecincts <- alarmdata::alarm_census_vest(state = "GA", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid() |>
  dplyr::select(
    PrecinctID = dplyr::all_of(x = gaPrecinctsIDColumn),
    County = dplyr::all_of(x = "county")
  )

# import location matches ----
gaLocationMatches <- readRDS(file = file.path(dataPath, gaLocationMatchesFilename))

# import comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# import georgia precincts shapefile ----

# create georgia precincts matrices for each comment ----

## initialize georgia precincts matrix ----

testDF1 <- data.frame(
  Name = c("Dunwoody", "Midtown", "University of Georgia", "Middle Georgia University"),
  AdminLevel = c("Municipality", "Neighborhood", "School", "School"),
  County = c("DeKalb", "Fulton County", "Clarke County", "Bibb County")
)

testDF2 <- gaLocationMatches |>
  tidyr::unnest(c(Precincts, Counties)) |>
  dplyr::distinct(Name, AdminLevel, County)

pairs <- reclin2::pair_blocking(testDF1, testDF2, on = "AdminLevel") |>
  reclin2::compare_pairs(
    on = c("AdminLevel", "Name", "County"),
    default_comparator = reclin2::cmp_jarowinkler()
  ) |>
  dplyr::mutate(Score = sqrt(Name^2 + County^2)) |>
  dplyr::group_by(.x) |>
  dplyr::filter(Score > 1.2)

## match precincts to each location ----
mapCOIPrecincts(
  commentInfo = gaWebCommentData[[5]],
  locationMatches = gaLocationMatches,
  precinctBoundaries = gaPrecincts
)

gaWebCommentData |>
  dplyr::filter(Name == "Sandy Springs") |>
  dplyr::select(LocationsToGroup) |>
  dplyr::filter(lengths(LocationsToGroup) > 0, LocationsToGroup != "NA") |>
  dplyr::pull(LocationsToGroup) |>
  purrr::list_c() |>
  table()

gaWebCommentData |>
  dplyr::filter(Name == "Sandy Springs") |>
  dplyr::select(LocationsToSeparate) |>
  dplyr::filter(lengths(LocationsToSeparate) > 0, LocationsToSeparate != "NA") |>
  dplyr::pull(LocationsToSeparate) |>
  purrr::list_c() |>
  table()

## add precinct groups to matrix rows ----

# save georgia precinct matrices ----

