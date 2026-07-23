
# Script 06: Match Georgia Web Comment Data Location Mentions to Precincts

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
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
  Name = c("Dunwoody", "Midtown", "Georgia University"),
  AdminLevel = c("City", "Neighborhood", "School"),
  County = c("DeKalb", "Fulton County", "Clarke County")
)

testDF2 <- gaLocationMatches |>
  tidyr::unnest(c(Precincts, Counties)) |>
  dplyr::distinct(Name, AdminLevel, County)

library(fastLink)

testDFBlock <- fastLink::blockData(
  dfA = testDF1,
  dfB = testDF2,
  varnames = "AdminLevel"
)

matches.out <- fastLink::fastLink(
  dfA = testDF1,
  dfB = testDF2, 
  varnames = c("AdminLevel", "Name", "County"),
  stringdist.match = c("AdminLevel", "Name", "County"),
  partial.match = c("AdminLevel", "Name", "County"),
  cut.a = 0.8
)

## match precincts to each location ----
mapCOIPrecincts(
  commentInfo = gaWebCommentData[[4]],
  locationMatches = gaLocationMatches,
  precinctBoundaries = gaPrecincts
)

## add precinct groups to matrix rows ----

# save georgia precinct matrices ----

