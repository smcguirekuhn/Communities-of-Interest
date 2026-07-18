
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
gaPrecinctsFilename <- "/ga_2024_gen_prec/ga_2024_gen_all_prec/ga_2024_gen_all_prec.shp"
gaLocationMatchesFilename <- "GALocationMatches.rds"
gaWebCommentDataFilename <- "GAWebCommentData.rds"

# import georgia precincts shapefile ----
gaPrecincts <- sf::st_read(dsn = file.path(dataPath, gaPrecinctsFilename)) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid()

# import location matches ----
gaLocationMatches <- readRDS(file = file.path(dataPath, gaLocationMatchesFilename))

# import comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# import georgia precincts shapefile ----

# create georgia precincts matrices for each comment ----

## initialize georgia precincts matrix ----

## match precincts to each location ----
mapCOIPrecincts(
  commentInfo = gaWebCommentData[[3]],
  locationMatches = gaLocationMatches,
  precinctBoundaries = gaPrecincts
)

## add precinct groups to matrix rows ----

# save georgia precinct matrices ----

