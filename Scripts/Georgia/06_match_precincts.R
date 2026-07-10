
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

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia"
gaLocationMatchesFilename <- "GALocationMatches.rds"
gaWebCommentDataFilename <- "GAWebCommentData.rds"

# import location matches ----
gaLocationMatches <- readRDS(file = file.path(dataPath, gaLocationMatchesFilename))

# import comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# import georgia precincts shapefile ----

# create georgia precincts matrices for each comment ----

## initialize georgia precincts matrix ----

## match precincts to each location ----
matchLocation(
  location = gaWebCommentData[[2]]$LocationsMentioned$Name[2],
  surroundingCounties = gaWebCommentData[[2]]$LocationsMentioned$SurroundingCounties[2] |> unlist(),
  adminLevel = "School",
  locationMatches = gaLocationMatches
)

## add precinct groups to matrix rows ----

# save georgia precinct matrices ----

