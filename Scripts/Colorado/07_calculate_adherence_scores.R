
# Script 07: Calculate Adherence Scores for Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(tigris)
library(redist)
library(alarmdata)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
allGroundTruthRequestsFilename <- "./Validation/GroundTruth/AllGroundTruthRequests.rds"

# import all ground truth locations ----
allGroundTruthLocations <- readRDS(file = file.path(allGroundTruthLocationsFilename))

# import all ground truth requests ----
allGroundTruthRequests <- readRDS(file = file.path(allGroundTruthRequestsFilename))

commentID <- 2192

precinctMatches <- allGroundTruthLocations |>
  dplyr::select(FullLocationName, Precincts)

# match grouped and separated locations to precincts ----
commentIDRequests <- allGroundTruthRequests |>
  dplyr::filter(State == "Colorado", CommentID == commentID) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    GroupingPrecincts = purrr::map(
      .x = Groupings,
      .f = \(grouping) {
        groupingPrecincts <- precinctMatches |>
          dplyr::filter(FullLocationName %in% grouping) |>
          dplyr::select(Precincts) |>
          tidyr::unnest(cols = Precincts) |>
          dplyr::distinct()
        return(groupingPrecincts)
      }
    ),
    SeparationPrecincts = purrr::map(
      .x = Separations,
      .f = \(grouping) {
        separationPrecincts <- precinctMatches |>
          dplyr::filter(FullLocationName %in% grouping) |>
          dplyr::select(Precincts) |>
          tidyr::unnest(cols = Precincts) |>
          dplyr::distinct()
        return(separationPrecincts)
      }
    )
  )

# import colorado plan simulations ----
coStateMap <- alarmdata::alarm_50state_map(state = "CO")
coStatePlans <- alarmdata::alarm_50state_plans(state = "CO", year = 2020)
coStatePrecinctMatrix <- as.data.frame(redist::get_plans_matrix(coStatePlans)) |>
  dplyr::mutate(PrecinctID = coStateMap[["GEOID"]], .before = 1)

# build grouped adherence table ----
groupedAdherenceTable <- purrr::map_dfr(
  .progress = "Calculating Grouped Adherence Scores",
  .x = 1:nrow(commentIDRequests),
  .f = \(request) {
    groupedPrecincts <- commentIDRequests |>
      dplyr::pull(GroupingPrecincts) |>
      purrr::pluck(request)
    calculateGroupedAdherence(
      statePrecinctMatrix = coStatePrecinctMatrix,
      groupedPrecincts = groupedPrecincts
    ) |> dplyr::mutate(Request = request, .before = 1)
  }
)

# build separated adherence table ----
separatedAdherenceTable <- purrr::map_dfr(
  .progress = "Calculating Separated Adherence Scores",
  .x = 1:nrow(commentIDRequests),
  .f = \(request) {
    groupedPrecincts <- commentIDRequests |>
      dplyr::pull(GroupingPrecincts) |>
      purrr::pluck(request)
    separatedPrecincts <- commentIDRequests |>
      dplyr::pull(SeparationPrecincts) |>
      purrr::pluck(request)
    calculateSeparatedAdherence(
      statePrecinctMatrix = coStatePrecinctMatrix,
      groupedPrecincts = groupedPrecincts,
      separatedPrecincts = separatedPrecincts
    ) |> dplyr::mutate(Request = request, .before = 1)
  }
)

adherenceScore <- (adherenceValues[["Adherence"]][1] - mean(adherenceValues[["Adherence"]][-1]))/
  sd(adherenceValues[["Adherence"]][-1])

separatedAdherenceTable |>
  dplyr::filter(Simulation != "cd_2020") |>
  ggplot2::ggplot() +
  ggplot2::geom_density(mapping = ggplot2::aes(x = Adherence, color = factor(Request))) +
  ggplot2::geom_vline(
    data = separatedAdherenceTable |> dplyr::filter(Simulation == "cd_2020"),
    mapping = ggplot2::aes(xintercept = Adherence, color = factor(Request)),
    linetype = "dotted"
  ) +
  ggplot2::facet_wrap(
    facets = ~Request,
    nrow = nrow(commentIDRequests),
    scales = "free_y"
  ) +
  ggplot2::theme_minimal()
