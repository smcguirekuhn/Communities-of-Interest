
# Ground Truth Formatting for Validation Tests Across All States

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(tibble)
library(dplyr)
library(jsonlite)
library(igraph)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
groundTruthDataPath <- "./Validation/GroundTruth/"
coGroundTruthFilename <- "COGroundTruthCommentData.json"
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"
allGroundTruthLocationsFilename <- "AllGroundTruthLocations.rds"
allGroundTruthRequestsFilename <- "AllGroundTruthRequests.rds"

# import colorado ground truth data ----
coGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, coGroundTruthFilename),
  simplifyVector = TRUE
)

# format colorado ground truth locations ----
coGroundTruthLocations <- coGroundTruthData |>
  dplyr::select(CommentID, LocationsMentioned) |>
  tidyr::unnest(cols = LocationsMentioned) |>
  dplyr::mutate(
    SubareaDescription = dplyr::case_when(
      CardinalDirectionSubarea != "NA" & !is.na(CardinalDirectionSubarea) ~ CardinalDirectionSubarea,
      AdditionalDescription != "NA" | !is.na(AdditionalDescription) ~ AdditionalDescription,
      .default = "NA"
    ),
    .before = "FullLocationName"
  ) |>
  dplyr::select(-c(CardinalDirectionSubarea, AdditionalDescription)) |>
  dplyr::mutate(State = "Colorado", .before = "CommentID") |>
  dplyr::arrange(CommentID)

# format colorado ground truth requests ----
coGroundTruthRequests <- purrr::map_dfr(
  .x = coGroundTruthLocations |> dplyr::pull(CommentID) |> unique(),
  .f = \(commentID) {
    
    ## isolate location nodes and relationships ----
    locationNodes <- coGroundTruthLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    relationships <- coGroundTruthData |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::select(Relationships) |>
      tidyr::unnest(cols = Relationships)
    
    ## create nested sets of grouped and separated locations ----
    if (nrow(relationships) > 0) {
      
      ### create location graphs ----
      locationGraphs <- createLocationGraphs(
        locationNodes = locationNodes,
        relationships = relationships
      )
      
      ### format location requests ----
      locationRequests <- locationGraphs |>
        purrr::pluck("GroupedGraph") |>
        igraph::components() |>
        purrr::pluck("membership") |>
        tibble::enframe(name = "Location", value = "Membership") |>
        dplyr::mutate(
          Separations = purrr::map(
            .x = Location,
            .f = \(location) {
              locationGraphs |>
                purrr::pluck("SeparatedGraph") |>
                igraph::neighbors(v = location) |>
                names() |>
                tibble::as_tibble_col(column_name = "Locations")
            }
          )
        ) |>
        tidyr::nest(Groupings = Location) |>
        dplyr::select(-Membership) |>
        dplyr::relocate(Separations, .after = Groupings) |>
        dplyr::mutate(State = "Colorado", CommentID = commentID, .before = 1)
    } else {
      locationRequests <- dplyr::tibble(
        State = "Colorado",
        CommentID = commentID,
        Location = locationNodes,
        Separations = list(dplyr::tibble(Location = character(0)))
      ) |>
        tidyr::nest(Groupings = Location) |>
        dplyr::relocate(Separations, .after = Groupings)
    }
    
    ## return location requests ----
    return(locationRequests)
  }
)

# import georgia ground truth data ----
gaGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, gaGroundTruthFilename),
  simplifyVector = TRUE
)

# format georgia ground truth locations ----
gaGroundTruthLocations <- gaGroundTruthData |>
  dplyr::select(CommentID, LocationsMentioned) |>
  dplyr::mutate(CommentID = as.integer(CommentID)) |>
  tidyr::unnest(cols = LocationsMentioned) |>
  dplyr::mutate(
    SubareaDescription = dplyr::case_when(
      CardinalDirectionSubarea != "NA" & !is.na(CardinalDirectionSubarea) ~ CardinalDirectionSubarea,
      AdditionalDescription != "NA" | !is.na(AdditionalDescription) ~ AdditionalDescription,
      .default = "NA"
    ),
    .before = "FullLocationName"
  ) |>
  dplyr::select(-c(CardinalDirectionSubarea, AdditionalDescription)) |>
  dplyr::mutate(State = "Georgia", .before = "CommentID") |>
  dplyr::arrange(CommentID)

# format georgia ground truth requests ----
gaGroundTruthRequests <- purrr::map_dfr(
  .x = gaGroundTruthLocations |> dplyr::pull(CommentID) |> unique(),
  .f = \(commentID) {
    
    ## isolate location nodes and relationships ----
    locationNodes <- gaGroundTruthLocations |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::pull(FullLocationName)
    relationships <- gaGroundTruthData |>
      dplyr::filter(CommentID == commentID) |>
      dplyr::select(Relationships) |>
      tidyr::unnest(cols = Relationships)
    
    ## create nested sets of grouped and separated locations ----
    if (nrow(relationships) > 0) {
      
      ### create location graphs ----
      locationGraphs <- createLocationGraphs(
        locationNodes = locationNodes,
        relationships = relationships
      )
      
      ### format location requests ----
      locationRequests <- locationGraphs |>
        purrr::pluck("GroupedGraph") |>
        igraph::components() |>
        purrr::pluck("membership") |>
        tibble::enframe(name = "Location", value = "Membership") |>
        dplyr::mutate(
          Separations = purrr::map(
            .x = Location,
            .f = \(location) {
              locationGraphs |>
                purrr::pluck("SeparatedGraph") |>
                igraph::neighbors(v = location) |>
                names() |>
                tibble::as_tibble_col(column_name = "Locations")
            }
          )
        ) |>
        tidyr::nest(Groupings = Location) |>
        dplyr::select(-Membership) |>
        dplyr::relocate(Separations, .after = Groupings) |>
        dplyr::mutate(State = "Georgia", CommentID = commentID, .before = 1)
    } else {
      locationRequests <- dplyr::tibble(
        State = "Georgia",
        CommentID = commentID,
        Location = locationNodes,
        Separations = list(dplyr::tibble(Location = character(0)))
      ) |>
        tidyr::nest(Groupings = Location) |>
        dplyr::relocate(Separations, .after = Groupings)
    }
    
    ## return location requests ----
    return(locationRequests)
  }
)

# combine all ground truth locations data ----
allGroundTruthLocations <- dplyr::bind_rows(
  coGroundTruthLocations,
  gaGroundTruthLocations
)

# save all ground truth locations data ----
saveRDS(allGroundTruthLocations, file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# combine all ground truth requests data ----
allGroundTruthRequests <- dplyr::bind_rows(
  coGroundTruthRequests,
  gaGroundTruthRequests
)

# save all ground truth requests data ----
saveRDS(allGroundTruthRequests, file = file.path(groundTruthDataPath, allGroundTruthRequestsFilename))
