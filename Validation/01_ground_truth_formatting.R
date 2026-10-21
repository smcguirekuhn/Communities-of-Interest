
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
dataPath <- "./Data/"
groundTruthDataPath <- "./Validation/GroundTruth/"
coGroundTruthFilename <- "COGroundTruthCommentData.json"
coLocationMatchesFilename <- "Colorado/COLocationMatches.rds"
gaGroundTruthFilename <- "GAGroundTruthCommentData.json"
gaLocationMatchesFilename <- "Georgia/GALocationMatches.rds"
paGroundTruthFilename <- "PAGroundTruthCommentData.json"
paLocationMatchesFilename <- "Pennsylvania/PALocationMatches.rds"
allGroundTruthLocationsFilename <- "AllGroundTruthLocations.rds"
allGroundTruthRequestsFilename <- "AllGroundTruthRequests.rds"

# Colorado Ground Truth ----

## import colorado ground truth data ----
coGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, coGroundTruthFilename),
  simplifyVector = TRUE
)

## import colorado location matches ----
coLocationMatches <- readRDS(file = file.path(dataPath, coLocationMatchesFilename))

## import colorado boundaries data ----
coPrecincts <- alarmdata::alarm_census_vest(state = "CO", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid()
coUrbanAreas <- tigris::urban_areas(year = 2020, filter_by = sf::st_union(coPrecincts))
coMunicipalities <- tigris::places(state = "CO", year = 2020) |> dplyr::filter(CLASSFP == "C1")

## format colorado ground truth locations ----
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
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID)

## group colorado ground truth locations by unique locations to match ----
coGroundTruthMatches <- coGroundTruthLocations |>
  dplyr::select(-FullLocationName) |>
  dplyr::group_by(State, Name, AdminLevel, SubareaDescription) |>
  dplyr::summarise(Frequency = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(Frequency))

## find location matches ----
coMatchedGroundTruthLocations <- purrr::pmap_dfr(
  .progress = "Matching Locations",
  .l = list(
    coGroundTruthMatches |> dplyr::pull(Name),
    coGroundTruthMatches |> dplyr::pull(AdminLevel),
    coGroundTruthMatches |> dplyr::pull(SubareaDescription)
  ),
  .f = \(location, adminLevel, subarea) {
    matchLocation(
      location = location,
      adminLevel = adminLevel,
      subarea = subarea,
      locationMatches = coLocationMatches,
      precincts = coPrecincts,
      urbanAreas = coUrbanAreas,
      municipalities = coMunicipalities
    )
  }
)

## pair ground truth locations with matched precincts ----
coGroundTruthLocations <- coGroundTruthLocations |>
  dplyr::left_join(y = coMatchedGroundTruthLocations, by = c("Name", "AdminLevel", "SubareaDescription")) |>
  dplyr::select(-c(MongeElkanDistance, CosineDistance))

## format colorado ground truth requests ----
coGroundTruthRequests <- purrr::map_dfr(
  .x = coGroundTruthLocations |> dplyr::pull(CommentID) |> unique(),
  .f = \(commentID) {
    mapPairwiseRequests(
      state = "Colorado",
      commentID = commentID,
      locationNodes = coGroundTruthLocations |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::pull(FullLocationName),
      requests = coGroundTruthData |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::select(Requests) |>
        tidyr::unnest(cols = Requests)
    )
  }
)

# Georgia Ground Truth ----

# import georgia ground truth data ----
gaGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, gaGroundTruthFilename),
  simplifyVector = TRUE
)

## import georgia location matches ----
gaLocationMatches <- readRDS(file = file.path(dataPath, gaLocationMatchesFilename))

## import georgia boundaries data ----
gaPrecincts <- alarmdata::alarm_census_vest(state = "GA", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid()
gaUrbanAreas <- tigris::urban_areas(year = 2020, filter_by = sf::st_union(gaPrecincts))
gaMunicipalities <- tigris::places(state = "GA", year = 2020) |> dplyr::filter(CLASSFP == "C1")

## format georgia ground truth locations ----
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
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID)

## group georgia ground truth locations by unique locations to match ----
gaGroundTruthMatches <- gaGroundTruthLocations |>
  dplyr::select(-FullLocationName) |>
  dplyr::group_by(State, Name, AdminLevel, SubareaDescription) |>
  dplyr::summarise(Frequency = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(Frequency))

## find location matches ----
gaMatchedGroundTruthLocations <- purrr::pmap_dfr(
  .progress = "Matching Locations",
  .l = list(
    gaGroundTruthMatches |> dplyr::pull(Name),
    gaGroundTruthMatches |> dplyr::pull(AdminLevel),
    gaGroundTruthMatches |> dplyr::pull(SubareaDescription)
  ),
  .f = \(location, adminLevel, subarea) {
    matchLocation(
      location = location,
      adminLevel = adminLevel,
      subarea = subarea,
      locationMatches = gaLocationMatches,
      precincts = gaPrecincts,
      urbanAreas = gaUrbanAreas,
      municipalities = gaMunicipalities
    )
  }
)

## pair ground truth locations with matched precincts ----
gaGroundTruthLocations <- gaGroundTruthLocations |>
  dplyr::left_join(y = gaMatchedGroundTruthLocations, by = c("Name", "AdminLevel", "SubareaDescription")) |>
  dplyr::select(-c(MongeElkanDistance, CosineDistance))

# format georgia ground truth requests ----
gaGroundTruthRequests <- purrr::map_dfr(
  .x = gaGroundTruthLocations |> dplyr::pull(CommentID) |> unique(),
  .f = \(commentID) {
    mapPairwiseRequests(
      state = "Georgia",
      commentID = commentID,
      locationNodes = gaGroundTruthLocations |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::pull(FullLocationName),
      requests = gaGroundTruthData |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::select(Requests) |>
        tidyr::unnest(cols = Requests)
    )
  }
)

# Pennsylvania Ground Truth ----

# import pennsylvania ground truth data ----
paGroundTruthData <- jsonlite::read_json(
  path = file.path(groundTruthDataPath, paGroundTruthFilename),
  simplifyVector = TRUE
)

## import pennsylvania location matches ----
paLocationMatches <- readRDS(file = file.path(dataPath, paLocationMatchesFilename))

## import pennsylvania boundaries data ----
paPrecincts <- alarmdata::alarm_census_vest(state = "PA", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid()
paUrbanAreas <- tigris::urban_areas(year = 2020, filter_by = sf::st_union(paPrecincts))
paMunicipalities <- tigris::places(state = "PA", year = 2020) |> dplyr::filter(CLASSFP == "C1")

# format pennsylvania ground truth locations ----
paGroundTruthLocations <- paGroundTruthData |>
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
  dplyr::mutate(State = "Pennsylvania", .before = "CommentID") |>
  dplyr::mutate(CommentID = as.numeric(CommentID)) |>
  dplyr::arrange(CommentID)

## group pennsylvania ground truth locations by unique locations to match ----
paGroundTruthMatches <- paGroundTruthLocations |>
  dplyr::select(-FullLocationName) |>
  dplyr::group_by(State, Name, AdminLevel, SubareaDescription) |>
  dplyr::summarise(Frequency = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(Frequency))

## find location matches ----
paMatchedGroundTruthLocations <- purrr::pmap_dfr(
  .progress = "Matching Locations",
  .l = list(
    paGroundTruthMatches |> dplyr::pull(Name),
    paGroundTruthMatches |> dplyr::pull(AdminLevel),
    paGroundTruthMatches |> dplyr::pull(SubareaDescription)
  ),
  .f = \(location, adminLevel, subarea) {
    matchLocation(
      location = location,
      adminLevel = adminLevel,
      subarea = subarea,
      locationMatches = paLocationMatches,
      precincts = paPrecincts,
      urbanAreas = paUrbanAreas,
      municipalities = paMunicipalities
    )
  }
)

## pair ground truth locations with matched precincts ----
paGroundTruthLocations <- paGroundTruthLocations |>
  dplyr::left_join(y = paMatchedGroundTruthLocations, by = c("Name", "AdminLevel", "SubareaDescription")) |>
  dplyr::select(-c(MongeElkanDistance, CosineDistance))

# format pennsylvania ground truth requests ----
paGroundTruthRequests <- purrr::map_dfr(
  .x = paGroundTruthLocations |> dplyr::pull(CommentID) |> unique(),
  .f = \(commentID) {
    mapPairwiseRequests(
      state = "Pennsylvania",
      commentID = commentID,
      locationNodes = paGroundTruthLocations |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::pull(FullLocationName),
      requests = paGroundTruthData |>
        dplyr::filter(CommentID == commentID) |>
        dplyr::select(Requests) |>
        tidyr::unnest(cols = Requests)
    )
  }
)

# combine all ground truth locations data ----
allGroundTruthLocations <- dplyr::bind_rows(
  coGroundTruthLocations,
  gaGroundTruthLocations,
  paGroundTruthLocations
)

# save all ground truth locations data ----
saveRDS(allGroundTruthLocations, file = file.path(groundTruthDataPath, allGroundTruthLocationsFilename))

# combine all ground truth requests data ----
allGroundTruthRequests <- dplyr::bind_rows(
  coGroundTruthRequests,
  gaGroundTruthRequests,
  paGroundTruthRequests
)

# save all ground truth requests data ----
saveRDS(allGroundTruthRequests, file = file.path(groundTruthDataPath, allGroundTruthRequestsFilename))
