
# Script 06: Match Locations from Colorado Web Comments to Precincts

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(janitor)
library(tigris)
library(patchwork)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
figurePath <- "./Figures/Colorado"
coWebCommentLocationsFilename <- "COWebCommentLocations.rds"
coWebCommentRequestsFilename <- "COWebCommentRequests.rds"
coLocationMatchesFilename <- "COLocationMatches.rds"
coWebCommentLocationsMatchedFilename <- "COWebCommentLocationsMatched.rds"
locationMatchingSummaryFigureFilename <- "LocationMatchingSummaryFigure.png"
requestMatchingSummaryFigureFilename <- "RequestMatchingSummaryFigure.png"

# import colorado web comment locations ----
coWebCommentLocations <- readRDS(file = file.path(dataPath, coWebCommentLocationsFilename))

# import colorado web comment requests ----
coWebCommentRequests <- readRDS(file = file.path(dataPath, coWebCommentRequestsFilename))

# import colorado location matches ----
coLocationMatches <- readRDS(file = file.path(dataPath, coLocationMatchesFilename))

## import colorado boundaries data ----
coPrecincts <- alarmdata::alarm_census_vest(state = "CO", geometry = TRUE) |>
  sf::st_transform(crs = "NAD83") |>
  sf::st_make_valid()
coUrbanAreas <- tigris::urban_areas(year = 2020, filter_by = sf::st_union(coPrecincts))
coMunicipalities <- tigris::places(state = "CO", year = 2020) |> dplyr::filter(CLASSFP == "C1")

# assign relevant state regions ----
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

# assign subarea descriptions ----
subareaDescriptions <- c(
  "Northern",
  "Northeastern",
  "Eastern",
  "Southeastern",
  "Southern",
  "Southwestern",
  "Western",
  "Northwestern",
  "Central",
  "Urban",
  "Rural",
  "Unincorporated"
)

# group colorado web comment locations by unique locations to match ----
coWebCommentUniqueLocations <- coWebCommentLocations |>
  dplyr::group_by(State, Name, AdminLevel, SubareaDescription, FullLocationName) |> 
  dplyr::summarise(Frequency = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(Frequency))

# assess duplicates ----
duplicates <- coWebCommentUniqueLocations |>
  janitor::get_dupes(FullLocationName, AdminLevel)

# reconcile erroneous region names and subarea descriptions ----
coWebCommentUniqueLocations <- coWebCommentUniqueLocations |>
  dplyr::mutate(
    SubareaDescription = dplyr::case_when(
      FullLocationName %in% duplicates$FullLocationName &
        SubareaDescription != "NA" &
        stringr::str_starts(string = Name, pattern = as.character(SubareaDescription)) ~ "NA",
      .default = SubareaDescription
    )
  ) |>
  dplyr::distinct(State, Name, AdminLevel, SubareaDescription, FullLocationName, .keep_all = TRUE) |>
  dplyr::group_by(FullLocationName) |>
  dplyr::mutate(
    Name = dplyr::case_when(
      Frequency == max(Frequency) ~ Name,
      .default = NA
    )
  ) |>
  dplyr::ungroup() |>
  tidyr::drop_na()

# ensure zero duplicates remain ----
coWebCommentUniqueLocations |> janitor::get_dupes(FullLocationName, AdminLevel)

# find location matches ----
coWebCommentLocationsMatched <- purrr::pmap_dfr(
  .progress = "Matching Locations",
  .l = list(
    coWebCommentUniqueLocations |> dplyr::pull(Name),
    coWebCommentUniqueLocations |> dplyr::pull(AdminLevel),
    coWebCommentUniqueLocations |> dplyr::pull(SubareaDescription),
    coWebCommentUniqueLocations |> dplyr::pull(FullLocationName)
  ),
  .f = \(location, adminLevel, subarea, fullLocationName) {
    matchLocation(
      location = location,
      adminLevel = adminLevel,
      subarea = subarea,
      locationMatches = coLocationMatches,
      precincts = coPrecincts,
      urbanAreas = coUrbanAreas,
      municipalities = coMunicipalities
    ) |> dplyr::mutate(FullLocationName = fullLocationName, .before = "Name")
  }
)

# save comment locations with matches ----
coWebCommentLocationsMatched |>
  saveRDS(file = file.path(dataPath, coWebCommentLocationsMatchedFilename))

# colorado location matching summary ----

# pair web comment locations with matched precincts ----
coWebCommentLocations <- coWebCommentLocations |>
  dplyr::left_join(
    y = coWebCommentLocationsMatched |>
      dplyr::select(FullLocationName, GEOID20, Match, Precincts),
    by = "FullLocationName"
  )

# create matching summary ----
coLocationMatchingSummary <- coWebCommentLocations |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(`Location Count` = dplyr::n(), Missing = sum(is.na(Match))) |>
  dplyr::ungroup() |>
  dplyr::mutate(`Proportion Missing` = Missing/`Location Count`) |>
  dplyr::arrange(dplyr::desc(`Proportion Missing`)) |>
  dplyr::mutate(
    `Location Count Bin` = dplyr::case_when(
      `Location Count` <= 15 ~ as.character(`Location Count`),
      `Location Count` > 15 & `Location Count` <= 20 ~ "16-20",
      .default = "20+"
    ) |> factor(levels = c(as.character(1:15), "16-20", "20+"))
  )

# create figure for mean proportion of matched locations by location count ----
proportionMatchedFigure <- coLocationMatchingSummary |>
  dplyr::group_by(`Location Count Bin`) |>
  dplyr::summarise(`Mean Proportion Matched` = 1 - mean(`Proportion Missing`)) |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = `Location Count Bin`,
      y = `Mean Proportion Matched`,
      group = 1,
    )
  ) +
  ggplot2::geom_line(color = "#405149", linewidth = 1.2) +
  ggplot2::labs(
    x = "Locations Mentioned",
    y = "Mean Proportion Matched",
    title = "Average Proportion of Matched Locations by Comment"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )

# create figure for comments by location count ----
commentCountFigure <- coLocationMatchingSummary |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = `Location Count Bin`), fill = "#405149") +
  ggplot2::labs(
    x = "Locations Mentioned",
    y = "Comments",
    title = "Locations Mentioned by Comment"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )

# create location matching summary figure ----
locationMatchingSummaryFigure <- proportionMatchedFigure / commentCountFigure

# save matching summary figure ----
ggplot2::ggsave(
  filename = file.path(figurePath, locationMatchingSummaryFigureFilename),
  plot = locationMatchingSummaryFigure,
  width = 8, 
  height = 8, 
  units = "in", 
  dpi = 600
)


# colorado request matching summary ----

# pair web comment requests with matched precincts ----
coWebCommentRequests <- coWebCommentRequests |>
  dplyr::left_join(
    y = coWebCommentLocationsMatched |>
      dplyr::select(Location1 = FullLocationName, Location1Match = Match, Precincts1 = Precincts),
    by = "Location1"
  ) |>
  dplyr::left_join(
    y = coWebCommentLocationsMatched |>
      dplyr::select(Location2 = FullLocationName, Location2Match = Match, Precincts2 = Precincts),
    by = "Location2"
  )

# create matching summary ----
coRequestMatchingSummary <- coWebCommentRequests |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(
    `Request Count` = dplyr::n(),
    Missing = sum(is.na(Location1Match) | is.na(Location2Match))) |>
  dplyr::ungroup() |>
  dplyr::mutate(`Proportion Missing` = Missing/`Request Count`) |>
  dplyr::arrange(dplyr::desc(`Proportion Missing`)) |>
  dplyr::mutate(
    `Request Count Bin` = dplyr::case_when(
      `Request Count` <= 10 ~ as.character(`Request Count`),
      `Request Count` > 10 & `Request Count` <= 15 ~ "11-15",
      `Request Count` > 15 & `Request Count` <= 20 ~ "16-20",
      `Request Count` > 20 & `Request Count` <= 25 ~ "21-25",
      `Request Count` > 25 & `Request Count` <= 30 ~ "26-30",
      `Request Count` > 30 & `Request Count` <= 35 ~ "31-35",
      `Request Count` > 35 & `Request Count` <= 40 ~ "36-40",
      `Request Count` > 40 & `Request Count` <= 45 ~ "41-45",
      `Request Count` > 45 & `Request Count` <= 50 ~ "46-50",
      .default = "50+"
    ) |>
      factor(
        levels = c(
          as.character(1:10),
          "11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "50+"
        )
      )
  )

# create figure for mean proportion of matched requests by request count ----
proportionMatchedFigure <- coRequestMatchingSummary |>
  dplyr::group_by(`Request Count Bin`) |>
  dplyr::summarise(`Mean Proportion Matched` = 1 - mean(`Proportion Missing`)) |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = `Request Count Bin`,
      y = `Mean Proportion Matched`,
      group = 1,
    )
  ) +
  ggplot2::geom_line(color = "#405149", linewidth = 1.2) +
  ggplot2::labs(
    x = "Requests Mentioned",
    y = "Mean Proportion Matched",
    title = "Average Proportion of Matched Requests by Comment"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )

# create figure for comments by request count ----
commentCountFigure <- coRequestMatchingSummary |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = `Request Count Bin`), fill = "#405149") +
  ggplot2::labs(
    x = "Requests Mentioned",
    y = "Comments",
    title = "Requestts Mentioned by Comment"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )

# create request matching summary figure ----
requestMatchingSummaryFigure <- proportionMatchedFigure / commentCountFigure

# save matching summary figure ----
ggplot2::ggsave(
  filename = file.path(figurePath, requestMatchingSummaryFigureFilename),
  plot = requestMatchingSummaryFigure,
  width = 8, 
  height = 8, 
  units = "in", 
  dpi = 600
)
