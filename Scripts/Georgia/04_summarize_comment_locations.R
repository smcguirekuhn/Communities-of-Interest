
# Script 04: Summarize and Visualize Georgia Web Comment Data Location Mentions

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(dplyr)
library(tidyr)
library(snakecase)
library(ggplot2)
library(patchwork)
library(tigris)
library(sf)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Georgia"
tablePath <- "./Tables/Georgia"
figurePath <- "./Figures/Georgia"
gaWebCommentDataFilename <- "GAWebCommentData.rds"
groupCountTableFilename <- "GroupCountTable.rds"
districtTypeTableFilename <- "DistrictTypeTable.rds"
cardinalDirectionSubareaTableFilename <- "CardinalDirectionSubareaTable.rds"
adminLevelPieChartFilename <- "AdminLevelPieChart.png"
commentMetricsFigureFilename <- "CommentMetricsFigure.png"
countyMentionsMapFilename <- "CountyMentionsMap.png"

# import comment data ----
gaWebCommentData <- readRDS(file = file.path(dataPath, gaWebCommentDataFilename))

# set admin level color scheme ----
adminLevels <- c(
  "Landmark" = "#9C751C",
  "School" = "#9C751C",
  "Neighborhood" = "#C49324",
  "Town" = "#DBBE7B",
  "City" = "#DBBE7B",
  "School District" = "#132B43",
  "County" = "#425568",
  "State House District" = "#7a8895",
  "State Senate District" = "#7a8895",
  "Congressional District" = "#7a8895",
  "Region" = "#b3bbc2",
  "NA" = "#999999"
)

# compile location data ----
locationData <- gaWebCommentData |>
  purrr::map(.f = \(commentInfo) commentInfo$LocationsMentioned) |>
  purrr::list_rbind(names_to = "CommentID")

# tables ----

## group count table ----
locationData |>
  dplyr::group_by(CommentID) |>
  dplyr::summarise(GroupCount = length(unique(Group))) |>
  dplyr::ungroup() |>
  dplyr::group_by(GroupCount) |>
  dplyr::summarise(CommentCount = dplyr::n()) |>
  dplyr::rename(
    "Unique Location Groups" = GroupCount,
    "Comment Count" = CommentCount
  ) |>
  saveRDS(file = file.path(tablePath, groupCountTableFilename))

## district type table ----
locationData |>
  dplyr::select(CommentID, DistrictTypes) |>
  tidyr::unnest_longer(col = DistrictTypes) |>
  dplyr::group_by(DistrictTypes) |>
  dplyr::summarise(CommentCount = length(unique(CommentID))) |>
  dplyr::rename(
    "District Type" = DistrictTypes,
    "Comment Count" = CommentCount
  ) |>
  saveRDS(file = file.path(tablePath, districtTypeTableFilename))

## cardinal direction subarea table ----
locationData |>
  dplyr::mutate(
    CardinalDirectionSubarea = dplyr::case_when(
      CardinalDirectionSubarea == "NA" ~ "Full Area",
      .default = CardinalDirectionSubarea
    )
  ) |>
  dplyr::mutate(
    CardinalDirectionSubarea = factor(
      x = CardinalDirectionSubarea,
      levels = c(
        "Full Area",
        "Northern",
        "Northeastern",
        "Eastern",
        "Southeastern",
        "Southern",
        "Southwestern",
        "Western",
        "Northwestern",
        "Central"
      )
    )
  ) |>
  dplyr::group_by(CardinalDirectionSubarea) |>
  dplyr::summarise(LocationCount = dplyr::n()) |>
  dplyr::rename(
    "Cardinal Direction Subarea" = CardinalDirectionSubarea,
    "Mentioned Locations" = LocationCount
  ) |>
  saveRDS(file = file.path(tablePath, cardinalDirectionSubareaTableFilename))

# figures ----

## create admin level pie chart ----
adminLevelPieChart <- locationData |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = "", fill = AdminLevel)) +
  ggplot2::geom_bar(width = 1) +
  ggplot2::coord_polar(theta = "y", start = 0) +
  ggplot2::geom_text(
    stat = "count", 
    mapping = ggplot2::aes(x = 1.25, label = ggplot2::after_stat(count)), 
    position = ggplot2::position_stack(vjust = 0.5),
    color = "#FFFFFF"
  ) +
  ggplot2::scale_fill_manual(values = adminLevels) +
  ggplot2::labs(
    title = "Location Mentions by Administrative Level",
    fill = "Admin Level"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

## save admin level pie chart ----
ggplot2::ggsave(
  filename = file.path(figurePath, adminLevelPieChartFilename),
  plot = adminLevelPieChart,
  bg = "#FFFFFF"
)

## create sentiment histogram ----
sentimentHistogram <- gaWebCommentData |>
  purrr::map(.f = \(commentInfo) data.frame(Sentiment = commentInfo$Sentiment)) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Sentiment)) +
  ggplot2::geom_histogram(fill = "#132B43", binwidth = 0.1) +
  ggplot2::labs(
    x = "Sentiment",
    y = "Comments",
    title = "Sentiment Scores"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

## create clarity histogram ----
clarityHistogram <- gaWebCommentData |>
  purrr::map(.f = \(commentInfo) data.frame(Clarity = commentInfo$Clarity)) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Clarity)) +
  ggplot2::geom_histogram(fill = "#132B43", binwidth = 0.1) +
  ggplot2::labs(
    x = "Clarity",
    y = "Comments",
    title = "Clarity Scores"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

## create character length histogram ----
characterLengthHistogram <- gaWebCommentData |>
  purrr::map(.f = \(commentInfo) data.frame(Characters = commentInfo$Characters)) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Characters)) +
  ggplot2::geom_histogram(fill = "#C49324", binwidth = 75) +
  ggplot2::labs(
    x = "Character Length",
    y = "Comments",
    title = "Character Lengths"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

## create location counts histogram ----
locationCountsHistogram <- gaWebCommentData |>
  purrr::map(.f = \(commentInfo) data.frame(Locations = nrow(commentInfo$LocationsMentioned))) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Locations)) +
  ggplot2::geom_histogram(fill = "#C49324", binwidth = 1) +
  ggplot2::labs(
    x = "Locations",
    y = "Comments",
    title = "Unique Locations Mentioned"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

## compile histograms into single figure ----
commentMetricsFigure <- (sentimentHistogram + clarityHistogram) /
  (characterLengthHistogram + locationCountsHistogram) +
  patchwork::plot_annotation(
    title = paste0("Comment Metrics: Georgia Web Comments"),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 20
      )
    )
  )

## save comment metrics figure ----
ggplot2::ggsave(
  filename = file.path(figurePath, commentMetricsFigureFilename),
  plot = commentMetricsFigure
)

## import georgia counties shapefile ----
gaCounties <- tigris::counties(state = "GA") |>
  dplyr::select(County = NAMELSAD)

## create county mentions map ----
countyMentionsMap <- locationData |>
  dplyr::select(CommentID, County = SurroundingCounties) |>
  tidyr::unnest_longer(County) |>
  dplyr::group_by(County) |>
  dplyr::summarise(Comments = length(unique(CommentID))) |>
  dplyr::right_join(gaCounties, by = "County") |>
  tidyr::replace_na(replace = list(Comments = 0)) |>
  sf::st_as_sf() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = log1p(Comments)), color = NA) +
  ggplot2::scale_fill_gradient(low = "#EEEEEE", high = "#132B43") +
  ggplot2::labs(
    title = paste(
      "Number of Comments Mentioning Locations in\n",
      "Each Georgia County"
    ),
    fill = "Logged Comment Count"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.justification = "center",
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )
  
## save county mentions map ----
ggplot2::ggsave(
  filename = file.path(figurePath, countyMentionsMapFilename),
  plot = countyMentionsMap,
  bg = "#FFFFFF"
)
