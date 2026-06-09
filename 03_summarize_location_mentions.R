
# Script 03: Summarize and Visualize Location Mentions

# reset global environment ----
rm(list = ls())

# import packages ----
library(dplyr)
library(tidyr)
library(purrr)
library(snakecase)
library(ggplot2)
library(patchwork)
library(tigris)
library(sf)

# source helper functions ----
source(file = list.files(path = "./Functions/", full.names = TRUE))

# assign import and export destinations ----
dataPath <- "./Data/"
figurePath <- "./Figures/"
commentDataFilename <- "CommentDataPartial.rds"
adminLevelPieChartFilename <- "AdminLevelPieChart.png"
commentMetricsFigureFilename <- "CommentMetricsFigure.png"

# import comment data ----
commentData <- readRDS(file = file.path(dataPath, commentDataFilename))

# set admin level color scheme ----
adminLevels <- c(
  "Neighborhood" = "#9C751C",
  "Township" = "#C49324",
  "Township, Borough" = "#C49324",
  "Borough" = "#CFA84F",
  "City" = "#DBBE7B",
  "School District" = "#132B43",
  "County" = "#425568",
  "Region" = "#717F8E",
  "Other" = "#EEEEEE"
)

# compile location data ----
locationData <- commentData |>
  purrr::map(.f = \(commentInfo) commentInfo$LocationsMentioned) |>
  purrr::list_rbind(names_to = "CommentID") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    AdminLevel = AdminLevel |>
      snakecase::to_title_case() |>
      paste(collapse = ", ") |>
      factor(levels = names(adminLevels)),
    Source = factor(x = "House")
  )

# create admin level pie chart ----
adminLevelPieChart <- locationData |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Source, fill = AdminLevel)) +
  ggplot2::geom_bar(width = 1) +
  ggplot2::coord_polar(theta = "y", start = 0) +
  ggplot2::scale_fill_manual(values = adminLevels) +
  ggplot2::labs(
    title = "Location Mentions by Administrative Level",
    fill = "Admin Level"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

# save admin level pie chart ----
ggplot2::ggsave(
  filename = file.path(figurePath, adminLevelPieChartFilename),
  plot = adminLevelPieChart
)

# create sentiment histogram ----
sentimentHistogram <- commentData |>
  purrr::map(.f = \(commentInfo) data.frame(Sentiment = commentInfo$Sentiment)) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Sentiment)) +
  ggplot2::geom_histogram(fill = "#132B43", binwidth = 0.05) +
  ggplot2::labs(
    x = "Comment Sentiment",
    y = "Frequency",
    title = "Comment Sentiment Scores"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

# create clarity histogram ----
clarityHistogram <- commentData |>
  purrr::map(.f = \(commentInfo) data.frame(Clarity = commentInfo$Clarity)) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Clarity)) +
  ggplot2::geom_histogram(fill = "#132B43", binwidth = 0.05) +
  ggplot2::labs(
    x = "Comment Clarity",
    y = "Frequency",
    title = "Comment Clarity Scores"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

# create character length histogram ----
characterLengthHistogram <- commentData |>
  purrr::map(.f = \(commentInfo) data.frame(Characters = commentInfo$Characters)) |>
  purrr::list_rbind(names_to = "CommentID") |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Characters)) +
  ggplot2::geom_histogram(fill = "#C49324", binwidth = 50) +
  ggplot2::labs(
    x = "Comment Character Length",
    y = "Frequency",
    title = "Comment Character Lengths"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))

# compile histograms into single figure ----
commentMetricsFigure <- (sentimentHistogram + clarityHistogram) /
  characterLengthHistogram +
  patchwork::plot_annotation(
    title = paste0("Comment Metrics: Pennsylvania House Testimony"),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 20
      )
    )
  )

# save comment metrics figure ----
ggplot2::ggsave(
  filename = file.path(figurePath, commentMetricsFigureFilename),
  plot = commentMetricsFigure
)
