
# Script 05: Create Descriptive Figures from Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(tigris)
library(ggplot2)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
figurePath <- "./Figures/Colorado"
coWebCommentsFilename <- "COWebComments.rds"
coCommenterMapFilename <- "COCommenterMap.png"

# import web comments ----
coWebComments <- readRDS(file = file.path(dataPath, coWebCommentsFilename))

# import colorado zip code boundaries ----
coZCTAs <- tigris::zctas(state = "CO", year = 2010)

# sort comment counts by zip code ----
coWebCommentZCTAs <- coWebComments |>
  dplyr::rowwise() |>
  dplyr::mutate(ZIPCode = strsplit(ZIPCode, split = "-")[[1]][1]) |>
  dplyr::filter(ZIPCode %in% coZCTAs[["ZCTA5CE10"]]) |>
  dplyr::group_by(ZIPCode) |>
  dplyr::summarise(Comments = dplyr::n()) |>
  dplyr::ungroup()

# add comment counts to zip code boundaries ----
coZCTAs <- coZCTAs |>
  dplyr::rename(ZIPCode = ZCTA5CE10) |>
  dplyr::left_join(coWebCommentZCTAs, by = "ZIPCode") |>
  tidyr::replace_na(replace = list(Comments = 0))

# import new legislative district boundaries ----
legislativeBoundaries2022 <- tigris::congressional_districts(state = "CO", year = 2022)

# create colorado commenter map ----
coCommenterMap <- coZCTAs |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = Comments), color = NA) +
  ggplot2::geom_sf(data = legislativeBoundaries2022, fill = NA, color = "#333333") + 
  ggplot2::geom_sf_text(
    data = legislativeBoundaries2022,
    mapping = ggplot2::aes(label = CD118FP),
    size = 5,
    color = "#333333"
  ) +
  ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#405149", trans = "log1p") +
  ggplot2::labs(
    title = "Colorado Web Commenter ZIP Codes",
    fill = "COI Comments",
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_text(hjust = 0.5),
    legend.title.position = "top",
    legend.justification = "center",
    legend.key.width = ggplot2::unit(0.1, "npc"),
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )

# save colorado commenter map ----
ggplot2::ggsave(
  filename = file.path(figurePath, coCommenterMapFilename),
  plot = coCommenterMap,
  width = 7, 
  height = 6, 
  units = "in", 
  dpi = 600
)
