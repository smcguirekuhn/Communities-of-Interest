
# Script 05: Create Descriptive Figures from Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(tigris)
library(ggplot2)
library(tidygraph)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
figurePath <- "./Figures/Colorado"
coWebCommentsFilename <- "COWebComments.rds"
allGroundTruthLocationsFilename <- "./Validation/GroundTruth/AllGroundTruthLocations.rds"
allGroundTruthRequestsFilename <- "./Validation/GroundTruth/AllGroundTruthRequests.rds"
coCommenterMapFilename <- "COCommenterMap.png"
exampleCommentNetworkFilename <- "ExampleCommentNetwork.png"

# import web comments ----
coWebComments <- readRDS(file = file.path(dataPath, coWebCommentsFilename))

# import all ground truth locations ----
allGroundTruthLocations <- readRDS(file = file.path(allGroundTruthLocationsFilename))

# import all ground truth requests ----
allGroundTruthRequests <- readRDS(file = file.path(allGroundTruthRequestsFilename))

# hand code node centroids ----
nodeCentroids <- dplyr::tibble(
  name = c(
    "Manitou Springs (City)",
    "Old Colorado City (Neighborhood)",
    "Downtown Colorado Springs (City)",
    "Southeastern Colorado Springs (City)",
    "Tri-Lakes area (Region)",
    "Monument (Town)",
    "Palmer Lake (Town)",
    "Colorado Springs (City)",
    "State House District 17 (State House District)",
    "State House District 18 (State House District)"
  ),
  display = c(
    "Manitou Springs",
    "Old Colorado City",
    "Downtown\nColorado Springs",
    "Southeastern\nColorado Springs",
    "Tri-Lakes area",
    "Monument",
    "Palmer Lake",
    "Colorado Springs",
    "State House\nDistrict 17",
    "State House\nDistrict 18"
  ),
  x = c(1, 2, 2.5, 3.5, 2, 2, 1, 3, 4, 2),
  y = c(2, 2, 1.5, 1, 4, 3.5, 4, 2, 1, 1.5)
)

# create comment requests matrix ----
commentRequestsMatrix <- createRequestsMatrix(
  locationNodes = allGroundTruthLocations |>
    dplyr::filter(State == "Colorado", CommentID == 2242) |>
    dplyr::pull(FullLocationName),
  commentRequests = allGroundTruthRequests |>
    dplyr::filter(State == "Colorado", CommentID == 2242)
)

commentRequestsMatrix$Separations <- commentRequestsMatrix$Separations |>
  igraph::graph_from_adjacency_matrix(mode = "undirected") |>
  tidygraph::as_tbl_graph() |>
  tidygraph::activate(edges) |>
  tidygraph::mutate(EdgeType = "Separated") |>
  tidygraph::as_tibble()

commentRequestsMatrix$Groupings <- commentRequestsMatrix$Groupings |>
  igraph::graph_from_adjacency_matrix(mode = "undirected") |>
  tidygraph::as_tbl_graph() |>
  tidygraph::left_join(y = nodeCentroids, by = c("name")) |>
  tidygraph::activate(edges) |>
  tidygraph::mutate(EdgeType = "Grouped") |>
  tidygraph::bind_edges(commentRequestsMatrix$Separations) |>
  igraph::as.igraph()

# create example comment network plot ----
exampleCommentNetwork <- commentRequestsMatrix$Groupings |>
  ggraph::ggraph(layout = "manual", x = x, y = y) +
  ggraph::geom_edge_link(mapping = ggplot2::aes(color = EdgeType), alpha = 0.5, linewidth = 1.75) +
  ggraph::geom_node_text(mapping = ggplot2::aes(label = display)) +
  ggraph::scale_edge_color_manual(
    values = c("Grouped" = "#BBBBBB", "Separated" = "#FF0000"),
    name = "Edge Type"
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(title = "Example Comment Location Network") +
  ggraph::theme_graph() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_text(hjust = 0.5),
    legend.title.position = "top",
    legend.justification = "center",
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )

# save example comment network ----
ggplot2::ggsave(
  filename = file.path(figurePath, exampleCommentNetworkFilename),
  plot = exampleCommentNetwork,
  width = 8, 
  height = 6, 
  units = "in", 
  dpi = 600
)

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
