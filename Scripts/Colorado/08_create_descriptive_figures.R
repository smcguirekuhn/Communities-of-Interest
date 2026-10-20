
# Script 08: Create Descriptive Figures from Colorado Web Comments

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(tigris)
library(ggplot2)
library(scales)
library(tidygraph)
library(wru)
library(zipWRUext2)
library(humaniformat)
library(rurality)

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
zctaRacialSamplingFilename <- "ZCTARacialSampling.png"

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


# colorado zip code modeling ----

## import colorado zip code boundaries ----
coZCTABoundaries <- tigris::zctas(state = "CO", year = 2010)

## import census data by zip code ----
coZCTADemographics <- wru::get_census_data(
  key = Sys.getenv("CENSUS_API_KEY"),
  states = "CO",
  census.geo = "zcta",
  year = "2020"
) |>
  purrr::pluck("CO") |>
  purrr::pluck("zcta") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    `Proportion White` = P12I_001N/sum(dplyr::c_across(dplyr::starts_with("P12"))),
    `Proportion Black` = P12J_001N/sum(dplyr::c_across(dplyr::starts_with("P12"))),
    `Proportion Hispanic` = P12H_001N/sum(dplyr::c_across(dplyr::starts_with("P12"))),
    `Proportion Asian` = P12L_001N/sum(dplyr::c_across(dplyr::starts_with("P12"))),
    `Proportion Other` = (P12K_001N + P12M_001N + P12N_001N + P12O_001N)/sum(dplyr::c_across(dplyr::starts_with("P12"))),
    `Total Population` = sum(dplyr::c_across(dplyr::starts_with("P12")))
  ) |>
  dplyr::filter(`Total Population` > 0) |>
  dplyr::select(ZIPCode = zcta, dplyr::starts_with("Proportion"), `Total Population`)

## add demographic covariates to zip code boundaries ----
coZCTAs <- coZCTABoundaries |>
  dplyr::rename(ZIPCode = ZCTA5CE10) |>
  dplyr::left_join(coZCTADemographics, by = "ZIPCode") |>
  dplyr::mutate(Rurality = rurality::get_ruca(zip = ZIPCode) |> dplyr::pull(primary_ruca)) |>
  dplyr::select(ZIPCode, dplyr::starts_with("Proportion"), `Total Population`, Rurality)

## sort comment counts by zip code ----
coWebCommentZCTAs <- coWebComments |>
  dplyr::rowwise() |>
  dplyr::mutate(ZIPCode = strsplit(ZIPCode, split = "-")[[1]][1]) |>
  dplyr::filter(ZIPCode %in% coZCTAs[["ZIPCode"]]) |>
  dplyr::mutate(
    ZIPCode = dplyr::case_when(
      ZIPCode %in% c("80477", "80488") ~ "80487",
      ZIPCode == "80497" ~ "80498",
      .default = ZIPCode
    )
  ) |>
  dplyr::group_by(ZIPCode) |>
  dplyr::summarise(Comments = dplyr::n()) |>
  dplyr::ungroup()

## add comment counts to zip code boundaries ----
coZCTAs <- coZCTAs |>
  dplyr::left_join(coWebCommentZCTAs, by = "ZIPCode") |>
  tidyr::replace_na(replace = list(Comments = 0))


# colorado comment density map ----

## import new legislative district boundaries ----
legislativeBoundaries2022 <- tigris::congressional_districts(state = "CO", year = 2022)

## create colorado commenter map ----
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

## save colorado commenter map ----
ggplot2::ggsave(
  filename = file.path(figurePath, coCommenterMapFilename),
  plot = coCommenterMap,
  width = 7, 
  height = 6, 
  units = "in", 
  dpi = 600
)


# zcta racial sampling figure ----

## add valid last names to commenter data ----
coWebCommenterNames <- coWebComments |>
  dplyr::rowwise() |>
  dplyr::mutate(ZIPCode = strsplit(ZIPCode, split = "-")[[1]][1]) |>
  dplyr::ungroup() |>
  dplyr::filter(ZIPCode %in% coZCTAs[["ZIPCode"]]) |>
  dplyr::select(Name, ZIPCode) |>
  dplyr::mutate(
    `Last Name` = Name |>
      humaniformat::format_period() |>
      humaniformat::format_reverse() |>
      humaniformat::last_name() |>
      stringr::str_to_lower() |>
      stringr::str_remove("[[:punct:]]+$")
  ) |>
  tidyr::drop_na() |>
  dplyr::filter(stringr::str_length(`Last Name`) > 1)

## estimate race of commenters using wru modeling by zcta ----
coWebCommenterRaceEstimates <- coWebCommenterNames |>
  zipWRUext2::zip_wru(
    state = "COLORADO",
    year1 = 2020,
    zip_col = "ZIPCode",
    surname_field = "Last Name"
  ) |>
  dplyr::left_join(coZCTAs |> sf::st_drop_geometry(), by = c("zcta5" = "ZIPCode")) |>
  tidyr::drop_na()

## set random seed ----
set.seed(seed = 1998)

## simulate 1000 draws from racial probability distributions ----
coWebCommenterRaceSimulations <- coWebCommenterRaceEstimates |>
  dplyr::rowwise() |>
  dplyr::mutate(
    `Simulated Race` = list(
      dplyr::tibble(
        Type = rep(c("Surname Sampling", "Census Sampling"), each = 1000),
        Simulation = rep(1:1000, 2),
        `Simulated Race` = c(
          sample(
            x = c("White", "Black", "Hispanic", "Asian", "Other"),
            size = 1000,
            prob = dplyr::c_across(dplyr::starts_with("pred")),
            replace = TRUE
          ),
          sample(
            x = c("White", "Black", "Hispanic", "Asian", "Other"),
            size = 1000,
            prob = dplyr::c_across(dplyr::starts_with("Proportion")),
            replace = TRUE
          )
        )
      )
    )
  ) |>
  dplyr::select(`Simulated Race`) |>
  tidyr::unnest(cols = "Simulated Race") |>
  dplyr::count(Type, Simulation, `Simulated Race`)

## create zcta racial sampling figure ----
zctaRacialSamplingFigure <- coWebCommenterRaceSimulations |>
  dplyr::mutate(
    `Simulated Race` = `Simulated Race` |>
      factor(levels = c("White", "Black", "Hispanic", "Asian", "Other")),
    n = n/nrow(coWebCommenterRaceEstimates)
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_density(
    mapping = ggplot2::aes(x = n, color = Type),
    alpha = 0.7,
    key_glyph = ggplot2::draw_key_path
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Surname Sampling" = "#405149",
      "Census Sampling" = "#C7A285"
    )
  ) +
  ggplot2::scale_x_continuous(
    trans = "logit",
    breaks = seq(0.1, 0.9, 0.1),
    labels = scales::label_percent(),
    limits = c(NA, 0.99)
  ) +
  ggplot2::facet_wrap(facets = ~`Simulated Race`, nrow = 5) +
  ggplot2::labs(
    title = "ZCTA Sampling of Colorado Commenter Race Estimates 
    Against Proportional Racial Sampling by ZCTA",
    x = "Commenter Proportion",
    y = "Density",
    color = "Simulated Sample"
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

## save example comment network ----
ggplot2::ggsave(
  filename = file.path(figurePath, zctaRacialSamplingFilename),
  plot = zctaRacialSamplingFigure,
  width = 8, 
  height = 8, 
  units = "in", 
  dpi = 600
)


# spatial modeling by zcta ----

## format relevant variables for modeling ----
coZCTAs <- coZCTAs |>
  dplyr::mutate(
    `Logged Population` = log1p(`Total Population`),
    `Percent White` = 100*`Proportion White`,
    `RUCA Code Category` = dplyr::case_when(
      Rurality %in% c(1:3) ~ "Metropolitan Area",
      Rurality %in% c(4:6) ~ "Micropolitan Area",
      Rurality %in% c(7:9) ~ "Small Town Area",
      Rurality == 10 ~ "Rural Area",
    ) |> factor(levels = c("Metropolitan Area", "Micropolitan Area", "Small Town Area", "Rural Area"))
  )

## create matrix of neighbor weights ----
neighborWeights <- spdep::nb2listw(
  neighbours = spdep::poly2nb(pl = coZCTAs, queen = TRUE),
  style = "W",
  zero.policy = TRUE
)

## fit full spatial error model ----
spatialErrorModel <- spatialreg::errorsarlm(
  formula = Comments ~ `Logged Population` + `Percent White` + `RUCA Code Category`,
  data = coZCTAs,
  listw = neighborWeights,
  zero.policy = TRUE
)

## show spatial error model summary ----
summary(spatialErrorModel, Nagelkerke = TRUE)

## fit full spatial lag model ----
spatialLagModel <- spatialreg::lagsarlm(
  formula = Comments ~ `Logged Population` + `Percent White` + `RUCA Code Category`,
  data = coZCTAs,
  listw = neighborWeights,
  zero.policy = TRUE
)

## show spatial error model summary ----
summary(spatialLagModel, Nagelkerke = TRUE)
