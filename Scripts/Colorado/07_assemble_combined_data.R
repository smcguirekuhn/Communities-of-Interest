
# Script 07: Assemble Combined Data Frames for Colorado

# reset global environment ----
rm(list = ls())

# import packages ----
library(purrr)
library(tidyr)
library(dplyr)
library(stringr)
library(tigris)
library(tidygraph)
library(wru)
library(zipWRUext2)
library(humaniformat)
library(rurality)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/Colorado/"
coWebCommentsFilename <- "COWebComments.rds"
coWebCommentRequestsFilename <- "COWebCommentRequests.rds"
coWebCommentLocationsMatchedFilename <- "COWebCommentLocationsMatched.rds"
coWebCommenterInfoFilename <- "COWebCommenterInfo.rds"

# import colorado web comments ----
coWebComments <- readRDS(file = file.path(dataPath, coWebCommentsFilename))

# import colorado web comment requests ----
coWebCommentRequests <- readRDS(file = file.path(dataPath, coWebCommentRequestsFilename))

# import colorado web comment locations with matches ----
coWebCommentLocationsMatched <- readRDS(file = file.path(dataPath, coWebCommentLocationsMatchedFilename))


# create web commenter info with local contexts ----

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

## format colorado web commenter zip codes ----
coWebCommenterInfo <- coWebComments |>
  dplyr::select(CommentID, Characters, COI, Date, Name, ZIPCode) |>
  dplyr::rowwise() |>
  dplyr::mutate(ZIPCode = strsplit(ZIPCode, split = "-")[[1]][1]) |>
  dplyr::filter(ZIPCode %in% coZCTAs[["ZIPCode"]]) |>
  dplyr::mutate(
    ZIPCode = dplyr::case_when(
      ZIPCode %in% c("80477", "80488") ~ "80487",
      ZIPCode == "80497" ~ "80498",
      .default = ZIPCode
    )
  )

## format colorado web commenter last names ----
coWebCommenterInfo <- coWebCommenterInfo |>
  dplyr::mutate(
    `Last Name` = Name |>
      humaniformat::format_period() |>
      humaniformat::format_reverse() |>
      humaniformat::last_name() |>
      stringr::str_to_lower() |>
      stringr::str_remove("[[:punct:]]+$"),
    .after = "Name"
  ) |>
  tidyr::drop_na() |>
  dplyr::filter(stringr::str_length(`Last Name`) > 1)

## estimate race of commenters using wru modeling by zcta ----
coWebCommenterInfo <- coWebCommenterInfo |>
  zipWRUext2::zip_wru(
    state = "COLORADO",
    year1 = 2020,
    zip_col = "ZIPCode",
    surname_field = "Last Name"
  ) |>
  dplyr::rename(
    `Last Name` = surname,
    `WRU White` = pred.whi,
    `WRU Black` = pred.bla,
    `WRU Hispanic` = pred.his,
    `WRU Asian` = pred.asi,
    `WRU Other` = pred.oth
  ) |>
  dplyr::select(-surname.match) |>
  dplyr::relocate(ZIPCode = zcta5, .after = "WRU Other") |>
  tidyr::drop_na()

## add zcta demographics to commenter info ----
coWebCommenterInfo <- coWebCommenterInfo |>
  dplyr::left_join(
    y = coZCTAs |>
      sf::st_point_on_surface() |>
      dplyr::mutate(
        Latitude = sf::st_coordinates(geometry)[, 2],
        Longitude = sf::st_coordinates(geometry)[, 1]
      ) |>
      sf::st_drop_geometry(),
    by = "ZIPCode"
  )

## arrange colorado web commenter info ----
coWebCommenterInfo <- coWebCommenterInfo |>
  dplyr::arrange(CommentID, Date) |>
  tidyr::drop_na()

## save colorado web commenter info ----
coWebCommenterInfo |> saveRDS(file = file.path(dataPath, coWebCommenterInfoFilename))


# create matched requests data ----

# pair web comment requests with matched precincts ----
coWebCommentRequestsMatched <- coWebCommentRequests |>
  dplyr::group_by(Location1, Location2, Request) |>
  dplyr::summarise(Commenters = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(Commenters)) |>
  dplyr::left_join(
    y = coWebCommentLocationsMatched |>
      dplyr::select(Location1 = FullLocationName, Location1Match = Match, Precincts1 = Precincts),
    by = "Location1"
  ) |>
  dplyr::left_join(
    y = coWebCommentLocationsMatched |>
      dplyr::select(Location2 = FullLocationName, Location2Match = Match, Precincts2 = Precincts),
    by = "Location2"
  ) |>
  tidyr::drop_na()

# import colorado plan simulations ----
coStateMap <- alarmdata::alarm_50state_map(state = "CO")
coStatePlans <- alarmdata::alarm_50state_plans(state = "CO", year = 2020)
coStatePrecinctMatrix <- as.data.frame(redist::get_plans_matrix(coStatePlans)) |>
  dplyr::mutate(PrecinctID = coStateMap[["GEOID"]], .before = 1)

# evaluate benchmarked congruence for popular requests ----
coWebCommentRequestCongruence <- coWebCommentRequestsMatched |>
  dplyr::slice(1:300) |>
  dplyr::mutate(
    `Benchmarked Congruence` = purrr::pmap_dbl(
      .progress = "Calculating Request Congruence",
      .l = list(Precincts1, Precincts2, Request),
      .f = \(precincts1, precincts2, request) {
        if (request %in% c("kept whole", "grouped")) {
          
          ## collect grouped precincts ----
          groupedPrecincts <- dplyr::bind_rows(precincts1, precincts2) |> dplyr::distinct()
          
          ## build grouped congruence matrix ----
          congruenceMatrix <- calculateGroupedCongruence(
            statePrecinctMatrix = coStatePrecinctMatrix,
            groupedPrecincts = groupedPrecincts
          )
          
          ## calculate benchmarked congruence score ----
          congruenceScore <- (congruenceMatrix[["Congruence"]][1] - mean(congruenceMatrix[["Congruence"]][-1]))/
            sd(congruenceMatrix[["Congruence"]][-1])
          
          ## return benchmarked congruence score ----
          return(congruenceScore)
        } else if (request == "separated") {
          
          ## build separated congruence matrix ----
          congruenceMatrix <- calculateSeparatedCongruence(
            statePrecinctMatrix = coStatePrecinctMatrix,
            group1Precincts = precincts1,
            group2Precincts = precincts2
          )
          
          ## calculate benchmarked congruence score ----
          congruenceScore <- (congruenceMatrix[["Congruence"]][1] - mean(congruenceMatrix[["Congruence"]][-1]))/
            sd(congruenceMatrix[["Congruence"]][-1])
          
          ## return benchmarked congruence score ----
          return(congruenceScore)
        }
      }
    )
  )

coWebCommentRequestCongruence |>
  tidyr::replace_na(replace = list(`Benchmarked Congruence` = 0)) |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Commenters, y = `Benchmarked Congruence`)) +
  ggplot2::geom_point(mapping = ggplot2::aes(color = factor(Request), shape = factor(Request))) +
  ggplot2::geom_hline(
    yintercept = qnorm(0.975),
    linetype = "dashed",
    color = "#FF0000"
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "#000000"
  ) +
  ggplot2::geom_hline(
    yintercept = qnorm(0.025),
    linetype = "dashed",
    color = "#FF0000"
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "kept whole" = "#bcc9d5",
      "grouped" = "#6e89a3",
      "separated" = "#384959"
    )
  ) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    y = "Benchmarked Congruence Z-Scores",
    title = "Benchmarked Congruence by Request",
    color = "Request Type",
    shape = "Request Type"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    )
  )
