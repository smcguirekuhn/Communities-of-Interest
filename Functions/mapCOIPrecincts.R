mapCOIPrecincts <- function(
    commentInfo,
    locationMatches,
    precinctBoundaries,
    titleFormat = glue::glue(
      "Comment {commentInfo$CommentID}: ",
      "{commentInfo$Name} of {commentInfo$County}"
    )
  ) {
  
  # check arguments ----
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts") %in% names (locationMatches)))
  stopifnot(all(c("sf", "data.frame") %in% class(precinctBoundaries)))
  stopifnot(length(titleFormat) == 1)
  stopifnot("character" %in% class(titleFormat))
  
  # add precinct ids ----
  locationsMentioned <- commentInfo$LocationsMentioned |>
    dplyr::mutate(
      Match = purrr::pmap(
        .l = list(Name, AdminLevel, SurroundingCounties),
        .f = \(location, adminLevel, surroundingCounties) {
          matchLocation(
            location = location,
            adminLevel = adminLevel |> as.character(),
            locationMatches = locationMatches,
            surroundingCounties = surroundingCounties
          ) |>
            tidyr::unnest(cols = Precincts) |>
            dplyr::select(
              Precincts = PrecinctID,
              Match,
              JaccardDistance,
              JaroWinklerDistance
            ) |>
            tibble::as_tibble()
        }
      )
    )
  
  # add polygon geometry ----
  locationsMentioned <- locationsMentioned |>
    tidyr::unnest(cols = Match) |>
    dplyr::filter(
      any(
        JaccardDistance < 0.3,
        JaroWinklerDistance < 0.3,
        AdminLevel %in% c("State House District", "State Senate District", "Congressional District")
      )
    ) |>
    dplyr::left_join(
      y = precinctBoundaries |> dplyr::select(PrecinctID),
      by = c("Precincts" = "PrecinctID")
    ) |>
    sf::st_as_sf()
  
  # create coi map ----
  coiMap <- locationsMentioned |>
    dplyr::mutate(Group = factor(Group)) |>
    dplyr::group_by(Name) |>
    dplyr::summarise(
      Precincts = dplyr::n(),
      Group = unique(Group),
      Match = unique(Match)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = Group),
      alpha = 0.3,
      color = "#AAAAAA",
      linewidth = 0.5
    ) +
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(label = Match, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,
      box.padding = 0.5,
      max.overlaps = Inf,
      alpha = 0.3
    ) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::labs(
      title = titleFormat,
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()
    
  # return coi map ----
  return(coiMap)
}