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
      Precincts = purrr::pmap(
        .l = list(Name, AdminLevel),
        .f = \(location, adminLevel) {
          matchLocation(
            location = location,
            adminLevel = adminLevel |> as.character(),
            locationMatches = locationMatches
          ) |> tidyr::unnest(cols = Precincts) |> dplyr::pull(UNIQUE_ID)
        }
      )
    )
  
  # add polygon geometry ----
  locationsMentioned <- locationsMentioned |>
    tidyr::unnest_longer(Precincts) |>
    dplyr::left_join(
      y = precinctBoundaries |> dplyr::select(UNIQUE_ID),
      by = c("Precincts" = "UNIQUE_ID")
    ) |>
    sf::st_as_sf()
  
  # create coi map ----
  coiMap <- locationsMentioned |>
    dplyr::mutate(Group = factor(Group)) |>
    dplyr::group_by(Name) |>
    dplyr::summarise(
      Precincts = dplyr::n(),
      Group = unique(Group)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = Group),
      alpha = 0.3,
      color = "#AAAAAA",
      linewidth = 0.5
    ) +
    ggplot2::geom_sf_text(
      mapping = ggplot2::aes(label = Name),
      fun.geometry = sf::st_centroid
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