mapCOIPrecincts <- function(commentInfo, locationMatches, precinctBoundaries) {
  
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
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = Group),
      alpha = 0.3
    )
    
  # return coi map ----
  return(coiMap)
}