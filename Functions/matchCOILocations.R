
matchCOILocations <- function(commentInfo, locationMatches, precinctBoundaries) {
  
  # check arguments ----
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts") %in% names (locationMatches)))
  stopifnot(all(c("sf", "data.frame") %in% class(precinctBoundaries)))
  
  # add precinct ids ----
  locationsMentioned <- commentInfo$LocationsMentioned |>
    dplyr::mutate(
      Match = purrr::pmap(
        .l = list(Name, AdminLevel),
        .f = \(location, adminLevel) {
          locationMatch <- location |>
            matchLocation(
              adminLevel = adminLevel |> as.character(),
              locationMatches = locationMatches
            ) |>
            tidyr::unnest(cols = Precincts) |>
            dplyr::select(Match, MongeElkanDistance,Precincts = PrecinctID) |>
            tibble::as_tibble()
          return(locationMatch)
        }
      )
    )
  
  # add polygon geometry ----
  locationsMentioned <- locationsMentioned |>
    tidyr::unnest(cols = Match) |>
    dplyr::filter(
      any(
        MongeElkanDistance < 0.4,
        AdminLevel %in% c("State House District", "State Senate District", "Congressional District")
      )
    ) |>
    dplyr::left_join(
      y = precinctBoundaries |> dplyr::select(PrecinctID),
      by = c("Precincts" = "PrecinctID")
    ) |>
    sf::st_as_sf()
  
  # return mentioned locations ----
  return(locationsMentioned)
}