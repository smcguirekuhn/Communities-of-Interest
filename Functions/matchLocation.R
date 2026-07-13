matchLocation <- function(
    location,
    adminLevel = c(
      "Landmark",
      "School",
      "Neighborhood",
      "Township",
      "Borough",
      "Town",
      "City",
      "School District",
      "County",
      "Legislative District",
      "Region",
      "NA"
    ),
    # surroundingCounties,
    locationMatches
  ) {
  
  # check arguments ----
  stopifnot(is.character(location))
  match.arg(adminLevel)
  # stopifnot(all(is.character(surroundingCounties)))
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts") %in% names(locationMatches)))
  
  # simplify municipal admin level assignments ----
  if (adminLevel %in% c("Township", "Borough", "Town", "City")) adminLevel <- "Municipality"
  
  # # contextualize location match candidates to surrounding counties ----
  # allCounties <- tigris::counties(state = "GA")
  # locationContext <- allCounties |> dplyr::filter(NAMELSAD %in% surroundingCounties)
  # locationContext <- locationContext |>
  #   dplyr::bind_rows(allCounties |> sf::st_filter(y = locationContext, .predicate = sf::st_touches)) |>
  #   dplyr::distinct() |>
  #   sf::st_union()
  # locationContext <- sf::st_sf(geometry = locationContext, Name = "Context")
  
  # identify best location match by string distance ----
  locationMatch <- locationMatches |>
    dplyr::filter(
      AdminLevel == adminLevel
      # lengths(sf::st_intersects(x = locationMatches, y = locationContext)) > 0
    ) |>
    dplyr::mutate(
      JaccardDistance = stringdist::stringdist(a = Name, b = location, method = "jaccard"),
      JaroWinklerDistance = stringdist::stringdist(a = Name, b = location, method = "jw"),
      .before = "Precincts"
    ) |>
    dplyr::slice_min(order_by = dplyr::tibble(JaccardDistance, JaroWinklerDistance)) |>
    dplyr::rename(Match = Name) |>
    dplyr::mutate(Name = location, .before = "Match")
  
  # check for unusual match counts ----
  if (nrow(locationMatch) == 0) {
    cli::cli_warn(message = glue::glue("No matches found for {location}"))
  } else if (nrow(locationMatch) > 1) {
    cli::cli_warn(message = glue::glue("Multiple matches found for {location}"))
  }
  
  # return location match ----
  return(locationMatch)
}