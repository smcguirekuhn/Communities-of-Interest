
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
      "State House District",
      "State Senate District",
      "Congressional District",
      "Region",
      "NA"
    ),
    subarea = c(
      "Northern",
      "Northeastern",
      "Eastern",
      "Southeastern",
      "Southern",
      "Southwestern",
      "Western",
      "Northwestern",
      "Central",
      "Urban",
      "Rural",
      "Unincorporated",
      "NA"
    ),
    locationMatches,
    mongeElkanThreshold = 0.15,
    cosineThreshold = 0.054,
    precincts = NULL,
    urbanAreas = NULL,
    municipalities = NULL
  ) {
  
  # check arguments ----
  stopifnot(is.character(location))
  stopifnot(is.data.frame(locationMatches))
  stopifnot(all(c("Name", "AdminLevel", "Precincts") %in% names(locationMatches)))
  
  # identify best location match by string distance ----
  if (adminLevel %in% c("Landmark", "School", "NA")) {
    locationMatch <- dplyr::tibble(Name = location, AdminLevel = adminLevel, Match = NA)
  } else {
    
    ## idenfity location match candidates ----
    locationMatch <- locationMatches |>
      dplyr::filter(AdminLevel == adminLevel) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        MongeElkanDistance = mongeElkanDistance(x = Name, y = location),
        CosineDistance = stringdist::stringdist(a = Name, b = location, method = "cosine"),
        .before = "Precincts"
      ) |>
      dplyr::ungroup() |>
      dplyr::rename(Match = Name) |>
      dplyr::mutate(Name = location, .before = "Match")
    
    ## filter to individual location match ----
    if (adminLevel == "Region") {
      locationMatch <- locationMatch |>
        dplyr::filter(MongeElkanDistance <= mongeElkanThreshold, CosineDistance == 0) |>
        dplyr::slice_min(order_by = CosineDistance, n = 1, with_ties = FALSE)
    } else {
      locationMatch <- locationMatch |>
        dplyr::filter(MongeElkanDistance <= mongeElkanThreshold, CosineDistance <= cosineThreshold) |>
        dplyr::slice_min(order_by = CosineDistance, n = 1, with_ties = FALSE)
    }
  }
  
  # check for unusual match counts before matching potential subareas ----
  if (nrow(locationMatch) == 0) {
    cli::cli_inform(message = glue::glue("No matches found for {location} ({adminLevel})"))
    locationMatch <- dplyr::tibble(Name = location, AdminLevel = adminLevel, Match = NA)
  } else if (subarea != "NA") {
    cli::cli_inform(message = glue::glue("Matching {subarea} precincts for {location} ({adminLevel})"))
    matchedPrecincts <- locationMatch |>
      dplyr::pull(Precincts) |>
      purrr::list_rbind() |>
      dplyr::left_join(y = precincts |> dplyr::select(GEOID20), by = c("PrecinctID" = "GEOID20")) |>
      sf::st_as_sf(crs = sf::st_crs(precincts)) |>
      assignSubareaPrecincts(
        urbanAreas = urbanAreas,
        municipalities = municipalities,
        subarea = subarea
      ) |>
      sf::st_drop_geometry()
    locationMatch <- locationMatch |> dplyr::mutate(Precincts = list(matchedPrecincts))
  }
  
  # add subarea to location match ----
  locationMatch <- locationMatch |> dplyr::mutate(SubareaDescription = subarea, .after = AdminLevel)
  
  # return location match ----
  return(locationMatch)
}

# monge-elkan hybrid distance function
mongeElkanDistance <- function(x, y) {
  
  ## tokenize input strings ----
  x <- x |> tolower() 
  y <- y |> tolower()
  xTokens <- x |> regmatches(m = gregexpr("[a-z0-9]+", x)) |> unlist()
  yTokens <- y |> regmatches(m = gregexpr("[a-z0-9]+", y)) |> unlist()
  
  ## penalize empty strings ----
  if (length(xTokens) == 0 || length(yTokens) == 0) return(1.0)
  
  ## extract numeric elements each token set ----
  numberTokensX <- xTokens[grepl(pattern = "\\b\\d+\\b", x = xTokens)]
  numberTokensY <- yTokens[grepl(pattern = "\\b\\d+\\b", x = yTokens)]
  
  ## penalize distance for mismatched numbers ----
  if (length(numberTokensX) > 0 && length(numberTokensY) > 0) {
    if (!any(numberTokensX %in% numberTokensY)) return(0.9)
  }
  
  ## calculate jaro-winkler distance matrix ----
  distanceMatrix <- stringdist::stringdistmatrix(a = xTokens, b = yTokens, method = "jw", p = 0.1)
  
  ## aggregate across jaro-winkler distance matrix using monge-elkan method ----
  distance <- distanceMatrix |> apply(MARGIN = 1, FUN = min) |> mean()
  
  ## return monge-eklan distance ----
  return(distance)
}