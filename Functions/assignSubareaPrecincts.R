
assignSubareaPrecincts <- function(
    precincts,
    subarea = c(
      "Northern",
      "Northeastern",
      "Eastern",
      "Southeastern",
      "Southern",
      "Southwestern",
      "Western",
      "Northwestern",
      "Central"
    )
) {
  
  # check arguments ----
  stopifnot("sf" %in% class(precincts))
  subarea <- match.arg(arg = subarea)
  
  # combine precincts into component polygons ----
  region <- sf::st_union(x = precincts) |> sf::st_collection_extract(type = "POLYGON")
  components <- region |> sf::st_cast(to = "POLYGON")
  components <- sf::st_sf(geometry = components)
  
  # find the largest inscribed circle across polygon components ----
  regionCenter <- components |>
    dplyr::mutate(inscribedCenter = polylabelr::poi(x = geometry, precision = 10)) |>
    tidyr::unnest_wider(col = "inscribedCenter") |>
    dplyr::slice_max(order_by = "dist") |>
    sf::st_drop_geometry() |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(precincts))
  # regionCenter <- centr::mean_center(sf::st_centroid(precincts), weight = "pop")
  
  # define bearings for each cardinal direction ----
  bearings <- c(
    Northern = 90,
    Northeastern = 45,
    Eastern = 0,
    Southeastern = 315,
    Southern = 270,
    Southwestern = 225,
    Western = 180,
    Northwestern = 135
  )
  
  # calculate distances between precincts and their region's center ---
  precincts <- precincts |>
    dplyr::mutate(
      Center = sf::st_point_on_surface(geometry),
      Distance = sf::st_distance(x = Center, y = sf::st_geometry(regionCenter))
    )
  
  # define distance cutoff for central precincts ----
  distanceCutoff <- quantile(as.numeric(precincts[["Distance"]]), probs = 1/sqrt(5))
  
  # assign subarea membership ----
  if (subarea == "Central") {
    precincts<- precincts |>
      dplyr::filter(as.numeric(Distance) <= distanceCutoff) |>
      dplyr::select(-c(Distance, Center))
  } else {
    precincts <- precincts |>
      dplyr::mutate(
        Angle = atan2(
          x = sf::st_coordinates(Center)[, "X"] - sf::st_coordinates(regionCenter)[, "X"],
          y = sf::st_coordinates(Center)[, "Y"] - sf::st_coordinates(regionCenter)[, "Y"]
        ),
        Angle = (Angle*180/pi + 360) %% 360,
        AngularDifference = (Angle - bearings[[subarea]] + 180) %% 360 - 180
      ) |>
      dplyr::filter(abs(AngularDifference) <= 45, as.numeric(Distance) >= distanceCutoff) |>
      dplyr::select(-c(Distance, Center, Angle, AngularDifference))
  }
  
  # return precincts data filtered to subarea membership ----
  return(precincts)
}
