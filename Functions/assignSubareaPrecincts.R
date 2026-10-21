
assignSubareaPrecincts <- function(
    precincts,
    urbanAreas,
    municipalities,
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
      "Unincorporated"
    )
) {
  
  # check arguments ----
  stopifnot("sf" %in% class(precincts))
  subarea <- match.arg(arg = subarea)
  
  # assign subarea membership ----
  if (subarea == "Urban") {
    
    ## filter to urban precincts ----
    precincts <- precincts |>
      sf::st_filter(y = urbanAreas |> sf::st_union(), .predicate = sf::st_intersects)
  } else if (subarea == "Rural") {
    
    ## filter to rural precincts ----
    precincts <- precincts |>
      sf::st_filter(y = urbanAreas |> sf::st_union(), .predicate = sf::st_disjoint)
  } else if (subarea == "Unincorporated") {
    
    ## filter to unincorporated precincts ----
    precincts <- precincts |>
      sf::st_filter(y = municipalities |> sf::st_union(), .predicate = sf::st_disjoint)
  } else {
    
    ## combine precincts into component polygons ----
    regionCenter <- sf::st_union(x = precincts) |> sf::st_point_on_surface()
    
    ## define bearings for each cardinal direction ----
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
    
    ## calculate distances between precincts and their region's center ---
    precincts <- precincts |>
      dplyr::mutate(
        Center = sf::st_point_on_surface(geometry),
        Distance = sf::st_distance(x = Center, y = sf::st_geometry(regionCenter))
      )
    
    ## define maximum distance for fuzzy assignment ----
    maxDistance <- quantile(as.numeric(precincts[["Distance"]]), probs = 0.95)
    
    ## define directional precinct subareas ----
    if (subarea == "Central") {
      
      ### filter to central precincts ----
      precincts<- precincts |>
        rowwise() |>
        dplyr::mutate(Weight = Weight*sqrt(1 - min(1, Distance/maxDistance))) |>
        dplyr::select(-c(Distance, Center))
    } else {
      
      ### filter to cardinal direction precincts ----
      precincts <- precincts |>
        dplyr::mutate(
          Angle = atan2(
            x = sf::st_coordinates(Center)[, "X"] - sf::st_coordinates(regionCenter)[, "X"],
            y = sf::st_coordinates(Center)[, "Y"] - sf::st_coordinates(regionCenter)[, "Y"]
          ),
          Angle = (Angle*180/pi + 360) %% 360,
          AngularDifference = (Angle - bearings[[subarea]] + 180) %% 360 - 180
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(Weight = Weight*sqrt(1 - min(1, abs(AngularDifference)/90))*sqrt(min(1, Distance/maxDistance))) |>
        dplyr::filter(Weight > 0) |>
        dplyr::select(-c(Distance, Center, Angle, AngularDifference))
    }
  }
  
  # return precincts data filtered to subarea membership ----
  return(precincts)
}
