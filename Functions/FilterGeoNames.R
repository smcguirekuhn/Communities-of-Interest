filterGeoNames <- function(
    geoNames,
    countyFIPSCodes,
    adminLevel = c(
      "Neighborhood",
      "Township",
      "Borough",
      "City",
      "SchoolDistrict",
      "County",
      "Region",
      "other"
    ),
    schoolDistricts = NULL
  ) {
  
  # check admin level argument ----
  match.arg(adminLevel)
  
  # gather appropriate set of geonames locations ----
  if (adminLevel == "SchoolDistrict") {
    
    ## assign school district names if location is a school district ----
    geoNamesSubset <- schoolDistricts
    
  } else if (adminLevel == "other") {
    
    ## assign broad list of geonames if administrative level is unclear ----
    geoNamesSubset <- geoNames |>
      dplyr::filter(Admin2Code %in% countyFIPSCodes) |>
      dplyr::select(Name)
    
  } else {
    
    ## assign feature code based on valid administrative level ----
    featureCode <- switch(
      EXPR = adminLevel,
      "Neighborhood" = "PPL",
      "Township" = "ADM3",
      "Borough" = "ADM3",
      "City" = "ADM3",
      "County" = "ADM2",
      "Region" = "RGNE"
    )
    
    ## overwrite feature code for erroneous admin level assignments ----
    if (is.null(featureCode)) featureCode <- NA
    
    ## assign filtered geonames ----
    geoNamesSubset <- geoNames |>
      dplyr::filter(Admin2Code %in% countyFIPSCodes, FeatureCode %in% featureCode) |>
      dplyr::select(Name)
  }
  
  # return geonames locations ----
  return(geoNamesSubset)
}