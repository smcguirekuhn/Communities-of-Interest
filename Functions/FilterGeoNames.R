filterGeoNames <- function(
    geoNames,
    countyFIPSCodes,
    adminLevel = c(
      "neighborhood",
      "township",
      "borough",
      "city",
      "school district",
      "county",
      "region",
      "other"
    ),
    schoolDistricts = NULL
  ) {
  
  # gather appropriate set of geonames locations ----
  if (adminLevel == "school district") {
    
    ## assign school district names if location is a school district ----
    geoNamesSubset <- data.frame(Name = schoolDistricts)
    
  } else if (adminLevel == "other") {
    
    ## assign broad list of geonames if administrative level is unclear ----
    geoNamesSubset <- geoNames |>
      dplyr::filter(Admin2Code %in% countyFIPSCodes) |>
      dplyr::select(Name)
    
  } else {
    
    ## assign feature code based on valid administrative level ----
    featureCode <- switch(
      EXPR = adminLevel,
      "neighborhood" = "PPL",
      "township" = "ADM3",
      "borough" = "ADM3",
      "city" = "ADM3",
      "county" = "ADM2",
      "region" = "RGNE"
    )
    
    ## overwrite feature code for erroneous admin level assignments ----
    if (is.null(featureCode)) featureCode <- NA
    
    ## assign filtered geonames ----
    geoNamesSubset <- geoNames |>
      dplyr::filter(Admin2Code %in% countyFIPSCodes, FeatureCode == featureCode) |>
      dplyr::select(Name)
  }
  
  # return geonames locations ----
  return(geoNamesSubset)
}