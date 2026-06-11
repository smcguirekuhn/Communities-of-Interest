
# Script 02: Match Location Mentions to GeoNames Entries

# reset global environment ----
rm(list = ls())

# import packages ----
library(dplyr)
library(tidyr)
library(purrr)
library(tigris)
library(sf)
library(stringdist)

# source helper functions ----
source(file = list.files(path = "./Functions/", full.names = TRUE))

# assign import and export destinations ----
dataPath <- "./Data/"
usGeoNamesFilename <- "US.txt"
paGeoNamesFilename <- "PAGeoNames.rds"
paSchoolDistrictsFilename <- "PASchoolDistricts.rds"
commentDataFilename <- "CommentDataHouse.rds"

# # import pennsylvania geonames (source data too large for github) ----
# paGeoNames <- read.delim(
#   file = file.path(dataPath, usGeoNamesFilename),
#   header = FALSE
# ) |>
#   stats::setNames(
#     nm = c(
#       "Geonameid", "Name", "Asciiname", "AlternateNames",
#       "Latitude", "Longitude",
#       "FeatureClass", "FeatureCode",
#       "CountryCode", "CC2",
#       "Admin1Code", "Admin2Code", "Admin3Code", "Admin4Code",
#       "Population", "Elevation", "Dem", "Timezone",
#       "ModificationDate"
#     )
#   ) |>
#   dplyr::filter(CountryCode == "US", Admin1Code == "PA") |>
#   dplyr::select(-c(CC2, Admin4Code, Dem, Timezone, ModificationDate))
# 
# # save pennsylvania geonames ----
# saveRDS(object = paGeoNames, file = file.path(dataPath, paGeoNamesFilename))

# import pennsylvania geonames ----
paGeoNames <- readRDS(file = file.path(dataPath, paGeoNamesFilename))

# expand pennsylvania geonames to include alternate names ----
paGeoNames <- paGeoNames |>
  dplyr::mutate(Name = paste(Name, Asciiname, AlternateNames, sep = ",")) |>
  tidyr::separate_longer_delim(cols = Name, delim = ",") |>
  dplyr::filter(Name != "") |>
  dplyr::distinct()

# # import pennsylvania school districts (census web connection may fail) ----
# paSchoolDistricts <- tigris::school_districts(state = "PA", year = 2020) |>
#   sf::st_drop_geometry() |>
#   dplyr::select(Name = NAME)
# 
# # save pennsylvania school districts ----
# saveRDS(object = paSchoolDistricts, file = file.path(dataPath, paSchoolDistrictsFilename))

# import pennsylvania school districts ----
paSchoolDistricts <- readRDS(file = file.path(dataPath, paSchoolDistrictsFilename))

# import pennsylvania counties shapefile ----
paCounties <- tigris::counties(state = "PA") |>
  dplyr::select(County = NAMELSAD)

# import comment data ----
commentData <- readRDS(file = file.path(dataPath, commentDataFilename))

# match geonames to comment data ----
commentData <- commentData |>
  purrr::map(
    .progress = TRUE,
    .f = \(commentInfo) {
      if (nrow(commentInfo$LocationsMentioned) > 0) {
        
        ## gather geonames matches for all mentioned locations in a comment ----
        geoNamesMatches <- purrr::map(
          .x = 1:nrow(commentInfo$LocationsMentioned),
          .f = \(locationID) {
            
            ### assign location name ----
            location <- commentInfo$LocationsMentioned$Name[locationID]
            
            ### assign location admin level ----
            adminLevels <- commentInfo$LocationsMentioned$AdminLevel[locationID]
            
            ### assign surrounding counties ----
            commentCounties <- paCounties |>
              dplyr::filter(County %in% unlist(commentInfo$LocationsMentioned$SurroundingCounties[locationID]))
            counties <- paCounties |>
              sf::st_filter(y = commentCounties, .predicate = sf::st_touches) |>
              dplyr::bind_rows(commentCounties) |>
              sf::st_drop_geometry() |>
              dplyr::pull(County)
            
            ### assign surrounding county fips codes for an individual location ----
            countyFIPSCodes <- tigris::fips_codes |>
              dplyr::filter(state == "PA", county %in% counties) |>
              dplyr::pull(county_code) |>
              as.numeric()
            
            ### filter possible geonames matches to candidate counties ----
            geoNamesSubset <- adminLevels |>
              purrr::list_c() |>
              purrr::map(
                .f = \(adminLevel) {
                  geoNamesSet <- paGeoNames |>
                    filterGeoNames(
                      countyFIPSCodes = countyFIPSCodes,
                      adminLevel = adminLevel,
                      schoolDistricts = paSchoolDistricts
                    )
                  return(geoNamesSet)
                }
              ) |>
              purrr::list_rbind() |>
              dplyr::distinct()
            
            ### match location mentions to their closest geonames candidates ----
            geoNames <- geoNamesSubset |>
              dplyr::mutate(
                JaccardDistance = stringdist::stringdist(
                  a = Name,
                  b = location,
                  method = "jaccard"
                )
              ) |>
              dplyr::slice_min(order_by = JaccardDistance) |>
              dplyr::pull(Name) |>
              paste(collapse = ", ")
            if (length(geoNames) == 0) geoNames <- NA
            
            ### return geonames location matches ----
            return(geoNames)
          }
        ) |> purrr::list_c()
      } else {
        geoNamesMatches <- character(0)
      }
      
      ## add geonames location matches to comment information ----
      commentInfo$LocationsMentioned$GeoNames <- geoNamesMatches
      
      ## return comment information ----
      return(commentInfo)
    }
  )

# save comment data ----
saveRDS(object = commentData, file = file.path(dataPath, commentDataFilename))
