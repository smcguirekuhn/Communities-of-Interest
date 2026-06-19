
# Script 01b: Extract Communities of Interest Information from Testimony Essays

# reset global environment ----
rm(list = ls())

# import packages ----
library(dplyr)
library(purrr)
library(data.table)
library(lubridate)
library(ellmer)

# assign import and export destinations ----
dataPath <- "./Data/"
essayLookupFilename <- "PAHouseEssayLookup.csv"
essayDataFilename <- "EssayDataHouse.rds"

# import lookup table for pennsylvania house redistricting essays ----
essays <- read.csv(file = file.path(dataPath, essayLookupFilename)) |>
  dplyr::mutate(Date = lubridate::dmy(x = Date))

# compile essay data ----
essayData <- purrr::map(
  .progress = TRUE,
  .x = essays$File,
  .f = purrr::safely(\(essayPath) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 5)
    
    ## read in essay text ----
    essay <- readLines(file.path(dataPath, essayPath))
    
    ## initialize chat object ----
    chat <- ellmer::chat_openrouter(model = "mistralai/mistral-large")
    
    ## gather individual essay data and mentioned locations ----
    chatOutput <- chat$chat_structured(
      essay,
      type = ellmer::type_object(
        
        ### locations included in the community of interest ----
        LocationsMentioned = ellmer::type_array(
          description = paste(
            "All individual geographic locations that this Pennsylvania commenter mentions,",
            "including landmarks, neighborhoods, townships, boroughs, school districts, counties, etc.",
            "Order locations by their appearance in the comment.",
            "Only return clearly-identified locations."
          ),
          items = ellmer::type_object(
            
            #### location name ----
            Name = ellmer::type_string(
              description = paste(
                "The identifiable, administrative name of the location.",
                "For example, if 'western suburbs of the city of Lancaster' is mentioned,",
                "return only 'City of Lancaster'.",
                "The location should thus read like the following examples:",
                "Fishtown, State College, Bucks County."
              )
            ),
            
            #### location administrative level -----
            AdminLevel = ellmer::type_object(
              .description = paste(
                "Administrative level of the location.",
                "Must be exclusive and decisive."
              ),
              
              ##### neighborhood indicator ----
              Neighborhood = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a neighborhood of a city.",
                  "Return 0 if the location is not a neighborhood and 1 if the location is a neighborhood."
                )
              ),
              
              ##### township indicator ----
              Township = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a township.",
                  "Return 0 if the location is not a township and 1 if the location is a township."
                )
              ),
              
              ##### borough indicator ----
              Borough = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a borough.",
                  "Return 0 if the location is not a borough and 1 if the location is a borough."
                )
              ),
              
              ##### city indicator ----
              City = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a city (not a township or borough).",
                  "Return 0 if the location is not a city and 1 if the location is a city."
                )
              ),
              
              ##### school district indicator ----
              SchoolDistrict = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a school district.",
                  "Return 0 if the location is not a school district and 1 if the location is a school district."
                )
              ),
              
              ##### county indicator ----
              County = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a county.",
                  "Return 0 if the location is not a county and 1 if the location is a county."
                )
              ),
              
              ##### region indicator ----
              Region = ellmer::type_integer(
                description = paste(
                  "Binary indicator of whether the mentioned location is likely a region of Pennsylvania.",
                  "Return 0 if the location is not a region and 1 if the location is a region."
                )
              )
            ),
            
            #### location extent (full/partial) ----
            Extent = ellmer::type_integer(
              description = paste(
                "Indicator of whether the commenter mentions the full extent of the location",
                "(i.e. 'Bucks County') or a portion of the location (i.e. 'Lower Montgomery County').",
                "Return 1 for a full location mention and 0 for a partial location mention."
              )
            ),
            
            #### location description ----
            Description = ellmer::type_string(
              description = paste(
                "Portions or subareas of the mentioned location, if applicable.",
                "If the commenter refers to the location in its entirety, return 'NA'.",
                "Subareas include cardinal direction specifications (i.e. 'North Philadelphia'),",
                "relative location references (i.e. 'Outskirts of Pittsburgh'),",
                "or colloquial references (i.e. 'Downtown Erie').",
                "Keep descriptions brief."
              )
            ),
            
            #### location counties ----
            SurroundingCounties = ellmer::type_array(
              items = ellmer::type_string(),
              description = paste(
                "An estimate of the Pennsylvania counties most closely corresponding to the location",
                "that this Pennsylvania commenter mentions.",
                "Format county names like the following examples:",
                "Centre County, Delaware County, Beaver County"
              )
            ),
            
            #### location group ----
            Group = ellmer::type_number(
              description = paste(
                "This Pennsylvania commenter is requesting that certain locations",
                "be grouped into various communities of interest.",
                "Return the group number of the location according to the commenter's specifications,",
                "starting with 1 for the first set of associated locations."
              )
            )
          )
        ),
        
        ### comment sentiment ----
        Sentiment = ellmer::type_number(
          description = paste0(
            "Positive/Negative sentiment of this Pennsylvania commenter's description ",
            "of a community of interest scaled from 0 to 1. ",
            "A score of 0 indicates completely negative emotional sentiment, ",
            "while a score of 1 indicates completely positive emotional sentiment."
          )
        ),
        
        ### comment clarity ----
        Clarity = ellmer::type_number(
          description = paste(
            "Clarity of this Pennsylvania commenter's description",
            "of a community of interest scaled from 0 to 1.",
            "Clarity should be based on how specific the commenter's mentioned",
            "locations are and how clearly they state their desire to include or",
            "exclude those locations from a community of interest.",
            "A score of 0 indicates no clarity regarding specific locations or communities,",
            "while a score of 1 indicates complete clarity."
          )
        )
      )
    )
    
    ## return comment information ----
    return(chatOutput)
  })
)

# extract essay errors ----
essayDataErrors <- essayData |>
  purrr::map(.f = \(essayInfo) essayInfo$error) |>
  purrr::list_c()
cli::cli_inform(message = c(">" = paste0("Erroneous Essay Count: ", length(essayDataErrors))))

# extract valid results ----
essayData <- essayData |> purrr::map(.f = \(essayInfo) essayInfo$result)

# simplify location administrative levels ----
essayData <- essayData |>
  purrr::map(
    .f = \(essayInfo) {
      
      ## assigned locations mentioned ----
      LocationsMentioned <- essayInfo$LocationsMentioned
      
      ## find designated administrative level ----
      if (nrow(LocationsMentioned) > 0) {
        adminLevels <- 1:nrow(LocationsMentioned) |>
          purrr::map(
            .f = \(locationID) {
              adminLevel <- LocationsMentioned$AdminLevel |>
                dplyr::slice(locationID) |>
                dplyr::select(dplyr::where(~any(.x == 1))) |>
                names()
              if (length(adminLevel) == 0) adminLevel <- "other"
              return(adminLevel)
            }
          )
      } else {
        adminLevels <- list()
      }
      
      ## format designated administrative level ----
      LocationsMentioned$AdminLevel <- adminLevels
      
      ## update comment information ----
      essayInfo$LocationsMentioned <- LocationsMentioned
      
      ## return comment information ----
      return(essayInfo)
    }
  )

# save essay data ----
saveRDS(object = essayData, file = file.path(dataPath, essayDataFilename))
