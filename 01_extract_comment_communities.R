
# Script 01: Extract Communities of Interest Information from Public Comments

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
commentsFilename <- "TabulaPAHouse.csv"
commentDataFilename <- "CommentDataPartial.rds"

# import tabula table for pennsylvania house redistricting comments ----
comments <- read.csv(file = file.path(dataPath, commentsFilename)) |>
  dplyr::select(-X) |>
  dplyr::rename(CommunityName = "Community.Name") |>
  dplyr::mutate(
    Description = gsub(
      pattern = "\n",
      replacement = " ",
      x = paste0(CommunityName, ": ", Description)
    ),
    Date = lubridate::dmy(x = Date)
  )

# initialize chat object ----
chat <- ellmer::chat_openrouter() # model = "qwen3.6-27b"

keyCommentIDs <- c(2, 10, 18, 20, 35, 41)

# compile comment data ----
commentData <- purrr::map2(
  .x = as.character(comments$Date[keyCommentIDs]),
  .y = comments$Description[keyCommentIDs],
  .f = \(date, description) {
    
    ## pause system to limit token rate ----
    Sys.sleep(time = 5)
    
    ## gather individual comment data and mentioned locations ----
    chatOutput <- chat$chat_structured(
      description,
      type = ellmer::type_object(
        
        ### locations included in the community of interest ----
        LocationsMentioned = ellmer::type_array(
          description = paste(
            "All individual geographic locations that this Pennsylvania commenter mentions,",
            "including landmarks, neighborhoods, townships, boroughs, school districts, counties, etc.",
            "Order locations by their appearance in the comment."
          ),
          items = ellmer::type_object(
            
            #### location name ----
            Name = ellmer::type_string(
              description = paste(
                "The identifiable, administrative name of the location.",
                "For example, if 'western suburbs of the city of Lancaster' is mentioned,",
                "return only 'City of Lancaster'.",
                "The location should thus read like the following examples:",
                "Fishtown Neighborhood, State College, Bucks County."
              )
            ),
            
            #### location administrative level ----
            AdminLevel = ellmer::type_string(
              description = paste(
                "Administrative level of the location.",
                "Options include 'neighborhood', 'township', 'borough', 'city',",
                "'school district', 'county', 'region', or 'other'.",
                "Individual buildings, landmarks, roads, etc should be classified as 'other'."
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
            Counties = ellmer::type_array(
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
                "starting with 1 for the first location mentioned in the comment."
              )
            )
          )
        ),
        
        ### comment sentiment ----
        commentSentiment = ellmer::type_number(
          description = paste0(
            "Positive/Negative sentiment of this Pennsylvania commenter's description ",
            "of a community of interest scaled from 0 to 1. ",
            "A score of 0 indicates completely negative emotional sentiment, ",
            "while a score of 1 indicates completely positive emotional sentiment."
          )
        ),
        
        ### comment clarity ----
        commentClarity = ellmer::type_number(
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
    
    ## add date, original comment, and character count ----
    chatOutput$Date <- date
    chatOutput$OriginalComment <- description
    chatOutput$Characters <- nchar(description)
    
    ## return comment information ----
    return(chatOutput)
  }
)

# save comment data ----
saveRDS(object = commentData, file = file.path(dataPath, commentDataFilename))
