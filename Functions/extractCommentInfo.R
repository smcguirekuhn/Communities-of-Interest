extractCommentInfo <- function(
    description,
    state,
    adminLevels = c(
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
    cardinalDirections = c(
      "Northern",
      "Northeastern",
      "Eastern",
      "Southeastern",
      "Southern",
      "Southwestern",
      "Western",
      "Northwestern",
      "Central",
      "NA"
    ),
    districtTypes = c(
      "State House",
      "State Senate",
      "Congressional",
      "NA"
    ),
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.character(description))
  stopifnot(state %in% state.name)
  match.arg(adminLevels, several.ok = TRUE)
  match.arg(cardinalDirections, several.ok = TRUE)
  match.arg(districtTypes, several.ok = TRUE)
  stopifnot(is.character(model))
  
  ## initialize chat object ----
  chat <- ellmer::chat_openrouter(model = model)
  
  # extract comment information ----
  commentInfo <- chat$chat_structured(
    description,
    type = ellmer::type_object(
      
      ## locations included in the community of interest ----
      LocationsMentioned = ellmer::type_array(
        description = glue::glue(
          "All individual geographic locations that this {state} commenter mentions,",
          "including landmarks, neighborhoods, townships, boroughs, school districts, counties, etc.",
          "Order locations by their appearance in the comment.",
          "Only return clearly-identified locations."
        ),
        items = ellmer::type_object(
          
          ### location name ----
          Name = ellmer::type_string(
            description = glue::glue(
              "The identifiable, administrative name of the location.",
              "For example, if 'western suburbs of the city of Springfield' is mentioned,",
              "return only 'City of Springfield'.",
              "The location should thus read like the following examples:",
              "Midtown, Franklin, Washington County."
            )
          ),
          
          ### location type -----
          AdminLevel = ellmer::type_enum(
            values = adminLevels,
            description = glue::glue(
              "Best estimate of the type or administrative level of the location.",
              "Must be exclusive and decisive.",
              "If the location's administrative level is unclear, return 'NA'."
            )
          ),
          
          ### cardinal direction subarea extent ----
          CardinalDirectionSubarea = ellmer::type_enum(
            values = cardinalDirections,
            description = glue::glue(
              "Cardinal Direction subareas of the location, if any are mentioned by the commenter.",
              "For example, if 'Northern Washington County' is mentioned, return 'Northern'.",
              "If the commenter refers to the entirety of the location (i.e. 'Washington County'),",
              "return 'NA'."
            )
          ),
          
          ### location description ----
          Description = ellmer::type_string(
            description = glue::glue(
              "Vernacular portions or subareas of the mentioned location, if applicable.",
              "If the commenter refers to the location in its entirety,",
              "or if they refer to a cardinal direction subarea of the location, return 'NA'.",
              "Subareas include relative location references (i.e. 'Outskirts of Springfield'),",
              "or colloquial references (i.e. 'Downtown Springfield').",
              "Keep descriptions brief."
            )
          ),
          
          ### location counties ----
          SurroundingCounties = ellmer::type_array(
            items = ellmer::type_string(),
            description = glue::glue(
              "An estimate of the {state} counties most closely corresponding to the location",
              "that this {state} commenter mentions.",
              "Format county names like the following examples:",
              "Washington County, Jackson County, Lincoln County"
            )
          ),
          
          ### district types ----
          DistrictTypes = ellmer::type_array(
            items = type_enum(values = districtTypes),
            description = glue::glue(
              "List the types of legislative districts likely being discussed by this {state} commenter",
              "when mentioning the location in their redistricting specifications.",
              "Return 'NA' unless it is very clear that a specific type of district is being discussed."
            )
          ),
          
          ### location group ----
          Group = ellmer::type_number(
            description = glue::glue(
              "Based on communities of interest, this {state} commenter is requesting that the",
              "locations they mention either be kept together or separated into various legislative districts.",
              "Return a group number of the location according to the commenter's",
              "desired redistricting outcome, starting with 1 for the first set of associated locations.",
              "Locations that the commenter suggests be separated should be assigned to different groups",
              "(i.e. 'Springfield should not be in the same district as Washington County'),",
              "as should locations that the commenter expresses frustration about currently being together",
              "(i.e. 'Grouping Springfield and Washington County is ridiculous').",
              "Scrutinize location group number assignments closely."
            )
          )
        )
      ),
      
      ## comment sentiment ----
      Sentiment = ellmer::type_number(
        description = glue::glue(
          "Positive/Negative sentiment of this {state} commenter's description ",
          "of a community of interest scaled from 0 to 1. ",
          "A score of 0 indicates completely negative emotional sentiment, ",
          "while a score of 1 indicates completely positive emotional sentiment."
        )
      ),
      
      ## comment clarity ----
      Clarity = ellmer::type_number(
        description = glue::glue(
          "Clarity of this {state} commenter's description",
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
  
  # return comment information ----
  return(commentInfo)
}