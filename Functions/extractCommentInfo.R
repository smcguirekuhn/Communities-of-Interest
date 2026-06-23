extractCommentInfo <- function(
    description,
    state,
    adminLevels = c(
      "Neighborhood",
      "Township",
      "Borough",
      "Town",
      "City",
      "SchoolDistrict",
      "County",
      "Region"
    ),
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.character(description))
  stopifnot(state %in% state.name)
  match.arg(adminLevels, several.ok = TRUE)
  stopifnot(is.character(model))
  
  # assign administrative level arguments ----
  adminLevelArguments <- adminLevels |>
    purrr::map(
      .f = \(adminLevel) {
        adminLevel <- adminLevel |> snakecase::to_title_case() |> stringr::str_to_lower()
        adminLevelArgument <- ellmer::type_integer(
          description = glue::glue(
            "Binary indicator of whether the mentioned location is likely a {adminLevel}.",
            "Return 0 if the location is not a {adminLevel} and 1 if the location is a {adminLevel}."
          )
        )
      }
    ) |>
    purrr::set_names(nm = adminLevels) |>
    append(
      values = list(
        .description = glue::glue(
          "Administrative level of the location.",
          "Must be exclusive and decisive."
        )
      ),
      after = 0
    )
  
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
          
          ### location administrative level -----
          AdminLevel = do.call(what = ellmer::type_object, args = adminLevelArguments),
          
          ### location extent (full/partial) ----
          Extent = ellmer::type_integer(
            description = glue::glue(
              "Indicator of whether the commenter mentions the full extent of the location",
              "(i.e. 'Washington County') or a portion of the location (i.e. 'Lower Jackson County').",
              "Return 1 for a full location mention and 0 for a partial location mention."
            )
          ),
          
          ### location description ----
          Description = ellmer::type_string(
            description = glue::glue(
              "Portions or subareas of the mentioned location, if applicable.",
              "If the commenter refers to the location in its entirety, return 'NA'.",
              "Subareas include cardinal direction specifications (i.e. 'North Springfield'),",
              "relative location references (i.e. 'Outskirts of Springfield'),",
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
          
          ### location group ----
          Group = ellmer::type_number(
            description = glue::glue(
              "This {state} commenter is requesting that their mentioned locations",
              "be kept together or separated into various community of interest groups.",
              "Return the group number of the location according to the commenter's specifications,",
              "starting with 1 for the first set of associated locations.",
              "Locations the commenter requests be separated should be assigned to different groups."
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