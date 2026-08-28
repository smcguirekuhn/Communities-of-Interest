extractCommentInfo <- function(
    prompts,
    localContext,
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
      "State House District",
      "State Senate District",
      "Congressional District",
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
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.list(prompts))
  stopifnot(is.character(localContext))
  match.arg(adminLevels, several.ok = TRUE)
  match.arg(cardinalDirections, several.ok = TRUE)
  stopifnot(is.character(model))
  
  # extract comment information ----
  commentInfo <- ellmer::parallel_chat_structured(
    chat = ellmer::chat_openrouter(model = model),
    prompts = prompts,
    type = ellmer::type_object(
      
      ## comment sentiment ----
      Sentiment = ellmer::type_number(
        description = ellmer::interpolate(
          "Positive/Negative sentiment of this commenter's description
          of a community of interest scaled from 0 to 1.
          A score of 0 indicates completely negative emotional sentiment,
          while a score of 1 indicates completely positive emotional sentiment."
        )
      ),
      
      ## comment clarity ----
      Clarity = ellmer::type_number(
        description = ellmer::interpolate(
          "Clarity of this commenter's description
          of a community of interest scaled from 0 to 1.
          Clarity should be based on how specific the commenter's mentioned
          locations are and how clearly they group or separate mentioned locations
          in a way that can be interpreted by legislative boundary drawers.
          A score of 0 indicates no clarity regarding specific locations or communities
          (i.e. 'Please don't split cities or counties' or 'No redistricting'),
          while a score of 1 indicates complete clarity
          (i.e. 'Springfield should not be included in a district with Washington County')."
        )
      ),
      
      ## locations included in the community of interest ----
      LocationsMentioned = ellmer::type_array(
        description = ellmer::interpolate(
          "All individual geographic locations that this commenter from {{localContext}} mentions,
          including landmarks, neighborhoods, townships, boroughs, towns, cities, school districts, counties, 
          legislative districts, and regions. Only return clearly-identified locations in the commenter's 
          home state relevant to the commenter's community of interest. 
          Order locations by their appearance in the comment.",
          localContext = localContext
        ),
        items = ellmer::type_object(
          
          ### location name ----
          Name = ellmer::type_string(
            description = ellmer::interpolate(
              "The identifiable, administrative name of the location.
              Unless the location is a region, do not include any 
              cardinal direction subareas in the name
              (i.e. return 'Washington County' if the comment mentions 'Northern Washington County').
              Location names should thus read like the following examples:
              'Green Lake' (a landmark),
              'Downtown Springfield' (a neighborhood),
              'Franklin' (a municipality),
              'Springfield School District' (a school district),
              'Washington County' (a county),
              'Congressional District 3' (a congressional district),
              'State House District 101' (a state house district),
              'State Senate District 50' (a state senate district),
              'Springfield Metro Area' (a region), and
              'Northern California' (a region)."
            )
          ),
          
          ### location type -----
          AdminLevel = ellmer::type_enum(
            values = adminLevels,
            description = ellmer::interpolate(
              "Best estimate of the type or administrative level of the location. Must be exclusive and decisive.
              If the location's administrative level is unclear, return 'NA'."
            )
          ),
          
          ### cardinal direction subarea extent ----
          CardinalDirectionSubarea = ellmer::type_enum(
            values = cardinalDirections,
            description = ellmer::interpolate(
              "Cardinal Direction subareas of the location, if any are mentioned by the commenter.
              For example, if 'Northern Washington County' is mentioned, return 'Northern'.
              If the commenter refers to the entirety of the location (i.e. 'Washington County'), return 'NA'.
              Return 'NA' if the cardinal direction subarea is implied in a region name
              (i.e. 'Northern California)."
            )
          ),
          
          ### location description ----
          AdditionalDescription = ellmer::type_string(
            description = ellmer::interpolate(
              "Vernacular portions or subareas of the mentioned location, if applicable.
              If the commenter refers to the location in its entirety,
              or if they refer to a cardinal direction subarea of the location, return 'NA'.
              Subareas include relative location references (i.e. 'Outskirts of Springfield'),
              or colloquial references (i.e. 'Downtown Springfield').
              Keep descriptions brief. Return 'NA' unless the description
              corresponds to a codifiable geographic area.
              Examples include 'Outskirts', 'Downtown', 'Rural Areas', and 'Unincorporated'."
            )
          )
        )
      )
    )
  )
  
  # return comment information ----
  return(commentInfo)
}