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
    districtTypes = c(
      "State House",
      "State Senate",
      "Congressional",
      "NA"
    ),
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.list(prompts))
  stopifnot(is.character(localContext))
  match.arg(adminLevels, several.ok = TRUE)
  match.arg(cardinalDirections, several.ok = TRUE)
  match.arg(districtTypes, several.ok = TRUE)
  stopifnot(is.character(model))
  
  # extract comment information ----
  commentInfo <- ellmer::parallel_chat_structured(
    chat = ellmer::chat_openrouter(model = model),
    prompts = prompts,
    type = ellmer::type_object(
      
      ## locations included in the community of interest ----
      LocationsMentioned = ellmer::type_array(
        description = ellmer::interpolate(
          "All individual geographic locations that this commenter from {{localContext}} mentions,
          including landmarks, neighborhoods, townships, boroughs, school districts, counties, etc..
          Only return clearly-identified locations in the commenter's state relevant to the commenter's 
          community of interest. Order locations by their appearance in the comment.",
          localContext = localContext
        ),
        items = ellmer::type_object(
          
          ### location name ----
          Name = ellmer::type_string(
            description = ellmer::interpolate(
              "The identifiable, administrative name of the location.
              For example, if 'western suburbs of the city of Springfield' is mentioned,
              return only 'City of Springfield'.
              The location should thus read like the following examples:
              Midtown, Franklin, Washington County."
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
              If the commenter refers to the entirety of the location (i.e. 'Washington County'), return 'NA'."
            )
          ),
          
          ### location description ----
          Description = ellmer::type_string(
            description = ellmer::interpolate(
              "Vernacular portions or subareas of the mentioned location, if applicable.
              If the commenter refers to the location in its entirety,
              or if they refer to a cardinal direction subarea of the location, return 'NA'.
              Subareas include relative location references (i.e. 'Outskirts of Springfield'),
              or colloquial references (i.e. 'Downtown Springfield').
              Keep descriptions brief."
            )
          ),
          
          ### district types ----
          DistrictTypes = ellmer::type_array(
            items = type_enum(values = districtTypes),
            description = ellmer::interpolate(
              "List the types of legislative districts likely being discussed by this commenter
              when mentioning the location in their redistricting specifications.
              Return 'NA' unless it is very clear that a specific type of district is being discussed."
            )
          ),
          
          ### locations to group ----
          LocationsToGroup = ellmer::type_array(
            description = ellmer::interpolate(
              "Based on communities of interest, this commenter is requesting that the
              locations they mention either be kept together or separated into various legislative districts.
              Return the names of any locations the commenter is asking to be fused with this location in their ideal district map.
              If this location is the only location mentioned in the comment, return 'NA'.
              Interpret the commenter's request, complaint, or concern, not the state of the map they are referring to.
              The following are examples of when locations should be grouped in a community of interest:
              'Vote no on the proposed map, which separates Springfield from Washington County.'
              'Springfield and Washington County have been separated. This is a clear effort to Gerrymander our community.'
              'Springfield and Washington County share similar economic interests.'
              'Why is Springfield in a different district than Washington County?',
              'Separating Springfield from Washington County is ridiculous',
              'Please Keep Springfield together with Washington County.',
              'Don't split Springfield from Washington County.', and
              'Springfield should be reunited with Washington County.'.
              In these cases, if 'Springfield' is the mentioned location, 'Washington County' would be returned.
              Return only the identifiable, administrative name of the location, not cardinal direction 
              subareas or other descriptions (i.e. return 'Washington County' not 'Northern Washington County'."
            ),
            items = ellmer::type_string()
          ),
          
          ### locations to separate ----
          LocationsToSeparate = ellmer::type_array(
            description = ellmer::interpolate(
              "Based on communities of interest, this commenter is requesting that the
              locations they mention either be kept together or separated into various legislative districts.
              Return the names of any locations the commenter is asking to be separated this location in their ideal district map.
              If this location is the only location mentioned in the comment, return 'NA'.
              Interpret the commenter's request, complaint, or concern, not the state of the map they are referring to.
              The following are examples of when locations should be separated in a community of interest:
              'Vote no on the proposed map, which combines Springfield with Washington County.'
              'Springfield and Washington County have been combined. This is a clear effort to Gerrymander our community.'
              'Springfield and Washington County do not share similar economic interests.'
              'Why is Springfield in the same district as Washington County?',
              'Combining Springfield and Washington County is ridiculous',
              'Please Keep Springfield separated from Washington County.',
              'Don't group Springfield with Washington County.', and
              'Springfield should be kept apart from Washington County.'.
              In these cases, if 'Springfield' is the mentioned location, 'Washington County' would be returned.
              Return only the identifiable, administrative name of the location, not cardinal direction 
              subareas or other descriptions (i.e. return 'Washington County' not 'Northern Washington County'."
            ),
            items = ellmer::type_string()
          )
        )
      ),
      
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
      )
    )
  )
  
  # return comment information ----
  return(commentInfo)
}