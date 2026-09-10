
extractCommentRequests <- function(
    comment,
    locationNames,
    districtTypes = c(
      "State House",
      "State Senate",
      "Congressional",
      "NA"
    ),
    model = "mistralai/mistral-large"
  ) {
  
  # check arguments ----
  stopifnot(is.character(comment))
  stopifnot(all(is.character(locationNames)))
  match.arg(districtTypes, several.ok = TRUE)
  stopifnot(is.character(model))

  # initialize chat object ----
  chat <- ellmer::chat_openrouter(model = model)

  # extract comment location relationships ----
  requests <- chat$chat_structured(
    comment,
    type = ellmer::type_array(
      description = ellmer::interpolate(
        "This commenter is providing public input about a legislative redistricting cycle, 
        requesting that the geographic locations they mention either be kept together or separated 
        into various districts based on communities of interest.
        Return any pairwise location relationships requested by the commenter.
        Interpret the commenter's request, complaint, or concern.
        Do not treat the composition of an existing or proposed district as a desired relationship 
        unless the commenter explicitly endorses or rejects that composition.\n\n
        
        The following are examples of a 'grouped' pairwise relationship between 'Springfield' and 'Washington County':\n
        - 'Vote no on the proposed map, which separates Springfield from Washington County.'\n
        - 'Springfield and Washington County have been separated. This is a clear effort to Gerrymander our community.'\n
        - 'Springfield and Washington County share similar economic interests.'\n
        - 'Why is Springfield in a different district than Washington County?'\n
        - 'Separating Springfield from Washington County is ridiculous.'\n
        - 'Please Keep Springfield together with Washington County.'\n
        - 'Don't split Springfield from Washington County.'\n
        - 'Springfield should be reunited with Washington County.'\n\n
        
        The following are examples of a 'separated' pairwise relationship between 'Springfield' and 'Washington County':\n
        - 'Vote no on the proposed map, which combines Springfield with Washington County.'\n
        - 'Springfield and Washington County have been combined. This is a clear effort to Gerrymander our community.'\n
        - 'Springfield and Washington County do not share similar economic interests.'\n
        - 'Why is Springfield in the same district as Washington County?'\n
        - 'Combining Springfield and Washington County is ridiculous.'\n
        - 'Please Keep Springfield separated from Washington County.'\n
        - 'Don't group Springfield with Washington County.'\n
        - 'Springfield should be kept apart from Washington County.'"
      ),
      items = ellmer::type_object(
        Location1 = ellmer::type_enum(values = locationNames),
        Location2 = ellmer::type_enum(values = locationNames),
        Relationship = ellmer::type_enum(
          description = "Return the nature of the requested relationship between locations.",
          values = c("grouped", "separated", "unclear")),
        DistrictType = ellmer::type_enum(
          description = ellmer::interpolate(
            "Return the legislative district type relevant to this pairwise relationship request,
            if mentioned by the commenter. If the relevant district types are unclear or if multiple types 
            are relevant, return 'NA'"
          ),
          values = districtTypes
        ),
        Confidence = ellmer::type_enum(values = c("low", "medium", "high"))
      )
    )
  )

  # return comment location relationships ----
  return(requests)
}
