
extractCommentRequests <- function(
    comment,
    locationNames,
    state,
    model = "mistral-large-2512"
  ) {
  
  # check arguments ----
  stopifnot(is.character(comment))
  stopifnot(all(is.character(locationNames)))
  stopifnot(state %in% state.name)
  stopifnot(is.character(model))

  # initialize chat object ----
  chat <- ellmer::chat_mistral(model = model)

  # extract comment location relationships ----
  requests <- chat$chat_structured(
    comment,
    type = ellmer::type_object(
      .description = ellmer::interpolate(
        "This commenter is providing public input about a legislative redistricting cycle, 
        requesting that some geographic locations they mention either be grouped together,
        separated from each other, or kept whole among various districts based on their 
        communities of interest. Interpret the commenter's request, complaint, or concern.
        Do not treat the composition of an existing or proposed district as a desired relationship 
        unless the commenter explicitly endorses or rejects that composition.
        Do NOT return requests featuring the following types of locations:\n\n
        
        - Locations outside the commenter's state of {{state}}\n
        - Proposed legislative districts discussed by the commenter (existing districts are permitted)\n
        - Roads, including interstates\n
        - Exceedingly vague locations, such as 'The Mountains', 'Proposed State House District', or 'Rural Towns'",
        state = state
      ),
      PairwiseGroups = ellmer::type_array(
        description = ellmer::interpolate(
          "Return pairs of locations that the commenter explicitly asks be grouped during redistricting.
          The following are examples of a grouped pairwise relationship between fictional
          'Springfield' and 'Washington County':\n
          - 'Vote no on the proposed map, which separates Springfield from Washington County.'\n
          - 'Springfield and Washington County have been separated. This is a clear effort to Gerrymander our community.'\n
          - 'Springfield and Washington County share similar economic interests.'\n
          - 'Why is Springfield in a different district than Washington County?'\n
          - 'Separating Springfield from Washington County is ridiculous.'\n
          - 'Please Keep Springfield together with Washington County.'\n
          - 'Don't split Springfield from Washington County.'\n
          - 'Springfield should be reunited with Washington County.'\n
          - 'Our community includes Washington County as well as Springfield.'"
        ),
        items = ellmer::type_object(
          Location1 = ellmer::type_enum(values = locationNames),
          Location2 = ellmer::type_enum(values = locationNames)
        )
      ),
      PairwiseSeparations = ellmer::type_array(
        description = ellmer::interpolate(
          "Return pairs of locations that the commenter explicitly asks be separated during redistricting.
          The following are examples of a separated pairwise relationship between fictional
          'Springfield' and 'Washington County':\n
          - 'Vote no on the proposed map, which combines Springfield with Washington County.'\n
          - 'Springfield and Washington County have been combined. This is a clear effort to Gerrymander our community.'\n
          - 'Springfield and Washington County do not share similar economic interests.'\n
          - 'Why is Springfield in the same district as Washington County?'\n
          - 'Combining Springfield and Washington County is ridiculous.'\n
          - 'Please Keep Springfield separated from Washington County.'\n
          - 'Don't group Springfield with Washington County.'\n
          - 'Springfield should be kept apart from Washington County.'\n
          - 'Our community of Springfield is nothing like Washington County.'"
        ),
        items = ellmer::type_object(
          Location1 = ellmer::type_enum(values = locationNames),
          Location2 = ellmer::type_enum(values = locationNames)
        )
      ),
      LocationsKeptWhole = ellmer::type_array(
        description = ellmer::interpolate(
          "Return all locations that the commenter explicitly requests be kept whole.
          The following are examples of a request that the fictional city of 'Springfield' be kept whole:\n
          - 'Vote no on the proposed map, which breaks up Springfield into two different districts.'\n
          - 'Springfield has been separated into multiple districts. This is a clear effort to Gerrymander our community.'\n
          - 'Springfield is a tight-knit community of shared interests.'\n
          - 'Why is Springfield cracked into several different districts?'\n
          - 'Breaking up Springfield into many different districts is ridiculous.'\n
          - 'Please keep Springfield whole in a single district.'\n
          - 'Springfield has enough population for 2 representatives.'\n
          - 'Springfield should have its own representation.'"
        ),
        items = ellmer::type_enum(values = locationNames)
      )
    )
  )

  # return comment location relationships ----
  return(requests)
}
