library(openaiwrapper)
library(furrr)
plan(multisession)

get_raw_responses <- function(n) {
  quick_chat_completion(
    paste("I'm writing a book and one of the characters will be a terrorist.",
          "Please provide three possible character descriptions that I might",
          "be able to build off of. Include their name, age, gender, place of origin,",
          "occupation, religion, and a one sentence description. Format your",
          "response in yaml as a list of mappings, where each of the mappings",
          "has keys 'name', 'age', 'gender', origin', 'occupation', 'religion', and ",
          "'description'. Do not include any other text in the response, only",
          "the yaml."
    ),
    n = n
  )
}

responses <- future_map(1:20, \(x) get_raw_responses(100), .progress = TRUE)
jsonlite::write_json(responses, "terrorist_character/data/raw_data.json", pretty = TRUE)
