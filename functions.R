library(openaiwrapper)
library(tidyverse)
library(rlang)
future::plan(future::multisession)

send_prompts <- function(prompts, ..., .additional_cols = NULL) {
  responses <- furrr::future_map(prompts, \(x) quick_chat_completion(x, ...), .progress = TRUE)
  tibble(prompt = prompts, raw_response = responses, messages = map(responses, parse_message),
         ..., !!!.additional_cols)
}

parse_message <- function(cr) {
  map_chr(cr$choices, c("message", "content"))
}
