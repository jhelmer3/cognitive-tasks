
# thank you https://labjs.readthedocs.io/en/latest/learn/deploy/3c-jatos.html

library(tidyverse)

zip_filename <- "jatos_results_20260609192225"
folder_filename <- zip_filename |> 
  str_extract("jatos_results_2026\\d{4}")

# here::here("Jatos", "jatos_results_data_20260430172842.txt") |>
#   rio::import(which = jsonlite::fromJSON(),
#               flatten = T) |> View()

?jsonlite::fromJSON

unzip(here::here("Jatos", glue::glue("{zip_filename}.zip")),
      exdir = here::here("Jatos", folder_filename))

#map_chr(id, \(i) glue::glue("study_result_{i}"))

metadata <- here::here("Jatos", folder_filename, "metadata.json") |>
  jsonlite::fromJSON() |>
  pluck("data") |>
  pluck("studyResults") |>
  pluck(1) |>
  as_tibble()

ids <- pull(metadata, "id")

id <- 261

dat <- map(ids, \(id) 
    here::here("Jatos", folder_filename, 
                          glue::glue("study_result_{id}"),
                          glue::glue("comp-result_{id}"),
                          "data.txt") |>
      read_file() |> 
      str_split("\n") |>
      first() |>
      discard(\(line) line == "") |>
      map(\(line) jsonlite::fromJSON(line) |>
            list_flatten(is_node = is.list) |>
            as_tibble() |>
            nest(data = starts_with("data")) |>
            mutate(id = id,
                   .before = everything())) |>
      list_rbind() |> 
      unnest(data)) |>
  list_rbind()

here::here("Jatos", folder_filename, 
           glue::glue("study_result_{id}"),
           glue::glue("comp-result_{id}"),
           "data.txt") |>
  read_file() |> 
  str_split("\n") |>
  first() |>
  discard(\(line) line == "") |>
  map(\(line) jsonlite::fromJSON(line) |>
        list_flatten(is_node = is.list) |>
        as_tibble() |>
        nest(data = starts_with("data"))) 

# here::here("Jatos", folder_filename, 
#            glue::glue("study_result_{id}"),
#            glue::glue("comp-result_{id}"),
#            "data.txt") |>
#   read_file() |> 
#   str_split("\n") |>
#   first() |>
#   discard(\(line) line == "") |>
#   map(\(line) jsonlite::fromJSON(line) |>
#         list_flatten(is_node = is.list) |>
#         as_tibble() |>
#         nest(data = starts_with("data"))) |>
#   list_rbind() |> 
#   unnest(data)

