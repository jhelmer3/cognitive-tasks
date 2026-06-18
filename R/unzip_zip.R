
unzip_zip <- function(zip_filename) {
  folder_filename <- str_extract(zip_filename, "jatos_results_2026\\d{4}")
  unzip(here::here("Jatos", glue::glue("{zip_filename}.zip")),
        exdir = here::here("Jatos", folder_filename))
  list.files(out_dir, full.names = TRUE)  # return the paths
}