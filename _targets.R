
library(targets)
library(tarchetypes) 

tar_option_set(
  packages = c("tidyverse"),
  format = "qs"
)

tar_source()

zip_filename <- "jatos_results_20260609223410"

# Replace the target list below with your own:
list(
  tar_target(unzipped_files, unzip_zip(zip_filename))
)
