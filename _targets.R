library(targets)
library(tarchetypes)

library(tidyverse)

source("simulations.R")

# Set target-specific options such as packages.
tar_option_set(packages = "tidyverse")

# analysis pipeline
tar_map(
  values = tibble(scripts = fs::dir_ls("Data", glob = "*.R"),
                  names = fs::path_ext_remove(fs::path_file(scripts))),
  names = "names",
  
  # Load data
  tar_target(script, scripts, format = "file"),
  tar_target(data, callr::r(source, args = list(file = script))$value),
  
  # Run simulations
  tar_target(DS_Original, simulate_IRIS_metric(data)),
  tar_target(DS_Option1, simulate_composite(data)),
  tar_target(DS_Option2, simulate_criterion(data)),
  tar_target(DS_Option3, simulate_subcriterion(data)),
  tar_target(DS_Option4, simulate_cells(data)),
  tar_target(DS_Option5, simulate_classifier(data)),
  
  # And again without the HLP indicators
  tar_target(data_nohlp, select(data, -starts_with("I9"))),
  tar_target(DS_Original_nohlp, simulate_IRIS_metric(data_nohlp)),
  tar_target(DS_Option1_nohlp, simulate_composite(data_nohlp)),
  tar_target(DS_Option2_nohlp, simulate_criterion(data_nohlp)),
  tar_target(DS_Option3_nohlp, simulate_subcriterion(data_nohlp)),
  tar_target(DS_Option4_nohlp, simulate_cells(data_nohlp)),
  tar_target(DS_Option5_nohlp, simulate_classifier(data_nohlp)))
