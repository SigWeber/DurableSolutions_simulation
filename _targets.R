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
  # tar_target(DS_Option4b, simulate_hclust(data)),
  tar_target(DS_Option5, simulate_classifier(data)),
  tar_target(DS_Option6, simulate_pairwise(data)),
  tar_target(DS_Option7, simulate_volumetric(data)),

  # And again without the HLP indicators
  tar_target(data_nohlp, select(data, -starts_with("I9"))),
  tar_target(DS_Original_nohlp, simulate_IRIS_metric(data_nohlp)),
  tar_target(DS_Option1_nohlp, simulate_composite(data_nohlp)),
  tar_target(DS_Option2_nohlp, simulate_criterion(data_nohlp)),
  tar_target(DS_Option3_nohlp, simulate_subcriterion(data_nohlp)),
  tar_target(DS_Option4_nohlp, simulate_cells(data_nohlp)),
  tar_target(DS_Option5_nohlp, simulate_classifier(data_nohlp)),
  tar_target(DS_Option6_nohlp, simulate_pairwise(data_nohlp)),
  tar_target(DS_Option7_nohlp, simulate_volumetric(data_nohlp)),

  # Missing data
  tar_target(data_missing, mutate(data, across(matches("^I\\d+"), replace_na, 1))),
  tar_target(DS_Original_missing, simulate_IRIS_metric(data_missing)),
  tar_target(DS_Option1_missing, simulate_composite(data_missing)),
  tar_target(DS_Option2_missing, simulate_criterion(data_missing)),
  tar_target(DS_Option3_missing, simulate_subcriterion(data_missing)),
  tar_target(DS_Option4_missing, simulate_cells(data_missing)),
  tar_target(DS_Option5_missing, simulate_classifier(data_missing)),
  tar_target(DS_Option6_missing, simulate_pairwise(data_missing)),
  tar_target(DS_Option7_missing, simulate_volumetric(data_missing)),
  
  # Welfare comparison
  tar_target(data_welfare, 
             bind_rows(data |> filter(ID == 0) |> select(-starts_with("HH_")),
                       data |> filter(ID == 0, !is.na(PERCAPITA)) |> select(-starts_with("HH_")) |> 
                         mutate(ID = 1, 
                                HH_WELFARE = ntile(PERCAPITA, 5), HH_placeholder1 = 1, HH_placeholder2 = 2))),
  tar_target(DS_Option2_welfare, simulate_criterion(data_welfare)),
  tar_target(DS_Option3_welfare, simulate_subcriterion(data_welfare)),
  tar_target(DS_Option4_welfare, simulate_cells(data_welfare))
)
