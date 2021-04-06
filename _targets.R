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
  tar_target(DS_Option4b, simulate_hclust(data)),
  # tar_target(DS_Option4b_complete1, simulate_hclust(data, method = "complete", maxdiff = 1)),
  # tar_target(DS_Option4b_complete2, simulate_hclust(data, method = "complete", maxdiff = 2)),
  # tar_target(DS_Option4b_average1, simulate_hclust(data, method = "average", maxdiff = 1)),
  # tar_target(DS_Option4b_average2, simulate_hclust(data, method = "average", maxdiff = 2)),
  tar_target(DS_Option5, simulate_classifier(data)),
 # tar_target(DS_Option5b, simulate_lasso(data)),
  
  # And again without the HLP indicators
  tar_target(data_nohlp, select(data, -starts_with("I9"))),
  tar_target(DS_Original_nohlp, simulate_IRIS_metric(data_nohlp)),
  tar_target(DS_Option1_nohlp, simulate_composite(data_nohlp)),
  tar_target(DS_Option2_nohlp, simulate_criterion(data_nohlp)),
  tar_target(DS_Option3_nohlp, simulate_subcriterion(data_nohlp)),
  tar_target(DS_Option4_nohlp, simulate_cells(data_nohlp)),
  tar_target(DS_Option4b_nohlp, simulate_hclust(data_nohlp)),
  # tar_target(DS_Option4b_nohlp_complete1, simulate_hclust(data_nohlp, method = "complete", maxdiff = 1)),
  # tar_target(DS_Option4b_nohlp_complete2, simulate_hclust(data_nohlp, method = "complete", maxdiff = 2)),
  # tar_target(DS_Option4b_nohlp_average1, simulate_hclust(data_nohlp, method = "average", maxdiff = 1)),
  # tar_target(DS_Option4b_nohlp_average2, simulate_hclust(data_nohlp, method = "average", maxdiff = 2)),
  tar_target(DS_Option5_nohlp, simulate_classifier(data_nohlp)),
  # tar_target(DS_Option5b_nohlp, simulate_lasso(data_nohlp))
  
  # Role reversal
  tar_target(data_nohlp_host, mutate(data_nohlp, ID = 1-ID)),
  tar_target(DS_Original_nohlp_host, simulate_IRIS_metric(data_nohlp_host)),
  tar_target(DS_Option1_nohlp_host, simulate_composite(data_nohlp_host)),
  tar_target(DS_Option2_nohlp_host, simulate_criterion(data_nohlp_host)),
  tar_target(DS_Option3_nohlp_host, simulate_subcriterion(data_nohlp_host)),
  tar_target(DS_Option4_nohlp_host, simulate_cells(data_nohlp_host)),
  tar_target(DS_Option4b_nohlp_host, simulate_hclust(data_nohlp_host)),
  # tar_target(DS_Option4b_nohlp_complete1_host, simulate_hclust(data_nohlp_host, method = "complete", maxdiff = 1)),
  # tar_target(DS_Option4b_nohlp_complete2_host, simulate_hclust(data_nohlp_host, method = "complete", maxdiff = 2)),
  # tar_target(DS_Option4b_nohlp_average1_host, simulate_hclust(data_nohlp_host, method = "average", maxdiff = 1)),
  # tar_target(DS_Option4b_nohlp_average2_host, simulate_hclust(data_nohlp_host, method = "average", maxdiff = 2)),
  tar_target(DS_Option5_nohlp_host, simulate_classifier(data_nohlp_host))#,
  # tar_target(DS_Option5b_nohlp_host, simulate_lasso(data_nohlp_host))
)
