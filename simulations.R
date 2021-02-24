# preparations ---------------------------------------------------------

# load required packages
library(tidyverse)

# load functions
source("functions.R")

future::plan(future::multisession)

extract_indicators <- function(data) {
  str_c("I", 1:10) %>% set_names() %>% map(~names(select(data, starts_with(paste0(., "_")))))
}

generate_combinations <- function(indicators) {
  expand_grid(!!!indicators)
}

run_simulation <- function(data, metric, ..., combinations = NULL) {
  # divide into IDPs and benchmark
  idps <- data %>% filter(ID == 1)
  host_community <- data %>% filter(ID == 0)
  
  # identify all possible combinations
  if (is.null(combinations))
    combinations <- data %>% extract_indicators() %>% generate_combinations()
  
  # draw 10,000 combinations only - should be enough for all practical purposes
  combinations <- combinations %>% slice_sample(n = 10000)
  
  # number of IDPs exiting the stock according to the chosen metric
  Durable_Solutions <- 
    1:nrow(combinations) %>% 
    furrr::future_map_int(metric, data = idps, benchmark = host_community, ..., sim_data = combinations)
  
  # result for analysis and plotting
  combinations %>% mutate(DS = Durable_Solutions, DS_perc = DS/nrow(idps))
}

# SIMULATIONS ############################################

# Original framework ----------------------------------------------------------
simulate_IRIS_metric <- function(data) {
  run_simulation(data, use_IRIS_metric)
}

# Option 1: Full composite ---------------------------------------------------
simulate_composite <- function(data) {
  run_simulation(data, use_composite)
}

# Option 2: composite indices at the criterion level ###########################################
simulate_criterion <- function(data) {
  run_simulation(data, use_criterion)
}

# Option 3: composite indices at the subcriterion level ###########################################
simulate_subcriterion <- function(data) {
  # aggregate indices for the levels with few indicators available
  tmp <- data %>% select(starts_with("I"), -ID) %>% as.matrix() %>% t() %>% as_tibble(rownames = "var")
  
  tmp <- tmp %>% mutate(ind = str_match(var, "I(\\d+)_")[,2])
  
  indic_cnt <- tmp %>% count(ind)
  
  tmp <- 
    tmp %>% 
    group_by(ind) %>% mutate(grp = if_else(rep(n() <= 3, n()), paste0("I", ind), var)) %>% 
    group_by(grp) %>% summarize(across(where(is.numeric), sum))
  
  tmp <- tmp %>% remove_rownames() %>% column_to_rownames(var = "grp") %>% as.matrix() %>% t() %>% as_tibble()
  
  # recover ID field that was lost during aggregation
  tmp <- tmp %>% mutate(ID = data$ID)
  
  # define the new indicators
  indicators_sub <- 
    indic_cnt %>% filter(n > 3) %>% pull(ind) %>% set_names(~paste0("index", .)) %>%
    map(~apply((combn(names(select(tmp, contains(paste0("I", ., "_")))),3)) %>% t(),1, paste,collapse= ";"))
  
  # define the new combinations
  combinations <- 
    reduce(indic_cnt %>% filter(n > 3) %>% pull(ind),
           ~separate(.x, col = paste0("index", .y), into = str_c("I", .y, "_Index_", 1:3), sep = ";"), 
           .init = expand_grid(!!!indicators_sub)) %>% 
    mutate(!!!(indic_cnt %>% filter(n <= 3) %>% pull(ind) %>% map_chr(~paste0("I", .)) %>% set_names()))
  
  run_simulation(tmp, use_subcriterion, combinations = combinations)
}

# Option 4: Comparison of homogenous cells ###########################################
simulate_cells <- function(data) {
  cells <- 
    apply((combn(names(data %>% select(starts_with("HH_"))),3)) %>% t(),1, paste,collapse= ";") %>% 
    # FIXME: the call to expand.grid() below has no effect. Is this intentional?
    # expand.grid() %>%
    # mutate_all(~as.character(.)) %>%
    as_tibble_col(column_name = "Var1") %>% 
    separate(col = Var1, into = c("cell_1", "cell_2","cell_3"), sep = ";")
  
  DS <- 
    1:nrow(cells) %>% 
    map_dfr(~run_simulation(data, use_cells, combination_cells = cells, y = .), 
            .id = "iteration")
  
  cells %>% mutate(iteration = as.character(row_number())) %>% left_join(DS, by = "iteration")
}

# Option 5: Use a classifier ------------------------------------------------------------
simulate_classifier <- function(data) {
  run_simulation(data, use_classifier)
}
