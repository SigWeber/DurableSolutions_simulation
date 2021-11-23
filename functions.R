########################################################
# Functions to implement different metrics for solutions
########################################################

library(tidyverse)

# Utility functions ----
calculate_composite <- function(data, sim_data, benchmark, x, grouping) {
  data <- bind_rows(data, benchmark) %>% select(ID, HHID, WT, all_of(as.character(sim_data[x,])))
  
  newdata <- 
    data %>% 
    pivot_longer(-c(ID, HHID, WT), names_to = "ind", values_to = "val") %>% 
    mutate(ind = str_match(ind, "^(I\\d+)")[,2],
           grp = grouping[ind]) %>% 
    group_by(ID, HHID, WT, grp) %>% 
    summarize(val = sum(val), .groups = "drop")
  
  benchmark <- newdata %>% filter(ID == 0) %>% group_by(grp) %>% summarize(benchmark = weighted.mean(val, WT, na.rm = TRUE))
  
  idps <- 
    newdata %>% 
    filter(ID == 1) %>% 
    left_join(benchmark, by = "grp") %>% 
    group_by(HHID) %>% 
    summarize(exited = all(val >= benchmark))
  
  idps %>% select(HHID, exited)
}

optimum_threshold <- function(ID, p) {
  roc_curve <- tibble(threshold = unique(p),
                      sensitivity = map_dbl(threshold, ~sum((p>.)*ID,na.rm=TRUE)/sum(ID)),
                      specificity = map_dbl(threshold, ~sum((p<.)*(!ID),na.rm=TRUE)/sum(!ID)))
  
  roc_curve %>% 
    arrange(desc(sensitivity+specificity)) %>% 
    pluck("threshold", 1)
}

# Original framework function --------------------------------------
use_IRIS_metric <- function(data, sim_data, benchmark, x) {
  data%>% 
    transmute(HHID,
              exited = data %>% select(as.character(sim_data[x,])) %>% reduce(~.x&.y))
}

# Option 1: Full composite ------------------------------------------
use_composite <- function(data, sim_data, benchmark, x) {
  grouping <- rep("-", 10) %>% set_names(str_c("I", 1:10))
  
  calculate_composite(data, sim_data, benchmark, x, grouping = grouping)
}

# Option 2: Composite measure at criterion level -----------------------------------
use_criterion <- function(data, sim_data, benchmark, x) {
  grouping <- c(I1 = "C1", I2 = "C2",
                I3 = "C2", I4 = "C2", I5 = "C2", I6 = "C2",
                I7 = "C3", I8 = "C3",
                I9 = "C4",
                I10 = "C5")
  
  calculate_composite(data, sim_data, benchmark, x, grouping = grouping)
}

# Option 3: Composite measure at subcriterion level -----------------------------------
use_subcriterion <- function(data, sim_data, benchmark, x){
  grouping <- str_c("SC", 1:10) %>% set_names(str_c("I", 1:10))
  
  calculate_composite(data, sim_data, benchmark, x, grouping = grouping)
}

# Option 4: Use population cells ------------------------------------------------------
use_cells <- function(x, y, data, benchmark, combination_cells, sim_data) {
  combination_indicators <- sim_data
  
  undecided_cases <- data %>% select(HHID, as.character(combination_indicators[x,])) %>% filter(!complete.cases(.))
  data <- data %>% select(HHID, WT, as.character(combination_indicators[x,]), as.character(combination_cells[y,])) %>% drop_na()
  
  averages_per_cell = data %>% 
    group_by(across(as.character(combination_cells[y,]))) %>% 
    summarise(across(as.character(combination_indicators[x,]), ~weighted.mean(., WT)), .groups = "drop")
  
  benchmark = benchmark %>% summarise(across(as.character(combination_indicators[x,]), ~weighted.mean(., WT, na.rm = TRUE)))
  
  # make comparison
  exiting_cells = averages_per_cell %>% 
    filter(across(as.character(combination_indicators[x,]), ~. >= as.numeric(benchmark[,cur_column()]))) %>% 
    select(as.character(combination_cells[y,]))
  
  bind_rows(data %>% semi_join(exiting_cells, by = names(exiting_cells)) %>% transmute(HHID, exited = TRUE),
            data %>% anti_join(exiting_cells, by = names(exiting_cells)) %>% transmute(HHID, exited = FALSE),
            undecided_cases %>% transmute(HHID, exited = NA))
    
}

# Option 4b: Use population cells w/hclust ------------------------------------------
use_hclust <- function(data, sim_data, benchmark, x, method, maxdiff) {
  benchmark <- benchmark %>% summarize(across(as.character(sim_data[x,]), ~weighted.mean(., WT, na.rm = TRUE)))
  undecided_cases <- data %>% select(HHID, as.character(sim_data[x,])) %>% filter(!complete.cases(.))
  data <- data %>% select(HHID, WT, as.character(sim_data[x,])) %>% drop_na()
  
  h <- data %>% select(-c(HHID, WT)) %>% dist(method = "binary") %>% hclust(method = method)
  
  data <- data %>% mutate(cell = cutree(h, h = maxdiff/(ncol(data)-2)))
  
  averages_per_cell <- data %>% group_by(cell) %>% summarize(across(-c(HHID, WT), ~weighted.mean(., WT)))
  
  exiting_cells <- averages_per_cell %>% 
    filter(across(-cell, ~. >= as.numeric(benchmark[,cur_column()]))) %>% 
    select(cell)
  
  bind_rows(data %>% semi_join(exiting_cells, by = names(exiting_cells)) %>% transmute(HHID, exited = TRUE),
            data %>% anti_join(exiting_cells, by = names(exiting_cells)) %>% transmute(HHID, exited = FALSE),
            undecided_cases %>% transmute(HHID, exited = NA))
}

# Option 5: Use a classifier ------------------------------------------------------------
use_classifier <- function(data, sim_data, benchmark, x) {
  # select the right variables per iteration
  data = bind_rows(data, benchmark) %>% select(ID, HHID, as.character(sim_data[x,]))

  # fit the model to classify
  model = glm(ID ~ ., family = "binomial", data = data %>% select(starts_with("I")))
  
  # predict whether IDP or non-displaced
  data$IDP_prob <- predict(model, data, type = "response")
  threshold <- optimum_threshold(data$ID, data$IDP_prob)
  
  data %>% filter(ID == 1) %>% transmute(HHID, exited = IDP_prob < threshold)
}

# Option 6: Use pairwise comparisons ----------------------------------------
use_pairwise <- function(data, sim_data, benchmark, x) {
  idps <- data |> select(WT, as.character(sim_data[x,]))
  hc <- benchmark |> select(WT, as.character(sim_data[x,]))
  
  hc <- hc |> count(across(-WT), wt = WT, name = "WT")
  bench <-
    cross_df(list(x = 1:nrow(hc),
                  y = 1:nrow(hc))) |>
    mutate(pass = map2_lgl(x, y, \(x, y) all(hc[x,1:(ncol(hc)-1)] >= hc[y,1:(ncol(hc)-1)]))) |>
    group_by(x) |>
    summarize(pass = weighted.mean(pass, hc$WT[y])) |>
    summarize(pass = weighted.mean(pass, hc$WT[x])) |> 
    pull(pass)
  
  idps <- idps |> count(across(-WT), wt = WT, name = "WT")
  exits <-
    cross_df(list(x = 1:nrow(idps),
                  y = 1:nrow(hc))) |>
    mutate(pass = map2_lgl(x, y, \(x, y) all(idps[x,1:(ncol(idps)-1)] >= hc[y,1:(ncol(hc)-1)]))) |>
    group_by(x) |>
    summarize(exited = weighted.mean(pass, hc$WT[y]) >= bench)
  
  data |> 
    left_join(idps |> mutate(exited = exits$exited) |> select(-WT)) |> 
    select(HHID, exited)
}


# Option 7: volu-metric ---------
use_volumetric <- function(data, sim_data, benchmark, x) {
  idps <- data |> summarize(across(as.character(sim_data[x,]), weighted.mean, WT, na.rm = TRUE))
  hc <- benchmark |> summarize(across(as.character(sim_data[x,]), weighted.mean, WT, na.rm = TRUE))
  
  p <- reduce2(unlist(idps), unlist(hc), ~..1*min(..2/..3, 1), .init = 1)
  
  data |> transmute(HHID, exited = rbernoulli(n(), p))
}