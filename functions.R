########################################################
# Functions to implement different metrics for solutions
########################################################

library(tidyverse)



# Original framework function --------------------------------------
use_IRIS_metric <- function(data, sim_data, x){
  # first only select the relevant variable for this simulation
  select_var = data[,as.character(sim_data[x,])]
  # then check whether any of them is zero (not passed)
  passed = select_var %>% filter(across(everything(), ~ .==1))
  # then count how many have passed on all dimensions
  nrow(passed)
}


# Option 1: Full composite ------------------------------------------

use_composite <- function(data, sim_data, benchmark, x){
  
  # identify for the indicator combination what the score is
  IDP_score = data %>% 
    select(as.character(sim_data[x,])) %>% 
    mutate(index = rowSums(., na.rm=T)) %>% 
    drop_na()
  
  benchmark_score = benchmark %>% 
    select(as.character(sim_data[x,])) %>% 
    drop_na() %>% 
    summarise(index = rowSums(.,na.rm=T))
  
  sum(IDP_score$index >= mean(benchmark_score$index, na.rm = T))
}

# Option 2: Composite measure at criterion level -----------------------------------

use_criterion <- function(data, sim_data, benchmark, x){
  
  # reduce data to complete cases
  data = data %>% 
    select(as.character(sim_data[x,])) %>% 
    drop_na()
  
  # identify for the indicator combination what the score is
  Criterion1_score = data %>% 
    select(contains("I1_"),contains("I2_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  Criterion2_score = data %>% 
    select(contains("I3_"),contains("I4_"),contains("I5_"),contains("I6_")) %>% 
    summarise(index = rowSums(.,na.rm=T)) 
  Criterion3_score= data %>% 
    select(contains("I7_"),contains("I8_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  Criterion4_score= data %>% 
    select(contains("I9_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  Criterion5_score= data %>% 
    select(contains("I10_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  
  # identify for benchmark what their scores are 
  benchmark = benchmark %>% 
    select(as.character(sim_data[x,])) %>% 
    drop_na()
  
  Criterion1_b = benchmark %>% 
    select(contains("I1_"),contains("I2_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  Criterion2_b = benchmark %>% 
    select(contains("I3_"),contains("I4_"),contains("I5_"),contains("I6_")) %>% 
    summarise(index = rowSums(.,na.rm=T)) 
  Criterion3_b= benchmark %>% 
    select(contains("I7_"),contains("I8_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  Criterion4_b= benchmark %>% 
    select(contains("I9_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  Criterion5_b= benchmark %>% 
    select(contains("I10_")) %>% summarise(index = rowSums(.,na.rm=T)) 
  
  result = data.frame(
    I1 = ifelse(Criterion1_score$index >= mean(Criterion1_b$index,na.rm=T), "Passed","Not passed"),
    I2 = ifelse(Criterion2_score$index >= mean(Criterion2_b$index,na.rm=T), "Passed","Not passed"),
    I3 = ifelse(Criterion3_score$index >= mean(Criterion3_b$index,na.rm=T), "Passed","Not passed"),
    I4 = ifelse(Criterion4_score$index >= mean(Criterion4_b$index,na.rm=T), "Passed","Not passed"),
    I5 = ifelse(Criterion5_score$index >= mean(Criterion5_b$index,na.rm=T), "Passed","Not passed"))
  
  nrow(result %>% filter(across(everything(), ~ !grepl('Not passed', .))))
}


# Option 3: Composite measure at subcriterion level -----------------------------------

use_subcriterion <- function(data, sim_data, benchmark, x){
  
  data = data %>% 
    select(as.character(sim_data[x,])) %>% 
    drop_na()
  
  benchmark = benchmark %>% 
    select(as.character(sim_data[x,])) %>% 
    drop_na()
  
  # identify for the indicator combination what the score is
  Subcriterion1_score = data %>% 
    select(starts_with("I1")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion2_score = data %>% 
    select(starts_with("I2")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion3_score = data %>% 
    select(starts_with("I3")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion4_score = data %>% 
    select(starts_with("I4")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion5_score = data %>% 
    select(starts_with("I5")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion6_score = data %>% 
    select(starts_with("I6")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion7_score = data %>% 
    select(starts_with("I7")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion8_score = data %>% 
    select(starts_with("I8")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion9_score = data %>% 
    select(starts_with("I9")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion10_score = data %>% 
    select(starts_with("I10")) %>% summarise(index = rowSums(., na.rm=T)) 
  
  
  # identify for benchmark what their scores are 
  Subcriterion1_b = benchmark %>% 
    select(starts_with("I1")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion2_b = benchmark %>% 
    select(starts_with("I2")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion3_b = benchmark %>% 
    select(starts_with("I3")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion4_b = benchmark %>% 
    select(starts_with("I4")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion5_b = benchmark %>% 
    select(starts_with("I5")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion6_b = benchmark %>% 
    select(starts_with("I6")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion7_b = benchmark%>% 
    select(starts_with("I7")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion8_b = benchmark %>% 
    select(starts_with("I8")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion9_b = benchmark %>% 
    select(starts_with("I9")) %>% summarise(index = rowSums(., na.rm=T)) 
  Subcriterion10_b = benchmark %>% 
    select(starts_with("I10")) %>% summarise(index = rowSums(., na.rm=T)) 
  
  
  result = data.frame(
    I1 = ifelse(Subcriterion1_score$index >= mean(Subcriterion1_b$index,na.rm=T), "Passed","Not passed"),
    I2 = ifelse(Subcriterion2_score$index >= mean(Subcriterion2_b$index,na.rm=T), "Passed","Not passed"),
    I3 = ifelse(Subcriterion3_score$index >= mean(Subcriterion3_b$index,na.rm=T), "Passed","Not passed"),
    I4 = ifelse(Subcriterion4_score$index >= mean(Subcriterion4_b$index,na.rm=T), "Passed","Not passed"),
    I5 = ifelse(Subcriterion5_score$index >= mean(Subcriterion5_b$index,na.rm=T), "Passed","Not passed"),
    I6 = ifelse(Subcriterion6_score$index >= mean(Subcriterion6_b$index,na.rm=T), "Passed","Not passed"),
    I7 = ifelse(Subcriterion7_score$index >= mean(Subcriterion7_b$index,na.rm=T), "Passed","Not passed"),
    I8 = ifelse(Subcriterion8_score$index >= mean(Subcriterion8_b$index,na.rm=T), "Passed","Not passed"),
    I9 = ifelse(Subcriterion9_score$index >= mean(Subcriterion9_b$index,na.rm=T), "Passed","Not passed"),
    I10 = ifelse(Subcriterion10_score$index >= mean(Subcriterion10_b$index,na.rm=T), "Passed","Not passed")
    )
  return(nrow(result %>% filter(across(everything(), ~ !grepl('Not passed', .)))))
}

# Option 4: Use population cells ------------------------------------------------------

use_cells <- function(x, y, data, benchmark, combination_cells,combination_indicators){
  
  averages_per_cell = data %>% 
    filter(ID == 1) %>% 
    select(-ID) %>% 
    group_by_at(vars(one_of(as.character(combination_cells[x,])))) %>% 
    mutate( n = n()) %>%  
    summarise_all(.,mean,na.rm = T) %>% 
    ungroup() %>% 
    select(as.character(combination_indicators[y,]),n) 
  
  benchmark = benchmark %>% select(as.character(combination_indicators[y,])) 
  
  # make comparison
  assessment_per_cell = averages_per_cell %>% 
    mutate_at(1, ~ifelse(.>= as.numeric(benchmark[,1]),1,0)) %>% 
    mutate_at(2, ~ifelse(.>= as.numeric(benchmark[,2]),1,0)) %>% 
    mutate_at(3, ~ifelse(.>= as.numeric(benchmark[,3]),1,0)) %>% 
    mutate_at(4, ~ifelse(.>= as.numeric(benchmark[,4]),1,0)) %>% 
    mutate_at(5, ~ifelse(.>= as.numeric(benchmark[,5]),1,0)) %>% 
    mutate_at(6, ~ifelse(.>= as.numeric(benchmark[,6]),1,0)) %>% 
    mutate_at(7, ~ifelse(.>= as.numeric(benchmark[,7]),1,0)) %>% 
    mutate_at(8, ~ifelse(.>= as.numeric(benchmark[,8]),1,0)) %>% 
    mutate_at(9, ~ifelse(.>= as.numeric(benchmark[,9]),1,0)) %>% 
    mutate_at(10, ~ifelse(.>= as.numeric(benchmark[,10]),1,0)) %>% 
    filter(across(starts_with("I"), ~ .==1)) 
  
  sum(assessment_per_cell$n, na.rm =T)
}

# Option 5: Use a classifier ------------------------------------------------------------
use_classifier <- function(data,sim_data,x){
  
  # select the right variables per iteration
  data = data %>% select(ID, as.character(sim_data[x,]))
  names(data) = sub("\\_.*", "", names(data))
  
  # fit the model to classify
  model = glm(ID ~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10, 
              family= "binomial",
              data = data)
  
  # predict whether IDP or non-displaced (cut-off: 0.5)
  data$IDP_prob <- predict(model, data, type= "response")
  data <- data %>% 
  mutate(IDP_pred = case_when(
    ID == 0 ~ 0,
    IDP_prob > 0.5 ~ 1, 
    IDP_prob < 0.5 ~ 0,
    TRUE ~ 1))
  
  # identify how many leave the stock of IDPs
  sum(data$ID)-sum(data$IDP_pred)

}






