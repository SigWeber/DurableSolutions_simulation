library(tidyverse)

colombia <- source("Data/Colombia-ECV2019/colombia_prep.R")$value

colombia_hh <- 
  colombia %>% 
  group_by(HHID = str_replace(HHID, "-\\d+$", ""), across(starts_with("HH_")), PERCAPITA, WT) %>% 
  summarize(across(starts_with("I"), compose(as.numeric, any, as.logical)), .groups = "drop")

# downsample host community
colombia_hh <- 
  bind_rows(colombia_hh %>% filter(ID == 1),
            colombia_hh %>% filter(ID == 0) %>% slice_sample(n = 10000) %>% mutate(WT = sum(!colombia$ID)/10000*WT))

# output final dataset ----
colombia_hh
