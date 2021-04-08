library(tidyverse)

colombia <- source("Data/Colombia-ECV2019/colombia.R")$value

# downsample host community
colombia_ind <- 
  bind_rows(colombia %>% filter(ID == 1),
            colombia %>% filter(ID == 0) %>% slice_sample(n = 10000) %>% mutate(WT = sum(!colombia$ID)/10000*WT))

# output final dataset ----
colombia_ind
