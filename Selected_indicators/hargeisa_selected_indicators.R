library(tidyverse)

# preparations ---------------------------------------------------------

# read data
data <- readxl::read_xlsx("Data/Hargeisa.xlsx", sheet = "Data", guess_max = 15000)
metadata <- readxl::read_xlsx("Data/Hargeisa.xlsx", sheet = "Metadata")

data <- data |> filter(str_detect(hargeisa1, "Host|Displaced"))
hargeisa_hh <- data |> select(hargeisa1, all_of(metadata |> filter(HouseInd == "house") |> pull(Name)), -hargeisa173, hargeisa179) |> distinct()
hargeisa_ind <- data |> select(all_of(metadata |> filter(HouseInd == "ind") |> pull(Name)), hargeisa173)

# identify idps vs hosts
hargeisa_hh <- hargeisa_hh |> mutate(ID = as.numeric(hargeisa1 != "Host community"))

# identify potential IASC indicators for each subcriteria -------------

# All indicators are coded as 1 if solution "achieved" and zero if not!!!

# 1.1 Victims of violence
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Proportion feeling very or moderately safe walking at night or during the day
    I1_SDG_16.1.4 = case_when(
      hargeisa133 == "No" ~ 0,
      hargeisa133 == "Yes" ~ 1,
      grepl("Only to some extent",hargeisa133) == T ~ 1),
  )

# 1.2. Freedom of movement
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Feeling free to move: approximated with visiting public places 
    I2_DS_1.4.1 = case_when(
      hargeisa137 == "No" ~ 1,
      hargeisa137 == "Yes" ~ 0)
  )

# 2.1. Food security 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Food Insecurity Experience Scale (using FAO package), approximated with ability to pay for food
    I3_DS_2.1.2 = case_when(
      hargeisa52 == "No" ~ 1,
      hargeisa52 == "Yes" ~ 0))

# 2.2 Shelter and housing 

# identify components of sdg indicator: all coded as 1 if NOT vulnerable
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    
    # Security of tenure: Proportion being legally recognized owner of dwelling and having a formal document to proof it
    # Proportion with documents to prove ownership of property 
    SDG_secure_tenure = case_when(
      grepl("No, ",hargeisa113) ~ 0,
      grepl("Yes, ",hargeisa113) ~ 1,
      TRUE ~ NA_real_),
    
    # Access to improved water sources: water sources are tank or bottled water
    SDG_improved_water =ifelse(hargeisa126 == "Tank" | hargeisa126 == "Bottled/bought", 1, 0),
    
    # Access to improved sanitation facilities: flushing toilet
    SDG_improved_sanitation = as.numeric(hargeisa109=="Yes"|hargeisa108 == "Yes"),
    
    # Structural quality of housing and location: pollution area or high risk area
    SDG_housing_location = ifelse(hargeisa121 == "Yes"|
                                    hargeisa122 == "Yes"|
                                    hargeisa123 == "Yes", 0, 1),
    
    #Structural quality of the housing and permanency of the structure:
    # missing
    
    # Sufficient living area: Proportion of households in which not more than three people share the same habitable room
    SDG_sufficient_space = ifelse(hargeisa14/3 <= hargeisa103, 1, 0)
  )


# combine to one indicator 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    I4_SDG_11.1.1 = ifelse(rowSums((hargeisa_hh %>% select(starts_with("SDG_"))) == 0) > 0, 0, 1)
  )

# 2.3 Medical services 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    
    # Target population who accessed essential health care services the last time they needed it in the past 12 months
    I5_DS_2.1.8  = case_when(
      hargeisa67 == "No" ~ 1,
      hargeisa67 == "Yes" & hargeisa73 == "Yes" ~ 1,
      hargeisa67 == "Yes" & hargeisa73 == "No" ~ 0,
      TRUE ~ NA_real_)
    )

# 2.4 Education 
hargeisa_ind <- hargeisa_ind %>% 
  mutate(
    
    # SDG indicator 4.1.2: Completion rate (primary education): not present, 
    # approximated with whether any household member ever went to school
    
    I6_SDG_4.1.2 = case_when(hargeisa37 == "Yes" ~ 1,
                               hargeisa37 == "No" ~ 0)
  )

# 3.1 Employment and livelihoods
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Unemployment: approximated with Breadwinner in the family
    I7_SDG_8.5.2 = ifelse(hargeisa166 > 0, 1, 0))

# 3.2 Economic security 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    #Poverty: Approximated with proportion capable of unexpected expenses without borrowing money
    I8_unexpect_expense = as.numeric(hargeisa60 == "No"), 
    
  )

# 4.1 Property restitution and compensation 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Proportion with documents to prove ownership of property left behind
    # Security of tenure
    I9_SDG_1.4.2 = SDG_secure_tenure 
  )

# 5.1. Documentation 
hargeisa_ind <- hargeisa_ind %>% 
  mutate(
    
    # DS Library indicator 5.1.1: Target population currently in possession of documentation 
    I10_DS_5.1.1 = case_when(hargeisa42 == "Has a certificate" |
                             hargeisa44 == "Yes" |
                             hargeisa45 == "Yes" |
                             hargeisa46 == "Yes" ~ 1,
                           hargeisa47 == "Yes" ~ 0)
    
  )


# prepare dataset for simulations -----------------------------------------------

# define household characteristics
hargeisa_hh <- 
  hargeisa_hh |> 
  mutate(HH_disp_type = hargeisa1,
         HH_origin_district = ifelse(is.na(hargeisa7), "Unknown", hargeisa7),
         HH_depart_yr = case_when(
           hargeisa28 <= 1990 ~ "Before 1990",
           hargeisa28 > 1990 & hargeisa28 <= 2000 ~ "Between 1990 and 2000",
           hargeisa28 > 2000 & hargeisa28 <= 2010 ~ "Between 2000 and 2010",
           hargeisa28 > 2010  ~ "After 2010",
           TRUE ~ "Unknown")) |> 
  left_join(hargeisa_ind |> 
              group_by(hargeisa179) |> 
              summarize(
                HH_gender = first(hargeisa3),
                HH_clan = first(hargeisa13)))

# aggregate individual-level data
hargeisa_ind_child <- 
  hargeisa_ind |> 
  select(age = hargeisa10, matches("^I(5|6)"), hargeisa179) |> 
  filter(between(age, 5, 17)) |> 
  group_by(hargeisa179) |> 
  summarize(across(matches("^I(5|6)"), ~any(.==1)))

hargeisa_ind_all <- 
  hargeisa_ind |> 
  group_by(hargeisa179) |> 
  summarize(across(starts_with("I10"), ~any(.==1)))

hargeisa_ind <- left_join(hargeisa_ind_all, hargeisa_ind_child) |> mutate(across(-hargeisa179, as.numeric))

# combine household and individual data
hargeisa <- left_join(hargeisa_hh |> select(ID, matches("^(I\\d+|HH)"), hargeisa179), hargeisa_ind)

# add household weights
hargeisa <- hargeisa %>% mutate(WT = 1)

# add household ID
hargeisa <- hargeisa %>% rename(HHID = hargeisa179)

# welfare-measure
hargeisa <- hargeisa |> mutate(PERCAPITA = NA_real_)

# select and reorder variables
hargeisa <- 
  hargeisa %>% 
  select(-contains("hargeisa")) |> 
  select(HHID, ID, WT, PERCAPITA, starts_with("HH_"), starts_with("I"))


# report difficulty of indicators ----------------------------------------------
difficulty <- bind_rows(
  hargeisa %>% filter(ID==1) %>% select(starts_with("I"),WT) %>% select(-ID) %>% 
    summarise_all(.,~weighted.mean(., WT, na.rm = TRUE)) %>% mutate(Stat="Difficulty"),
  hargeisa %>% filter(ID==1) %>% select(starts_with("I"),WT) %>% select(-ID) %>% 
    summarise_all(.,~sum(WT*is.na(.))/sum(WT),na.rm=T) %>% mutate(Stat="Missingness")) %>% 
  pivot_longer(.,cols=c(starts_with("I")),names_to = "Indicator",values_to = "Value") %>% 
  pivot_wider(.,id_cols = "Indicator",names_from ="Stat",values_from = "Value") %>% 
  mutate(Difficulty = round(Difficulty*100,2),Missingness = round(Missingness*100,2)) %>% 
  mutate(Difficulty = ifelse(Indicator == "I3_DS_2.1.2", NA,Difficulty))
#write_csv(difficulty, "Selected_indicators/hargeisa_difficulty.csv")

# run simulations --------------------------------------------------------------
source("simulations.R")
simulation_hargeisa <- bind_rows(
  simulate_IRIS_metric(hargeisa) %>% mutate(metric="Pass/fail"),
  simulate_composite(hargeisa)%>% mutate(metric="1: Full composite"),
  simulate_criterion(hargeisa)%>% mutate(metric="2: Composite at criterion level"),
  cbind((simulate_IRIS_metric(hargeisa))[1,1:10],data.frame(Durable_Solutions = NA,DS=0,DS_perc=0))%>% 
    mutate(metric="3: Composite at subcriterion level"),
  simulate_cells(hargeisa) %>% 
    group_by(across(starts_with("I"))) %>% 
    summarise(DS=mean(DS),DS_perc=mean(DS_perc))%>% mutate(metric="4: Homogeneous cells"),
  simulate_classifier(hargeisa)%>% mutate(metric="5: Classifier/regression-based"))%>% 
  mutate(`Percentage exiting the stock`=round(DS_perc*100,2),
         `IDP households exiting the stock`=round(DS)) %>% 
  select(metric,`IDP households exiting the stock`,`Percentage exiting the stock`) %>% 
  rename(`Metric option`=metric)

#write_csv(simulation_hargeisa, "Selected_indicators/hargeisa_selected_indicators.csv")
