library(tidyverse)

# preparations ---------------------------------------------------------

# read data
sudan <- readxl::read_excel("Data/Sudan.xlsx", sheet = 3, na = "NA", guess_max = 15000)
sudan_ind <- readxl::read_excel("Data/Sudan.xlsx", sheet = 4, na = "NA", guess_max = 15000)

# identify idps vs hosts and add household weights
sudan <- sudan %>% 
  rename(ID = migr_idp, WT = weight) %>% 
  filter(!is.na(ID))


# identify potential IASC indicators for each subcriteria -------------

# All indicators are coded as 1 if solution "achieved" and zero if not!!!

# 1.1 Victims of violence
sudan <- sudan %>% 
  mutate(
    # Proportion feeling safe walking at night or during the day
    I1_SDG_16.1.4 = case_when(
      H_3_4_safe_walking_night < 3 ~ 1,
      H_3_5_safe_walking_day < 3 ~ 1,
      H_3_4_safe_walking_night >= 3 ~ 0,
      H_3_5_safe_walking_day >= 3 ~ 0,
      TRUE ~ NA_real_),
    
    # Proportion not experiencing a robbery
    I1_sec_inc = ifelse(C_6_1_shocks_yn__8 == 1, 0,1)
    )



# 1.2. Freedom of movement
# Feeling free to move not covered in questionnaire


# 2.1. Food security 

# Food Insecurity Experience Scale (using FAO package)

# identify columns and turn them into binary indicators if any vulnerability appears
food_insecurity_components <- sudan %>% 
  select(c("C_4_3_nomoney", 
           "C_4_4_cop_lessprefrerred",
           "C_4_5_cop_borrow_food",
           "C_4_6_cop_limitportion",
           "C_4_7_cop_limitadult",
           "C_4_8_cop_reducemeals",
           "C_4_9_cop_sellassets", 
           "C_4_10_cop_sellfem")) %>% 
  summarise_all(~ifelse(.>=1,1,0)) %>% 
  mutate(I3_DS_2.1.2 = rowSums(.))

sudan$I3_DS_2.1.2 <- (8-food_insecurity_components$I3_DS_2.1.2) # high values are high food security!

# Fixing needed: fit weighted Rasch - I have no idea if and how the Rasch model is broken down to the individual level,
# currently just use an ordinal scale
#rr <- RM.w(food_insecurity_components[,1:8],nigeria$WT)
# pp <- prob.assign(rr, sthres = seq(-5, 5, 0.01))

sudan <- sudan  %>% 
  mutate(
    # Proportion always able to pay for food in last 7 days
    I3_pay_food = ifelse(C_4_3_nomoney == 1,0,1),
    
    # Proportion not having to borrow money for food
    I3_no_borrow = ifelse(C_4_5_cop_borrow_food == 1,0,1)
  )

# 2.2 Shelter and housing 

# identify components of sdg indicator: all coded as 1 if NOT vulnerable
sudan <- sudan %>% 
  mutate(
    
    # Security of tenure: Proportion being legally recognized owner of dwelling and having a formal document to proof it
    SDG_secure_tenure = case_when(
      C_1_6_land_legal_main == 1 & C_1_7_land_legal_main_d %in% c(1,2) ~ 1,
      C_1_6_land_legal_main == 1 & C_1_7_land_legal_main_d %in% 3  ~ 0,
      C_1_6_land_legal_main == 0  ~ 0,
      is.na(C_1_6_land_legal_main) == T ~ 0,
      TRUE ~ NA_real_),

    # Access to improved water sources
    SDG_improved_water = case_when(
      C_1_8_water_home == 1 ~ 1,
      C_1_8_water_home == 10 ~ 1,
      C_1_8_water_home == 1000 ~ NA_real_,
      C_1_8_water_home == 11 ~ 0,
      C_1_8_water_home == 12 ~ 0,
      C_1_8_water_home == 2 ~ 1,
      C_1_8_water_home == 3 ~ 1,
      C_1_8_water_home == 4 ~ 1,
      C_1_8_water_home == 5 ~ 1,
      C_1_8_water_home == 6 ~ 0,
      C_1_8_water_home == 7 ~ 1,
      C_1_8_water_home == 9 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Access to improved sanitation facilities
    SDG_improved_sanitation  = case_when(
      C_1_21_toilet == 1 ~ 1,
      C_1_21_toilet == 10 ~ 0,
      C_1_21_toilet == 1000 ~ NA_real_,
      C_1_21_toilet == 12 ~ 0,
      C_1_21_toilet == 13 ~ 0,
      C_1_21_toilet == 2 ~ 1,
      C_1_21_toilet == 3 ~ 1,
      C_1_21_toilet == 4 ~ 0,
      C_1_21_toilet == 5 ~ 1,
      C_1_21_toilet == 6 ~ 1,
      C_1_21_toilet == 7 ~ 1,
      C_1_21_toilet == 8 ~ 0,
      C_1_21_toilet == 9 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Structural quality of housing and location: no indicator present on hazardous zones, housing on garbage or in high industrial pollution area
    
    # Structural quality of the housing and permanency of the structure:
    SDG_permanency = case_when(
      # living in non-durable (incomplete, not intended, makeshift)
      C_1_1_housingtype >= 11 ~ 0, 
      C_1_1_housingtype < 11 ~ 1,
      # squatting or living in temporary shelter by UNHCR
      C_1_4_tenure %in% c(5,6,7,8) ~ 0,
      C_1_4_tenure == 1000  ~ NA_real_,
      C_1_4_tenure %in% c(1,2,3,4) ~ 1,
    TRUE ~ NA_real_),
    
    # Sufficient living area: Proportion of households in which not more than three people share the same habitable room
    SDG_sufficient_space = ifelse(household_size/C_1_3_slrooms_n<3, 1, 0)
  )



# combine to one indicator 
sudan <- sudan  %>% 
  mutate(
    I4_SDG_11.1.1 = ifelse(rowSums((sudan %>% select(starts_with("SDG_"))) == 0) > 0, 0, 1),
    
    # Proportion living in housing/shelter  that is not overcrowded (< X persons per room)
    I4_hous_overcrowd = ifelse(household_size/3 > C_1_3_slrooms_n, 0, 1)
  )

# 2.3 Medical services 
sudan <- sudan %>% 
  mutate(
    
    # Target population who accessed essential health care services the last time they needed it in the past 12 months
    # Not covered in the questionnaire, approximated with satisfaction with health services
    I5_DS_2.1.8 = ifelse(H_2_4_health_satisfaction > 3, 0, 1)
  )

# 2.4 Education 
sudan_ind <- 
  sudan_ind |> 
  
  # SDG indicator 4.1.2: Completion rate (primary education): not present, 
  # approximated with whether children ever went to school
  mutate(
    # Children went to school
    I6_SDG_4.1.2  = ifelse(B_3_6_hhm_edu_ever == 0,0,1),
    
    # Children currently go to school
    I6_educ_child = ifelse(B_3_3_hhm_edu_current == 0,0,1)
  )


# 3.1 Employment and livelihoods
sudan_ind <-sudan_ind %>% 
  mutate(
    
    # Unemployment rate:  Proportion engaging in any paid job
    I7_SDG_8.5.2= ifelse(is.na(B_4_2_1_emp_7d_prim) ==  F,1,0),
    
    # Proportion not reporting unemployment
    I7_job_unemploy = as.numeric(B_4_1_1_hhm_job_s == 1 & B_4_1_3_hhm_available != 0)
    
  )


# 3.2 Economic security 
sudan <- sudan %>% 
  mutate(
    # Poverty:  Proportion not being poor (190)
    I8_SDG_8.5.2  = ifelse(poorPPP_190 < 0.5,1,0),
    
    # Mobile money account
    I8_econ_account = C_4_16_acc_mobile_money,
    
    
    # Proportion not being poor (190)
    I8_poor32 =  ifelse(poorPPP_320 < 0.5,1,0)) %>% 
    
    #Above mean access to market (distance)
    mutate(I8_econ_market = (C_1_25_tmarket_d * 60) + C_1_25_tmarket_h + (C_1_25_tmarket_m / 60)) %>% 
      mutate(I8_econ_market = ifelse(I8_econ_market >= mean(I8_econ_market,na.rm=T),0,1))

         
# 4.1 Property restitution and compensation /security of tenure
sudan <- sudan %>% 
  mutate(
    # Security of tenure
    I9_SDG_1.4.2 = SDG_secure_tenure
  )


# 5.1. Documentation 
sudan_ind <- sudan_ind %>% 
  mutate(
    # DS Library indicator 5.1.1: Target population currently in possession of documentation 
    I10_DS_5.1.1 = ifelse(B_1_13_legal_id == 1,1,0)
    
  )


# prepare dataset for simulations -----------------------------------------------

# define household characteristics
sudan <- sudan %>% 
  mutate(HH_house_type = case_when(
    C_1_1_housingtype == 1 ~ "Tent",
    C_1_1_housingtype == 2 ~ "Dwelling of straw mats",
    C_1_1_housingtype == 3 ~ "Tukul/gottiya - sticks",
    C_1_1_housingtype == 4 ~ "Tukul/gottiya - mud",
    C_1_1_housingtype == 5 ~ "Flat or apartment",
    C_1_1_housingtype == 7 ~ "House of one floor - mud",
    C_1_1_housingtype == 8 ~ "House of one floor - brick",
    C_1_1_housingtype == 9 ~ "House constructed of wood",
    C_1_1_housingtype == 11 ~ "Incomplete structure",
    TRUE ~ "Other"),
    
    HH_year_o_farrival = year_arrival,
    HH_times_displaced = pmin(I_1_7_disp_site, 3),
    HH_arrived_with = case_when(
      I_1_11_disp_arrive_with == 1 ~ "Alone",
      I_1_11_disp_arrive_with == 2 ~ "With family",
      I_1_11_disp_arrive_with == 3 ~ "With a larger group but not the family"),
    HH_community_location = case_when(
      I_1_16_disp_comm_loc == 1 ~ "Same district",
      I_1_16_disp_comm_loc == 2 ~ "Same state",
      I_1_16_disp_comm_loc == 5 ~ "Different state",
      I_1_16_disp_comm_loc == 6 ~ "Outside Sudan")
    )

# aggregate individual data
sudan_ind_children <- 
  sudan_ind |> 
  select(household_id, agegrp = B_0_1_hhm_age, starts_with("I6")) |> 
  filter(agegrp %in% c("06 to 11", "12 to 14", "15 to 17")) |> 
  group_by(household_id) |> 
  summarize(across(starts_with("I6"), compose(as.numeric, any, as.logical)))

sudan_ind_workingage <- 
  sudan_ind |> 
  select(household_id, agegrp = B_0_1_hhm_age, starts_with("I7")) |> 
  filter(agegrp %in% c("15 to 17", "18 to 24", "24 to 49", "50 to 66")) |> 
  group_by(household_id) |> 
  summarize(across(starts_with("I7"), compose(as.numeric, any, as.logical)))

sudan_ind_all <- 
  sudan_ind |> 
  select(household_id, agegrp = B_0_1_hhm_age, starts_with("I10")) |> 
  group_by(household_id) |> 
  summarize(across(starts_with("I10")|starts_with("I9"), compose(as.numeric, any, as.logical)))

sudan_ind <- list(sudan_ind_all, sudan_ind_children, sudan_ind_workingage) |> reduce(left_join)

# combine household and individual data
sudan <- sudan |> left_join(sudan_ind)

# add household ID
sudan <- sudan %>% mutate(HHID = household_id)

# welfare-measure
sudan <- sudan |> mutate(PERCAPITA = tc_imp)

# select variables
sudan <- sudan %>% select(ID, WT, starts_with("HH_"), starts_with("I"), -starts_with("I_"), PERCAPITA, HHID)

# address non-applicability as non-vulnerability
sudan <- sudan %>% 
  mutate_at(vars(starts_with("I6")), ~ case_when(
    is.na(.)==T ~ 1, 
    TRUE ~ .)) %>%   
  mutate_at(vars(starts_with("I7")), ~ case_when(
      is.na(.)==T ~ 1, 
      TRUE ~ .)) 

# run simulations --------------------------------------------------------------
source("simulations.R")

simulation_sudan <- bind_rows(
  simulate_IRIS_metric(sudan) %>% mutate(metric="Pass/fail"),
  simulate_composite(sudan)%>% mutate(metric="1: Full composite"),
  simulate_criterion(sudan)%>% mutate(metric="2: Composite at criterion level"),
  simulate_subcriterion(sudan)%>% mutate(metric="3: Composite at subcriterion level"),
  simulate_cells(sudan) %>% mutate(metric="4: Homogeneous cells"),
  simulate_classifier(sudan)%>% mutate(metric="5: Classifier/regression-based"))%>% 
  select(-Durable_Solutions)

#write_csv(simulation_sudan, "Preliminary_full_circle/sudan_fullcircle.csv")
