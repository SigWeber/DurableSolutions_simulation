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

# 1.1 Victims of violence
sudan <- sudan %>% 
  mutate(

    # Proportion feeling safe at night
    I1_sec_night = ifelse(H_3_4_safe_walking_night < 3,1,0),
    
    # Proportion feeling safe at night
    I1_sec_day = ifelse(H_3_5_safe_walking_day < 3,1,0),
    
    # Proportion not experiencing a robbery
    I1_sec_inc = ifelse(C_6_1_shocks_yn__8 == 1, 0,1),
    
    # Proportion of respondents reporting an experienced crime to police
    I1_sec_rep = H_3_8_report_yn,
  )

# 2.1. Food security 
sudan <- sudan  %>% 
  mutate(
    # Proportion always able to pay for food in last 7 days
    I3_pay_food = ifelse(C_4_3_nomoney == 1,0,1),
    
    # Proportion not having to borrow money for food
    I3_no_borrow = ifelse(C_4_5_cop_borrow_food == 1,0,1)
  )
    

# 2.2 Shelter and housing 
sudan <- sudan %>% 
  mutate(
    # Proportion living in housing/shelter  that is not overcrowded (< X persons per room)
    I4_hous_overcrowd = ifelse(household_size/3 > C_1_3_slrooms_n, 0, 1),
    
    # Proportion living in housing they legally own 
    I4_hous_ownership = case_when(
      is.na(C_1_6_land_legal_main) == T ~ 0,
      C_1_6_land_legal_main == 1 ~ 1,
      C_1_6_land_legal_main == 0 ~0,
      TRUE ~ NA_real_),
    
    # Proportion having access to improved drinking water sources
    I4_hous_water = case_when(
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
    
    # Proportion having access to improved sanitation 
    I4_hous_toilet = case_when(
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
    )
  )

# 2.3 Medical services 

sudan <- sudan %>% 
  
  # Proportion in below average distance to health center
  mutate(I5_med_dist = (C_1_27_thealth_d * 60) + C_1_27_thealth_h + (C_1_27_thealth_m / 60)) %>% 
  mutate(I5_med_dist = ifelse(I5_med_dist >= mean(I5_med_dist,na.rm=T),0,1),
         
         # Proportion satisfied with health care services
         I5_med_satis = ifelse(H_2_4_health_satisfaction > 3, 1, 0)
         )


# 2.4 Education 
sudan <- 
  sudan %>% 
  mutate(
    # Walking distance to next school below average
    I6_edu_dist = (C_1_26_tedu_d * 60) + C_1_26_tedu_d + (C_1_26_tedu_d / 60),
    I6_edu_dist = ifelse(I6_edu_dist >= mean(I6_edu_dist,na.rm=T),0,1))

sudan_ind <- 
  sudan_ind |> 
  mutate(
    # Children went to school
    I6_ever_school = ifelse(B_3_6_hhm_edu_ever == 0,0,1),
    
    # Children currently go to school
    I6_educ_child = ifelse(B_3_3_hhm_edu_current == 0,0,1)
  )

# 3.1 Employment and livelihoods
sudan_ind <- sudan_ind %>% 
  mutate(
    # Proportion engaging in any paid job
    I7_job_employ = ifelse(is.na(B_4_2_1_emp_7d_prim) ==  F,1,0),
    
    # Proportion not reporting unemployment
    I7_job_unemploy = as.numeric(B_4_1_1_hhm_job_s == 1 & B_4_1_3_hhm_available != 0)
    )

# 3.2 Economic security 
sudan <- sudan %>%
  #Above mean access to market (distance)
  mutate(I8_econ_market = (C_1_25_tmarket_d * 60) + C_1_25_tmarket_h + (C_1_25_tmarket_m / 60)) %>% 
  mutate(I8_econ_market = ifelse(I8_econ_market >= mean(I8_econ_market,na.rm=T),0,1),
         
  # Mobile money account
  I8_econ_account = C_4_16_acc_mobile_money,
  
  # Proportion not being poor (190)
  I8_poor190 = ifelse(poorPPP_190 < 0.5,1,0),
  
  # Proportion not being poor (190)
  I8_poor32 =  ifelse(poorPPP_320 < 0.5,1,0)
  )
         
         
# 4.1 Property restitution and compensation 
sudan <- sudan %>% 
  mutate(
    # # Proportion that own property left behind
    # I9_hlp_own = C_2_6_land_legal_main_disp,
    # 
    # # Proportion with access to compensation mechanisms
    # I9_hlp_access = H_2_11_legal_access_disp,
    # 
    # # Proportion with documentation to prove ownership
    # I9_hlp_doc = ifelse(C_2_7_land_legal_main_disp_d<=3,1,0)
    
    # Proportion living in housing they legally own (same as I4_hous_ownership)
    I9_hous_ownership = case_when(
      is.na(C_1_6_land_legal_main) == T ~ 0,
      C_1_6_land_legal_main == 1 ~ 1,
      C_1_6_land_legal_main == 0 ~0,
      TRUE ~ NA_real_)
  )

# 5.1. Documentation 
sudan_ind <- sudan_ind %>% 
  mutate(
    # Proportion with documents to prove identity 
    I10_doc_id = ifelse(B_1_13_legal_id == 1,1,0),

    # Proportion having a birth certificate
    I10_doc_birth = ifelse(B_1_11_birth_cert_yn == 1, 1, 0)
    
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
  summarize(across(starts_with("I6"), compose(as.numeric, all, as.logical)))

sudan_ind_workingage <- 
  sudan_ind |> 
  select(household_id, agegrp = B_0_1_hhm_age, starts_with("I7")) |> 
  filter(agegrp %in% c("15 to 17", "18 to 24", "24 to 49", "50 to 66")) |> 
  group_by(household_id) |> 
  summarize(across(starts_with("I7"), compose(as.numeric, all, as.logical)))

sudan_ind_all <- 
  sudan_ind |> 
  select(household_id, agegrp = B_0_1_hhm_age, starts_with("I10")) |> 
  group_by(household_id) |> 
  summarize(across(starts_with("I10"), compose(as.numeric, all, as.logical)))

sudan_ind <- list(sudan_ind_all, sudan_ind_children, sudan_ind_workingage) |> reduce(left_join)

# combine household and individual data
sudan <- sudan |> left_join(sudan_ind)

# add household ID
sudan <- sudan %>% mutate(HHID = household_id)

# welfare-measure
sudan <- sudan |> mutate(PERCAPITA = tc_imp)

# select variables
sudan <- sudan %>% select(ID, WT, starts_with("HH_"), starts_with("I"), -starts_with("I_"), PERCAPITA, HHID)

# final dataset ----
sudan
