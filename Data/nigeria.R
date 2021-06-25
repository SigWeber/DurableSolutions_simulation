library(tidyverse)

# preparations ---------------------------------------------------------

# read data
nigeria <- read_csv("Data/Nigeria.csv", guess_max = 10000)

# identify idps vs hosts
nigeria <- nigeria %>% rename(ID = migr_idp)

# add household weights
nigeria <- nigeria %>% rename(WT = weight)

# identify potential IASC indicators for each subcriteria -------------


# 1.1 Victims of violence
nigeria<- nigeria %>% 
  mutate(
    # Proportion feeling very or moderately safe
    I1_sec_saf = case_when(
      H_4_1_safe_violence == 1 ~ 1,
      H_4_1_safe_violence == 2 ~ 1,
      H_4_1_safe_violence == 3 ~ 0,
      H_4_1_safe_violence == 4 ~ 0,
      H_4_1_safe_violence == 5 ~ 0,
      TRUE ~ NA_real_),
    # Proportion feeling very or moderately safe walking at night
    I1_sec_night = case_when(
      H_4_2_safe_walking_night == 1 ~ 1,
      H_4_2_safe_walking_night == 2 ~ 1,
      H_4_2_safe_walking_night == 3 ~ 0,
      H_4_2_safe_walking_night == 4 ~ 0,
      H_4_2_safe_walking_night == 5 ~ 0,
      TRUE ~ NA_real_),
    # Proportion feeling very or moderately safe walking during the day
    I1_sec_day = case_when(
      H_4_3_safe_walking_day == 1 ~ 1,
      H_4_3_safe_walking_day == 2 ~ 1,
      H_4_3_safe_walking_day == 3 ~ 0,
      H_4_3_safe_walking_day == 4 ~ 0,
      H_4_3_safe_walking_day == 5 ~ 0,
      TRUE ~ NA_real_),
    
    # Proportion experiencing a security incident
    I1_sec_inc  = ((rowSums(nigeria %>% select(contains("H_4_11")) == 1, na.rm=T) > 0) * 1),
    
    # Proportion experiencing and reporting a security incident
    I1_sec_rep = H_4_12_report_yn,
    
    # Proportion accessing formal dispute resolution mechanisms
    I1_sec_formal = ifelse(H_4_4_dispute_resolve > 5 & H_4_4_dispute_resolve < 1000,1,0),
    
    # Proportion finding it easy to access dispute resolution mechanisms
    I1_sec_easy = case_when(
      H_4_6_report_access == 1 ~ 1,
      H_4_6_report_access == 2 ~ 1,
      H_4_6_report_access == 3 ~ 0,
      H_4_6_report_access == 4 ~ 0,
      H_4_6_report_access == 5 ~ 0,
      H_4_7_report_challenge == 1 ~ 0, 
      TRUE ~ NA_real_),
    
    # Proportion finding dispute resolution effective
    I1_sec_eff = case_when(
      H_4_10_effective_disp == 1 ~ 1,
      H_4_10_effective_disp == 2 ~ 1,
      H_4_10_effective_disp == 3 ~ 0,
      H_4_10_effective_disp == 4 ~ 0,
      H_4_10_effective_disp == 5 ~ 0,
      H_4_10_effective_disp == 1 ~ 0, 
      TRUE ~ NA_real_)
  )


# 1.2. Freedom of movement
nigeria <- nigeria %>% 
  mutate(
    # Feeling free to move 
    I2_feel_free = H_4_1_move_free
  )


# 2.1. Food security 
nigeria <- nigeria %>% 
  # Food insecurity scale
  mutate(I3_food_insecurity_scale = rowSums(
    nigeria[,c("C_4_1_nomoney","C_4_2_cop_lessprefrerred","C_4_3_cop_borrow_food",
               "C_4_4_cop_limitportion","C_4_5_cop_limitadult","C_4_6_cop_reducemeals",
               "C_4_7_cop_sellassets", "C_4_8_cop_sellfem")], na.rm=T))


# 2.2 Shelter and housing 
nigeria <- nigeria %>% 
  mutate(
    
    # Proportion living in non-durable (incomplete, not intended, makeshift)
    I4_hous_indurable = ifelse(C_1_1_housingtype >= 11,1,0),
    
    # Proportion living in overcrowded shelter
    I4_hous_overcrowd = ifelse(hhdensity_sl > 1 , 1, 0),
    
    # Proportion squatting
    I4_hous_squat = case_when(
      C_1_7_tenure == 2  ~ 1,
      C_1_7_tenure == 7  ~ 1,
      C_1_7_tenure == 8  ~ 1,
      C_1_7_tenure == 9  ~ 1,
      C_1_7_tenure == 1000  ~ NA_real_,
      TRUE ~ 0), 
    
    # Proportion squatting or living in temporary shelter by UNHCR
    I4_hous_temporary = case_when(
      C_1_7_tenure == 1  ~ 1,
      C_1_7_tenure == 2  ~ 1,
      C_1_7_tenure == 7  ~ 1,
      C_1_7_tenure == 8  ~ 1,
      C_1_7_tenure == 9  ~ 1,
      C_1_7_tenure == 1000  ~ NA_real_,
      TRUE ~ 0), 
    
    # Proportion owning or renting their housing
    I4_hous_ownrent = case_when(
      C_1_7_tenure == 5  ~ 1,
      C_1_7_tenure == 6  ~ 1,
      C_1_7_tenure == 1000  ~ NA_real_,
      TRUE ~ 0),
    
    # Proportion owning or renting their land legally
    I4_land_tenure = case_when(
      C_3_1_land_access_yn == 0 ~ 0,
      C_3_4_land_tenure == 1 ~ 1,
      C_3_4_land_tenure == 1 ~ 2,
      C_3_4_land_tenure == 1000  ~ 0,
      C_3_4_land_tenure == 3 ~ 0,
      C_3_4_land_tenure == 4  ~ 0,
      TRUE ~ NA_real_
    ),
    
    
    # Proportion getting their drinking water from improved sources 
    I4_hous_water = ifelse(watersource == 1, 1,0),
    
    # Proportion facing no water obstacles
    I4_hous_waccess = ifelse(C_1_14_water_obstacle == 0,1,0),
    
    
    # Proportion having access to improved sanitation facilities
    I4_hous_toilet = ifelse(C_1_21_toilet%in% c(1:7,9),1,0)
  )


# 2.3 Medical services 
nigeria <- nigeria %>% 
  mutate(
    
    # Proportion with access to essential health care when needed. 
    I5_med_access = case_when(
      C_4_10_disease_yn == 0 ~ 1,
      C_4_10_disease_yn == 1 & C_4_12_med_yn == 1 ~ 1,
      C_4_10_disease_yn == 1 & C_4_12_med_yn == 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Duration to next health facility (standardized)
    I5_med_dist = (thealth_d*24) + thealth_h + (thealth_m/60)
  )

# 2.4 Education
nigeria <- nigeria %>% 
  mutate(
    # Duration to next education facility 
    I6_edu_dist = (tedu_d*24) + tedu_h + (tedu_m/60),
    
    # Proportion satisfied with school
    I6_edu_satis = ifelse(H_2_7_school_satisfaction %in% 1:2, 1, 0)
  )


# 3.1 Employment and livelihoods
nigeria <- nigeria %>% 
  mutate(
    #  Proportion whose income is generated from wages, salaries, own business or pension
    I7_salary = ifelse(C_5_1_lhood %in% c(4,5,8),1,0)
  )


# 3.2 Economic security 
nigeria <- nigeria %>% 
  mutate(
    # Proportion below 1.9 USD PPP 2011 Poverty Line
    I8_poor = poor, 
    
    # Proportion Below 1.25 USD PPP 2011 poverty line
    I8_poor125 = poor125, 
    
    # Proportion Below 3.1 USD PPP 2011 poverty line
    I8_poor31 = poor31, 
    
    # Ratio of food vs total consumption
    I8_foodtotal = mi_cons_f/tc_imp,
    
    # Proportion consuming more than average
    I8_consume = tc_imp / mean(tc_imp, na.rm = T),
    
    # Proportion having a bank account
    I8_bank = C_4_15_bank_account,
    
    # Distance to market
    I8_market_dist = (tmarket_d*24) + tmarket_h + (tmarket_m/60)
  )

# 4.1 Property restitution and compensation 
nigeria <- nigeria %>% 
  mutate(
    # Proportion being legally recognized owner of dwelling
    I9_hlp_right = case_when(
      tenure == 6 & C_1_9_land_legal_main == 1 ~ 1,
      tenure == 6 & C_1_9_land_legal_main == 0 ~ 0,
      tenure != 6 ~ 0,
      TRUE ~ NA_real_),
    
    # Proportion having access to compensation mechanisms
    I9_hlp_compensation = case_when(
      ID == 0 ~ 1,
      ID == 1 & H_2_11_legal_access_disp == 1 ~ 1,
      ID == 1 & H_2_11_legal_access_disp == 0 ~ 0,
      TRUE ~ NA_real_)
  )

# 5.1. Documentation 
nigeria  <- nigeria  %>% 
  mutate(
    
    # Proportion with documents or access to replace missing documents if lost
    I10_doc_replace =  case_when(
      ID == 0 & is.na(H_2_9_legal_id_disp) == T ~ 1,
      H_2_9_legal_id_disp == 0 ~ 1,
      H_2_9_legal_id_disp == 1 & H_2_10_legal_id_acc_disp == 1 ~ 1,
      H_2_9_legal_id_disp == 1 & H_2_10_legal_id_acc_disp == 0 ~ 0,
      TRUE ~ NA_real_)
    
  )


# prepare dataset for simulations -----------------------------------------------

# define household characteristics
nigeria <- 
  nigeria %>% 
  # not enough cases in cells when grouping by month to justify keeping it as an option
  mutate(# HH_disp_month = substr(ifelse(I_1_3_disp_date=="N/A",NA,I_1_3_disp_date), start=1,stop=7),
         HH_disp_year = substr(ifelse(I_1_3_disp_date=="N/A",NA,I_1_3_disp_date), start=1,stop=4),
         # HH_arriv_month = substr(ifelse(I_1_7_disp_arrive_date=="N/A",NA,I_1_7_disp_arrive_date), start=1,stop=7),
         HH_arriv_year = substr(ifelse(I_1_7_disp_arrive_date=="N/A",NA,I_1_7_disp_arrive_date), start=1,stop=4),
         HH_origin = case_when(
           I_1_1_disp_from == 1 ~ "Same ward",
           I_1_1_disp_from == 2 ~ "Same local government area",
           I_1_1_disp_from == 3 ~ "Same state",
           I_1_1_disp_from == 4 ~ "Different state in Nigeria",
           I_1_1_disp_from == 5 ~ "Outside Nigeria",
           TRUE ~ NA_character_),
         HH_region = region)

# welfare-measure
nigeria <- nigeria |> mutate(PERCAPITA = tc_imp)

# select variables
nigeria <- nigeria %>% select(ID, starts_with("I"), -starts_with("I_"), starts_with("HH_"), PERCAPITA, WT)

# unify direction of indicators: 1 for passing 
nigeria <- nigeria %>% 
  mutate(
    I1_sec_saf = ifelse(I1_sec_saf >= 1, 1, 0),
    I1_sec_night = ifelse(I1_sec_night >= 1, 1, 0),
    I1_sec_day = ifelse(I1_sec_day >= 1, 1, 0),
    I1_sec_inc = ifelse(I1_sec_inc <= 0, 1 ,0),
    I1_sec_rep = ifelse(I1_sec_rep >= 1, 1, 0),
    I1_sec_formal = ifelse(I1_sec_formal >= 1, 0, 1),
    I1_sec_easy= ifelse(I1_sec_easy >= 1, 0, 1),
    I1_sec_eff = ifelse(I1_sec_eff >= 1,1, 0),
    I2_feel_free = ifelse(I2_feel_free >= 1,1,0),
    I3_food_insecurity_scale = ifelse(I3_food_insecurity_scale>= mean(I3_food_insecurity_scale,na.rm = T),1,0), 
    I4_hous_indurable = ifelse(I4_hous_indurable <= 0, 1, 0),
    I4_hous_overcrowd = ifelse(I4_hous_overcrowd <= 0,1, 0),
    I4_hous_squat = ifelse(I4_hous_squat <= 0, 1,0),
    I4_hous_temporary = ifelse(I4_hous_temporary <= 0, 1, 0),
    I4_hous_ownrent = ifelse(I4_hous_ownrent >= 1, 1, 0),
    I4_land_tenure = ifelse(I4_land_tenure >= 1, 1, 0),
    I4_hous_water = ifelse(I4_hous_water >= 1, 1, 0),
    I4_hous_waccess = ifelse(I4_hous_waccess >= 1, 1, 0),
    I4_hous_toilet = ifelse(I4_hous_toilet >= 1, 1, 0),
    I5_med_access = ifelse(I5_med_access >= 1, 1, 0),
    I5_med_dist = ifelse(I5_med_dist <= mean(I5_med_dist,na.rm=T),1,0), 
    I6_edu_dist= ifelse(I6_edu_dist <= mean(I6_edu_dist,na.rm=T),1,0), 
    I6_edu_satis= ifelse(I6_edu_satis >= 1, 1, 0),
    I7_salary = ifelse(I7_salary >= 1, 1, 0),
    I8_poor = ifelse(I8_poor <= 0, 1, 0),
    I8_poor31  = ifelse(I8_poor31 <= 0, 1, 0),
    I8_poor125  = ifelse(I8_poor125 <= 0, 1, 0),
    I8_foodtotal= ifelse(I8_foodtotal <= mean(I8_foodtotal,na.rm=T),1,0), 
    I8_consume = ifelse( I8_consume>= mean(I8_consume,na.rm=T),1,0),
    I8_bank  = ifelse(I8_bank >= 1,1,0),  
    I8_market_dist = ifelse(I8_market_dist <= mean(I8_market_dist,na.rm=T),1,0),
    I9_hlp_right = ifelse(I9_hlp_right >= 1, 1, 0),
    I9_hlp_compensation = ifelse(I9_hlp_compensation >= 1, 1, 0),
    I10_doc_replace= ifelse(I10_doc_replace >= 1, 1, 0)
  )

# add household ID
nigeria <- nigeria %>% mutate(HHID = row_number())

# final dataset ----
nigeria