library(tidyverse)

# preparations ---------------------------------------------------------

# read data
hargeisa <- readxl::read_xlsx("Data/Hargeisa.xlsx", sheet = 3, guess_max = 15000)

# identify idps vs hosts
hargeisa <- hargeisa %>% 
  filter(hargeisa1 != "Refugee returnee") %>% 
  mutate(ID = ifelse(grepl("Displaced",hargeisa1)==T,1,0))

# add household weights
hargeisa <- hargeisa %>% rename(WT = hargeisa180)

# identify potential IASC indicators for each subcriteria -------------

# 1.1 Victims of violence

hargeisa <- hargeisa %>% 
  mutate(
    # Proportion experiencing a security incident
    I1_sec_inc = case_when(
      hargeisa127 == "No" ~ 0,
      hargeisa127 == "Yes" ~ 1,
      TRUE ~ NA_real_),
    # Proportion feeling safe
    I1_sec_saf = case_when(
      hargeisa133 == "No" ~ 0,
      hargeisa133 == "Yes" ~ 1,
      grepl("Only to some extent",hargeisa133) == T ~ 0,
      TRUE ~ NA_real_),
    # Proportion worried about theft, crime, or vandalism
    I1_sec_wor = case_when(
      is.na(hargeisa135) == T ~ 0,
      is.na(hargeisa136) == T ~ 0,
      is.na(hargeisa135) == F ~ 1,
      is.na(hargeisa136) == F ~ 1,
      TRUE ~ NA_real_
    )
  )

# 1.2. Freedom of movement
hargeisa <- hargeisa %>% 
  mutate(
    # Problems visiting public places
    I2_move = case_when(
      hargeisa137 == "No" ~ 0,
      hargeisa137 == "Yes" ~ 1,
      TRUE ~ NA_real_)
  )

# 2.1. Food security 
hargeisa <- hargeisa %>% 
  mutate(
    # Inability to pay for food
    I3_pay_food = case_when(
      hargeisa52 == "No" ~ 0,
      hargeisa52 == "Yes" ~ 1,
      TRUE ~ NA_real_),
    # Number of meals per day
    I3_meals = hargeisa59)

# 2.2 Shelter and housing 
hargeisa <- hargeisa %>% 
  mutate(
    # Proportion living in overcrowded housing/shelter (> X persons per room)
    I4_hous_overcrowd = ifelse(hargeisa14 > hargeisa103, 1, 0),
    
    # Proportion living in inadequate housing conditions (risk zones)
    I4_hous_inadequat = ifelse(hargeisa121 == "Yes"|
                                 hargeisa122 == "Yes"|
                                 hargeisa123 == "Yes", 1, 0),
    
    # Proportion having access to electricity
    I4_hous_elect = case_when(
      hargeisa124 == "Yes" ~ 1,
      hargeisa124 == "No" ~ 0,
      TRUE ~ NA_real_),
    
    # Proportion getting their drinking water from tank
    I4_hous_water = ifelse(hargeisa126 == "Tank", 1, 0),
    
    # Proportion having flushing toilet in household
    I4_hous_toilet = ifelse(hargeisa109 == "Yes",1,0),
    
    # Proportion having bath/shower in household
    I4_hous_bath = ifelse(hargeisa108 == "Yes",1,0)
  )

# 2.3 Medical services 
hargeisa <- hargeisa %>% 
  mutate(
    
    # Proportion with access to essential health care when needed. 
    I5_med_access = case_when(
      hargeisa67 == "Yes" & hargeisa73 == "Yes" ~ 1,
      hargeisa67 != "Yes" & hargeisa73 == "No" ~ 0,
      hargeisa67 == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Births attended by skilled health personnel within 
    I5_med_birth = case_when(
      hargeisa92 %in% c("Clinic","Hospital") ~ 1,
      hargeisa92 %in% c("Home","Other") ~ 0,
      hargeisa93 %in% c("Doctor","Nurse/midwife") ~ 1,
      hargeisa93 %in% c("Family member","Traditional birth attendant") ~ 0,
      TRUE ~ 1
    ),
    
    # Child vaccinated
    I5_med_vac= case_when(
      hargeisa62 == "Yes" ~ 1, 
      hargeisa62 == "No" ~ 0,
      hargeisa62 == "Don't know" ~ 0,
      is.na(hargeisa62) == T ~ 1, 
      TRUE ~ NA_real_
    ))

# 2.4 Education 
hargeisa <- hargeisa %>% 
  mutate(
    # Proportion children (5-17) is able to read
    I6_educ_child = case_when(hargeisa36 == "Yes" ~ 1,
                              hargeisa36 == "No" ~ 0,
                              is.na(hargeisa36)== T ~ 1),
    # Proportion children (5-17) ever attending school
    I6_ever_school = case_when(hargeisa37 == "Yes" ~ 1,
                               hargeisa37 == "No" ~ 0,
                               is.na(hargeisa37)== T ~ 1),
    # Proportion children (5-17) currently attending school
    I6_curr_school = case_when(hargeisa39 == "Yes" ~ 1,
                               hargeisa39 == "No" ~ 0,
                               is.na(hargeisa39)== T ~ 1),
    # proportion children (5-17) attending secondary school
    I6_sec_school = case_when(
      hargeisa40 %in% c("Secondary school","University")~1, 
      hargeisa38 %in% c("Secondary school","University")~1,
      is.na(hargeisa$hargeisa40) == T ~ 1, 
      is.na(hargeisa$hargeisa38) == T ~ 1, 
      TRUE ~ 0),
    # Proportion owning a mobile phone
    I6_mobile_phone = ifelse(hargeisa111 == "Yes",1,0)
  )

# 3.1 Employment and livelihoods
hargeisa <- hargeisa %>% 
  mutate(
    # Breadwinner in the family
    I7_breadwinner = ifelse(hargeisa166 > 0, 1, 0))

# 3.2 Economic security 
hargeisa <- hargeisa %>% 
  mutate(
    # Proportion capable of unexpected expenses without borrowing money
    I8_unexpect_expense = ifelse(hargeisa60 == "No",1,0), 
    # Proportion experiencing difficulties paying rent
    I8_rent_problems = case_when(
      hargeisa100 == "Tenant" & hargeisa101 == "Yes" ~ 1, 
      hargeisa100 == "Tenant" & hargeisa101 == "No" ~ 0, 
      hargeisa100 != "Tenant" ~ 0, 
      TRUE ~ NA_real_
    ),
    # Average number of assets
    I8_assets_num = 
      ifelse(hargeisa105 == "Yes",1,0)+
      ifelse(hargeisa106 == "Yes",1,0)+
      ifelse(hargeisa107 == "Yes",1,0)+
      ifelse(hargeisa108 == "Yes",1,0)+
      ifelse(hargeisa109 == "Yes",1,0)+
      ifelse(hargeisa110 == "Yes",1,0)+
      ifelse(hargeisa111 == "Yes",1,0)
  )

# 4.1 Property restitution and compensation 
hargeisa <- hargeisa %>% 
  mutate(
    # Proportion with documents to prove ownership of property left behind
    I9_hlp_doc = 
      case_when(
        ID == 0 ~ 1,
        ID == 1 & hargeisa118 == "Yes" ~ 1,
        ID == 1 & hargeisa118 != "Yes" ~ 0,
        hargeisa114 == "No" ~ 1, 
        TRUE ~ NA_real_),
    # Proportion with access to compensation mechanisms
    I9_hlp_access = 
      case_when(
        ID == 0 ~ 1,
        ID == 1 & grepl("Yes",hargeisa119)==T ~ 1,
        ID == 1 & grepl("Yes",hargeisa119)==F ~ 0,
        hargeisa114 == "No" ~ 1, 
        TRUE ~ NA_real_), 
    # Proportion with effective restoration or compensation
    I9_hlp_restored = 
      case_when(
        ID == 0 ~ 1,
        ID == 1 & hargeisa120 == "Yes" ~ 1,
        ID == 1 & hargeisa120 != "Yes" ~ 0,
        hargeisa114 == "No" ~ 1, 
        TRUE ~ NA_real_)
  )

# 5.1. Documentation 
hargeisa <- hargeisa %>% 
  mutate(
    # Proportion with documents to prove identity 
    I10_doc_id = ifelse(hargeisa42 == "Has a certificate"|
                          hargeisa44 == "Yes" |
                          hargeisa45 == "Yes" |
                          hargeisa46 == "Yes", 1, 0),
    
    # Proportion with documents or access to replace missing documents if lost
    I10_doc_replace =  case_when(
      hargeisa42 == "Has a certificate" ~ 1,  
      hargeisa42 == "Never registered" ~ 0,  
      hargeisa42 == "Registered at birth but no certificate" ~ 0, 
      hargeisa44 == "Yes" ~ 1,
      hargeisa44 == "No" ~ 0,
      hargeisa45 == "Yes" ~ 1,
      hargeisa45 == "No" ~ 0,
      hargeisa46 == "Yes" ~ 1,
      hargeisa46 == "No" ~ 0,
      hargeisa50 == "Yes" ~ 1,
      hargeisa50 == "No" ~ 0,
      TRUE ~ NA_real_),
    
    # Proportion having a birth certificate
    I10_doc_birth = ifelse(hargeisa42 == "Has a certificate"|
                             hargeisa42 == "Registered at birth but no certificate", 1,0)
    
  )


# prepare dataset for simulations -----------------------------------------------

# define household characteristics
hargeisa <- 
  hargeisa %>% 
  mutate(HH_disp_type = hargeisa1,
         HH_gender = hargeisa3,
         HH_origin_district = ifelse(is.na(hargeisa7)==T, "Unknown", hargeisa7),
         HH_clan = ifelse(is.na(hargeisa13)==T, "Unknown", hargeisa13),
         HH_depart_yr = case_when(
           hargeisa28 <= 1990 ~ "Before 1990",
           hargeisa28 > 1990 & hargeisa28 <= 2000 ~ "Between 1990 and 2000",
           hargeisa28 > 2000 & hargeisa28 <= 2010 ~ "Between 2000 and 2010",
           hargeisa28 > 2010  ~ "After 2010",
           TRUE ~ "Unknown"))

# select variables
hargeisa <- hargeisa %>% select(-contains("hargeisa"))

# unify direction of indicators: 1 for passing 
hargeisa <- hargeisa %>% 
  mutate(
    I1_sec_inc = ifelse(I1_sec_inc == 1, 0,1),
    I1_sec_saf = ifelse(I1_sec_saf == 1, 1,0),
    I1_sec_wor = ifelse(I1_sec_wor == 1, 0,1),
    I2_move = ifelse(I2_move == 1, 0,1),
    I3_pay_food = ifelse(I3_pay_food == 1, 0,1),
    I3_meals = ifelse(I3_meals < mean(I3_meals, na.rm = T), 0,1),
    I4_hous_overcrowd = ifelse(I4_hous_overcrowd ==1, 0, 1),
    I4_hous_inadequat = ifelse(I4_hous_inadequat ==1, 0, 1),
    I4_hous_elect = ifelse(I4_hous_elect == 1, 1, 0),
    I4_hous_water = ifelse(I4_hous_water ==1, 1, 0),
    I4_hous_toilet = ifelse(I4_hous_toilet == 1 , 1, 0),
    I4_hous_bath = ifelse(I4_hous_bath == 1 , 1, 0),
    I5_med_access = ifelse(I5_med_access ==1, 1, 0),
    I5_med_birth = ifelse(I5_med_birth == 1, 1, 0),
    I5_med_vac = ifelse(I5_med_vac == 1, 1, 0),
    I6_educ_child = ifelse(I6_educ_child ==1, 1, 0),
    I6_ever_school = ifelse(I6_ever_school == 1, 1, 0),
    I6_curr_school = ifelse(I6_curr_school == 1, 1, 0),
    I6_sec_school = ifelse(I6_sec_school ==1, 1, 0),
    I6_mobile_phone = ifelse(I6_mobile_phone == 1, 1, 0),
    I7_breadwinner = ifelse(I7_breadwinner ==1, 1, 0),
    I8_unexpect_expense = ifelse(I8_unexpect_expense == 1, 1, 0),
    I8_rent_problems = ifelse(I8_rent_problems == 1, 0, 1),
    I8_assets_num = ifelse(I8_assets_num >= mean(I8_assets_num, na.rm= T), 1, 0),
    I9_hlp_doc = ifelse(I9_hlp_doc ==1, 1, 0),
    I9_hlp_access = ifelse(I9_hlp_access ==1, 1, 0),
    I9_hlp_restored = ifelse(I9_hlp_restored ==1, 1, 0),
    I10_doc_id = ifelse(I10_doc_id ==1, 1, 0),
    I10_doc_replace = ifelse(I10_doc_replace == 1, 1, 0),
    I10_doc_birth = ifelse(I10_doc_birth == 1, 1, 0)
  )

# add household ID
hargeisa <- hargeisa %>% mutate(HHID = row_number())

# final dataset ----
hargeisa