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

# 1.1 Victims of violence

hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Proportion experiencing a security incident
    I1_sec_inc = case_when(
      hargeisa127 == "No" ~ 1,
      hargeisa127 == "Yes" ~ 0),
    
    # Proportion feeling safe
    I1_sec_saf = case_when(
      hargeisa133 == "No" ~ 0,
      hargeisa133 == "Yes" ~ 1,
      grepl("Only to some extent",hargeisa133) == T ~ 0),
  )

# 1.2. Freedom of movement
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Problems visiting public places
    I2_move = case_when(
      hargeisa137 == "No" ~ 1,
      hargeisa137 == "Yes" ~ 0)
  )

# 2.1. Food security 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Inability to pay for food
    I3_pay_food = case_when(
      hargeisa52 == "No" ~ 1,
      hargeisa52 == "Yes" ~ 0),
    
    # Number of meals per day
    I3_meals = as.numeric(hargeisa59==3))

# 2.2 Shelter and housing 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Proportion living in overcrowded housing/shelter (> X persons per room)
    I4_hous_overcrowd = ifelse(hargeisa14/3 <= hargeisa103, 1, 0),
    
    # Proportion living in inadequate housing conditions (risk zones)
    I4_hous_inadequat = ifelse(hargeisa121 == "Yes"|
                                 hargeisa122 == "Yes"|
                                 hargeisa123 == "Yes", 0, 1),
    
    # Proportion having access to electricity
    I4_hous_elect = case_when(
      hargeisa124 == "Yes" ~ 1,
      hargeisa124 == "No" ~ 0),
    
    # Proportion having access to clean water
    I4_hous_water = ifelse(hargeisa126 == "Tank" | hargeisa126 == "Bottled/bought", 1, 0),
    
    # Proportion having flushing toilet in household
    I4_hous_toilet = as.numeric(hargeisa109=="Yes"),
    
    # Proportion having bath/shower in household
    I4_hous_bath = as.numeric(hargeisa108 == "Yes")
  )

# 2.3 Medical services 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    
    # Proportion with access to essential health care when needed. 
    I5_med_access = case_when(
      hargeisa67 == "Yes" & hargeisa73 == "Yes" ~ 1,
      hargeisa67 == "Yes" & hargeisa73 == "No" ~ 0),
    
    # Births attended by skilled health personnel within 
    I5_med_birth = case_when(
      hargeisa93 %in% c("Doctor","Nurse/midwife") ~ 1,
      hargeisa93 %in% c("Family member","Traditional birth attendant") ~ 0,
    ))

hargeisa_ind <- hargeisa_ind |> 
  mutate(
    # Child vaccinated
    I5_med_vac= case_when(
      hargeisa62 == "Yes" ~ 1, 
      hargeisa62 == "No" ~ 0)
  )

# 2.4 Education 
hargeisa_ind <- hargeisa_ind %>% 
  mutate(
    # Proportion children (5-17) is able to read
    I6_educ_child = case_when(hargeisa36 == "Yes" ~ 1,
                              hargeisa36 == "No" ~ 0),
    
    # Proportion children (5-17) ever attended school
    I6_ever_school = case_when(hargeisa37 == "Yes" ~ 1,
                               hargeisa37 == "No" ~ 0),
    
    # Proportion children (5-17) currently attending school
    I6_curr_school = case_when(hargeisa39 == "Yes" ~ 1,
                               hargeisa39 == "No" ~ 0),
    
    # proportion children (5-17) attending secondary school
    I6_sec_school = case_when(
      hargeisa40 %in% c("Secondary school","University", "Vocational")~1, 
      !is.na(hargeisa40) ~ 0)
  )

# 3.1 Employment and livelihoods
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Breadwinner in the family
    I7_breadwinner = ifelse(hargeisa166 > 0, 1, 0))

# 3.2 Economic security 
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Proportion capable of unexpected expenses without borrowing money
    I8_unexpect_expense = as.numeric(hargeisa60 == "No"), 
    
    # Proportion experiencing difficulties paying rent
    I8_rent_problems = case_when(
      hargeisa100 == "Tenant" & hargeisa101 == "Yes" ~ 0, 
      hargeisa100 == "Tenant" & hargeisa101 == "No" ~ 1),
    
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
hargeisa_hh <- hargeisa_hh %>% 
  mutate(
    # Proportion with documents to prove ownership of property left behind
    I9_hlp_doc =
      case_when(
        ID == 0 ~ 1,
        ID == 1 & hargeisa118 == "Yes" ~ 1,
        ID == 1 & (hargeisa118 == "No" | hargeisa118 == "Only partially") ~ 0),

    # Proportion with access to compensation mechanisms
    I9_hlp_access =
      case_when(
        ID == 0 ~ 1,
        ID == 1 & grepl("Yes", hargeisa119) ~ 1,
        ID == 1 & hargeisa119 == "No" ~ 0),

    # Proportion with effective restoration or compensation
    I9_hlp_restored =
      case_when(
        ID == 0 ~ 1,
        ID == 1 & hargeisa120 == "Yes" ~ 1,
        ID == 1 & str_detect(hargeisa120, "^No") ~ 0)
  )

# 5.1. Documentation 
hargeisa_ind <- hargeisa_ind %>% 
  mutate(
    # Proportion with documents to prove identity 
    I10_doc_id = case_when(hargeisa42 == "Has a certificate" |
                             hargeisa44 == "Yes" |
                             hargeisa45 == "Yes" |
                             hargeisa46 == "Yes" ~ 1,
                           hargeisa47 == "Yes" ~ 0),
    
    # Proportion with documents or access to replace missing documents if lost
    I10_doc_replace =  case_when(
      hargeisa50 == "Yes" ~ 1,
      hargeisa50 == "No" ~ 0,
      TRUE ~ I10_doc_id),
    
    # Proportion having a birth certificate
    I10_doc_birth = ifelse(hargeisa42 == "Has a certificate"|
                             hargeisa42 == "Registered at birth but no certificate", 1,0)
    
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

# convert numeric indicator to 1/0
hargeisa <- 
  hargeisa |> 
  mutate(I8_assets_num = as.numeric(I8_assets_num >= (hargeisa |> filter(ID == 0) |> pull(I8_assets_num) |> mean(na.rm = TRUE))))

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

# final dataset ----
hargeisa
