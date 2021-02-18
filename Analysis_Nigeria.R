##########################################################
# Project: Simulation of IASC criteria - Nigeria
# Author: Sigrid Weber (sweber1@worldbank.org)
# Last updated: November 27, 2020
##########################################################


# preparations ---------------------------------------------------------

# load required packages
library(tidyverse)
library(doParallel)
library(hrbrthemes)
library(patchwork)

# load functions
source("functions.R")

# read data
nigeria <- read_csv("Data/Nigeria.csv")

# identify idps vs hosts
nigeria <- nigeria %>% rename(ID = migr_idp)

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

# select variables
nigeria <- nigeria %>% select(ID, starts_with("I"), -starts_with("I_"),
                              region, 
                              I_1_3_disp_date, 
                              I_1_7_disp_arrive_date,
                              I_1_1_disp_from) %>% 
  rename(disp_date =  I_1_3_disp_date,
         arriv_date = I_1_7_disp_arrive_date,
         origin = I_1_1_disp_from)


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

# save a version for later use in script
nigeria_raw <- nigeria

# divide into IDPs and benchmark
nigeria <- nigeria %>% select(ID, starts_with("I"), -starts_with("I_"))
benchmarks <- nigeria %>% filter(ID == 0)
nigeria <- nigeria %>% filter(ID == 1)

# see for how many no assessment is possible
naniar::vis_miss(nigeria)

# SIMULATIONS ############################################

# Original framework ----------------------------------------------------------

# define the indicators
indicators <- list(
  I1 = names(nigeria %>% select(contains("I1_"))),
  I2 = names(nigeria %>% select(contains("I2_"))),
  I3 = names(nigeria %>% select(contains("I3_"))),
  I4 = names(nigeria %>% select(contains("I4_"))),
  I5 = names(nigeria %>% select(contains("I5_"))),
  I6 = names(nigeria %>% select(contains("I6_"))),
  I7 = names(nigeria %>% select(contains("I7_"))),
  I8 = names(nigeria %>% select(contains("I8_"))),
  I9 = names(nigeria %>% select(contains("I9_"))),
  I10 = names(nigeria %>% select(contains("I10_")))
)

# identify all possible combinations
combinations <- expand.grid(indicators)%>% 
  mutate_all(~as.character(.))


# Identify durable solution according to IRIS framework
Durable_Solutions <- sapply(1:nrow(combinations),
                            data = nigeria, # Dataset
                            sim_data = combinations, # Dataset of possible indicator combinations
                            use_IRIS_metric # Function
                            )

# Save for analysis and plotting
DS_original <- combinations %>% 
  mutate(DS = Durable_Solutions,
         DS_perc = DS/ nrow(nigeria))


# Plot result 
plot_original <- DS_original %>% 
  ggplot(aes(x=DS_perc))+
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8)+
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  geom_vline(aes(xintercept = mean(DS_perc)), 
             linetype = "dashed", size = 0.6, alpha = 0.5)+
  ggtitle("Pass/fail measure")+
  xlab("Simulated proportion overcoming vulnerabilities")+
  ylab("Simulation density")+
  xlim(0,0.5)

plot_original

model <- broom::tidy(lm((DS_perc*100) ~ I1 + I4+I5+I6 + I8+I9, data= DS_original))
model <- model %>% 
  mutate(ymin = estimate - (2 * std.error),
         ymax = estimate + (2 * std.error)) %>% 
  filter(term != "(Intercept)")

# Option 1: Full composite ---------------------------------------------------

Durable_Solutions <- sapply(1:nrow(combinations),
                            data = nigeria,
                            sim_data = combinations,
                            benchmark = benchmarks,
                            use_composite)

DS_option1 <- combinations %>% 
  mutate(DS = Durable_Solutions,
         DS_perc = DS/ nrow(nigeria))

# Plot result 
plot_option1 <- DS_option1 %>% 
  ggplot(aes(x=DS_perc))+
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8)+
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  geom_vline(aes(xintercept = mean(DS_perc)), 
             linetype = "dashed", size = 0.6, alpha = 0.5)+
  ggtitle("1: Full composite")+
  xlab("Simulated proportion overcoming vulnerabilities")+
  ylab("Simulation density")+
  xlim(0,0.6)

plot_option1


model1 <- broom::tidy(lm((DS_perc*100) ~ I1+I4+I5+I6+I8+I9, data= DS_option1))
model1 <- model1 %>% 
  mutate(ymin = estimate - (2 * std.error),
         ymax = estimate + (2 * std.error)) %>% 
  filter(term != "(Intercept)")

# Option 2: composite indices at the criterion level ###########################################

Durable_Solutions <- sapply(1:nrow(combinations),
                            data = nigeria,
                            sim_data = combinations,
                            benchmark = benchmarks,
                            use_criterion)

DS_option2 <- combinations %>% 
  mutate(DS = Durable_Solutions,
         DS_perc = DS/ nrow(nigeria))

# Plot result 
plot_option2 <- DS_option2 %>% 
  ggplot(aes(x=DS_perc))+
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8)+
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  geom_vline(aes(xintercept = mean(DS_perc)), 
             linetype = "dashed", size = 0.6, alpha = 0.5)+
  ggtitle("2: Composite at criterion level")+
  xlab("Simulated proportion overcoming vulnerabilities")+
  ylab("Simulation density")+
  xlim(0,0.5)

plot_option2

model2 <- broom::tidy(lm((DS_perc*100) ~ I1+I4+I5+I6+I8+I9, data= DS_option2))
model2 <- model2 %>% 
  mutate(ymin = estimate - (2 * std.error),
         ymax = estimate + (2 * std.error)) %>% 
  filter(term != "(Intercept)") 

# Option 3: composite indices at the subcriterion level ###########################################

# define indices for the levels with few indicators available
nigeria <- nigeria %>% 
  mutate(I2 = I2_feel_free,
         I3 = I3_food_insecurity_scale,
         I5 = I5_med_access + I5_med_dist, 
         I6 = I6_edu_dist+I6_edu_satis,
         I7 = I7_salary,
         I9 = I9_hlp_right + I9_hlp_compensation,
         I10 = I10_doc_replace)

benchmarks <- benchmarks %>% 
  mutate(I2 = I2_feel_free,
         I3 = I3_food_insecurity_scale,
         I5 = I5_med_access + I5_med_dist, 
         I6 = I6_edu_dist+I6_edu_satis,
         I7 = I7_salary,
         I9 = I9_hlp_right + I9_hlp_compensation,
         I10 = I10_doc_replace)


# define the new indicators
indicators_sub <- list(
  index1 = apply((combn(names(nigeria %>% select(contains("I1_"))),3)) %>% t(),1, paste,collapse= ";"),
  index4 = apply((combn(names(nigeria %>% select(contains("I4_"))),3)) %>% t(),1, paste,collapse= ";"),
  index8 = apply((combn(names(nigeria %>% select(contains("I8_"))),3)) %>% t(),1, paste,collapse= ";")
  )


# define the new combinations
combinations <- expand.grid(indicators_sub)%>% 
  mutate_all(~as.character(.)) %>%
  separate(col = index1, into = c("I1_Index_1", "I1_Index_2","I1_Index_3"), sep = ";") %>% 
  separate(col = index4, into = c("I4_Index_1", "I4_Index_2","I4_Index_3"), sep = ";") %>% 
  separate(col = index8, into = c("I8_Index_1", "I8_Index_2","I8_Index_3"), sep = ";") %>% 
  mutate(I2 = "I2",I3 = "I3",I5 = "I5", I6 = "I6",I7 = "I7", I9 = "I9", I10 = "I10")


set.seed(2020)
combinations <- sample_n(combinations, 10000)

# run parallel as otherwise slow
cl <- parallel::makeCluster(4)
registerDoParallel(cl)
Durable_Solutions <- foreach(i = 1:nrow(combinations), 
                             .packages = "tidyverse") %dopar% {
                               use_subcriterion(x = i,
                                                data = nigeria,
                                                sim_data = combinations,
                                                benchmark = benchmarks)
                             }

stopCluster(cl)

Durable_Solutions <- unlist(Durable_Solutions)
DS_option3 <- combinations %>% 
  mutate(DS = Durable_Solutions,
         DS_perc = DS/ nrow(nigeria))


# Plot result 
plot_option3 <- DS_option3 %>% 
  ggplot(aes(x=DS_perc))+
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8)+
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  geom_vline(aes(xintercept = mean(DS_perc)), 
             linetype = "dashed", size = 0.6, alpha = 0.5)+
  ggtitle("3: Composite at sub-criterion level")+
  xlab("Simulated proportion overcoming vulnerabilities")+
  ylab("Simulation density")
plot_option3

# Option 4: Comparison of homogenous cells ###########################################

cells <- nigeria_raw %>% 
  mutate(disp_month = substr(ifelse(disp_date== "N/A",NA,disp_date), start=1,stop=7),
         disp_year = substr(ifelse(disp_date== "N/A",NA,disp_date), start=1,stop=4),
         arriv_month = substr(ifelse(arriv_date== "N/A",NA,arriv_date), start=1,stop=7),
         arriv_year = substr(ifelse(arriv_date== "N/A",NA,arriv_date), start=1,stop=4),
         origin = case_when(
           origin == 1 ~ "Same ward",
           origin == 2 ~ "Same local government area",
           origin == 3 ~ "Same state",
           origin == 4 ~ "Different state in Nigeria",
           origin == 5 ~ "Outside Nigeria",
           TRUE ~ NA_character_)) %>% 
  select(-disp_date,-arriv_date)

benchmarks <- cells %>% 
  filter(ID == 0)%>% 
  select(starts_with("I"), -starts_with("I_")) %>% 
  summarise_all(.,mean,na.rm = T)

cell_options <- list(
  arriv_date_1 = c("arriv_year","arriv_month"),
  disp_date_1 = c("disp_year","disp_month"),
  location_1 = c("region", "origin"))

# identify all possible combinations
combinations <- expand.grid(cell_options)%>% 
  mutate_all(~as.character(.))

combinations1 <- expand.grid(indicators)%>% 
  mutate_all(~as.character(.))

# run simulation
cl <- parallel::makeCluster(4)
registerDoParallel(cl)

Durable_Solutions <- foreach(i = 1:nrow(combinations1), .packages = "tidyverse") %dopar% {
  sapply(1:nrow(combinations),
         y=i,
         data = cells, 
         combination_cells= combinations,
         combination_indicators=combinations1,
         benchmark= benchmarks,
         use_cells)
}

stopCluster(cl)

# make the indicators dataframe
DS_option4_ind <- data.frame(matrix(unlist(Durable_Solutions), 
                                   nrow=length(Durable_Solutions), 
                                   byrow=T))
names(DS_option4_ind) <- paste0("DS_",1:8)

DS_option4_ind <- cbind(combinations1,DS_option4_ind)
DS_option4_ind <- DS_option4_ind %>% 
  pivot_longer(.,cols= starts_with("DS_"),values_to = "DS", names_to = "iteration")%>% 
  mutate( DS_perc = DS/ nrow(nigeria))

# make the cell indicator dataframe
DS_option4_cells <- cbind(
  combinations, 
  data.frame(matrix(unlist(Durable_Solutions), nrow=8, byrow=F))) 

DS_option4_cells <- DS_option4_cells%>% 
  pivot_longer(.,cols= starts_with("X"),values_to = "DS", names_to = "iteration") %>% 
  mutate( DS_perc = DS/ nrow(nigeria))

# Plot result 
plot_option4 <- DS_option4_cells %>% 
  ggplot(aes(x=DS_perc))+
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8)+
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  geom_vline(aes(xintercept = mean(DS_perc)), 
             linetype = "dashed", size = 0.6, alpha = 0.5)+
  ggtitle("4: Comparison of homogenous cells")+
  xlab("Simulated proportion overcoming vulnerabilities")+
  ylab("Simulation density")+
  xlim(0,0.5)
plot_option4

# Check impact of indicators
model4 <- broom::tidy(lm((DS_perc*100) ~ I1+I4+I5+I6+I8+I9, data= DS_option4_ind))
model4 <- model4 %>% 
  mutate(ymin = estimate - (2 * std.error),
         ymax = estimate + (2 * std.error)) %>% 
  filter(term != "(Intercept)") 

# Option 5: Use a classifier ------------------------------------------------------------

# define the indicators
indicators <- list(
  I1 = names(nigeria_raw %>% select(contains("I1_"))),
  I2 = names(nigeria_raw  %>% select(contains("I2_"))),
  I3 = names(nigeria_raw  %>% select(contains("I3_"))),
  I4 = names(nigeria_raw  %>% select(contains("I4_"))),
  I5 = names(nigeria_raw  %>% select(contains("I5_"))),
  I6 = names(nigeria_raw  %>% select(contains("I6_"))),
  I7 = names(nigeria_raw  %>% select(contains("I7_"))),
  I8 = names(nigeria_raw  %>% select(contains("I8_"))),
  I9 = names(nigeria_raw  %>% select(contains("I9_"))),
  I10 = names(nigeria_raw  %>% select(contains("I10_")))
)


# identify all possible combinations
combinations <- expand.grid(indicators)%>% 
  mutate_all(~as.character(.))

# Identify durable solution 
# run parallel as otherwise slow
cl <- parallel::makeCluster(4)
registerDoParallel(cl)
Durable_Solutions <- foreach(i = 1:nrow(combinations), 
                             .packages = "tidyverse") %dopar% {
                               use_classifier(x = i,
                                              data = nigeria_raw,
                                              sim_data = combinations)
                             }

stopCluster(cl)

Durable_Solutions <- unlist(Durable_Solutions)

# Save for analysis and plotting
DS_Option5 <- combinations %>% 
  mutate(DS = Durable_Solutions,
         DS_perc = DS/ nrow(nigeria))

# Plot result 
plot_option5 <- DS_Option5 %>% 
  ggplot(aes(x=DS_perc))+
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8)+
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  geom_vline(aes(xintercept = mean(DS_perc)), 
             linetype = "dashed", size = 0.6, alpha = 0.5)+
  ggtitle("5: Classifier/ regression-based")+
  xlab("Simulated proportion overcoming vulnerabilities")+
  ylab("Simulation density")+
  xlim(0,0.6)

plot_option5

model5 <- broom::tidy(lm((DS_perc*100) ~ I1+I4+I5+I6+I8+I9, data= DS_Option5))
model5 <- model5 %>% 
  mutate(ymin = estimate - (2 * std.error),
         ymax = estimate + (2 * std.error)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(category = gsub("([0-9]+).*$", "\\1", term),
         term = sub(".*?_","",term)) %>% 
  mutate(term = factor(term, levels=rev(term), ordered=TRUE))


# VISUALIZATION ########################################

(plot_original + plot_option1) / 
  (plot_option2 + plot_option3)/ 
  (plot_option4 + plot_option5)
ggsave("visualization/Nigeria_option.png",
       width = 11, height = 8)

indicator_data <- data.frame(
  term = model$term,
  estimate =
    sapply(1:24,  
           function(x){
             mean(model$estimate[x],model1$estimate[x],model2$estimate[x],
                  model4$estimate[x],model5$estimate[x])}),
  ymin =
    sapply(1:24,  
           function(x){
             mean(model$ymin[x],model1$ymin[x],model2$ymin[x],
                  model4$ymin[x],model5$ymin[x])}),
  ymax =
    sapply(1:24,  
           function(x){
             mean(model$ymax[x],model1$ymax[x],model2$ymax[x],
                  model4$ymax[x],model5$ymax[x])})
  
)%>% 
  mutate(term = factor(term, levels=rev(term), ordered=TRUE)) %>% 
  mutate(term1 = 
           c("1.1: Access to dispute resolution","1.1: Effective dispute resolution","1.1: Formal dispute resolution","1.1: Experience security incident", "1.1: Feeling safe at night","1.1: Report security incident","1.1: Feeling safe",
             "2.2: No overcrowded housing","2.2: Owning/renting house","2.2: No squatting","2.2: Temporary accommendation","2.2: Toilet facilities","2.2: No obstacles to water", "2.2: Water access","2.2. Legally own tenure",
              "2.3: Distance to health facility", 
             "2.4: Satisfied with school",    
             "3.2: Consume above average", "3.2: Low food to total ratio","3.2: Distance to market","3.2: Below 1.9 USD Poverty Line","3.2: Below 1.25 USD Poverty Line","3.2: Below 3.1 USD Poverty Line",
             "4.1: Legally own dwelling"
           ))%>% 
  mutate(term1 = factor(term1, levels=rev(term1), ordered=TRUE))


ggplot(indicator_data, 
       aes(x=term1, y=estimate)) + 
  geom_hline(yintercept=0, colour="#8C2318", size=1) +  # Line at 0
  geom_pointrange(aes(ymin=ymin, ymax=ymax)) + 
  labs(x="Indicator", y="Average effect estimate across metrices (in percentages of IDP stock)") +  # Labels
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_rect(aes(ymin=0.4, ymax= 0.82, xmin=0, xmax=Inf),
            fill= "lightskyblue3",
            alpha = 0.01) +
  geom_rect(aes(ymin=-0.21, ymax= 0.23, xmin=0, xmax=Inf),
            fill= "lightskyblue4",
            alpha = 0.01) +
  geom_rect(aes(ymin=1.1, ymax= 1.43, xmin=0, xmax=Inf),
            fill= "lightskyblue",
            alpha = 0.01) + 
  coord_flip() +  # Rotate the plot
  theme_ipsum(plot_title_size = 13, base_size = 10)+
  theme(legend.position = "none")

ggsave("visualization/Nigeria_indicators.png",width = 8, height = 5)

