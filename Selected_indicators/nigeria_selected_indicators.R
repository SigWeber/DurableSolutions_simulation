library(RM.weights)
library(tidyverse)


# preparations ---------------------------------------------------------

# read data
# sorry for setting the wd so ugly, work in progres :-D 
#setwd("C:/Users/sigri/Dropbox/DurableSolutions_simulation")
nigeria <- read_csv("Data/Nigeria.csv", guess_max = 10000)

# identify idps vs hosts
nigeria <- nigeria %>% rename(ID = migr_idp)

# add household weights
nigeria <- nigeria %>% rename(WT = weight)

# identify potential IASC indicators for each subcriteria -------------

# All indicators are coded as 1 if solution "achieved" and zero if not!!!

# 1.1 Victims of violence
nigeria<- nigeria %>% 
  mutate(
    # Proportion feeling very or moderately safe walking at night or during the day
    I1_SDG_16.1.4 = case_when(
      H_4_2_safe_walking_night == 1 ~ 1,
      H_4_2_safe_walking_night == 2 ~ 1,
      H_4_3_safe_walking_day == 1 ~ 1,
      H_4_3_safe_walking_day == 2 ~ 1,
      H_4_3_safe_walking_day %in% 3:5 |H_4_2_safe_walking_night %in% 3:5 ~ 0,
      TRUE ~ NA_real_))

# 1.2. Freedom of movement
nigeria <- nigeria %>% 
  mutate(
    # Feeling free to move 
    I2_DS_1.4.1 = H_4_1_move_free
  )

# 2.1. Food security 

# Food Insecurity Experience Scale (using FAO package)

# identify columns and turn them into binary indicators if any vulnerability appears
food_insecurity_components <- nigeria %>% 
  select(c("C_4_1_nomoney","C_4_2_cop_lessprefrerred","C_4_3_cop_borrow_food",
           "C_4_4_cop_limitportion","C_4_5_cop_limitadult","C_4_6_cop_reducemeals",
           "C_4_7_cop_sellassets", "C_4_8_cop_sellfem")) %>% 
  summarise_all(~ifelse(.>1,1,.)) %>% 
  mutate(I3_DS_2.1.2 = rowSums(.))

nigeria$I3_DS_2.1.2 <- (8-food_insecurity_components$I3_DS_2.1.2) # high values are high food security!

# Fixing needed: fit weighted Rasch - I have no idea if and how the Rasch model is broken down to the individual level,
# currently just use an ordinal scale
#rr <- RM.w(food_insecurity_components[,1:8],nigeria$WT)
# pp <- prob.assign(rr, sthres = seq(-5, 5, 0.01))

# 2.2 Shelter and housing 


# identify components of sdg indicator: all coded as 1 if NOT vulnerable
nigeria <- nigeria %>% 
  mutate(
    
    # Security of tenure: Proportion being legally recognized owner of dwelling and having a formal document to proof it
    SDG_secure_tenure = case_when(
      C_1_9_land_legal_main == 1 & C_1_10_land_legal_main_d %in% c(1,2) ~ 1,
      C_1_9_land_legal_main == 1 & C_1_10_land_legal_main_d %in% 3  ~ 0,
      C_1_9_land_legal_main == 0  ~ 0,
      is.na(C_1_9_land_legal_main) == T ~ 0,
      TRUE ~ NA_real_),
    
    # Access to improved water sources
    SDG_improved_water = ifelse(watersource == 1, 1,0),
    
    # Access to improved sanitation facilities
    SDG_improved_sanitation = ifelse(sanitation == 1,1,0),
    
    # Structural quality of housing and location: no indicator present on hazardious zones, housing on garbage or in high industrial pollution area
    
    #Structural quality of the housing and permanency of the structure:
    SDG_permanency = case_when(
      # living in non-durable (incomplete, not intended, makeshift)
      C_1_1_housingtype >= 11 ~ 0, 
      C_1_1_housingtype <= 11 ~ 1, 
      # squatting or living in temporary shelter by UNHCR
      C_1_7_tenure %in% c(1,2,7,8,9) ~ 0,
      C_1_7_tenure == 1000  ~ NA_real_,
      C_1_7_tenure %in% c(3,4,5,6) ~ 1,
      TRUE ~ NA_real_),
    
    # Sufficient living area: Proportion of households in which not more than three people share the same habitable room
    SDG_sufficient_space = ifelse(hhdensity > 3 ,0, 1)
    )
    

# combine to one indicator 
nigeria <- nigeria %>% 
  mutate(
    I4_SDG_11.1.1 = ifelse(rowSums((nigeria %>% select(starts_with("SDG_"))) == 0) > 0, 0, 1)
    )
    

# 2.3 Medical services 
nigeria <- nigeria %>% 
  mutate(
    
    # Target population who accessed essential health care services the last time they needed it in the past 12 months
    I5_DS_2.1.8 = case_when(
      C_4_10_disease_yn == 0 ~ 1,
      C_4_10_disease_yn == 1 & C_4_12_med_yn == 1 ~ 1,
      C_4_10_disease_yn == 1 & C_4_12_med_yn == 0 ~ 0,
      TRUE ~ NA_real_)
  )
    

# 2.4 Education
nigeria <- nigeria %>% 
  mutate(
    
    # SDG indicator 4.1.2: Completion rate (primary education): not present, 
    # approximated with whether any household member is going to primary school 
    I6_SDG_4.1.2 = ifelse(H_2_7_school_satisfaction == 0, 0, 1)

  )


# 3.1 Employment and livelihoods
nigeria <- nigeria %>% 
  mutate(
    
    # Unemployment proxied with those who generate income from work
  
    I7_SDG_8.5.2 = ifelse(C_5_1_lhood %in% c(1,2,3,4,5,8),1,0)
  )


# 3.2 Economic security 
nigeria <- nigeria %>% 
  mutate(
    #Poverty
    I8_SDG_8.5.2 = ifelse(poor ==1,0,1)
  )
    

# 4.1 Property restitution and compensation /security of tenure
nigeria <- nigeria %>% 
  mutate(
    # Security of tenure
    I9_SDG_1.4.2 = SDG_secure_tenure
  )

# 5.1. Documentation 
nigeria  <- nigeria  %>% 
  mutate(
    
    # DS Library indicator 5.1.1: Target population currently in possession of documentation 
    I10_DS_5.1.1 =  case_when(
      ID == 0 ~ 1,
      H_2_9_legal_id_disp == 0 ~ 1,
      H_2_9_legal_id_disp == 1 & H_2_10_legal_id_acc_disp == 1 ~ 1,
      H_2_9_legal_id_disp == 1 & H_2_10_legal_id_acc_disp == 0 ~ 0,
      TRUE ~ NA_real_)
    
  )


# prepare dataset for simulations -----------------------------------------------

# define household characteristics
nigeria <- 
  nigeria %>% 
  mutate(HH_disp_year = substr(ifelse(I_1_3_disp_date=="N/A",NA,I_1_3_disp_date), start=1,stop=4),
         HH_arriv_year = substr(ifelse(I_1_7_disp_arrive_date=="N/A",NA,I_1_7_disp_arrive_date), start=1,stop=4),
         HH_origin = case_when(
           I_1_1_disp_from == 1 ~ "Same ward",
           I_1_1_disp_from == 2 ~ "Same local government area",
           I_1_1_disp_from == 3 ~ "Same state",
           I_1_1_disp_from == 4 ~ "Different state in Nigeria",
           I_1_1_disp_from == 5 ~ "Outside Nigeria",
           TRUE ~ NA_character_),
         HH_region = region)

# select variables
nigeria <- nigeria %>% select(ID, starts_with("I"), -starts_with("I_"), starts_with("HH_"), WT)

# add household ID
nigeria <- nigeria %>% mutate(HHID = row_number())

# report difficulty of indicators
difficulty <- bind_rows(
  nigeria %>% filter(ID==1) %>% select(starts_with("I"),WT) %>% select(-ID) %>% 
  summarise_all(.,~weighted.mean(., WT, na.rm = TRUE)) %>% mutate(Stat="Difficulty"),
  nigeria %>% filter(ID==1) %>% select(starts_with("I"),WT) %>% select(-ID) %>% 
    summarise_all(.,~sum(WT*is.na(.))/sum(WT),na.rm=T) %>% mutate(Stat="Missingness")) %>% 
  pivot_longer(.,cols=c(starts_with("I")),names_to = "Indicator",values_to = "Value") %>% 
  pivot_wider(.,id_cols = "Indicator",names_from ="Stat",values_from = "Value") %>% 
  mutate(Difficulty = round(Difficulty*100,2),Missingness = round(Missingness*100,2)) %>% 
  mutate(Difficulty = ifelse(Indicator == "I3_DS_2.1.2", NA,Difficulty))
#write_csv(difficulty, "Selected_indicators/nigeria_difficulty.csv")

# run simulations --------------------------------------------------------------
source("simulations.R")
simulation_nigeria <- bind_rows(
  simulate_IRIS_metric(nigeria) %>% mutate(metric="Pass/fail"),
  simulate_composite(nigeria)%>% mutate(metric="1: Full composite"),
  simulate_criterion(nigeria)%>% mutate(metric="2: Composite at criterion level"),
  cbind((simulate_IRIS_metric(nigeria))[1,1:10],data.frame(Durable_Solutions = NA,DS=0,DS_perc=0))%>% 
    mutate(metric="3: Composite at subcriterion level"),
  simulate_cells(nigeria) %>% 
    group_by(across(starts_with("I"))) %>% 
    summarise(DS=mean(DS),DS_perc=mean(DS_perc))%>% mutate(metric="4: Homogeneous cells"),
  simulate_classifier(nigeria)%>% mutate(metric="4: Classifier/regression-based"))%>% 
  mutate(`Percentage exiting the stock`=round(DS_perc*100,2),
         `IDP households exiting the stock`=round(DS)) %>% 
  select(metric,`IDP households exiting the stock`,`Percentage exiting the stock`) %>% 
  rename(`Metric option`=metric)

#write_csv(simulation_nigeria, "Selected_indicators/nigeria_selected_indicators.csv")
