library(tidyverse)
library(haven)
library(fs)

# Data from: COLOMBIA - Encuesta Nacional de Calidad de Vida - ECV 2019 - http://microdatos.dane.gov.co/index.php/catalog/678/study-description
# Poverty lines from: Pobreza monetaria en Colombia (Año 2019) - https://www.dane.gov.co/files/investigaciones/condiciones_vida/pobreza/2019/Boletin-pobreza-monetaria_2019.pdf

# preparations ---------------------------------------------------------
# read data
datasets <- dir_ls("Data/Colombia-ECV2019", glob = "*.dta") %>% map(compose(as_factor, read_dta))

# bind together datasets with the same unit of analysis
dwellings <- datasets %>% keep(~all(.$SECUENCIA_P == 1) & all(.$SECUENCIA_ENCUESTA == 1)) %>% reduce(full_join)
households <- datasets %>% keep(~all(.$SECUENCIA_P == 1) & !all(.$SECUENCIA_ENCUESTA == 1)) %>% reduce(full_join)
persons <- datasets %>% keep(~!all(.$SECUENCIA_P == 1) & !all(.$SECUENCIA_ENCUESTA == 1)) %>% reduce(full_join)

# harmonize key variables - notice how SECUENCIA_ENCUESTA changes meaning between datasets
persons <- persons %>% rename(dwelling = DIRECTORIO, household = SECUENCIA_P, person = SECUENCIA_ENCUESTA, WT = FEX_C)
households <- households %>% rename(dwelling = DIRECTORIO, household = SECUENCIA_ENCUESTA, WT = FEX_C)
dwellings <- dwellings %>% rename(dwelling = DIRECTORIO, WT = FEX_C)

# identify IDPs
persons <- 
  persons %>% 
  mutate(ID = !str_detect(P756, "otro país") & str_detect(P6096, "(riesgo para su vida|desastre natural)"),
         ID = replace_na(ID, FALSE))

# identify potential IASC indicators for each subcriteria -------------

# All indicators are coded as 1 if solution "achieved" and zero if not!!!

# 1.1 Victims of violence
persons <- persons %>% 
  mutate(
    # Proportion feeling very or moderately safe walking at night or during the day:
    # approximated with satisfaction with current level of security
    I1_SDG_16.1.4 = str_detect(P1898, "([6-9]|'Totalmente satisfecho\\(a\\))"))


# 1.2 Freedom of movement
## Not applicable...

# 2.1. Food security 
households <- 
  households %>% 
  mutate(
    # Living above the food security line
    I3_DS_2.1.2  = PERCAPITA > 137350)

# 2.2 Shelter and housing 
households <- households %>% 
  mutate(
    
    # Security of tenure: Proportion being legally recognized owner of dwelling and having a formal document to proof it
    I4_SDG_secure_tenure = !str_detect(P5095, "(hecho|colectiva)"),
    
    
    # Structural quality of housing and location: no indicator present on hazardious zones, housing on garbage or in high industrial pollution area
    
    #Structural quality of the housing and permanency of the structure:
 #   SDG_permanency 
 
    # Sufficient living area: Proportion of households in which not more than three people share the same habitable room
    I4_SDG_sufficient_space = CANT_PERSONAS_HOGAR/P5000 <= 3
  )

dwellings <- 
  dwellings %>% 
  mutate(
    # Access to improved water sources
    I4_SDG_improved_water = P8520S5 != "No",
    
    # Access to improved sanitation
    I4_SDG_improved_sanitation = P8520S5 != "No")


# 2.3 Medical services 
persons <- 
  persons %>% 
  mutate(
    # Target population who accessed essential health care services the last time they needed it in the past 12 months
    # Approximated with Satisfaction with current level of health
    I5_DS_2.1.8 = str_detect(P1897, "([6-9]|'Totalmente satisfecho\\(a\\))"))

# 2.4 Education
persons <- 
  persons %>% 
  mutate(
    # SDG indicator 4.1.2: Completion rate (primary education):  School attendance
    I6_SDG_4.1.2 = P8586 != "No")

# 3.1 Employment and livelihoods
persons <- 
  persons %>% 
  mutate(
    # Unemployment: Not unemployed
    I7_SDG_8.5.2 = P6240 != "Buscando trabajo")

# 3.2 Economic security
households <- 
  households %>% 
  mutate(
    # Living above the national poverty line
    I8_SDG_8.5.2 = PERCAPITA > 327674)

# 4.1 Property restitution and compensation 
households <- households %>% 
  mutate(I9_SDG_1.4.2 = I4_SDG_secure_tenure)

# 5.1. Documentation 
persons <- 
  persons %>% 
  mutate(
    # DS Library indicator 5.1.1: Target population currently in possession of documentation 
    I10_DS_5.1.1 = P1894 != "No tiene documento de identidad")

# prepare dataset for simulations -----------------------------------------------

# aggregate individual level data
# FIXME: we're splitting households to separate IDPs from non-IDPs. Is this the right thing to do?
colombia_ind_children <- 
  persons |> 
  select(ID, dwelling, household, age = P6040, matches("^I6")) |> 
  filter(between(age, 5, 17)) |> 
  group_by(ID, dwelling, household) |> 
  summarize(across(matches("^I6"), compose(as.numeric, any)))

colombia_ind_workingage <- 
  persons |> 
  select(ID, dwelling, household, age = P6040, matches("^I(4|7|8)")) |> 
  filter(between(age, 15, 64)) |> 
  group_by(ID, dwelling, household) |> 
  summarize(across(matches("^I(4|7|8)"), compose(as.numeric, any)))

colombia_ind_adults <- 
  persons |> 
  select(ID, dwelling, household, age = P6040, matches("^I(1|5|10)")) |> 
  filter(age >= 15) |> 
  group_by(ID, dwelling, household) |> 
  summarize(across(matches("^I(1|5|10)"), compose(as.numeric, any)))

colombia_ind <- 
  persons |> 
  distinct(ID, dwelling, household) |> 
  list(colombia_ind_children, 
       colombia_ind_adults,
       colombia_ind_workingage) |> 
  reduce(left_join)

# build household dataset
colombia_hh <- 
  left_join(households %>% select(dwelling, household, starts_with("I"), -starts_with("I_")),
            dwellings %>% select(dwelling, starts_with("I")))

# merge everything into one dataset
colombia <- left_join(colombia_hh, colombia_ind)

# combine I4 indicators
colombia <- colombia %>% 
  mutate(
    I4_SDG_11.1.1 = ifelse(rowSums((colombia %>% select(starts_with("I4_SDG_"))) ) < 4, 0, 1)
  ) %>% 
  select(-matches("I4_SDG_([a-z])"))


# add weighting variable
colombia <- colombia |> left_join(persons |> count(ID, dwelling, household, wt = WT, name = "WT"))

# add case characteristics for grouping
colombia <- 
  colombia %>% 
  left_join(dwellings |> 
              transmute(dwelling,
                        HH_region = REGION,
                        HH_urban = CLASE == "Cabecera")) |> 
  left_join(persons |> 
              filter(P6051 == "Jefe (a) del hogar") |> 
              transmute(dwelling, household,
                        HH_head_gender = if_else(P6020 == "Hombre", "Male", "Female"),
                        HH_years_disp = pmin(P767, 35) |> cut_width(5, boundary = 0),
                        HH_place_of_origin = P6076S1,
                        HH_ethnic_minority = P6080 != "Ninguno de los anteriores"))

colombia <- colombia %>% mutate(across(starts_with("HH_"), compose(fct_explicit_na, as_factor)))

# massage data into the right format
colombia <- colombia %>% mutate(across(where(is.logical), as.numeric))

# welfare-measure
colombia <- colombia |> left_join(households |> select(dwelling, household, PERCAPITA))

# add case identifier
colombia <- 
  colombia %>% 
  mutate(HHID = str_c(dwelling, household, sep = "-")) %>% 
  select(-dwelling, -household)

# downsample host community
colombia <- 
  bind_rows(colombia %>% filter(ID == 1),
            colombia %>% filter(ID == 0) %>% slice_sample(n = 5000) %>% mutate(WT = sum(!colombia$ID)/5000*WT))


# report difficulty of indicators ----------------------------------------------
difficulty <- bind_rows(
  colombia %>% filter(ID==1) %>% select(starts_with("I"),WT) %>% select(-ID) %>% 
    summarise_all(.,~weighted.mean(., WT, na.rm = TRUE)) %>% mutate(Stat="Difficulty"),
  colombia %>% filter(ID==1) %>% select(starts_with("I"),WT) %>% select(-ID) %>% 
    summarise_all(.,~sum(WT*is.na(.))/sum(WT),na.rm=T) %>% mutate(Stat="Missingness")) %>% 
  pivot_longer(.,cols=c(starts_with("I")),names_to = "Indicator",values_to = "Value") %>% 
  pivot_wider(.,id_cols = "Indicator",names_from ="Stat",values_from = "Value") %>% 
  mutate(Difficulty = round(Difficulty*100,2),Missingness = round(Missingness*100,2),
         order = as.numeric(gsub("I([0-9]+).*$", "\\1", Indicator))) %>% 
  arrange(order) %>% select(-order)
#write_csv(difficulty, "Selected_indicators/colombia_difficulty.csv")

# run simulations --------------------------------------------------------------
source("simulations.R")
simulation_colombia <- bind_rows(
  simulate_IRIS_metric(colombia) %>% mutate(metric="Pass/fail"),
  simulate_composite(colombia)%>% mutate(metric="1: Full composite"),
  simulate_criterion(colombia)%>% mutate(metric="2: Composite at criterion level"),
  cbind((simulate_IRIS_metric(colombia))[1,1:9],data.frame(Durable_Solutions = NA,DS=0,DS_perc=0))%>% 
    mutate(metric="3: Composite at subcriterion level"),
  simulate_cells(colombia) %>% 
    group_by(across(starts_with("I"))) %>% 
    summarise(DS=mean(DS),DS_perc=mean(DS_perc))%>% mutate(metric="4: Homogeneous cells"),
  simulate_classifier(colombia)%>% mutate(metric="5: Classifier/regression-based"))%>% 
  mutate(`Percentage exiting the stock`=round(DS_perc*100,2),
         `IDP households exiting the stock`=round(DS)) %>% 
  select(metric,`IDP households exiting the stock`,`Percentage exiting the stock`) %>% 
  rename(`Metric option`=metric)

#write_csv(simulation_colombia, "Selected_indicators/colombia_selected_indicators.csv")

