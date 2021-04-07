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
# 1.1 Victims of violence
persons <- 
  persons %>%
  mutate(
    # Satisfaction with current level of security
    I1_sec_satisfaction = str_detect(P1898, "([6-9]|'Totalmente satisfecho\\(a\\))"))

dwellings <- 
  dwellings %>% 
  mutate(
    # Experienced natural disaster in last 12 months
    I1_natural_disaster = transmute(., across(starts_with("P4065S"), ~.=="No")) %>% reduce(~.x&.y))

# 1.2 Freedom of movement

## Not applicable...

# 2.1. Food security 
households <- 
  households %>% 
  mutate(
    # Living above the food security line
    I3_food_secure = PERCAPITA > 137350)

# 2.2 Shelter and housing 
households <- 
  households %>% 
  mutate(
    # Overcrowded households
    I4_overcrowding = CANT_PERSONAS_HOGAR/P5000 >= 3,
    
    # Legally occupied dwelling
    I4_legal_tenure = !str_detect(P5095, "(hecho|colectiva)"))

dwellings <- 
  dwellings %>% 
  mutate(
    # Access to clean water services
    I4_water = P8520S5 == "Sí",
    
    # Access to improved sanitation
    I4_sanitation = P8520S5 == "Sí")

# 2.3 Medical services 
persons <- 
  persons %>% 
  mutate(
    # Satisfaction with current level of health
    I5_health_satisfaction = str_detect(P1897, "([6-9]|'Totalmente satisfecho\\(a\\))"),
    
    # In possession of health insurance
    I5_health_insurance = P6090 == "Sí")

# 2.4 Education
persons <- 
  persons %>% 
  mutate(
    # Can read and write
    I6_literate = P6160 == "Sí",
    
    # School attendance
    I6_school_attend = P8586 == "Sí",
    
    # Attending official educational establishment
    I6_school_official = P5673 == " Oficial")

# 3.1 Employment and livelihoods
persons <- 
  persons %>% 
  mutate(
    # Labor force participation rate
    I7_lfs_pax = P6240 == "Trabajando" | P6240 == "Buscando trabajo",
    
    # Employment rate
    I7_in_employment = P6240 == "Trabajando",
    
    # Not unemployed
    I7_not_unemployed = P6240 != "Buscando trabajo",
    
    # Employment duration - indefinite or fixed term
    I7_employment_term = P6460 == "A termino Indefinido",
    
    # Unsafe working conditions
    I7_unsafe_job = is.na(P1709S12),
    
    # Job satisfaction
    I7_job_satisfaction = str_detect(P1899, "([6-9]|'Totalmente satisfecho\\(a\\))"))

# 3.2 Economic security
households <- 
  households %>% 
  mutate(
    # Living above the national poverty line
    I8_poor = PERCAPITA > 327674,
    
    # Written lease - reduced risk of eviction
    I8_written_lease = P3006 == "Escrito",
    
    # Not defaulting on utility bills
    I8_default_on_bills = is.na(P5016S5))

persons <- 
  persons %>% 
  mutate(
    # Written employment contract - job security
    I8_written_job_contract = P6450 == "Escrito",
    
    # Satisfaction with current level of income
    I8_income_satisfaction = str_detect(P1896, "([6-9]|'Totalmente satisfecho\\(a\\))"))

# 4.1 Property restitution and compensation 

## Not applicable...

# 5.1. Documentation 
persons <- 
  persons %>% 
  mutate(
    # In possession of identity documents
    I10_id_doc = P1894 != "No tiene documento de identidad")

# prepare dataset for simulations -----------------------------------------------
# merge everything into one dataset
colombia <- persons %>% select(dwelling, household, person, WT, starts_with("I"))
colombia <- colombia %>% left_join(households %>% select(dwelling, household, starts_with("I"), -starts_with("I_")))
colombia <- colombia %>% left_join(dwellings %>% select(dwelling, starts_with("I")))

# add case characteristics for grouping
colombia <- 
  colombia %>% 
  left_join(persons %>% 
              transmute(dwelling, household, person,
                        HH_gender = P6020,
                        HH_agegrp = case_when(between(P6040, 0, 14) ~ "Children",
                                              between(P6040, 15, 24) ~ "Youth",
                                              between(P6040, 25, 59) ~ "Adults",
                                              between(P6040, 60, Inf) ~ "Elderly"),
                        HH_marital_status = P5502,
                        HH_adm1 = P756S1,
                        HH_disp_dur = cut(P767, breaks = seq(0, 100, 5)),
                        HH_ethnic_minority = P6080 != "Ninguno de los anteriores",
                        HH_peasant = P2059 == "Si" | P2061 == "Si"))

colombia <- colombia %>% mutate(across(starts_with("HH_"), compose(fct_explicit_na, as_factor)))

# massage data into the write format
colombia <- colombia %>% mutate(across(where(is.logical), as.numeric))

# add case identifier
colombia <- colombia %>% mutate(HHID = str_c(dwelling, household, person, sep = "-")) %>% select(-dwelling, -household, -person)

# output final dataset ----
colombia
