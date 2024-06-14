### Data analysis for the manuscript:
### Seroprevalence of Leptospira antibodies in dogs and cats attending to municipal 
### spay/neuter campaigns, Santa Fe, Argentina.
### Logistic regression analysis
### Author: Tamara Ricardo
### Last update:
# Fri Apr  5 11:05:00 2024 ------------------------------


# LOAD PACKAGES -----------------------------------------------------------
pacman::p_load(
  # Descriptive statistics
  gtsummary,
  
  # Model fit
  glmmTMB,
  
  # Residuals and inference
  performance,
  DHARMa,
  
  # Data management
  flextable,
  gtools,
  rio,
  janitor,
  tidyverse)


# Load and clean data -----------------------------------------------------
data_imu <- import("clean/data_IMUSA_clean.xlsx") %>% 
  
  ### Relevel variables
  mutate(
    funcion = fct_relevel(funcion, "Guardia/caza", after = Inf),
    
    lugar_mascota_limpieza = fct_lump_min(
      lugar_mascota_limpieza,  
      min = 20, 
      other_level = "Ocasionalmente") %>% 
      fct_relevel("Diariamente", after = 0)) %>% 
  
  ### Convert character to factor
  mutate(across(where(is.character) & 
                  !contains("id_") & 
                  !matches("nombre_animal") &
                  !matches("raza") &
                  !contains("cuales"), 
                .fns = ~ as.factor(.x))) 
  
  
# Create dataset for dog samples ------------------------------------------
data_dog <- data_imu %>%
  
  ### Select only dog samples
  filter(animal == "Perro") %>%
  
  ### Select relevant variables
  select(id_muestra, MAT_id, MAT_res, adm_dis, barrio_res, 
         edad_yrs, edad_cat, sexo, raza_cat, bcs_cat, funcion, alimentacion, 
         sale_calle:cto_otros, cto_anim_propios:n_perros_gatos_cat,
         vio_roedores:lugar_mascota_limpieza) %>% 
  
  ### Select only full records
  drop_na()


# Explore data ------------------------------------------------------------
names(data_imu)

glimpse(data_imu)

skimr::skim(data_imu)

skimr::skim(data_dog)


### Number of dogs, cats and dogs/cats
summary(data_imu$n_perros)

summary(data_imu$n_gatos)

summary(data_imu$n_perros_gatos)

tabyl(data_imu$n_perros_gatos_cat)


### Number of samples per administrative district
data_imu %>% 
tbl_summary(by = animal,
            include = adm_dis, 
            digits = list(all_categorical() ~ c(0, 1)),
            sort = list(everything() ~ "frequency")) %>%

add_overall()


### Descriptives by sex, age, breed and BCS
data_imu %>% 
  tbl_summary(by = animal,
              include = c(sexo, tuvo_cria, tuvo_abortos, edad_yrs, edad_cat, 
                          raza_cat, bcs_cat),
              missing = "no",
              digits = list(all_categorical() ~ c(0, 1))) %>% 
  # total
  add_overall() %>% 
  
  # P-valor
  add_p() %>% 
  bold_p()

  
### Descriptives by role and vaccination
data_imu %>% 
  tbl_summary(by = animal,
              include = c(funcion, alimentacion, vac_alguna, antiparasitario,
                          vac_hexavalente, vac_rabia, vac_rabia_cast, vac_cuando),
              missing = "no",
              digits = list(all_categorical() ~ c(0, 1))) %>% 
  # total
  add_overall() %>% 
  
  # P-value
  add_p() %>% 
  bold_p()


### Seroprevalence by species and serovar
data_imu %>% 
  tbl_summary(by = animal,
              include = c(MAT_res:tarassovi),
              digits = list(all_categorical() ~ c(0, 1))
              ) %>% 
  
  add_p()


# Logistic regression models ----------------------------------------------
### Check random effect
# administrative district
fit1 <- glmmTMB(MAT_res ~ 1 + (1|adm_dis), data = data_dog, family = binomial)

# neighborhood
fit2 <- glmmTMB(MAT_res ~ 1 + (1|barrio_res), data = data_dog, family = binomial)

# administrative district/neighborhood
fit3 <- glmmTMB(MAT_res ~ 1 + (1|adm_dis/barrio_res), data = data_dog, family = binomial)

# compare models
compare_performance(fit1, fit2, fit3, metrics = c("AIC","BIC"), rank = T)


# Table 1: univariate GLMMs for dogs --------------------------------------
tab1 <- data_dog %>%
  
  select(MAT_res, barrio_res, sexo, edad_yrs, edad_cat, bcs_cat,
         funcion, alimentacion, sale_calle, sale_calle_suelto,
         starts_with("caza_"),starts_with("cto_"),
         n_perros_gatos_cat, starts_with("vio_"),
         patio_tierra_cesped, starts_with("lugar_")) %>%
  
  tbl_uvregression(
    y = MAT_res,
    method = glmmTMB::glmmTMB,
    formula = "{y} ~ {x} + (1|barrio_res)",
    method.args = list(family = binomial),
    exponentiate = T,
    hide_n = T,
    show_single_row = c(starts_with("cto"), starts_with("sale"),
                        starts_with("caza"), vio_roedores, patio_tierra_cesped),
    label = list(
      sexo ~ "Sex",
      edad_yrs ~ "Age (years)",
      edad_cat ~ "Age category",
      bcs_cat ~ "BCS",
      funcion ~ "Role in the household",
      alimentacion ~ "Type of feeding",
      sale_calle ~ "Street access",
      sale_calle_suelto ~ "Unsupervised street access",
      caza_animales ~ "Hunting behavior",
      caza_roedores ~ "Hunting: rodents",
      caza_silvestres ~ "Hunting: wild animals",
      cto_basurales ~ "Exposure to dumpyards",
      cto_agua_barro ~ "Exposure to environmental water/mud",
      cto_perros ~ "Contact with other dogs",
      cto_gatos ~ "Contact with cats",
      cto_otros ~ "Contact with other animals",
      cto_anim_propios ~ "Contact with own dogs/cats",
      cto_anim_vecinos ~ "Contact with neighbor dogs/cats",
      cto_anim_callejeros ~ "Contact with stray dogs/cats",
      n_perros_gatos_cat ~ "Number of owned dogs/cats",
      vio_roedores ~ "Rodent sightings",
      vio_roedores_frec ~ "Frequency of rodent sightings",
      patio_tierra_cesped ~ "Presence of a yard",
      lugar_mascota ~ "Type of housing",
      lugar_mascota_limpieza ~ "Frequency of cleaning the kennel/bed"
    )
  ) %>%
  
  bold_p(t = .1) %>%
  italicize_labels()


## Format table and export
tab1 %>% 
  
  as_flex_table() %>% 
  
  font(fontname = "Calibri", part = "all") %>% 
  
  fontsize(size = 12, part = "all") %>% 
  
  set_caption(caption = "Table 1. Results of the univariate binomial GLMMs to assess potential predictors of seropositivity in domestic dogs using neighborhood of residence as a random intercept, Santa Fe, Argentina (2022).") %>% 
  
  save_as_docx(path = "tab1.docx")


### Multivariate models
fit <- glmmTMB(MAT_res ~ sexo + edad_cat + sale_calle + cto_basurales + 
                vio_roedores_frec + (1|barrio_res),
              family = binomial, data = data_dog)

fits <- glmmTMB(MAT_res ~ sexo + edad_cat + sale_calle_suelto + cto_basurales + 
                  vio_roedores_frec + (1|barrio_res),
                family = binomial, data = data_dog)


compare_performance(fit, fits, metrics = "common", rank = T)


### Variable selection
drop1(fit)

fit1 = update(fit, ~.-edad_cat)

drop1(fit1)

fit2 = update(fit1, ~.-cto_basurales)

drop1(fit2)

fit3 = update(fit2, ~.-sexo)

drop1(fit3)

fit4 = update(fit3, ~.-vio_roedores_frec)

# Compare models
compare_performance(fit, fit1, fit2, fit3, fit4, metrics = "AIC", rank = T)

# Check model residuals
testResiduals(fit4)

### Coefficients
tbl_regression(fit4, exponentiate = T)

r2(fit4)
