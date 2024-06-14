### Limpieza de datos para el manuscrito: 
### Seroprevalence of Leptospira antibodies in dogs and cats attending to 
### municipal spay/neuter campaigns, Santa Fe, Argentina.
### Author: Tamara Ricardo
### Last modification:
# Thu Apr  4 09:30:10 2024 ------------------------------


# Carga paquetes ----------------------------------------------------------
pacman::p_load(
  sf,
  rio,
  janitor,
  gtools,
  tidylog,
  tidyverse)


# Carga datos crudos ------------------------------------------------------
### Muestreos
imu_raw <- import("raw/data_IMUSA_raw.xls")

### Radios censales
tracts <- st_read("clean/shp/VAR_CT_SF.geojson") %>% 
  select(redcode)


# Limpieza de datos -------------------------------------------------------
imu_clean <- imu_raw %>% 
  
  ### Estandariza nombres de columnas
  clean_names() %>% 
  
  rename(
    fecha_muestra = fecha,
    adm_dis = distrito,
    lat = latitud,
    lon = longitud,
    vac_alguna = vacuna_antiparas,
    vac_rabia = vac_antirrabica,
    vac_hexavalente = vac_sext_canina,
    vac_cuando = vacunas_cuando,
    sale_calle_suelto = sale_suelto,
    cto_otros_cuales = otros_cuales,
    cto_anim_vecinos = vecinos,
    cto_anim_callejeros = callejeros,
    n_perros = propios_perros,
    n_gatos = propios_gatos,
    lugar_mascota_limpieza = donde_duerme_limp,
    MAT_id = idmat, 
    MAT_res = res_mat) %>% 
  
  rename_with(.cols = starts_with("con_"),
              .fn = ~ str_replace(.x, "con_", "cto_")) %>%
  
  
  ### Filtra muestras que no cumplen con los criterios de inclusión
  filter(!is.na(barrio_res) & 
           !is.na(MAT_res) & 
           nombre_animal != "CHATRÁN") %>%
  
  
  ### Modifica columnas existentes
  rowwise() %>% 
  
  mutate(
    # fecha toma de muestra
    fecha_muestra = convert_to_date(fecha_muestra),
    
    # origen de la muestra
    origen_muestra = if_else(grepl("Móv", origen_muestra), "IMUSA móvil", "IMUSA"),
    
    # latitud y longitud
    across(.cols = c(lat, lon), 
           .fns = ~ paste0(str_sub(.x, 1, 3), ".", str_sub(.x, 4)) %>% as.numeric()),
    
    # edad
    edad = str_replace_all(edad, ",", ".") %>% as.numeric(),
    
    # BCS
    bcs_cat = fct_collapse(bcs_cat, 
                           "Delgado/muy delgado" = c("Delgado", "Muy delgado"),
                           "Sobrepeso/obesidad" = c("Sobrepeso", "Obesidad")),
    
    # vacunas (cualquiera)
    vac_alguna = if_else(all(is.na(c_across(vac_rabia:vac_trival_felina))),
                         "NS", vac_alguna),
    
    # vacunas cuando
    vac_cuando = case_when(vac_alguna == "No" ~ "Nunca",
                           vac_alguna == "NS" ~ "No recuerda",
                           TRUE ~ vac_cuando),

    # preñeces
    tuvo_cria = if_else(sexo == "Hembra", tuvo_cria, NA),
    
    # abortos
    tuvo_abortos = if_else(tuvo_cria == "No", "No", tuvo_abortos),
    
    ## Reemplaza NA's
    vio_roedores_frec = replace_na(vio_roedores_frec, "Nunca"),
    
    across(.cols = starts_with("n_"),
           .fns = ~ replace_na(.x, 0)),
    
    across(.cols =  starts_with("caza_")|
             starts_with("cto_an")|
             contains("suelto")|
             matches("vio_roedores"),
           .fns = ~ replace_na(.x, "No")),
    
    ## Estandariza etiquetas
    across(.cols = where(is.character),
           .fns = ~ str_replace_all(.x, "Sí", "Yes")),
    
    ## corrige cantidad de perros
    n_perros = if_else(animal == "Perro", n_perros + 1, n_perros),
    
    ## corrige cantidad de gatos
    n_gatos = if_else(animal == "Gato", n_gatos + 1, n_gatos),
  ) %>% 
  
  
  ### Crea nuevas variables
  # ubicación geográfica aproximada
  mutate(aprox_ubicacion = if_else(grepl("[Aa]prox", observaciones1), "Yes", "No"),
         .after = lon) %>% 
  
  # edad en años
  mutate(edad_yrs = if_else(edad_tipo == "Meses", round(edad/12, 1), edad),
         .after = edad_tipo) %>% 
  
  # grupo etario
  mutate(edad_cat = case_when(
    animal == "Perro" & edad_yrs<=2 |
    animal == "Gato" & edad_yrs<=3 ~ "Adolescente",
    animal == "Perro" & edad_yrs>=7 |
    animal == "Gato" & edad_yrs>10 ~ "Senior",
    TRUE ~ "Adulto"),
    .after = edad_yrs) %>% 

  # categoriza raza
  mutate(raza_cat = case_when(raza == "Mestizo" ~ "Mestizo",
                              is.na(raza) ~ NA_character_,
                              TRUE ~ "Raza definida"),
         .after = raza) %>% 
  
  # vacuna antirrábica durante la castración
  mutate(vac_rabia_cast = if_else(grepl("coloc", vacuna_coment), "Yes", "No"),
         .after = vac_rabia) %>% 

  # función del animal
  mutate(funcion = case_when(fun_guardia %in% c("No", NA_character_) & 
                               fun_caza =="No" ~ "Mascota",
                             
                             fun_mascota == "No" ~ "Guardia/caza",
                             
                             TRUE ~ "Mascota + guardia/caza"), 
         .before = fun_mascota) %>% 
  
  # alimentación
  mutate(alimentacion = case_when(alim_balanceado == "Yes" & 
                                    alim_carne_cruda == "No" & 
                                    alim_casero == "No" & 
                                    alim_sobras == "No" ~ "Balanceado",
                                  
                                  alim_balanceado == "Yes" & 
                                    (alim_carne_cruda == "Yes"|
                                       alim_casero == "Yes"|
                                       alim_sobras == "Yes") ~ "Balanceado y casero/sobras",
                                  
                                  alim_balanceado=="No" ~ "Casero/sobras"),
         .before = alim_balanceado) %>% 
  
  # número perros y gatos
  mutate(n_perros_gatos = sum(c_across(n_perros:n_gatos), na.rm = T),
         .after = n_gatos) %>% 
  
  ungroup() %>% 
  
  mutate(n_perros_gatos_cat = quantcut(
      n_perros_gatos, labels = c("1-2 perros/gatos", "2-3 perros/gatos",
                                 "3-6 perros/gatos", "6+ perros/gatos")),
      .after = n_perros_gatos) %>% 
  
  ### Convierte caracteres a factor
  mutate(across(where(is.character) & 
                  !contains("id") & 
                  !matches("barrio_res") &
                  !matches("nombre_animal") &
                  !matches("raza") &
                  !contains("cuales"), 
                .fns = ~ as.factor(.x))) %>% 
  
  
  ### Ordena columnas
  select(id_muestra, everything(), -unique_key, -etapa, -vacuna_coment, -observaciones1,
         -caza_aves, -caza_cuales, -donde_duerme)



# Añade radios censales ---------------------------------------------------
imu_clean <- imu_clean %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
  
  st_join(tracts, join = st_nearest_feature) %>% 
  
  select(id_muestra:barrio_res, redcode, everything()) %>% 
  
  st_drop_geometry()


### Exporta datos
export(imu_clean, "clean/data_IMUSA_clean.xlsx", col_names = T)


## Diccionario datos
tibble(variable = colnames(imu_clean),
       tipo = map_chr(imu_clean, class),
       niveles = ifelse(sapply(imu_clean, is.factor),
                        sapply(imu_clean, levels) %>%
                          str_remove_all("c\\(") %>% str_remove_all("\\)"), NA)) %>%
  export(file = "clean/diccionario_IMUSA.xlsx")


# Explora datos -----------------------------------------------------------
glimpse(imu_raw)

skimr::skim(imu_raw)

names(imu_clean)

tabyl(imu_clean$vio_roedores_frec)

tabyl(imu_clean$vac_cuando)

skimr::skim(imu_clean)