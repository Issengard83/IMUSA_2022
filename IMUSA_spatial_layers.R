### Spatial layers generated for the statistical analysis of the manuscript: 
### Seroprevalence of Leptospira antibodies in dogs and cats attending to municipal 
### spay/neuter campaigns, Santa Fe, Argentina.
### Author: Tamara Ricardo
### Last update: 
# Fri Jun 14 11:18:46 2024 ------------------------------


# LOAD PACKAGES -----------------------------------------------------------
pacman::p_load(
  sf,
  xlsx2geojson,
  rio,
  janitor,
  tidyverse
  )


# Socioeconomic indicators by census tract --------------------------------
var_ct <- excel_to_geojson("../GIS/shp/Estratificación de radios censales - ud - Santa Fe.xlsx") |>

    ## select relevant variables
  select(
    census_tract = codigo_de_radio_1,
    households = cantidad_total_de_hogares,
    hh_nbi = hogares_con_al_menos_un_indicador_nbi,
    hh_overcrowding = hogares_con_hacinamiento_3_personas_por_cuarto,
    hh_no_tap_water = hogares_sin_caneria_de_agua_en_la_vivienda,
    hh_no_sewage = hogares_sin_cloaca_red_publica,
    hh_pit_cesspool = hogares_con_desague_a_hoyo_o_pozo_ciego_sin_camara,
    hh_poor_housing = hogares_con_calidad_de_la_vivienda_precaria,
    socioeconomic_strata = estrato_por_radio
  ) |> 
  
  ## add incidence of chronic poverty
  st_join(
    excel_to_geojson("../GIS/shp/Pobreza Crónica x Radio Censal - ud - Santa Fe.xlsx") |>
  
          # select relevant variables
      select(hh_chronic_poverty = hogares_en_situacion_de_pobreza_cronica),
    
    join = st_equals
  ) |> 

  ## add indicators of urbanization
  st_join(
    excel_to_geojson("../GIS/shp/Entorno urbano - Radios, 2010 - ud - Santa Fe.xlsx") |> 
    
      # select relevant variables
      select(
        hh_garbage_collection = hogares_con_recoleccion_regular_de_basura,
        hh_paved_roads = hogares_con_al_menos_una_cuadra_pavimentada,
      ),
    
    join = st_equals
  ) |> 
  
  ## Transform to percentages
  mutate(across(starts_with("hh_"), 
                .fns = ~round(.x*100/households, 2))) |> 
  
  ## Categorize incidence of chronic poverty
  mutate(hh_chronic_poverty_cat = cut(
    hh_chronic_poverty, breaks = c(-Inf, 1, 5, 10, Inf),
    labels = c("Very low (<1%)", "Low (1-5%)", 
               "Moderate (5-10%)", "High/Very high (>10%)"))) |> 

  ## Close gaps
  st_buffer(dist = 0.001)


# Export layer
st_write(var_ct, "gis/strata_ct.geojson", 
         delete_dsn = T,
         delete.layer = T)


# Administrative districts ------------------------------------------------
adm_dis <- st_read("raw/dist_SF.kml", stringsAsFactors = T) %>% 
  
  ## Remove point features
  filter(st_geometry_type(.)!="POINT") %>% 
  
  ## Clean variable names
  clean_names() %>% 
  rename("adm_dis" = "name") %>% 
  
  ## Translate district names
  mutate_at("adm_dis", 
            .funs = ~ fct_recode(.x, 
                                 "NW" = "Distrito Noroeste",
                                 "NE" = "Distrito Noreste",
                                 "SW" = "Distrito Suroeste",
                                 "North" = "Distrito Norte",
                                 "East" = "Distrito Este",
                                 "West" = "Distrito Oeste",
                                 "Center" = "Distrito Centro",
                                 "La Costa" = "Distrito La Costa") %>%   
              fct_drop) |> 
  
  ## Validate
  st_make_valid()


# Export layer
st_write(adm_dis, "gis/adm_dis.geojson", 
         delete_dsn = T,
         delete.layer = T)


# Informal settlements ----------------------------------------------------
inf_shp <- excel_to_geojson("../GIS/shp/Registro Nacional de Barrios Populares 2023 - Santa Fe.xlsx") |> 
  # select relevant columns
  select(name = nombre_del_barrio,
         category = tipologia_barrial,
         electricity = situacion_predominante_sobre_la_conexion_a_la_energia_electrica,
         sewage = situacion_predominante_sobre_la_conexion_a_la_red_cloacal,
         tap_water = situacion_predominante_sobre_la_conexion_a_la_red_de_agua)

# Export layer
st_write(inf_shp, "gis/inf_shp.geojson", 
         delete_dsn = T,
         delete.layer = T)

# IMUSA locations ---------------------------------------------------------
imu_loc <- import("raw/IMUSA.xlsx") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) |> 
  st_make_valid()
  
  
### Save shapefile
st_write(imu_loc, "gis/IMUSA_loc.geojson", 
         delete_dsn = T,
         delete.layer = T)


# Sampled points ----------------------------------------------------------
imu_shp <- import("clean/data_IMUSA_clean.xlsx") |> 
  
  # select relevant variables
  select(id_muestra, origen_muestra, animal, MAT_res, lon, lat) |> 
  
  # modify levels
  mutate(origen_muestra = if_else(grepl("Móvil", origen_muestra), 
                                  "IMUSA mobile truck location", 
                                  "IMUSA fixed units")) |> 
  
  # transform to spatial object
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  
  # join socioeconomic indicators (census tract)
  st_join(var_ct, join = st_nearest_feature) %>% 
  
  # drop unused factor levels
  mutate(across(.cols = where(is.factor), .fns = fct_drop)) |> 
  
  # validate
  st_make_valid()


### Save shapefile
st_write(imu_shp, "gis/IMUSA_points.geojson", 
         delete_dsn = T,
         delete.layer = T)


### Clean working environment
rm(list = ls())

pacman::p_unload("all")
