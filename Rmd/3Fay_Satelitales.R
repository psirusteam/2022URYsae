#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(reticulate) # Conexión con Python
library(rgee) # Conexión con Google Earth Engine
library(sf) # Paquete para manejar datos geográficos
library(concaveman)
library(geojsonio)
library(magrittr)

####################################################
### Loading datasets: EH and Population census ###
####################################################
tasa_desocupacion <-
  readRDS("URY/2020/1.Ingreso/Data/tasa_desocupacion.rds")


#######################################
### configuración inicial de Python ###
#######################################

#rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
rgee_environment_dir =  "C:/Users/agutierrez1/Anaconda3/envs/rgee_py/python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)
###################################################
### Arreglar la shape                           ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando URYivia
URY <- read_sf("URY/ShapeDeptoURU/URY_adm1.shp")
URY %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2),
                     nombre = toupper(NAME_1),
                     depto = case_when(depto == "01" ~ "02",
                                       depto == "02" ~ "03",
                                       depto == "03" ~ "04",
                                       depto == "04" ~ "05",
                                       depto == "05" ~ "06",
                                       depto == "06" ~ "07",
                                       depto == "07" ~ "08",
                                       depto == "08" ~ "09",
                                       depto == "09" ~ "10",
                                       depto == "10" ~ "01",
                                       TRUE ~ depto))
###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

URY_luces <- ee_extract(
  x = luces,
  y = URY["depto"],
  ee$Reducer$mean(),
  sf = FALSE
)

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

URY_urbano_cultivo <- ee_extract(
                   x = tiposuelo,
                   y = URY["depto"],
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) 
###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(URY_luces) %>% 
  full_join(URY_urbano_cultivo)

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "URY/2020/1.Ingreso/Data/tasa_desocupacion.rds")

