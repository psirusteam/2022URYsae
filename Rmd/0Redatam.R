#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
# https://redatam.org/es/microdatos
# https://redatamr.ideasybits.com/docs/examples/
### Cleaning R environment ###


rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
## Diccionario traído directamente de los repositorios en Celade
Uruguay <-  redatam.open("C:/Users/guerr/Downloads/CP2011URY/BaseRPub/CPV2011_uruguay_publica.dicX")

redatam.entities(Uruguay)
redatam.variables(Uruguay, entName =  "SEGMENTO")
redatam.variables(Uruguay, "VIVIENDA")

CONTEOS <- redatam.query(
  Uruguay,
  "freq SEGMENTO.REDCODEN
  by  VIVIENDA.URBRUR
                      by PERSONA.PH02
                      by PERSONA.NA01
                      by PERSONA.ER02
                      by PERSONA.AESTUDIO", tot.omit = FALSE)

rm("$table1")

# Eliminando totales de la tabla
CONTEOS2 <-
  CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))


## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n)) %>%
        data.frame()
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n)) %>%
        data.frame()
    })

group_by(CONTEOS2, AESTUDIO6_value, AESTUDIO6_label) %>%
  summarise(n = sum(value)) %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()

censo_mrp <- CONTEOS2 %>%
  transmute(segm = str_pad(
              string = REDCODEN1_value,
              width = 6,
              pad = "0"
            ),
            area = case_when(URBRUR2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(PH023_value) ,

            edad = case_when(
              NA014_value  < 15 ~ "1", # 5 a 14
              NA014_value  < 30 ~ "2", # 15 a 29
              NA014_value  < 45 ~ "3", # 30 a 44
              NA014_value  < 65 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas

            anoest = case_when(
              NA014_value < 4| is.na( AESTUDIO6_value) ~ "98",     # No aplica
               AESTUDIO6_value == 88 ~ "99", #NS/NR
               AESTUDIO6_value %in% 0 ~ "1",  # Sin educacion
               AESTUDIO6_value %in% c(1:6) ~ "2",  # 1-6
               AESTUDIO6_value %in% c(7:12) ~ "3",  # 7-12
               AESTUDIO6_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),

           etnia = case_when(
              ER025_value %in% 4 ~ "1", #indigena
              ER025_value %in% 1 ~ "2", #afro negro mulato
              TRUE ~ "3" ),# Otro

            value) %>%
  group_by(segm, area, sexo, edad, etnia, anoest) %>%
  summarise(n = sum(value), .groups = "drop")


## tasa de rango de edad por municipio 
edad_labels <-  c(
  "1" = "edad_0_14",
  "2" = "edad_15_29",
  "3" = "edad_30_44",
  "4" = "edad_45_64",
  "5" = "edad_65_más"
)

tasa_edad <-
  censo_mrp %>%
  mutate(edad =   dplyr::recode(as.character(edad) ,!!!edad_labels)) %>% 
  group_by(segm, edad) %>%
  summarise(n = sum(n))  %>%
  group_by(segm) %>%
  mutate(n = n/sum(n)) %>% 
  pivot_wider(names_from = "edad", values_from = n)

## tasa de anios de estudio por municipio 
anoest_labels <- c(
  "1" = "Sin_educacion",
  "2" = "años1_6",
  "3" = "años7_12",
  "4" = "Más_de_12",
  "98" = "No_aplica",
  "99" = "NS_NR"
)

tasa_anoest <-
  censo_mrp %>%
  mutate(anoest =   dplyr::recode(as.character(anoest) ,!!!anoest_labels)) %>% 
  group_by(segm, anoest) %>%
  summarise(n = sum(n))  %>%
  group_by(segm) %>%
  mutate(n = n/sum(n)) %>% 
  filter(!anoest %in% c("No_aplica","NS_NR")) %>% 
  pivot_wider(names_from = "anoest", values_from = n)


## tasa de Hombres y Mujeres por municipio 
tasa_sexo <-
  censo_mrp %>% mutate(sexo = ifelse(sexo == 1, "Hombre", "Mujer")) %>%
  group_by(segm, sexo) %>%
  summarise(n = sum(n))  %>%
  pivot_wider(names_from = "sexo", values_from = n) %>%
  mutate(Hombre = Hombre / (Hombre + Mujer),
         Mujer  = 1 - Hombre)

## tasa de etnia por municipio
tasa_etnia <- censo_mrp %>%
  mutate(etnia = case_when(etnia == 1 ~ "Indigena",
                           etnia == 2 ~ "Afro",
                           TRUE ~ "Otro")) %>%
  group_by(segm, etnia) %>%
  summarise(n = sum(n))  %>%
  pivot_wider(names_from = "etnia",
              values_from = n,
              values_fill = 0) %>%
  mutate(
    Afro = Afro / (Afro + Indigena + Otro),
    Indigena = Indigena / (Afro + Indigena + Otro),
    Otro = Otro / (Afro + Indigena + Otro),
  )

## tasa de ruralidad
tasa_area <- censo_mrp %>%
  mutate(area = case_when(area == 0 ~ "Rural",
                          TRUE ~ "Urbano")) %>%
  group_by(segm, area) %>%
  summarise(n = sum(n))  %>%
  pivot_wider(names_from = "area",
              values_from = n,
              values_fill = 0) %>%
  mutate(Rural = Rural / (Rural + Urbano),
         Urbano  = 1 - Rural)


# Tasa de personas con más 12 años de educación  y > 20 años. -------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by PERSONA.NA01
                          by PERSONA.AESTUDIO",
                         tot.omit = FALSE)
EDUCACION <- CONTEOS %>%
  filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

tasa_edu_sup <- EDUCACION %>%
  mutate(Pobx = ifelse(NA012_value > 20 & AESTUDIO3_value > 12,
                       value, 0),
         PobT = ifelse(NA012_value > 20, value, 0)) %>%
  group_by( segm = REDCODEN1_label) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            escolaridad = Pobx/PobT)

# Tasa de personas analfabeta. ------------------------------------------
# Población de 15 años y más que no sabe leer y escribir dividido por la 
# población de 15 años y más, multiplicado por 100.
CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by PERSONA.NA01
                          by PERSONA.ED08",
                         tot.omit = FALSE)
ALFABETA <- CONTEOS %>%
  filter(!NA012_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value),
         !ED083_label %in% c("__tot__", "__na__"))

tasa_alfabeta <- ALFABETA %>%
  mutate(Pobx = ifelse(NA012_value >= 15 & ED083_label == "No",
                       value, 0),
         PobT = ifelse(NA012_value >= 20, value, 0)) %>%
  group_by(segm = REDCODEN1_label) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            alfabeta = Pobx/PobT)

# Tasa de acceso a Internet  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.CE11",
                         tot.omit = FALSE)
INTERNET <- CONTEOS %>%
  filter(!CE112_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_Internet <- INTERNET %>%
  mutate(Pobx = ifelse(CE112_label == "Si",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            internet = Pobx/PobT)


# NBI AGUA  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIAGUA",
                         tot.omit = FALSE)
NBIAGUA <- CONTEOS %>%
  filter(!NBIAGUA2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIAGUA2 <- NBIAGUA %>%
  mutate(Pobx = ifelse(NBIAGUA2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIAGUA = Pobx/PobT)


# NBI AGUA CALIENTE  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBICALE",
                         tot.omit = FALSE)
NBICALE <- CONTEOS %>%
  filter(!NBICALE2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBICALE2 <- NBICALE %>%
  mutate(Pobx = ifelse(NBICALE2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBICALE = Pobx/PobT)

# NBI CALEFACCION  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBICALF",
                         tot.omit = FALSE)
NBICALF <- CONTEOS %>%
  filter(!NBICALF2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBICALF <- NBICALF %>%
  mutate(Pobx = ifelse(NBICALF2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBICALEF = Pobx/PobT)

# NBI CONFORT  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBICONF",
                         tot.omit = FALSE)
NBICONF <- CONTEOS %>%
  filter(!NBICONF2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBICONF <- NBICONF %>%
  mutate(Pobx = ifelse(NBICONF2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBICONF = Pobx/PobT)

# NBI EDUCACION  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIEDU",
                         tot.omit = FALSE)
NBIEDU <- CONTEOS %>%
  filter(!NBIEDU2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIEDU <- NBIEDU %>%
  mutate(Pobx = ifelse(NBIEDU2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIEDU = Pobx/PobT)

# NBI ELECTRICIDAD  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIELC",
                         tot.omit = FALSE)
NBIELC <- CONTEOS %>%
  filter(!NBIELC2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIELC <- NBIELC %>%
  mutate(Pobx = ifelse(NBIELC2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIELC = Pobx/PobT)


# NBI ESPACIO PARA COCINAR  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBICOC",
                         tot.omit = FALSE)
NBICOC <- CONTEOS %>%
  filter(!NBICOC2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBICOC <- NBICOC %>%
  mutate(Pobx = ifelse(NBICOC2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBICOC = Pobx/PobT)


# NBI ESPACIO HABITABLE  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIHAC",
                         tot.omit = FALSE)
NBIHAC <- CONTEOS %>%
  filter(!NBIHAC2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIHAC <- NBIHAC %>%
  mutate(Pobx = ifelse(NBIHAC2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIHAC = Pobx/PobT)


# NBI MATERIALIDAD  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIMAT",
                         tot.omit = FALSE)
NBIMAT <- CONTEOS %>%
  filter(!NBIMAT2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIMAT <- NBIMAT %>%
  mutate(Pobx = ifelse(NBIMAT2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIMAT = Pobx/PobT)

# NBI REFRIGERACION DE ALIMENTOS  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIREF",
                         tot.omit = FALSE)
NBIREF <- CONTEOS %>%
  filter(!NBIREF2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIREF <- NBIREF %>%
  mutate(Pobx = ifelse(NBIREF2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIREF = Pobx/PobT)

# NBI SANEAMIENTO  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBISAN",
                         tot.omit = FALSE)
NBISAN <- CONTEOS %>%
  filter(!NBISAN2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBISAN <- NBISAN %>%
  mutate(Pobx = ifelse(NBISAN2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBISAN = Pobx/PobT)


# NBI SANEAMIENTO  ----------------------------------------------

CONTEOS <- redatam.query(Uruguay,
                         "freq SEGMENTO.REDCODEN
                          by HOGAR.NBIVIV",
                         tot.omit = FALSE)
NBIVIV <- CONTEOS %>%
  filter(!NBIVIV2_label %in% c("__tot__", "__na__") ,
         !is.na(REDCODEN1_value))

tasa_NBIVIV <- NBIVIV %>%
  mutate(Pobx = ifelse(NBIVIV2_label == "Con NBI",
                       value, 0),
         PobT = value) %>%
  group_by(
    segm = REDCODEN1_label
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(segm,
            NBIVIV = Pobx/PobT)

## Tasa de desopucación 

OCUPACION <-
  redatam.query(Uruguay, "freq SEGMENTO.REDCODEN by PERSONA.POBCOAC",
                tot.omit = FALSE)

group_by(OCUPACION, POBCOAC2_value, POBCOAC2_label) %>%
  summarise(n = sum(value))

OCUPACION2 <- OCUPACION %>%
  filter(!POBCOAC2_label %in% c("__tot__", "No relevado", "__na__"))

group_by(OCUPACION2, POBCOAC2_value, POBCOAC2_label) %>% summarise(n = sum(value))


OCUPACION2 <- OCUPACION2  %>% transmute(
  segm = REDCODEN1_label,

  ocupados = ifelse(POBCOAC2_value  %in% c(2), 1, 0),
  desocupados = ifelse(POBCOAC2_value  %in% c(3, 4), 1, 0),
  value
) %>% group_by(segm, ocupados, desocupados) %>%
  summarise(value = sum(value))


tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion  <- tabla %>%
  transmute(segm,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))


statelevel_predictors_df <- list(
  tasa_area,
  tasa_edad,
  tasa_anoest,
  tasa_etnia,
  tasa_sexo,
  tasa_alfabeta,
  tasa_edu_sup,
  tasa_NBIAGUA2,
  tasa_NBICALE2,
  tasa_NBICALF,
  tasa_NBICOC,
  tasa_NBICONF,
  tasa_NBIEDU,
  tasa_NBIELC,
  tasa_NBIHAC,
  tasa_NBIMAT,
  tasa_NBIREF,
  tasa_NBISAN,
  tasa_NBIVIV,
  tasa_desocupacion
) %>%
  reduce(.f = inner_join)

