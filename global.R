options(java.parameters = "-Xmx8g")

library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(readxl)
library(RJDBC)
library(DBI)
library(lubridate)
library(recoder)
library(openair)
library(ggplot2)
library(rlang)
library(latex2exp)

# Tablas ------------------------------------------------------------------

     tb_es <- read_excel("tablas/tabla_maestra.xlsx", sheet = "estaciones")
  tb_an_v1 <- read_excel("tablas/tabla_maestra.xlsx", sheet = "ant_nue_validado1")
  tb_an_c1 <- read_excel("tablas/tabla_maestra.xlsx", sheet = "ant_nue_crudo1")
     tb_cr <- read_excel("tablas/tabla_maestra.xlsx", sheet = "fechas_cru")
   tb_ugm3 <- read_excel("tablas/tabla_maestra.xlsx", sheet = "ppb_ugm3")
  tb_ro_vi <- read_excel("tablas/tabla_maestra.xlsx", sheet = "rosadeviento")
tb_ro_co_g <- read_excel("tablas/tabla_maestra.xlsx", sheet = "rosadecontaminante")
tb_se_ti_g <- read_excel("tablas/tabla_maestra.xlsx", sheet = "seriedetiempo")

# Estación ----------------------------------------------------------------

id_st <- 2 # CA-CC-01

st <- 
  tb_es %>%
  dplyr::filter(ID_ESTACION == !!id_st) %>% 
  dplyr::pull(COD_ESTACION) 

# Nombre de variables de datos validado -----------------------------------

# a) Horarios:

ant_v1_1h <-
  tb_an_v1 %>% 
  tidyr::drop_na(!!st) %>% 
  dplyr::filter(TEMPORALIDAD != "24h") %>% 
  dplyr::pull(ANT_VAR_VAL)

nue_v1_1h <- 
  tb_an_v1 %>% 
  tidyr::drop_na(!!st) %>% 
  dplyr::filter(TEMPORALIDAD != "24h") %>% 
  dplyr::pull(!!st)

# b) Diarios:

ant_v1_24h <-
  tb_an_v1 %>% 
  tidyr::drop_na(!!st) %>% 
  dplyr::filter(TEMPORALIDAD == "24h") %>% 
  dplyr::pull(ANT_VAR_VAL)

nue_v1_24h <- 
  tb_an_v1 %>% 
  tidyr::drop_na(!!st) %>% 
  dplyr::filter(TEMPORALIDAD == "24h") %>% 
  dplyr::pull(!!st)

# Datos validados --------------------------------------------------------

# a) Horarios:

bdv_1h <-
  read.csv(
    file = paste0("datos/", st, " HISTORICO 1h.csv"),
    header = T,
    stringsAsFactors = F,
    sep = ";",
    dec = ".",
    encoding = "UTF-8",
    fileEncoding = "UTF-8-BOM"
  ) %>%
  dplyr::mutate(date = as.POSIXct(strptime(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))) %>%
  dplyr::rename(!!!setNames(ant_v1_1h, nue_v1_1h))

# b) Diarios:

bdv_24h <-
  read.csv(
    file = paste0("datos/", st, " HISTORICO 24h.csv"),
    header = T,
    stringsAsFactors = F,
    sep = ";",
    dec = ".",
    encoding = "UTF-8",
    fileEncoding = "UTF-8-BOM"
  ) %>%
  dplyr::mutate(date = as.Date(date, "%d/%m/%Y")) %>%
  dplyr::rename(!!!setNames(ant_v1_24h, nue_v1_24h))

# Fechas de datos validados -----------------------------------------------

# a) Horarios:

VF_1h <-
  bdv_1h %>%
  tidyr::gather(key = "VARIABLES", value = "VALOR", 2:ncol(.), factor_key = T) %>% 
  tidyr::drop_na(VALOR) %>% 
  dplyr::group_by(VARIABLES) %>% 
  dplyr::summarise(VFI = min(date), VFF = max(date), .groups = "drop") %>% 
  dplyr::mutate(
    VAL = dplyr::if_else(is.na(VFI), 0, 1)) # 1: presenta histórico, 0: no presenta histórico

# b) Diarios:

VF_24h <-
  bdv_24h %>%
  tidyr::gather(key = "VARIABLES", value = "VALOR", 2:ncol(.), factor_key = T) %>% 
  tidyr::drop_na(VALOR) %>% 
  dplyr::group_by(VARIABLES) %>% 
  dplyr::summarise(VFI = min(date), VFF = max(date), .groups = "drop") %>% 
  dplyr::mutate(
    VAL = dplyr::if_else(is.na(VFI), 0, 1)) # 1: presenta histórico, 0: no presenta histórico

# Nombre de variables de datos crudos a 5 minutos -------------------------

ant_c1 <-
  tb_an_c1 %>% 
  tidyr::drop_na(!!st) %>% 
  dplyr::pull(ANT_VAR_CRU)

nue_c1 <- 
  tb_an_c1 %>% 
  tidyr::drop_na(!!st) %>% 
  dplyr::pull(!!st)

# Fechas de datos crudos --------------------------------------------------

CF <-
  tb_cr %>% 
  dplyr::filter(ID_ESTACION == id_st) %>%
  dplyr::mutate(
    CFI = ymd_hms(CFI),
    CFF = ymd_hms(CFF)
    ) %>% 
  tidyr::drop_na(CFI) %>% 
  dplyr::mutate(
    VARIABLES = factor(VARIABLES, levels = unique(VARIABLES)),
    AUX = dplyr::if_else(stringr::str_detect(VARIABLES, "_ESTADO$|_FLAGS$"), "NA_character_", "NA_real_"),
    EDN = dplyr::if_else(
      stringr::str_detect(VARIABLES, "_ESTADO$|_FLAGS$"),
      NA_real_,
      dplyr::if_else(is.na(CFI), 0, 1)
    )) %>% # 1: transmite, 0: no transmite
  dplyr::mutate(VARIABLES1 = stringr::str_replace(VARIABLES, "_PPB$|_ESTADO$|_FLAGS$", "_UGM3")) %>% 
  dplyr::left_join(x = ., y = VF_1h, by = c("VARIABLES1" = "VARIABLES")) %>% 
    dplyr::mutate(
      CFI = dplyr::if_else(is.na(VFF), CFI, dplyr::if_else(VFF > CFI, VFF, CFI))
    ) %>% 
  dplyr::select(VARIABLES, CFI, CFF, AUX, EDN) # Evaluación de datos numéricos

# De ppb a ugm3 -----------------------------------------------------------

ppb_ugm3 <-
  tb_ugm3 %>% 
  dplyr::filter(ANTES %in% !!nue_c1)

# Coordenadas -------------------------------------------------------------

coord <- 
  tb_es %>% 
  dplyr::filter(ID_ESTACION == !!id_st) %>% 
  dplyr::select(LAT1, LON1, LAT2, LON2)

# Tabla de rosa de contaminante -------------------------------------------

sel_var <- 
  VF_1h %>% 
  dplyr::filter(stringr::str_detect(VARIABLES, "_UGM3$")) %>% 
  dplyr::pull(VARIABLES) %>% 
  as.character(.)

tb_ro_co <- 
  tb_ro_co_g %>% 
  dplyr::filter(VARIABLES %in% !!sel_var)


# Serie de tiempo ---------------------------------------------------------

tb_se_ti <-
  tb_se_ti_g %>%
  dplyr::filter(ID_ESTACION == !!id_st)

# Fuentes -----------------------------------------------------------------

source(file = "funciones/cargadata_1h.R", local = T, encoding = "UTF-8")
source(file = "funciones/cargadata_24h.R", local = T, encoding = "UTF-8")
source(file = "funciones/rosadeviento.R", local = T, encoding = "UTF-8")
source(file = "funciones/rosadevientomapa.R", local = T, encoding = "UTF-8")
source(file = "funciones/rosadecontaminante.R", local = T, encoding = "UTF-8")
source(file = "funciones/rosadecontaminantemapa.R", local = T, encoding = "UTF-8")
source(file = "funciones/seriedetiempo.R", local = T, encoding = "UTF-8")