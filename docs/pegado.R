# =============================================================================-
# Fecha: 2022-02-21 
# Paquetes a utilizar y pegado de la endireh 2021
# Autora: Ana Escoto
# =============================================================================-

# Introducción: `

## Paquetes ----

if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere
pacman::p_load(tidyverse,
               haven,
               sjlabelled, 
               janitor,
               ggpubr,
               magrittr,
               EDA,
               explore,
               gt)

## Cargando los datos ----

load("datos/bd_endireh_2021.RData")

names(TVIV)
names(TSDem)
names(TB_SEC_III) 
names(TB_SEC_IVaVD)

# Ojo hay variables que se repiten

endireh2021<-TVIV %>% 
  merge(TSDem, by=c("ID_VIV"), all.x = TRUE) %>% # fusiona TVIV con TSDEm usando en by el ID.
  select(-ends_with(".y")) %>% # quita todas las variables que terminan en .x
  rename_with(~ stringr::str_remove(.x,  pattern = ".x"),  ends_with(".x")) # elimina el .y de todas las variables
 
names(endireh2021)

endireh2021 %<>% 
  merge(TB_SEC_III, by=c("ID_VIV", "ID_PER"), all.x = TRUE) %>% 
  select(-ends_with(".y")) %>% # quita todas las variables que terminan en .x
  rename_with(~ stringr::str_remove(.x,  pattern = ".x"),  ends_with(".x")) 

names(endireh2021)

endireh2021 %<>% 
  merge(TB_SEC_IVaVD, by=c("ID_VIV", "ID_PER"), all.x = TRUE) %>% 
  select(-ends_with(".y")) %>% # quita todas las variables que terminan en .x
  rename_with(~ stringr::str_remove(.x,  pattern = ".x"),  ends_with(".x")) 

remove(TVIV, TSDem, TB_SEC_III, TB_SEC_IVaVD)

# 
# endireh2021_CDMX<-endireh2021 %>% 
#   filter(CVE_ENT=="09")
#          
# remove(endireh2021)

