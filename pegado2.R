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
# 

names(endireh2021)


endireh2021_corta<-endireh2021 %>% 
  select(ID_VIV:T_INSTRUM,starts_with("P4"), starts_with("P6_"), 
         starts_with("P8_"), starts_with("P13_"), starts_with("P15_"),
         starts_with("P18_"))


remove(endireh2021)


endireh2021_corta_muj<-endireh2021_corta %>% 
  filter(!is.na(T_INSTRUM)) %>% 
  dplyr::mutate_at(vars(PAREN, NIV, GRA, P2_8:P2_16 ), ~ as.numeric(.x))   # Vuelve numérico todas las variables .x
  

remove(endireh2021_corta)

names(endireh2021_corta_muj)

## Etiquetado de variables y valores de variables ----

### Variables generales ----

niv<-c("Ninguno",
       "Preescolar",
       "Primaria",
       "Secundaria",
       "Preparatoria o bachillerato",
       "Estudios técnicos o comerciales con primaria terminada",
       "Estudios técnicos o comerciales con secundaria terminada",
       "Estudios técnicos o comerciales con preparatoria terminada",
       "Normal con primaria o secundaria terminada",
       "Normal licenciatura",
       "Licenciatura o profesional",
       "Posgrado (Especialidad, Maestría o Doctorado)")

endireh2021_corta_muj %<>% 
  mutate(NIV=set_labels(NIV, labels=niv))

rm(niv)

# años de escolaridad


endireh2021_corta_muj %<>% 
  mutate(ANIOS_ESC = case_when(
    NIV==0 ~ 0, 
    NIV==1 ~ 0, 
    NIV==2 ~ GRA, 
    NIV==3 ~ GRA + 6, 
    NIV==4 ~ GRA + 9, 
    NIV==5 ~ GRA + 6, 
    NIV==6 ~ GRA + 9, 
    NIV==7 ~ GRA + 12, 
    NIV==8 ~ GRA + 6, 
    NIV==9 ~ GRA + 12, 
    NIV==10 ~ GRA + 12, 
    NIV==11 ~ GRA + 16  ))

# Descargar los datos abiertos 
# https://www.inegi.org.mx/contenidos/programas/endireh/2021/datosabiertos/conjunto_de_datos_endireh_2021_csv.zip


vars_demo<-c("PAREN",
             "SEXO",
             "EDAD",
             "P2_5",
             "P2_6",
             "NIV",
             "GRA",
             "P2_8",
             "P2_9",
             "P2_10",
             "P2_11",
             "P2_12",
             "P2_13",
             "P2_14",
             "P2_15",
             "P2_16")
#### loop ----
for (i in vars_demo) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TSDem/catalogos/",i,".csv"),
                       locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)

}
  

### label seccion demo ----
endireh2021_corta_muj %<>%		
  mutate(	NOMBRE	= set_label(NOMBRE, label="ETIQUETA")) %>% 
  mutate(	ID_VIV	= set_label(ID_VIV, label="Identificador de vivienda seleccionada")) %>% 
  mutate(	ID_PER	= set_label(ID_PER, label="Identificador de la mujer")) %>% 
  mutate(	UPM	= set_label(UPM, label="Control de vivienda (UPM)")) %>% 
  mutate(	VIV_SEL	= set_label(VIV_SEL, label="Vivienda seleccionada")) %>% 
  mutate(	CVE_ENT	= set_label(CVE_ENT, label="Clave Entidad")) %>% 
  mutate(	NOM_ENT	= set_label(NOM_ENT, label="Nombre Entidad Federativa")) %>% 
  mutate(	CVE_MUN	= set_label(CVE_MUN, label="Clave Municipio")) %>% 
  mutate(	NOM_MUN	= set_label(NOM_MUN, label="Nombre Municipio")) %>% 
  mutate(	HOGAR	= set_label(HOGAR, label="Control del hogar")) %>% 
  mutate(	N_REN	= set_label(N_REN, label="Número de renglón")) %>% 
  mutate(	NOMBRE	= set_label(NOMBRE, label="NOMBRE")) %>% 
  mutate(	PAREN	= set_label(PAREN, label="Parentesco")) %>% 
  mutate(	SEXO	= set_label(SEXO, label="SEXO")) %>% 
  mutate(	EDAD	= set_label(EDAD, label="EDAD")) %>% 
  mutate(	P2_5	= set_label(P2_5, label="Residencia en la vivienda de la madre")) %>% 
  mutate(	P2_6	= set_label(P2_6, label="Residencia en la vivienda del padre")) %>% 
  mutate(	NIV	= set_label(NIV, label="Nivel")) %>% 
  mutate(	GRA	= set_label(GRA, label="Grado")) %>% 
  mutate(	P2_8	= set_label(P2_8, label="Alfabetismo")) %>% 
  mutate(	P2_9	= set_label(P2_9, label="Asistencia escolar")) %>% 
  mutate(	P2_10	= set_label(P2_10, label="Pertenencia indígena")) %>% 
  mutate(	P2_11	= set_label(P2_11, label="Lengua indígena")) %>% 
  mutate(	P2_12	= set_label(P2_12, label="Habla español")) %>% 
  mutate(	P2_13	= set_label(P2_13, label="Condición de actividad")) %>% 
  mutate(	P2_14	= set_label(P2_14, label="Verificación de condición de actividad")) %>% 
  mutate(	P2_15	= set_label(P2_15, label="Posición en la ocupación")) %>% 
  mutate(	P2_16	= set_label(P2_16, label="Estado conyugal")) %>% 
  mutate(	COD_M15	= set_label(COD_M15, label="Código Mujer 15 años y más")) %>% 
  mutate(	CODIGO	= set_label(CODIGO, label="Código de la mujer seleccionada")) %>% 
  mutate(	REN_MUJ_EL	= set_label(REN_MUJ_EL, label="Número de renglón de la mujer elegida")) %>% 
  mutate(	REN_INF_AD	= set_label(REN_INF_AD, label="Número de renglón de la informante")) %>% 
  mutate(	FAC_VIV	= set_label(FAC_VIV, label="Factor de expansión de la vivienda")) %>% 
  mutate(	FAC_MUJ	= set_label(FAC_MUJ, label="Factor de expansión mujer")) %>% 
  mutate(	DOMINIO	= set_label(DOMINIO, label="DOMINIO")) %>% 
  mutate(	ESTRATO	= set_label(ESTRATO, label="ESTRATO")) %>% 
  mutate(	EST_DIS	= set_label(EST_DIS, label="Estrato de diseño muestral")) %>% 
  mutate(	UPM_DIS	= set_label(UPM_DIS, label="Unidad primaria de muestreo"))

### label seccion 4 ----

endireh2021_corta_muj %<>%		
  mutate(	P4AB_1	= set_label(P4AB_1, label="Tipo de unión con la pareja o expareja")) %>% 
  mutate(	P4AB_2	= set_label(P4AB_2, label="Tiempo desde que su esposo o pareja no vive con usted")) %>% 
  mutate(	P4A_1	= set_label(P4A_1, label="Última visita del esposo o pareja")) %>% 
  mutate(	P4A_2	= set_label(P4A_2, label="Frecuencia de las visitas de la pareja ausente")) %>% 
  mutate(	P4B_1	= set_label(P4B_1, label="Situación con el exesposo o expareja")) %>% 
  mutate(	P4B_2	= set_label(P4B_2, label="Lugar de residencia del exesposo o expareja")) %>% 
  mutate(	P4BC_1	= set_label(P4BC_1, label="Edad de la pareja o expareja")) %>% 
  mutate(	P4BC_2	= set_label(P4BC_2, label="Nivel de escolaridad de la pareja o expareja ")) %>% 
  mutate(	P4C_1	= set_label(P4C_1, label="Asistencia escolar actual de la pareja o expareja")) %>% 
  mutate(	P4BC_3	= set_label(P4BC_3, label="Consideración de la pareja o expareja de ser indígena. ")) %>% 
  mutate(	P4BC_4	= set_label(P4BC_4, label="Pareja o expareja hablante de lengua indígena")) %>% 
  mutate(	P4BC_5	= set_label(P4BC_5, label="Pareja o expareja también hablante de español")) %>% 
  mutate(	P4_1	= set_label(P4_1, label="Situación laboral")) %>% 
  mutate(	P4_2	= set_label(P4_2, label="Salario")) %>% 
  mutate(	P4_2_1	= set_label(P4_2_1, label="Temporalidad del salario de la informante")) %>% 
  mutate(	P4_3	= set_label(P4_3, label="Ingreso económico de la pareja o expareja de la informante ")) %>% 
  mutate(	P4_4	= set_label(P4_4, label="Ocupación de la pareja o expareja. ")) %>% 
  mutate(	P4_4_CVE	= set_label(P4_4_CVE, label="Clave de la ocupación de la pareja o expareja de acuerdo al Sistema Nacional de Clasificación de Ocupaciones del INEGI (SINCO, 2019)")) %>% 
  mutate(	P4_5_AB	= set_label(P4_5_AB, label="Ingreso económico de la pareja o expareja.")) %>% 
  mutate(	P4_5_1_AB	= set_label(P4_5_1_AB, label="Temporalidad del salario de la pareja o expareja de la informante")) %>% 
  mutate(	P4_6_AB	= set_label(P4_6_AB, label="Aporte económico por parte de la pareja o expareja de la informante")) %>% 
  mutate(	P4_7_AB	= set_label(P4_7_AB, label="Cantidad de la aportación económica")) %>% 
  mutate(	P4_8_1	= set_label(P4_8_1, label="Ingreso por jubilación o pensión")) %>% 
  mutate(	P4_8_2	= set_label(P4_8_2, label="Ingreso por parte de familiares o conocidos que vivan en Estados Unidos de América")) %>% 
  mutate(	P4_8_3	= set_label(P4_8_3, label="Ingreso por parte de familiares o conocidos que viven dentro del país.")) %>% 
  mutate(	P4_8_4	= set_label(P4_8_4, label="Ingreso por beca escolar para sus hijos e hijas")) %>% 
  mutate(	P4_8_5	= set_label(P4_8_5, label="Ingreso por beca escolar propia")) %>% 
  mutate(	P4_8_6	= set_label(P4_8_6, label="Ingreso por pertenecer a algún programa del gobierno")) %>% 
  mutate(	P4_8_7	= set_label(P4_8_7, label="Ingreso por otro medio ")) %>% 
  mutate(	P4_9_1	= set_label(P4_9_1, label="Cantidad recibida al mes por jubilación o pensión")) %>% 
  mutate(	P4_9_2	= set_label(P4_9_2, label="Cantidad recibida al mes por parte de familiares o conocidos que viven en EUA")) %>% 
  mutate(	P4_10_2_1	= set_label(P4_10_2_1, label="Aporte económico por parte del primer familiar o conocido que vive en EUA. ")) %>% 
  mutate(	P4_10_2_2	= set_label(P4_10_2_2, label="Aporte económico por parte del segundo familiar o conocido que vive en EUA. ")) %>% 
  mutate(	P4_10_2_3	= set_label(P4_10_2_3, label="Aporte económico por parte del tercer familiar o conocido que vive en EUA. ")) %>% 
  mutate(	P4_9_3	= set_label(P4_9_3, label="Cantidad recibida al mes por parte de familiares o conocidos que viven en México")) %>% 
  mutate(	P4_10_3_1	= set_label(P4_10_3_1, label="Aporte económico por parte del primer familiar o conocido que vive dentro del país")) %>% 
  mutate(	P4_10_3_2	= set_label(P4_10_3_2, label="Aporte económico por parte del segundo familiar o conocido que vive dentro del país")) %>% 
  mutate(	P4_10_3_3	= set_label(P4_10_3_3, label="Aporte económico por parte del tercer familiar o conocido que vive dentro del país")) %>% 
  mutate(	P4_9_4	= set_label(P4_9_4, label="Cantidad recibida al mes por becas escolares para sus hijos e hijas")) %>% 
  mutate(	P4_9_5	= set_label(P4_9_5, label="Cantidad recibida al mes por becas escolares para la informante")) %>% 
  mutate(	P4_9_6	= set_label(P4_9_6, label="Cantidad recibida al mes por programas del gobierno")) %>% 
  mutate(	P4_9_7	= set_label(P4_9_7, label="Cantidad recibida al mes por otro tipo de situación")) %>% 
  mutate(	P4_11	= set_label(P4_11, label="Autonomía para usar los recursos monetarios en lo que se desee")) %>% 
  mutate(	P4_12_1	= set_label(P4_12_1, label="Propietario(s) de terreno(s) o tierras de cultivo")) %>% 
  mutate(	P4_12_2	= set_label(P4_12_2, label="Propietario(s) de automóvil(es) o camioneta(s)")) %>% 
  mutate(	P4_12_3	= set_label(P4_12_3, label="Propietario(s) de ahorros")) %>% 
  mutate(	P4_12_4	= set_label(P4_12_4, label="Propietario(s) de la vivienda habitada")) %>% 
  mutate(	P4_12_5	= set_label(P4_12_5, label="Propietario(s) de locales, bodegas u oficinas")) %>% 
  mutate(	P4_12_6	= set_label(P4_12_6, label="Propietario(s) de puestos fijos")) %>% 
  mutate(	P4_12_7	= set_label(P4_12_7, label="Propietario(s) de otra casa o departamento")) %>% 
  mutate(	P4_13_1	= set_label(P4_13_1, label="Persona propietaria de bienes inmuebles ")) %>% 
  mutate(	P4_13_2	= set_label(P4_13_2, label="Persona propietaria de automóvil(es) o camioneta(s)")) %>% 
  mutate(	P4_13_3	= set_label(P4_13_3, label="Persona propietaria de ahorros")) %>% 
  mutate(	P4_13_4	= set_label(P4_13_4, label="Persona propietaria de la vivienda que habita la informante  ")) %>% 
  mutate(	P4_13_5	= set_label(P4_13_5, label="Persona propietaria de locales, bodegas u oficinas")) %>% 
  mutate(	P4_13_6	= set_label(P4_13_6, label="Persona propietaria de puestos fijos")) %>% 
  mutate(	P4_13_7	= set_label(P4_13_7, label="Persona propietaria de otra casa o departamento"))


vars_sec4<-c("P4AB_1",
             "P4AB_2",
             "P4A_1",
             "P4A_2",
             "P4B_1",
             "P4B_2",
             "P4BC_1",
             "P4BC_2",
             "P4C_1",
             "P4BC_3",
             "P4BC_4",
             "P4BC_5",
             "P4_1",
             "P4_2",
             "P4_2_1",
             "P4_3",
             "P4_4",
             "P4_4_CVE",
             "P4_5_AB",
             "P4_5_1_AB",
             "P4_6_AB",
             "P4_7_AB",
             "P4_8_1",
             "P4_8_2",
             "P4_8_3",
             "P4_8_4",
             "P4_8_5",
             "P4_8_6",
             "P4_8_7",
             "P4_9_1",
             "P4_9_2",
             "P4_10_2_1",
             "P4_10_2_2",
             "P4_10_2_3",
             "P4_9_3",
             "P4_10_3_1",
             "P4_10_3_2",
             "P4_10_3_3",
             "P4_9_4",
             "P4_9_5",
             "P4_9_6",
             "P4_9_7",
             "P4_11",
             "P4_12_1",
             "P4_12_2",
             "P4_12_3",
             "P4_12_4",
             "P4_12_5",
             "P4_12_6",
             "P4_12_7")
             
vars_sec4_bis<-c(
             "P4_13_1",
             "P4_13_2",
             "P4_13_3",
             "P4_13_4",
             "P4_13_5",
             "P4_13_6",
             "P4_13_7")

#### loop ----
for (i in vars_sec4) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_IV/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)
  
}

for (i in vars_sec4_bis) {
  j<-i
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_IV/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip[1:9])
  
}



### label seccion 6 ----
endireh2021_corta_muj %<>%		
  mutate(	NOMBRE	= set_label(NOMBRE, label="ETIQUETA")) %>% 
  mutate(	ID_VIV	= set_label(ID_VIV, label="Identificador de vivienda seleccionada")) %>% 
  mutate(	ID_PER	= set_label(ID_PER, label="Identificador de la mujer")) %>% 
  mutate(	UPM	= set_label(UPM, label="Control de vivienda (UPM)")) %>% 
  mutate(	VIV_SEL	= set_label(VIV_SEL, label="Vivienda seleccionada")) %>% 
  mutate(	HOGAR	= set_label(HOGAR, label="Control del hogar")) %>% 
  mutate(	N_REN	= set_label(N_REN, label="Número de renglón de la mujer elegida")) %>% 
  mutate(	DOMINIO	= set_label(DOMINIO, label="DOMINIO")) %>% 
  mutate(	CVE_ENT	= set_label(CVE_ENT, label="Clave Entidad")) %>% 
  mutate(	NOM_ENT	= set_label(NOM_ENT, label="Nombre Entidad Federativa")) %>% 
  mutate(	CVE_MUN	= set_label(CVE_MUN, label="Clave Municipio")) %>% 
  mutate(	NOM_MUN	= set_label(NOM_MUN, label="Nombre Municipio")) %>% 
  mutate(	T_INSTRUM	= set_label(T_INSTRUM, label="Tipo de cuestionario aplicado a la mujer elegida")) %>% 
  mutate(	P6_1_1	= set_label(P6_1_1, label="Creencia sobre quien debe ser responsable del cuidado de de los hijos(as), de las personas enfermas y ancianas.")) %>% 
  mutate(	P6_1_2	= set_label(P6_1_2, label="Creencia sobre quien debe ganar más salario en el trabajo.")) %>% 
  mutate(	P6_1_3	= set_label(P6_1_3, label="Creencia sobre quien debe ser el responsable de las tareas de la casa.")) %>% 
  mutate(	P6_1_4	= set_label(P6_1_4, label="Creencia sobre quien debe ser el responsable de traer dinero para la casa.")) %>% 
  mutate(	P6_1_5	= set_label(P6_1_5, label="Creencia sobre quien tiene mayor capacidad para trabajar y/o estudiar.")) %>% 
  mutate(	P6_2_1	= set_label(P6_2_1, label="Creencia sobre si hombres y mujeres tienen el mismo derecho a salir por las noches a divertirse.")) %>% 
  mutate(	P6_2_2	= set_label(P6_2_2, label="Creencia sobre que las mujeres que tienen hijos(as) trabajen, aún si no tienen necesidad de hacerlo.")) %>% 
  mutate(	P6_2_3	= set_label(P6_2_3, label="Creencia sobre estar de acuerdo en que las mujeres que se visten con escotes provocan que los hombres las molesten.")) %>% 
  mutate(	P6_2_4	= set_label(P6_2_4, label="Creencia sobre estar de acuerdo en que las mujeres casadas deben tener relaciones sexuales con su esposo cuando él quiera."))

vars_sec6<-c("P6_1_1",
            "P6_1_2",
            "P6_1_3",
            "P6_1_4",
            "P6_1_5",
            "P6_2_1",
            "P6_2_2",
            "P6_2_3",
            "P6_2_4")

for (i in vars_sec6) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_VI/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)
  
}

endireh2021_corta_muj %>% 
  mutate(P6_1_1=as_label(P6_1_1)) %>% 
  tabyl(P6_1_1)

### label seccion 8 ----

endireh2021_corta_muj %<>%		
  mutate(	NOMBRE	= set_label(NOMBRE, label="ETIQUETA")) %>% 
  mutate(	ID_VIV	= set_label(ID_VIV, label="Identificador de vivienda seleccionada")) %>% 
  mutate(	ID_PER	= set_label(ID_PER, label="Identificador de la mujer")) %>% 
  mutate(	UPM	= set_label(UPM, label="Control de vivienda (UPM)")) %>% 
  mutate(	VIV_SEL	= set_label(VIV_SEL, label="Vivienda seleccionada")) %>% 
  mutate(	HOGAR	= set_label(HOGAR, label="Control del hogar")) %>% 
  mutate(	N_REN	= set_label(N_REN, label="Número de renglón de la mujer elegida")) %>% 
  mutate(	DOMINIO	= set_label(DOMINIO, label="DOMINIO")) %>% 
  mutate(	CVE_ENT	= set_label(CVE_ENT, label="Clave Entidad")) %>% 
  mutate(	NOM_ENT	= set_label(NOM_ENT, label="Nombre Entidad Federativa")) %>% 
  mutate(	CVE_MUN	= set_label(CVE_MUN, label="Clave Municipio")) %>% 
  mutate(	NOM_MUN	= set_label(NOM_MUN, label="Nombre Municipio")) %>% 
  mutate(	T_INSTRUM	= set_label(T_INSTRUM, label="Tipo de cuestionario aplicado a la mujer elegida")) %>% 
  mutate(	P8_1	= set_label(P8_1, label="Ha trabajado por un pago")) %>% 
  mutate(	P8_2	= set_label(P8_2, label="Trabajó al menos una semana de octubre de 2016 a la fecha")) %>% 
  mutate(	P8_3_1_1	= set_label(P8_3_1_1, label="Condicionantes laborales por prueba de embarazo: para ingresar")) %>% 
  mutate(	P8_3_1_2	= set_label(P8_3_1_2, label="Condicionantes laborales por prueba de embarazo: para continuar")) %>% 
  mutate(	P8_3_2_1	= set_label(P8_3_2_1, label="Discriminación laboral por embarazo: Despido")) %>% 
  mutate(	P8_3_2_2	= set_label(P8_3_2_2, label="Discriminación laboral por embarazo: No recontratación")) %>% 
  mutate(	P8_3_2_3	= set_label(P8_3_2_3, label="Discriminación laboral por embarazo: Disminución del salario o prestaciones")) %>% 
  mutate(	P8_4	= set_label(P8_4, label="Trabajo a partir de octubre de 2020")) %>% 
  mutate(	P8_5	= set_label(P8_5, label="Posición en la ocupación")) %>% 
  mutate(	P8_6	= set_label(P8_6, label="Ocupación")) %>% 
  mutate(	P8_6_CVE	= set_label(P8_6_CVE, label="Clave de la ocupación de la pareja o expareja de acuerdo al Sistema Nacional de Clasificación de Ocupaciones del INEGI (SINCO, 2019)")) %>% 
  mutate(	P8_7	= set_label(P8_7, label="Lugar de trabajo")) %>% 
  mutate(	P8_8_1	= set_label(P8_8_1, label="Discriminación laboral: salario menor que los hombres")) %>% 
  mutate(	P8_8_2	= set_label(P8_8_2, label="Discriminación laboral: menor oportunidad de ascenso que los hombres")) %>% 
  mutate(	P8_8_3	= set_label(P8_8_3, label="Discriminación laboral: menores prestaciones que los hombres")) %>% 
  mutate(	P8_8_4	= set_label(P8_8_4, label="Discriminación laboral: por edad, estado civil o tener hijos")) %>% 
  mutate(	P8_8_5	= set_label(P8_8_5, label="Discriminación laboral: prueba de embarazo para ingreso o permanencia")) %>% 
  mutate(	P8_8_6	= set_label(P8_8_6, label="Discriminación laboral: medidas injustificadas por embarazo")) %>% 
  mutate(	P8_8_7	= set_label(P8_8_7, label="Discriminación laboral: limitación profesional para favorecer a un hombre")) %>% 
  mutate(	P8_8_8	= set_label(P8_8_8, label="Discriminación laboral: labores reservadas para hombres")) %>% 
  mutate(	P8_8_9	= set_label(P8_8_9, label="Discriminación laboral: descalificación por género")) %>% 
  mutate(	P8_9_1	= set_label(P8_9_1, label="Víctima de violencia laboral: Intimidación sexual/acoso sexual por medios digitales o mediáticos.")) %>% 
  mutate(	P8_9_2	= set_label(P8_9_2, label="Víctima de violencia laboral: intimidación y acecho psicológico por medios digitales o mediáticos")) %>% 
  mutate(	P8_9_3	= set_label(P8_9_3, label="Víctima de violencia laboral: propuesta de mejoras a cambio de relaciones sexuales")) %>% 
  mutate(	P8_9_4	= set_label(P8_9_4, label="Víctima de violencia laboral: represalias por negarse a tener relaciones sexuales")) %>% 
  mutate(	P8_9_5	= set_label(P8_9_5, label="Víctima de violencia laboral: temor de agresión sexual")) %>% 
  mutate(	P8_9_6	= set_label(P8_9_6, label="Víctima de violencia laboral: comentarios sexuales ofensivos")) %>% 
  mutate(	P8_9_7	= set_label(P8_9_7, label="Víctima de violencia laboral: ofensas y humillaciones por condición de género")) %>% 
  mutate(	P8_9_8	= set_label(P8_9_8, label="Víctima de violencia laboral: patadas o golpes")) %>% 
  mutate(	P8_9_9	= set_label(P8_9_9, label="Víctima de violencia laboral: ataque o agresión con armas")) %>% 
  mutate(	P8_9_10	= set_label(P8_9_10, label="Víctima de violencia laboral: obligación a ver material de tipo sexual")) %>% 
  mutate(	P8_9_11	= set_label(P8_9_11, label="Víctima de violencia laboral: demeritación por argumento sexual")) %>% 
  mutate(	P8_9_12	= set_label(P8_9_12, label="Víctima de violencia laboral: acecho")) %>% 
  mutate(	P8_9_13	= set_label(P8_9_13, label="Víctima de violencia laboral: intento de violación sexual")) %>% 
  mutate(	P8_9_14	= set_label(P8_9_14, label="Víctima de violencia laboral: violación sexual")) %>% 
  mutate(	P8_9_15	= set_label(P8_9_15, label="Víctima de violencia laboral: manoseos o tocamientos")) %>% 
  mutate(	P8_9_16	= set_label(P8_9_16, label="Víctima de violencia laboral: exhibicionismo sexual")) %>% 
  mutate(	P8_9_17	= set_label(P8_9_17, label="Víctima de violencia laboral: invisibilización por género")) %>% 
  mutate(	P8_9_18	= set_label(P8_9_18, label="Víctima de violencia laboral: descalificación por trabajar")) %>% 
  mutate(	P8_9_19	= set_label(P8_9_19, label="Víctima de violencia laboral: otras agresiones físicas")) %>% 
  mutate(	P8_10_1_1	= set_label(P8_10_1_1, label="Primera persona que la intimidó sexualmente/acoso sexualmente por medios electrónicos en el trabajo")) %>% 
  mutate(	P8_10_1_2	= set_label(P8_10_1_2, label="Segunda persona que la intimidó sexualmente/acoso sexualmente por medios electrónicos en el trabajo")) %>% 
  mutate(	P8_10_1_3	= set_label(P8_10_1_3, label="Tercera persona que la intimidó sexualmente/acoso sexualmente por medios electrónicos en el trabajo")) %>% 
  mutate(	P8_11_1	= set_label(P8_11_1, label="Frecuencia de intimidación sexual/acoso sexual por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_1_1	= set_label(P8_12_1_1, label="Primera persona que la intimidó sexualmente/acoso sexualmente por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_1_2	= set_label(P8_12_1_2, label="Segunda persona que la intimidó sexualmente/acoso sexualmente por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_1_3	= set_label(P8_12_1_3, label="Tercera persona que la intimidó sexualmente/acoso sexualmente por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_10_2_1	= set_label(P8_10_2_1, label="Primera persona que la intimidó y acecho psicologicamente por medios electrónicos en el trabajo")) %>% 
  mutate(	P8_10_2_2	= set_label(P8_10_2_2, label="Segunda persona que la intimidó y acecho psicologicamente por medios electrónicos en el trabajo")) %>% 
  mutate(	P8_10_2_3	= set_label(P8_10_2_3, label="Tercera persona que la intimidó y acecho psicologicamente por medios electrónicos en el trabajo")) %>% 
  mutate(	P8_11_2	= set_label(P8_11_2, label="Frecuencia de intimidación y acecho psicológico por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_2_1	= set_label(P8_12_2_1, label="Primera persona que la intimidó y acecho psicologicamente por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_2_2	= set_label(P8_12_2_2, label="Segunda persona que la intimidó y acecho psicologicamente por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_2_3	= set_label(P8_12_2_3, label="Tercera persona que la intimidó y acecho psicologicamente por medios electrónicos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_10_3_1	= set_label(P8_10_3_1, label="Primera persona que le propuso mejoras en el trabajo a cambio de relaciones sexuales")) %>% 
  mutate(	P8_10_3_2	= set_label(P8_10_3_2, label="Segunda persona que le propuso mejoras en el trabajo a cambio de relaciones sexuales")) %>% 
  mutate(	P8_10_3_3	= set_label(P8_10_3_3, label="Tercera persona que le propuso mejoras en el trabajo a cambio de relaciones sexuales")) %>% 
  mutate(	P8_11_3	= set_label(P8_11_3, label="Frecuencia de propuesta de mejoras a cambio de relaciones sexuales en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_3_1	= set_label(P8_12_3_1, label="Primera persona que le propuso o insinuó mejoras a cambio de relaciones sexuales desde octubre de 2020")) %>% 
  mutate(	P8_12_3_2	= set_label(P8_12_3_2, label="Segunda persona que le propuso o insinuó mejoras a cambio de relaciones sexuales desde octubre de 2020")) %>% 
  mutate(	P8_12_3_3	= set_label(P8_12_3_3, label="Tercera persona que le propuso o insinuó mejoras a cambio de relaciones sexuales desde octubre de 2020")) %>% 
  mutate(	P8_13_3_1	= set_label(P8_13_3_1, label="Primer lugar físico de propuestas o insinuaciones sobre mejoras a cambio de relaciones sexuales en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_3_2	= set_label(P8_13_3_2, label="Segundo lugar físico de propuestas o insinuaciones sobre mejoras a cambio de relaciones sexuales en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_3_3	= set_label(P8_13_3_3, label="Tercer lugar físico de propuestas o insinuaciones sobre mejoras a cambio de relaciones sexuales en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_4_1	= set_label(P8_10_4_1, label="Primera persona que tomó represalias en el trabajo por negarse a tener relaciones sexuales")) %>% 
  mutate(	P8_10_4_2	= set_label(P8_10_4_2, label="Segunda persona que tomó represalias en el trabajo por negarse a tener relaciones sexuales")) %>% 
  mutate(	P8_10_4_3	= set_label(P8_10_4_3, label="Tercera persona que tomó represalias en el trabajo por negarse a tener relaciones sexuales")) %>% 
  mutate(	P8_11_4	= set_label(P8_11_4, label="Frecuencia de represalias por negarse a tener relaciones sexuales en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_4_1	= set_label(P8_12_4_1, label="Primera persona que tomó represalias en el trabajo por negarse a tener relaciones sexuales, de octubre de 2020 a la fecha")) %>% 
  mutate(	P8_12_4_2	= set_label(P8_12_4_2, label="Segunda persona que tomó represalias en el trabajo por negarse a tener relaciones sexuales, de octubre de 2020 a la fecha")) %>% 
  mutate(	P8_12_4_3	= set_label(P8_12_4_3, label="Tercera persona que tomó represalias en el trabajo por negarse a tener relaciones sexuales, de octubre de 2020 a la fecha")) %>% 
  mutate(	P8_13_4_1	= set_label(P8_13_4_1, label="Primer lugar físico de represalias en el trabajo por negarse a tener relaciones sexuales desde octubre de 2020")) %>% 
  mutate(	P8_13_4_2	= set_label(P8_13_4_2, label="Segundo lugar físico de represalias en el trabajo por negarse a tener relaciones sexuales desde octubre de 2020")) %>% 
  mutate(	P8_13_4_3	= set_label(P8_13_4_3, label="Tercer lugar físico de represalias en el trabajo por negarse a tener relaciones sexuales desde octubre de 2020")) %>% 
  mutate(	P8_10_5_1	= set_label(P8_10_5_1, label="Primera persona que le infundió temor de agresión sexual en el trabajo")) %>% 
  mutate(	P8_10_5_2	= set_label(P8_10_5_2, label="Segunda persona que le infundió temor de agresión sexual en el trabajo")) %>% 
  mutate(	P8_10_5_3	= set_label(P8_10_5_3, label="Tercera persona que le infundió temor de agresión sexual en el trabajo")) %>% 
  mutate(	P8_11_5	= set_label(P8_11_5, label="Frecuencia de infusión de temor de agresión sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_5_1	= set_label(P8_12_5_1, label="Primera persona que le infundió temor de agresión sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_5_2	= set_label(P8_12_5_2, label="Segunda persona que le infundió temor de agresión sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_5_3	= set_label(P8_12_5_3, label="Tercera persona que le infundió temor de agresión sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_13_5_1	= set_label(P8_13_5_1, label="Primer lugar físico de infusión de temor de agresión sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_5_2	= set_label(P8_13_5_2, label="Segundo lugar físico de infusión de temor de agresión sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_5_3	= set_label(P8_13_5_3, label="Tercer lugar físico de infusión de temor de agresión sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_6_1	= set_label(P8_10_6_1, label="Primera persona que le hizo comentarios ofensivos en el trabajo")) %>% 
  mutate(	P8_10_6_2	= set_label(P8_10_6_2, label="Segunda persona que le hizo comentarios ofensivos en el trabajo")) %>% 
  mutate(	P8_10_6_3	= set_label(P8_10_6_3, label="Tercera persona que le hizo comentarios ofensivos en el trabajo")) %>% 
  mutate(	P8_11_6	= set_label(P8_11_6, label="Frecuencia de comentarios ofensivos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_6_1	= set_label(P8_12_6_1, label="Primera persona que le hizo comentarios ofensivos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_6_2	= set_label(P8_12_6_2, label="Segunda persona que le hizo comentarios ofensivos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_6_3	= set_label(P8_12_6_3, label="Tercera persona que le hizo comentarios ofensivos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_13_6_1	= set_label(P8_13_6_1, label="Primer lugar físico de comentarios ofensivos en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_6_2	= set_label(P8_13_6_2, label="Segundo lugar físico de comentarios ofensivos en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_6_3	= set_label(P8_13_6_3, label="Tercer lugar físico de comentarios ofensivos en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_7_1	= set_label(P8_10_7_1, label="Primera persona que la ofendió por ser mujer en el trabajo")) %>% 
  mutate(	P8_10_7_2	= set_label(P8_10_7_2, label="Segunda persona que la ofendió por ser mujer en el trabajo")) %>% 
  mutate(	P8_10_7_3	= set_label(P8_10_7_3, label="Tercera persona que la ofendió por ser mujer en el trabajo")) %>% 
  mutate(	P8_11_7	= set_label(P8_11_7, label="Frecuencia de ofensas por condición de género en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_7_1	= set_label(P8_12_7_1, label="Primera persona que la ofendió por ser mujer en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_7_2	= set_label(P8_12_7_2, label="Segunda persona que la ofendió por ser mujer en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_7_3	= set_label(P8_12_7_3, label="Tercera persona que la ofendió por ser mujer en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_13_7_1	= set_label(P8_13_7_1, label="Primer lugar físico de la ofensa por ser mujer en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_7_2	= set_label(P8_13_7_2, label="Segundo lugar físico de la ofensa por ser mujer en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_7_3	= set_label(P8_13_7_3, label="Tercer lugar físico de la ofensa por ser mujer en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_8_1	= set_label(P8_10_8_1, label="Primera persona que la pateó o golpeó en el trabajo")) %>% 
  mutate(	P8_10_8_2	= set_label(P8_10_8_2, label="Segunda persona que la pateó o golpeó en el trabajo")) %>% 
  mutate(	P8_10_8_3	= set_label(P8_10_8_3, label="Tercera persona que la pateó o golpeó en el trabajo")) %>% 
  mutate(	P8_11_8	= set_label(P8_11_8, label="Frecuencia de patadas o golpes en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_8_1	= set_label(P8_12_8_1, label="Primera persona que la pateó o golpeó en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_8_2	= set_label(P8_12_8_2, label="Segunda persona que la pateó o golpeó en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_8_3	= set_label(P8_12_8_3, label="Tercera persona que la pateó o golpeó en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_13_8_1	= set_label(P8_13_8_1, label="Primer lugar físico de patadas o golpes en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_8_2	= set_label(P8_13_8_2, label="Segundo lugar físico de patadas o golpes en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_8_3	= set_label(P8_13_8_3, label="Tercer lugar físico de patadas o golpes en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_9_1	= set_label(P8_10_9_1, label="Primera persona que la atacó o agredió con armas en el trabajo")) %>% 
  mutate(	P8_10_9_2	= set_label(P8_10_9_2, label="Segunda persona que la atacó o agredió con armas en el trabajo")) %>% 
  mutate(	P8_10_9_3	= set_label(P8_10_9_3, label="Tercera persona que la atacó o agredió con armas en el trabajo")) %>% 
  mutate(	P8_11_9	= set_label(P8_11_9, label="Frecuencia de ataque o agresión con armas en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_9_1	= set_label(P8_12_9_1, label="Primera persona que la atacó o agredió con armas en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_9_2	= set_label(P8_12_9_2, label="Segunda persona que la atacó o agredió con armas en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_9_3	= set_label(P8_12_9_3, label="Tercera persona que la atacó o agredió con armas en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_13_9_1	= set_label(P8_13_9_1, label="Primer lugar físico de ataque o agresión con armas en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_9_2	= set_label(P8_13_9_2, label="Segundo lugar físico de ataque o agresión con armas en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_9_3	= set_label(P8_13_9_3, label="Tercer lugar físico de ataque o agresión con armas en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_10_1	= set_label(P8_10_10_1, label="Primera persona del trabajo que la obligó a ver material de tipo sexual")) %>% 
  mutate(	P8_10_10_2	= set_label(P8_10_10_2, label="Segunda persona del trabajo que la obligó a ver material de tipo sexual")) %>% 
  mutate(	P8_10_10_3	= set_label(P8_10_10_3, label="Tercera persona del trabajo que la obligó a ver material de tipo sexual")) %>% 
  mutate(	P8_11_10	= set_label(P8_11_10, label="Frecuencia de obligación a ver material de tipo sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_10_1	= set_label(P8_12_10_1, label="Primera persona del trabajo que la obligó a ver material de tipo sexual desde octubre de 2020")) %>% 
  mutate(	P8_12_10_2	= set_label(P8_12_10_2, label="Segunda persona del trabajo que la obligó a ver material de tipo sexual desde octubre de 2020")) %>% 
  mutate(	P8_12_10_3	= set_label(P8_12_10_3, label="Tercera persona del trabajo que la obligó a ver material de tipo sexual desde octubre de 2020")) %>% 
  mutate(	P8_13_10_1	= set_label(P8_13_10_1, label="Primer lugar físico donde le obligaron a ver material de tipo sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_10_2	= set_label(P8_13_10_2, label="Segundo lugar físico donde le obligaron a ver material de tipo sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_10_3	= set_label(P8_13_10_3, label="Tercer lugar físico donde le obligaron a ver material de tipo sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_11_1	= set_label(P8_10_11_1, label="Primera persona del trabajo que la demeritó con un argumento sexual")) %>% 
  mutate(	P8_10_11_2	= set_label(P8_10_11_2, label="Segunda persona del trabajo que la demeritó con un argumento sexual")) %>% 
  mutate(	P8_10_11_3	= set_label(P8_10_11_3, label="Tercera persona del trabajo que la demeritó con un argumento sexual")) %>% 
  mutate(	P8_11_11	= set_label(P8_11_11, label="Frecuencia de demeritación por argumento sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_11_1	= set_label(P8_12_11_1, label="Primera persona del trabajo que la demeritó con un argumento sexual desde octubre de 2020")) %>% 
  mutate(	P8_12_11_2	= set_label(P8_12_11_2, label="Segunda persona del trabajo que la demeritó con un argumento sexual desde octubre de 2020")) %>% 
  mutate(	P8_12_11_3	= set_label(P8_12_11_3, label="Tercera persona del trabajo que la demeritó con un argumento sexual desde octubre de 2020")) %>% 
  mutate(	P8_13_11_1	= set_label(P8_13_11_1, label="Primer lugar físico de demeritación por argumento sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_11_2	= set_label(P8_13_11_2, label="Segundo lugar físico de demeritación por argumento sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_11_3	= set_label(P8_13_11_3, label="Tercer lugar físico de demeritación por argumento sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_12_1	= set_label(P8_10_12_1, label="Primera persona del trabajo que la acechó")) %>% 
  mutate(	P8_10_12_2	= set_label(P8_10_12_2, label="Segunda persona del trabajo que la acechó")) %>% 
  mutate(	P8_10_12_3	= set_label(P8_10_12_3, label="Tercera persona del trabajo que la acechó")) %>% 
  mutate(	P8_11_12	= set_label(P8_11_12, label="Frecuencia de acecho en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_12_1	= set_label(P8_12_12_1, label="Primera persona del trabajo que la acechó desde octubre de 2020")) %>% 
  mutate(	P8_12_12_2	= set_label(P8_12_12_2, label="Segunda persona del trabajo que la acechó desde octubre de 2020")) %>% 
  mutate(	P8_12_12_3	= set_label(P8_12_12_3, label="Tercera persona del trabajo que la acechó desde octubre de 2020")) %>% 
  mutate(	P8_13_12_1	= set_label(P8_13_12_1, label="Primer lugar físico de acecho en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_12_2	= set_label(P8_13_12_2, label="Segundo lugar físico de acecho en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_12_3	= set_label(P8_13_12_3, label="Tercer lugar físico de acecho en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_13_1	= set_label(P8_10_13_1, label="Primera persona del trabajo que la intentó violar")) %>% 
  mutate(	P8_10_13_2	= set_label(P8_10_13_2, label="Segunda persona del trabajo que la intentó violar")) %>% 
  mutate(	P8_10_13_3	= set_label(P8_10_13_3, label="Tercera persona del trabajo que la intentó violar")) %>% 
  mutate(	P8_11_13	= set_label(P8_11_13, label="Frecuencia de intento de violación sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_13_1	= set_label(P8_12_13_1, label="Primera persona del trabajo que la intentó violar desde octubre de 2020")) %>% 
  mutate(	P8_12_13_2	= set_label(P8_12_13_2, label="Segunda persona del trabajo que la intentó violar desde octubre de 2020")) %>% 
  mutate(	P8_12_13_3	= set_label(P8_12_13_3, label="Tercera persona del trabajo que la intentó violar desde octubre de 2020")) %>% 
  mutate(	P8_13_13_1	= set_label(P8_13_13_1, label="Primer lugar físico de intento de violación en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_13_2	= set_label(P8_13_13_2, label="Segundo lugar físico de intento de violación en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_13_3	= set_label(P8_13_13_3, label="Tercer lugar físico de intento de violación en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_14_1	= set_label(P8_10_14_1, label="Primera persona del trabajo que la violó")) %>% 
  mutate(	P8_10_14_2	= set_label(P8_10_14_2, label="Segunda persona del trabajo que la violó")) %>% 
  mutate(	P8_10_14_3	= set_label(P8_10_14_3, label="Tercera persona del trabajo que la violó")) %>% 
  mutate(	P8_11_14	= set_label(P8_11_14, label="Frecuencia de violación sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_14_1	= set_label(P8_12_14_1, label="Primera persona del trabajo que la violó desde octubre de 2020")) %>% 
  mutate(	P8_12_14_2	= set_label(P8_12_14_2, label="Segunda persona del trabajo que la violó desde octubre de 2020")) %>% 
  mutate(	P8_12_14_3	= set_label(P8_12_14_3, label="Tercera persona del trabajo que la violó desde octubre de 2020")) %>% 
  mutate(	P8_13_14_1	= set_label(P8_13_14_1, label="Primer lugar físico de violación en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_14_2	= set_label(P8_13_14_2, label="Segundo lugar físico de violación en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_14_3	= set_label(P8_13_14_3, label="Tercer lugar físico de violación en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_15_1	= set_label(P8_10_15_1, label="Primera persona del trabajo que la manoseó o tocó")) %>% 
  mutate(	P8_10_15_2	= set_label(P8_10_15_2, label="Segunda persona del trabajo que la manoseó o tocó")) %>% 
  mutate(	P8_10_15_3	= set_label(P8_10_15_3, label="Tercera persona del trabajo que la manoseó o tocó")) %>% 
  mutate(	P8_11_15	= set_label(P8_11_15, label="Frecuencia de manoseos o tocamientos en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_15_1	= set_label(P8_12_15_1, label="Primera persona del trabajo que la manoseó o tocó desde octubre de 2020")) %>% 
  mutate(	P8_12_15_2	= set_label(P8_12_15_2, label="Segunda persona del trabajo que la manoseó o tocó desde octubre de 2020")) %>% 
  mutate(	P8_12_15_3	= set_label(P8_12_15_3, label="Tercera persona del trabajo que la manoseó o tocó desde octubre de 2020")) %>% 
  mutate(	P8_13_15_1	= set_label(P8_13_15_1, label="Primer lugar físico de manoseos o tocamientos en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_15_2	= set_label(P8_13_15_2, label="Segundo lugar físico de manoseos o tocamientos en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_15_3	= set_label(P8_13_15_3, label="Tercer lugar físico de manoseos o tocamientos en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_16_1	= set_label(P8_10_16_1, label="Primera persona del trabajo que le exhibió sus partes íntimas")) %>% 
  mutate(	P8_10_16_2	= set_label(P8_10_16_2, label="Segunda persona del trabajo que le exhibió sus partes íntimas")) %>% 
  mutate(	P8_10_16_3	= set_label(P8_10_16_3, label="Tercera persona del trabajo que le exhibió sus partes íntimas")) %>% 
  mutate(	P8_11_16	= set_label(P8_11_16, label="Frecuencia de exhibicionismo sexual en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_16_1	= set_label(P8_12_16_1, label="Primera persona del trabajo que le exhibió sus partes íntimas desde octubre de 2020")) %>% 
  mutate(	P8_12_16_2	= set_label(P8_12_16_2, label="Segunda persona del trabajo que le exhibió sus partes íntimas desde octubre de 2020")) %>% 
  mutate(	P8_12_16_3	= set_label(P8_12_16_3, label="Tercera persona del trabajo que le exhibió sus partes íntimas desde octubre de 2020")) %>% 
  mutate(	P8_13_16_1	= set_label(P8_13_16_1, label="Primer lugar físico de exhibicionismo sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_16_2	= set_label(P8_13_16_2, label="Segundo lugar físico de exhibicionismo sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_16_3	= set_label(P8_13_16_3, label="Tercer lugar físico de exhibicionismo sexual en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_17_1	= set_label(P8_10_17_1, label="Primera persona del trabajo que la invisibilizó")) %>% 
  mutate(	P8_10_17_2	= set_label(P8_10_17_2, label="Segunda persona del trabajo que la invisibilizó")) %>% 
  mutate(	P8_10_17_3	= set_label(P8_10_17_3, label="Tercera persona del trabajo que la invisibilizó")) %>% 
  mutate(	P8_11_17	= set_label(P8_11_17, label="Frecuencia de invisibilización en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_17_1	= set_label(P8_12_17_1, label="Primera persona del trabajo que la invisibilizó desde octubre de 2020")) %>% 
  mutate(	P8_12_17_2	= set_label(P8_12_17_2, label="Segunda persona del trabajo que la invisibilizó desde octubre de 2020")) %>% 
  mutate(	P8_12_17_3	= set_label(P8_12_17_3, label="Tercera persona del trabajo que la invisibilizó desde octubre de 2020")) %>% 
  mutate(	P8_13_17_1	= set_label(P8_13_17_1, label="Primer lugar físico de invisibilización en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_17_2	= set_label(P8_13_17_2, label="Segundo lugar físico de invisibilización en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_17_3	= set_label(P8_13_17_3, label="Tercer lugar físico de invisibilización en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_18_1	= set_label(P8_10_18_1, label="Primera persona del trabajo que la descalificó por trabajar")) %>% 
  mutate(	P8_10_18_2	= set_label(P8_10_18_2, label="Segunda persona del trabajo que la descalificó por trabajar")) %>% 
  mutate(	P8_10_18_3	= set_label(P8_10_18_3, label="Tercera persona del trabajo que la descalificó por trabajar")) %>% 
  mutate(	P8_11_18	= set_label(P8_11_18, label="Frecuencia de descalificación por trabajar, en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_18_1	= set_label(P8_12_18_1, label="Primera persona del trabajo que la descalificó por trabajar desde octubre de 2020")) %>% 
  mutate(	P8_12_18_2	= set_label(P8_12_18_2, label="Segunda persona del trabajo que la descalificó por trabajar desde octubre de 2020")) %>% 
  mutate(	P8_12_18_3	= set_label(P8_12_18_3, label="Tercera persona del trabajo que la descalificó por trabajar desde octubre de 2020")) %>% 
  mutate(	P8_13_18_1	= set_label(P8_13_18_1, label="Primer lugar físico de descalificación por trabajar en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_18_2	= set_label(P8_13_18_2, label="Segundo lugar físico de descalificación por trabajar en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_18_3	= set_label(P8_13_18_3, label="Tercer lugar físico de descalificación por trabajar en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_10_19_1	= set_label(P8_10_19_1, label="Primera persona del trabajo que la agredió físicamente de otra forma")) %>% 
  mutate(	P8_10_19_2	= set_label(P8_10_19_2, label="Segunda persona del trabajo que la agredió físicamente de otra forma")) %>% 
  mutate(	P8_10_19_3	= set_label(P8_10_19_3, label="Tercera persona del trabajo que la agredió físicamente de otra forma")) %>% 
  mutate(	P8_11_19	= set_label(P8_11_19, label="Frecuencia de otras agresiones físicas en el trabajo desde octubre de 2020")) %>% 
  mutate(	P8_12_19_1	= set_label(P8_12_19_1, label="Primera persona del trabajo que la agredió físicamente de otra forma desde octubre de 2020")) %>% 
  mutate(	P8_12_19_2	= set_label(P8_12_19_2, label="Segunda persona del trabajo que la agredió físicamente de otra forma desde octubre de 2020")) %>% 
  mutate(	P8_12_19_3	= set_label(P8_12_19_3, label="Tercera persona del trabajo que la agredió físicamente de otra forma desde octubre de 2020")) %>% 
  mutate(	P8_13_19_1	= set_label(P8_13_19_1, label="Primer lugar físico de otras agresiones físicas en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_19_2	= set_label(P8_13_19_2, label="Segundo lugar físico de otras agresiones físicas en el ámbito laboral desde octubre de 2020")) %>% 
  mutate(	P8_13_19_3	= set_label(P8_13_19_3, label="Tercer lugar físico de otras agresiones físicas en el ámbito laboral desde octubre de 2020"))


vars_sec8<-c("P8_1",
             "P8_2",
             "P8_3_1_1",
             "P8_3_1_2",
             "P8_3_2_1",
             "P8_3_2_2",
             "P8_3_2_3",
             "P8_4",
             "P8_5",
             "P8_6",
             "P8_6_CVE",
             "P8_7",
             "P8_8_1",
             "P8_8_2",
             "P8_8_3",
             "P8_8_4",
             "P8_8_5",
             "P8_8_6",
             "P8_8_7",
             "P8_8_8",
             "P8_9_1",
             "P8_9_2",
             "P8_9_3",
             "P8_9_4",
             "P8_9_5",
             "P8_9_6",
             "P8_9_7",
             "P8_9_8",
             "P8_9_9",
             "P8_9_10",
             "P8_9_11",
             "P8_9_12",
             "P8_9_13",
             "P8_9_14",
             "P8_9_15",
             "P8_9_16",
             "P8_9_17",
             "P8_9_18",
             "P8_9_19",
             "P8_10_1_1",
             "P8_10_1_2",
             "P8_10_1_3",
             "P8_11_1",
             "P8_12_1_1",
             "P8_12_1_2",
             "P8_12_1_3",
             "P8_10_2_1",
             "P8_10_2_2",
             "P8_10_2_3",
             "P8_11_2",
             "P8_12_2_1",
             "P8_12_2_2",
             "P8_12_2_3",
             "P8_10_3_1",
             "P8_10_3_2",
             "P8_10_3_3",
             "P8_11_3",
             "P8_12_3_1",
             "P8_12_3_2",
             "P8_12_3_3",
             "P8_13_3_1",
             "P8_13_3_2",
             "P8_13_3_3",
             "P8_10_4_1",
             "P8_10_4_2",
             "P8_10_4_3",
             "P8_11_4",
             "P8_12_4_1",
             "P8_12_4_2",
             "P8_12_4_3",
             "P8_13_4_1",
             "P8_13_4_2",
             "P8_13_4_3",
             "P8_10_5_1",
             "P8_10_5_2",
             "P8_10_5_3",
             "P8_11_5",
             "P8_12_5_1",
             "P8_12_5_2",
             "P8_12_5_3",
             "P8_13_5_1",
             "P8_13_5_2",
             "P8_13_5_3",
             "P8_10_6_1",
             "P8_10_6_2",
             "P8_10_6_3",
             "P8_11_6",
             "P8_12_6_1",
             "P8_12_6_2",
             "P8_12_6_3",
             "P8_13_6_1",
             "P8_13_6_2",
             "P8_13_6_3",
             "P8_10_7_1",
             "P8_10_7_2",
             "P8_10_7_3",
             "P8_11_7",
             "P8_12_7_1",
             "P8_12_7_2",
             "P8_12_7_3",
             "P8_13_7_1",
             "P8_13_7_2",
             "P8_13_7_3",
             "P8_10_8_1",
             "P8_10_8_2",
             "P8_10_8_3",
             "P8_11_8",
             "P8_12_8_1",
             "P8_12_8_2",
             "P8_12_8_3",
             "P8_13_8_1",
             "P8_13_8_2",
             "P8_13_8_3",
             "P8_10_9_1",
             "P8_10_9_2",
             "P8_10_9_3",
             "P8_11_9",
             "P8_12_9_1",
             "P8_12_9_2",
             "P8_12_9_3",
             "P8_13_9_1",
             "P8_13_9_2",
             "P8_13_9_3",
             "P8_10_10_1",
             "P8_10_10_2",
             "P8_10_10_3",
             "P8_11_10",
             "P8_12_10_1",
             "P8_12_10_2",
             "P8_12_10_3",
             "P8_13_10_1",
             "P8_13_10_2",
             "P8_13_10_3",
             "P8_10_11_1",
             "P8_10_11_2",
             "P8_10_11_3",
             "P8_11_11",
             "P8_12_11_1",
             "P8_12_11_2",
             "P8_12_11_3",
             "P8_13_11_1",
             "P8_13_11_2",
             "P8_13_11_3",
             "P8_10_12_1",
             "P8_10_12_2",
             "P8_10_12_3",
             "P8_11_12",
             "P8_12_12_1",
             "P8_12_12_2",
             "P8_12_12_3",
             "P8_13_12_1",
             "P8_13_12_2",
             "P8_13_12_3",
             "P8_10_13_1",
             "P8_10_13_2",
             "P8_10_13_3",
             "P8_11_13",
             "P8_12_13_1",
             "P8_12_13_2",
             "P8_12_13_3",
             "P8_13_13_1",
             "P8_13_13_2",
             "P8_13_13_3",
             "P8_10_14_1",
             "P8_10_14_2",
             "P8_10_14_3",
             "P8_11_14",
             "P8_12_14_1",
             "P8_12_14_2",
             "P8_12_14_3",
             "P8_13_14_1",
             "P8_13_14_2",
             "P8_13_14_3",
             "P8_10_15_1",
             "P8_10_15_2",
             "P8_10_15_3",
             "P8_11_15",
             "P8_12_15_1",
             "P8_12_15_2",
             "P8_12_15_3",
             "P8_13_15_1",
             "P8_13_15_2",
             "P8_13_15_3",
             "P8_10_16_1",
             "P8_10_16_2",
             "P8_10_16_3",
             "P8_11_16",
             "P8_12_16_1",
             "P8_12_16_2",
             "P8_12_16_3",
             "P8_13_16_1",
             "P8_13_16_2",
             "P8_13_16_3",
             "P8_10_17_1",
             "P8_10_17_2",
             "P8_10_17_3",
             "P8_11_17",
             "P8_12_17_1",
             "P8_12_17_2",
             "P8_12_17_3",
             "P8_13_17_1",
             "P8_13_17_2",
             "P8_13_17_3",
             "P8_10_18_1",
             "P8_10_18_2",
             "P8_10_18_3",
             "P8_11_18",
             "P8_12_18_1",
             "P8_12_18_2",
             "P8_12_18_3",
             "P8_13_18_1",
             "P8_13_18_2",
             "P8_13_18_3",
             "P8_10_19_1",
             "P8_10_19_2",
             "P8_10_19_3",
             "P8_11_19",
             "P8_12_19_1",
             "P8_12_19_2",
             "P8_12_19_3",
             "P8_13_19_1",
             "P8_13_19_2",
             "P8_13_19_3")

#### loop ----
for (i in vars_sec8) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_VIII/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)
  
}

# variables en las que falla el loop              "P8_8_9"


### label seccion 13 ----

endireh2021_corta_muj %<>%		
  mutate(	P13_B	= set_label(P13_B, label="Situación con la expareja")) %>% 
  mutate(	P13_B_1_1	= set_label(P13_B_1_1, label="Trato con la expareja solo por sus hijos")) %>% 
  mutate(	P13_B_1_2	= set_label(P13_B_1_2, label="Trato con la expareja por asuntos económicos")) %>% 
  mutate(	P13_B_1_3	= set_label(P13_B_1_3, label="Trato con la expareja por amistad")) %>% 
  mutate(	P13_B_1_4	= set_label(P13_B_1_4, label="Trato con la expareja, como pareja sin vivir juntos")) %>% 
  mutate(	P13_B_1_5	= set_label(P13_B_1_5, label="No tiene trato con la expareja")) %>% 
  mutate(	P13_B_1_6	= set_label(P13_B_1_6, label="Expareja falleció después de la separación")) %>% 
  mutate(	P13_B_1_7	= set_label(P13_B_1_7, label="Trato con la expareja de otra forma")) %>% 
  mutate(	P13_B_2	= set_label(P13_B_2, label="Tiempo sin ver a la expareja")) %>% 
  mutate(	P13_B_3	= set_label(P13_B_3, label="Tipo de trato con la expareja")) %>% 
  mutate(	P13_C_1	= set_label(P13_C_1, label="Condición de tener o haber tenido pareja")) %>% 
  mutate(	P13_C_2	= set_label(P13_C_2, label="Tiempo desde el término de su relación de pareja")) %>% 
  mutate(	P13_C_3	= set_label(P13_C_3, label="Tiempo de duración de la relación de pareja")) %>% 
  mutate(	P13_C_4	= set_label(P13_C_4, label="Situación conyugal de la última pareja")) %>% 
  mutate(	P13_1	= set_label(P13_1, label="Número de hijos")) %>% 
  mutate(	P13_2	= set_label(P13_2, label="Edad al nacer el primer hijo")) %>% 
  mutate(	P13_3	= set_label(P13_3, label="Hijos en común con la última pareja")) %>% 
  mutate(	P13_4	= set_label(P13_4, label="Hijos de la pareja con otras mujeres")) %>% 
  mutate(	P13_5	= set_label(P13_5, label="Sexo de la pareja actual")) %>% 
  mutate(	P13_6	= set_label(P13_6, label="Edad al tener su primera relación sexual")) %>% 
  mutate(	P13_7	= set_label(P13_7, label="Consentimiento de la primera relación sexual")) %>% 
  mutate(	P13_8_C	= set_label(P13_8_C, label="Condición de tener relaciones sexuales con su pareja actual")) %>% 
  mutate(	P13_8	= set_label(P13_8, label="Edad al iniciar última relación de pareja")) %>% 
  mutate(	P13_9	= set_label(P13_9, label="Edad al comienzo de la vida en matrimonio o concubinato")) %>% 
  mutate(	P13_10	= set_label(P13_10, label="Edad de la pareja al comienzo de la vida en matrimonio o concubinato")) %>% 
  mutate(	P13_10_C	= set_label(P13_10_C, label="Edad de la pareja al comienzo de la relación")) %>% 
  mutate(	P13_11	= set_label(P13_11, label="Razón de la unión con la expareja")) %>% 
  mutate(	P13_11_C	= set_label(P13_11_C, label="Parejas que ha tenido")) %>% 
  mutate(	P13_12	= set_label(P13_12, label="Personas con las que vivía en el matrimonio o concubinato")) %>% 
  mutate(	P13_12_C	= set_label(P13_12_C, label="Edad al tener su primer novio")) %>% 
  mutate(	P13_13	= set_label(P13_13, label="Número de veces casada o unida, contando su actual unión o matrimonio")) %>% 
  mutate(	P13_13_C	= set_label(P13_13_C, label="Condición de haber vivido en unión libre o en matrimonio con otra persona")) %>% 
  mutate(	P13_14	= set_label(P13_14, label="Edad al momento de su primer matrimonio o unión libre")) %>% 
  mutate(	P13_14_C	= set_label(P13_14_C, label="Número de veces casada o unida")) %>% 
  mutate(	P13_15AB	= set_label(P13_15AB, label="Edad de la pareja al momento de su primer matrimonio o unión libre")) %>% 
  mutate(	P13_15C	= set_label(P13_15C, label="Edad de la informante al momento de su primer matrimonio o unión libre")) %>% 
  mutate(	P13_16_1	= set_label(P13_16_1, label="Razón de la separación: su pareja tenía otra relación")) %>% 
  mutate(	P13_16_2	= set_label(P13_16_2, label="Razón de la separación: su pareja la abandonó")) %>% 
  mutate(	P13_16_3	= set_label(P13_16_3, label="Razón de la separación: Abandonó a su pareja")) %>% 
  mutate(	P13_16_4	= set_label(P13_16_4, label="Razón de la separación: su pareja no trabajaba")) %>% 
  mutate(	P13_16_5	= set_label(P13_16_5, label="Razón de la separación: su pareja no contribuía económicamente")) %>% 
  mutate(	P13_16_6	= set_label(P13_16_6, label="Razón de la separación: No se querían")) %>% 
  mutate(	P13_16_7	= set_label(P13_16_7, label="Razón de la separación: Otra relación suya")) %>% 
  mutate(	P13_16_8	= set_label(P13_16_8, label="Razón de la separación: Problemas de salud")) %>% 
  mutate(	P13_16_9	= set_label(P13_16_9, label="Razón de la separación: Su pareja no quería que trabajara")) %>% 
  mutate(	P13_16_10	= set_label(P13_16_10, label="Razón de la separación: Su pareja tenía problemas de alcohol o drogas")) %>% 
  mutate(	P13_16_11	= set_label(P13_16_11, label="Razón de la separación: Su pareja era grosera o agresiva")) %>% 
  mutate(	P13_16_12	= set_label(P13_16_12, label="Razón de la separación: Se fue a otro lugar")) %>% 
  mutate(	P13_16_13	= set_label(P13_16_13, label="Razón de la separación: Violencia física")) %>% 
  mutate(	P13_16_14	= set_label(P13_16_14, label="Razón de la separación: Violencia sexual")) %>% 
  mutate(	P13_16_15	= set_label(P13_16_15, label="Razón de la separación: Fallecimiento")) %>% 
  mutate(	P13_16_16	= set_label(P13_16_16, label="Razón de la separación: Otro")) %>% 
  mutate(	P13_16_16E	= set_label(P13_16_16E, label="Razón de la separación: Especifique")) %>% 
  mutate(	P13_17_1	= set_label(P13_17_1, label="Agresiones por la expareja: violencia física")) %>% 
  mutate(	P13_17_2	= set_label(P13_17_2, label="Agresiones por la expareja: con armas")) %>% 
  mutate(	P13_17_3	= set_label(P13_17_3, label="Agresiones por la expareja: violencia verbal")) %>% 
  mutate(	P13_17_4	= set_label(P13_17_4, label="Agresiones por la expareja: a otros integrantes de su hogar")) %>% 
  mutate(	P13_17_5	= set_label(P13_17_5, label="Agresiones por la expareja: Violación sexual o intento")) %>% 
  mutate(	P13_17_6	= set_label(P13_17_6, label="Agresiones por la expareja: Robo o despojo"))

vars_sec13<-c("P13_1",
              "P13_2",
              "P13_3",
              "P13_4",
              "P13_5",
              "P13_6",
              "P13_7",
              "P13_8_C",
              "P13_8",
              "P13_9",
              "P13_10",
              "P13_10_C",
              "P13_11",
              "P13_11_C",
              "P13_12",
              "P13_12_C",
              "P13_13",
              "P13_13_C",
              "P13_14",
              "P13_14_C",
              "P13_15AB",
              "P13_15C",
              "P13_16_1",
              "P13_16_2",
              "P13_16_3",
              "P13_16_4",
              "P13_16_5",
              "P13_16_6",
              "P13_16_7",
              "P13_16_8",
              "P13_16_9",
              "P13_16_10",
              "P13_16_11",
              "P13_16_12",
              "P13_16_13",
              "P13_16_14",
              "P13_16_15",
              "P13_16_16",
              "P13_16_16E",
              "P13_17_1",
              "P13_17_2",
              "P13_17_3",
              "P13_17_4",
              "P13_17_5",
              "P13_17_6")


#### loop ----
for (i in vars_sec13) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_XIII/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)
  
}

### label sección 13 bis ----

endireh2021_corta_muj %<>%		
  mutate(	P13_1_1_1	= set_label(P13_1_1_1, label="Conflicto con la pareja por su supuesto engaño")) %>% 
  mutate(	P13_1_1_2	= set_label(P13_1_1_2, label="Conflicto con la pareja por convivir con familia y amigos fuera de la casa")) %>% 
  mutate(	P13_1_1_3	= set_label(P13_1_1_3, label="Conflicto con la pareja por sus celos")) %>% 
  mutate(	P13_1_1_4	= set_label(P13_1_1_4, label="Conflicto por la pareja por no tener deseos sexuales")) %>% 
  mutate(	P13_1_1_5	= set_label(P13_1_1_5, label="Conflicto por la pareja por no tener cercanía entre ellos")) %>% 
  mutate(	P13_1_1_6	= set_label(P13_1_1_6, label="Conflicto con la pareja por desobedecerle")) %>% 
  mutate(	P13_1_1_7	= set_label(P13_1_1_7, label="Conflicto con la pareja por trabajar o estudiar")) %>% 
  mutate(	P13_1_1_8	= set_label(P13_1_1_8, label="Conflicto por la pareja por consumir alcohol o drogas")) %>% 
  mutate(	P13_1_1_9	= set_label(P13_1_1_9, label="Conflicto por la pareja por no colaborar en los quehaceres de la casa")) %>% 
  mutate(	P13_1_1_10	= set_label(P13_1_1_10, label="Conflicto por la pareja por su supuesto incumplimiento como madre o esposa")) %>% 
  mutate(	P13_1_1_11	= set_label(P13_1_1_11, label="Conflicto con la pareja por no tener hijos")) %>% 
  mutate(	P13_1_1_12	= set_label(P13_1_1_12, label="Conflicto por la pareja por el trato con sus hijos")) %>% 
  mutate(	P13_1_1_13	= set_label(P13_1_1_13, label="Conflicto con la pareja porque esta se enojaba sin razón aparente")) %>% 
  mutate(	P13_1_1_14	= set_label(P13_1_1_14, label="Conflicto con la pareja por otra situación")) %>% 
  mutate(	P13_1_2_1	= set_label(P13_1_2_1, label="Conflicto por supuesta infidelidad de la pareja")) %>% 
  mutate(	P13_1_2_2	= set_label(P13_1_2_2, label="Conflicto con la pareja por convivencia con familia y amigos fuera de la casa")) %>% 
  mutate(	P13_1_2_3	= set_label(P13_1_2_3, label="Conflicto con la pareja porque esta es celosa")) %>% 
  mutate(	P13_1_2_4	= set_label(P13_1_2_4, label="Conflicto con la pareja porque esta no desea tener relaciones sexuales")) %>% 
  mutate(	P13_1_2_5	= set_label(P13_1_2_5, label="Conflicto con la pareja porque esta no hay cercanía entre ellos")) %>% 
  mutate(	P13_1_2_6	= set_label(P13_1_2_6, label="Conflicto por desobediencia de la pareja")) %>% 
  mutate(	P13_1_2_7	= set_label(P13_1_2_7, label="Conflicto con la pareja porque esta no trabajaba")) %>% 
  mutate(	P13_1_2_8	= set_label(P13_1_2_8, label="Conflicto por alcoholismo o drogadicción de la pareja")) %>% 
  mutate(	P13_1_2_9	= set_label(P13_1_2_9, label="Conflicto con la pareja porque esta no ayudaba en las labores domésticas")) %>% 
  mutate(	P13_1_2_10	= set_label(P13_1_2_10, label="Conflicto con la pareja porque esta no cumplía como esposo o como padre")) %>% 
  mutate(	P13_1_2_11	= set_label(P13_1_2_11, label="Conflicto con la pareja porque esta no quería tener hijos")) %>% 
  mutate(	P13_1_2_12	= set_label(P13_1_2_12, label="Conflicto con la pareja porque a esta no le gustaba su forma de educar a los hijos.")) %>% 
  mutate(	P13_1_2_13	= set_label(P13_1_2_13, label="Conflicto con la pareja porque se enojaba por todo")) %>% 
  mutate(	P13_1_2_14	= set_label(P13_1_2_14, label="Conflicto con la pareja por una situación distinta")) %>% 
  mutate(	P13_1_3_1	= set_label(P13_1_3_1, label="Consecuencia del enojo de la pareja o expareja hacia la informante: indiferencia")) %>% 
  mutate(	P13_1_3_2	= set_label(P13_1_3_2, label="Consecuencia del enojo de la pareja o expareja hacia la informante: discutir o gritar")) %>% 
  mutate(	P13_1_3_3	= set_label(P13_1_3_3, label="Consecuencia del enojo de la pareja o expareja hacia la informante: ofender o insultar")) %>% 
  mutate(	P13_1_3_4	= set_label(P13_1_3_4, label="Consecuencia del enojo de la pareja o expareja hacia la informante: golpear o aventar cosas")) %>% 
  mutate(	P13_1_3_5	= set_label(P13_1_3_5, label="Consecuencia del enojo de la pareja o expareja hacia la informante: empujar o jalonear")) %>% 
  mutate(	P13_1_3_6	= set_label(P13_1_3_6, label="Consecuencia del enojo de la pareja o expareja hacia la informante: amenazar con golpearla o abandonarla")) %>% 
  mutate(	P13_1_3_7	= set_label(P13_1_3_7, label="Consecuencia del enojo de la pareja o expareja hacia la informante: golpear o agredir físicamente")) %>% 
  mutate(	P13_1_3_8	= set_label(P13_1_3_8, label="Consecuencia del enojo de la pareja o expareja hacia la informante: dejar de dar dinero para los gastos")) %>% 
  mutate(	P13_1_3_9	= set_label(P13_1_3_9, label="Consecuencia del enojo de la pareja o expareja hacia la informante: irse o ausentarse")) %>% 
  mutate(	P13_1_3_10	= set_label(P13_1_3_10, label="Consecuencia del enojo de la pareja o expareja hacia la informante: hablar o platicar para resolver el conflicto")) %>% 
  mutate(	P13_1_3_11	= set_label(P13_1_3_11, label="Consecuencia del enojo de la pareja o expareja hacia la informante: Otro")) %>% 
  mutate(	P13_1_3_12	= set_label(P13_1_3_12, label="Consecuencia del enojo de la pareja o expareja hacia la informante: No hay")) %>% 
  mutate(	P13_1_4_1	= set_label(P13_1_4_1, label="Consecuencia del enojo de la informante hacia su pareja o expareja: indiferencia")) %>% 
  mutate(	P13_1_4_2	= set_label(P13_1_4_2, label="Consecuencia del enojo de la informante hacia su pareja o expareja: discutir o gritar")) %>% 
  mutate(	P13_1_4_3	= set_label(P13_1_4_3, label="Consecuencia del enojo de la informante hacia su pareja o expareja: ofender o insultar")) %>% 
  mutate(	P13_1_4_4	= set_label(P13_1_4_4, label="Consecuencia del enojo de la informante hacia su pareja o expareja: golpear o aventar cosas")) %>% 
  mutate(	P13_1_4_5	= set_label(P13_1_4_5, label="Consecuencia del enojo de la informante hacia su pareja o expareja: empujar o jalonear")) %>% 
  mutate(	P13_1_4_6	= set_label(P13_1_4_6, label="Consecuencia del enojo de la informante hacia su pareja o expareja: amenazar con golpes o abandono")) %>% 
  mutate(	P13_1_4_7	= set_label(P13_1_4_7, label="Consecuencia del enojo de la informante hacia su pareja o expareja: agredir físicamente")) %>% 
  mutate(	P13_1_4_8	= set_label(P13_1_4_8, label="Consecuencia del enojo de la informante hacia su pareja o expareja: dejar de aportar dinero para la casa")) %>% 
  mutate(	P13_1_4_9	= set_label(P13_1_4_9, label="Consecuencia del enojo de la informante hacia su pareja o expareja: irse o ausentarse")) %>% 
  mutate(	P13_1_4_10	= set_label(P13_1_4_10, label="Consecuencia del enojo de la informante hacia su pareja o expareja: platicar para resolver los conflictos")) %>% 
  mutate(	P13_1_4_11	= set_label(P13_1_4_11, label="Consecuencia del enojo de la informante hacia su pareja o expareja: Otro")) %>% 
  mutate(	P13_1_4_12	= set_label(P13_1_4_12, label="Consecuencia del enojo de la informante hacia su pareja o expareja: No tienen problemas o conflictos")) %>% 
  mutate(	P13_1_5	= set_label(P13_1_5, label="Percepción de cambio en la frecuencia de problemas o conflictos en el ámbito de pareja"))


vars_sec13_bis<-c("P13_1_1_1",
                  "P13_1_1_2",
                  "P13_1_1_3",
                  "P13_1_1_4",
                  "P13_1_1_5",
                  "P13_1_1_6",
                  "P13_1_1_7",
                  "P13_1_1_8",
                  "P13_1_1_9",
                  "P13_1_1_10",
                  "P13_1_1_11",
                  "P13_1_1_12",
                  "P13_1_1_13",
                  "P13_1_1_14",
                  "P13_1_2_1",
                  "P13_1_2_2",
                  "P13_1_2_3",
                  "P13_1_2_4",
                  "P13_1_2_5",
                  "P13_1_2_6",
                  "P13_1_2_7",
                  "P13_1_2_8",
                  "P13_1_2_9",
                  "P13_1_2_10",
                  "P13_1_2_11",
                  "P13_1_2_12",
                  "P13_1_2_13",
                  "P13_1_2_14",
                  "P13_1_3_1",
                  "P13_1_3_2",
                  "P13_1_3_3",
                  "P13_1_3_4",
                  "P13_1_3_5",
                  "P13_1_3_6",
                  "P13_1_3_7",
                  "P13_1_3_8",
                  "P13_1_3_9",
                  "P13_1_3_10",
                  "P13_1_3_11",
                  "P13_1_3_12",
                  "P13_1_4_1",
                  "P13_1_4_2",
                  "P13_1_4_3",
                  "P13_1_4_4",
                  "P13_1_4_5",
                  "P13_1_4_6",
                  "P13_1_4_7",
                  "P13_1_4_8",
                  "P13_1_4_9",
                  "P13_1_4_10",
                  "P13_1_4_11",
                  "P13_1_4_12",
                  "P13_1_5")


#### loop ----
for (i in vars_sec13_bis) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_XIII.I/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)
  
}


### label sección 15 ----
endireh2021_corta_muj %<>%		
  mutate(	P15_1AB_1	= set_label(P15_1AB_1, label="Decidir sobre trabajar o estudiar")) %>% 
  mutate(	P15_1AB_2	= set_label(P15_1AB_2, label="Decidir sobre salir de su casa")) %>% 
  mutate(	P15_1AB_3	= set_label(P15_1AB_3, label="Decidir sobre el dinero que gana")) %>% 
  mutate(	P15_1AB_4	= set_label(P15_1AB_4, label="Decidir sobre comprar cosas para ella")) %>% 
  mutate(	P15_1AB_5	= set_label(P15_1AB_5, label="Decidir cuando quiere participar en la vida social")) %>% 
  mutate(	P15_1AB_6	= set_label(P15_1AB_6, label="Decidir cuando quiere participar en la vida política")) %>% 
  mutate(	P15_1AB_7	= set_label(P15_1AB_7, label="Decidir sobre cómo se gasta o economiza el dinero")) %>% 
  mutate(	P15_1AB_8	= set_label(P15_1AB_8, label="Decidir qué hacer con el dinero que él gana")) %>% 
  mutate(	P15_1AB_9	= set_label(P15_1AB_9, label="Decidir sobre su tipo de ropa y arreglo personal")) %>% 
  mutate(	P15_1AB_10	= set_label(P15_1AB_10, label="Decidir sobre los permisos a las hijas e hijos")) %>% 
  mutate(	P15_1AB_11	= set_label(P15_1AB_11, label="Decidir sobre su cambio de domicilio")) %>% 
  mutate(	P15_1AB_12	= set_label(P15_1AB_12, label="Decidir cuándo tener relaciones sexuales")) %>% 
  mutate(	P15_1AB_13	= set_label(P15_1AB_13, label="Decidir sobre el uso de anticonceptivos")) %>% 
  mutate(	P15_1AB_14	= set_label(P15_1AB_14, label="Decidir sobre el cuidado de la salud sexual y reproductiva")) %>% 
  mutate(	P15_1AB_15	= set_label(P15_1AB_15, label="Decidir sobre quién debe usar los métodos anticonceptivos")) %>% 
  mutate(	P15_1AB_16	= set_label(P15_1AB_16, label="Decidir sobre tener o no hijos")) %>% 
  mutate(	P15_1AB_17	= set_label(P15_1AB_17, label="Decidir sobre cuándo y cuántos hijos (as) tener")) %>% 
  mutate(	P15_2AB_1	= set_label(P15_2AB_1, label="Actitud del esposo o pareja; exesposo o expareja ante el que la informante trabaje o estudie")) %>% 
  mutate(	P15_2AB_2	= set_label(P15_2AB_2, label="Actitud del esposo o pareja; exesposo o expareja ante el que la informante salga de su casa")) %>% 
  mutate(	P15_2AB_3	= set_label(P15_2AB_3, label="Actitud del esposo o pareja; exesposo o expareja ante el que la informante decida qué hacer con el dinero que ella gana")) %>% 
  mutate(	P15_2AB_4	= set_label(P15_2AB_4, label="Actitud del esposo o pareja; exesposo o expareja ante el que la informante compre cosas para ella")) %>% 
  mutate(	P15_2AB_5	= set_label(P15_2AB_5, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante de participar en la vida social de su comunidad")) %>% 
  mutate(	P15_2AB_6	= set_label(P15_2AB_6, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante de participar en la vida política de su comunidad")) %>% 
  mutate(	P15_2AB_7	= set_label(P15_2AB_7, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre la forma de gastar o economizar el dinero")) %>% 
  mutate(	P15_2AB_8	= set_label(P15_2AB_8, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre el uso del dinero que él gana(ba)")) %>% 
  mutate(	P15_2AB_9	= set_label(P15_2AB_9, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre el tipo de ropa y arreglo personal de ella")) %>% 
  mutate(	P15_2AB_10	= set_label(P15_2AB_10, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre los permisos a los hijos e hijas")) %>% 
  mutate(	P15_2AB_11	= set_label(P15_2AB_11, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante de cambiarse o mudarse de casa o ciudad")) %>% 
  mutate(	P15_2AB_12	= set_label(P15_2AB_12, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre cuándo tener relaciones sexuales")) %>% 
  mutate(	P15_2AB_13	= set_label(P15_2AB_13, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre el uso de anticonceptivos")) %>% 
  mutate(	P15_2AB_14	= set_label(P15_2AB_14, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión sobre el cuidado de su salud sexual y reproductiva")) %>% 
  mutate(	P15_2AB_15	= set_label(P15_2AB_15, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre quién debe usar los métodos anticonceptivos")) %>% 
  mutate(	P15_2AB_16	= set_label(P15_2AB_16, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre tener o no tener hijos")) %>% 
  mutate(	P15_2AB_17	= set_label(P15_2AB_17, label="Actitud del esposo o pareja; exesposo o expareja ante la decisión de la informante sobre cuándo y cuántos hijos tener")) %>% 
  mutate(	P15_3AB_1	= set_label(P15_3AB_1, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Trabajar por pago o remuneración")) %>% 
  mutate(	P15_3AB_2	= set_label(P15_3AB_2, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Ir de compras")) %>% 
  mutate(	P15_3AB_3	= set_label(P15_3AB_3, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Visitar a parientes o amistades")) %>% 
  mutate(	P15_3AB_4	= set_label(P15_3AB_4, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Comprar algo o cambiar el arreglo personal")) %>% 
  mutate(	P15_3AB_5	= set_label(P15_3AB_5, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Participar en alguna actividad vecinal o política")) %>% 
  mutate(	P15_3AB_6	= set_label(P15_3AB_6, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Hacer amistad con una persona")) %>% 
  mutate(	P15_3AB_7	= set_label(P15_3AB_7, label="Arreglos con su esposo o pareja; exesposo o expareja para realizar actividades: Votar por algún partido o candidato")) %>% 
  mutate(	P15_1C_1	= set_label(P15_1C_1, label="Decisión sobre estudiar")) %>% 
  mutate(	P15_1C_2	= set_label(P15_1C_2, label="Decisión sobre trabajar")) %>% 
  mutate(	P15_1C_3	= set_label(P15_1C_3, label="Decisión sobre salir de su casa")) %>% 
  mutate(	P15_1C_4	= set_label(P15_1C_4, label="Decisión sobre salir a divertirse")) %>% 
  mutate(	P15_1C_5	= set_label(P15_1C_5, label="Decisión sobre salir a visitar familiares o amigas(os)")) %>% 
  mutate(	P15_1C_6	= set_label(P15_1C_6, label="Decisión sobre el uso del dinero que gana")) %>% 
  mutate(	P15_1C_7	= set_label(P15_1C_7, label="Decisión sobre comprar cosas para ella")) %>% 
  mutate(	P15_1C_8	= set_label(P15_1C_8, label="Decisión sobre participar en la vida social")) %>% 
  mutate(	P15_1C_9	= set_label(P15_1C_9, label="Decisión sobre participar en la vida social política")) %>% 
  mutate(	P15_1C_10	= set_label(P15_1C_10, label="Decisión sobre la ropa y arreglo personal")) %>% 
  mutate(	P15_1C_11	= set_label(P15_1C_11, label="Decisión sobre el voto")) %>% 
  mutate(	P15_1C_12	= set_label(P15_1C_12, label="Decisión sobre tener relaciones sexuales")) %>% 
  mutate(	P15_1C_13	= set_label(P15_1C_13, label="Decisión sobre si se usan anticonceptivos")) %>% 
  mutate(	P15_1C_14	= set_label(P15_1C_14, label="Decisión sobre el cuidado de la salud sexual y reproductiva")) %>% 
  mutate(	P15_1C_15	= set_label(P15_1C_15, label="Decisión sobre quién usa métodos anticonceptivos")) %>% 
  mutate(	P15_2C_1	= set_label(P15_2C_1, label="Arreglos para poder estudiar")) %>% 
  mutate(	P15_2C_2	= set_label(P15_2C_2, label="Arreglos para poder trabajar")) %>% 
  mutate(	P15_2C_3	= set_label(P15_2C_3, label="Arreglos para poder salir de casa")) %>% 
  mutate(	P15_2C_4	= set_label(P15_2C_4, label="Arreglos para poder salir de fiesta")) %>% 
  mutate(	P15_2C_5	= set_label(P15_2C_5, label="Arreglos para poder visitar a familiares o amigas(os)")) %>% 
  mutate(	P15_2C_6	= set_label(P15_2C_6, label="Arreglos para poder usar el dinero")) %>% 
  mutate(	P15_2C_7	= set_label(P15_2C_7, label="Arreglos para poder comprar cosas")) %>% 
  mutate(	P15_2C_8	= set_label(P15_2C_8, label="Arreglos para poder participar en la vida social de su comunidad")) %>% 
  mutate(	P15_2C_9	= set_label(P15_2C_9, label="Arreglos para poder participar en la vida política de su comunidad")) %>% 
  mutate(	P15_2C_10	= set_label(P15_2C_10, label="Arreglos sobre el tipo de ropa que usa y arreglo personal")) %>% 
  mutate(	P15_2C_11	= set_label(P15_2C_11, label="Arreglos para poder votar")) %>% 
  mutate(	P15_2C_12	= set_label(P15_2C_12, label="Arreglos para tener relaciones sexuales")) %>% 
  mutate(	P15_2C_13	= set_label(P15_2C_13, label="Arreglos para el uso de anticonceptivos")) %>% 
  mutate(	P15_2C_14	= set_label(P15_2C_14, label="Arreglos sobre el cuidado de su salud sexual y reproductiva")) %>% 
  mutate(	P15_2C_15	= set_label(P15_2C_15, label="Arreglos para saber quién usa los métodos anticonceptivos"))


vars_sec15<-c("P15_1AB_1",
              "P15_1AB_2",
              "P15_1AB_3",
              "P15_1AB_4",
              "P15_1AB_5",
              "P15_1AB_6",
              "P15_1AB_7",
              "P15_1AB_8",
              "P15_1AB_9",
              "P15_1AB_10",
              "P15_1AB_11",
              "P15_1AB_12",
              "P15_1AB_13",
              "P15_1AB_14",
              "P15_1AB_15",
              "P15_1AB_16",
              "P15_1AB_17",
              "P15_2AB_1",
              "P15_2AB_2",
              "P15_2AB_3",
              "P15_2AB_4",
              "P15_2AB_5",
              "P15_2AB_6",
              "P15_2AB_7",
              "P15_2AB_8",
              "P15_2AB_9",
              "P15_2AB_10",
              "P15_2AB_11",
              "P15_2AB_12",
              "P15_2AB_13",
              "P15_2AB_14",
              "P15_2AB_15",
              "P15_2AB_16",
              "P15_2AB_17",
              "P15_3AB_1",
              "P15_3AB_2",
              "P15_3AB_3",
              "P15_3AB_4",
              "P15_3AB_5",
              "P15_3AB_6",
              "P15_3AB_7",
              "P15_1C_1",
              "P15_1C_2",
              "P15_1C_3",
              "P15_1C_4",
              "P15_1C_5",
              "P15_1C_6",
              "P15_1C_7",
              "P15_1C_8",
              "P15_1C_9",
              "P15_1C_10",
              "P15_1C_11",
              "P15_1C_12",
              "P15_1C_13",
              "P15_1C_14",
              "P15_1C_15",
              "P15_2C_1",
              "P15_2C_2",
              "P15_2C_3",
              "P15_2C_4",
              "P15_2C_5",
              "P15_2C_6",
              "P15_2C_7",
              "P15_2C_8",
              "P15_2C_9",
              "P15_2C_10",
              "P15_2C_11",
              "P15_2C_12",
              "P15_2C_13",
              "P15_2C_14",
              "P15_2C_15")



#### loop ----
for (i in vars_sec15) {
  
  j<-i
  
  x <- read_csv(paste0("C:/Users/Usuario/Downloads/conjunto_de_datos_TB_SEC_XV/catalogos/",i,".csv"),
                locale = locale(encoding = "latin1"))
  
  endireh2021_corta_muj[[j]]<-as.numeric(endireh2021_corta_muj[[j]])
  
  endireh2021_corta_muj[[j]]<-set_labels(endireh2021_corta_muj[[j]], labels=x$descrip)
  
}

rm(x, i, j, vars_demo, vars_sec4, vars_sec4_bis, vars_sec6, vars_sec8, vars_sec13, vars_sec13_bis, vars_sec15)


# Indices para trabajar con variables cuantitativas ----

## Indice de roles -----

roles_6_2 <-  endireh2021_corta_muj %>% 
  select(ID_VIV, ID_PER, starts_with("P6_2") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P6_2")), ~na_if(.x, 3)) %>%  # manda los 3 a missing
  dplyr::mutate_at(vars(P6_2_1,P6_2_3), ~ (2 - .x)) %>%  # cambia la lógica de la numeración a 2 se le resta el valor. Cambia sentido
  dplyr::mutate_at(vars(P6_2_3,P6_2_4), ~ (.x - 1))  # cambia la lógica de la numeración a 2 se le resta el valor. Cambia sentido


roles_6_2 %<>% 
  mutate(index_roles= rowMeans(across(starts_with("P6_2")), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_roles)

endireh2021_corta_muj %<>% 
  merge(roles_6_2,by=c("ID_VIV", "ID_PER"), all.x=T) %>% 
  mutate(index_roles=set_label(index_roles, label="Indice roles de género (acuerdo/desacuerdo"))

rm(roles_6_2)


## Indice de violencia laboral ----

# Esta lleva filtro

# P8_1 1 - Sí, trabajo alguna vez

# P8_4  1 - Sí, trabajo en el último año

labo_8_9<-  endireh2021_corta_muj %>% 
  filter(P8_1==1) %>% 
  select(ID_VIV, ID_PER, starts_with("P8_9") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P8_9")), ~as.numeric(.x)) %>%  # vuelve numérico
  dplyr::mutate_at(vars(starts_with("P8_9")), ~na_if(.x, 9)) %>%  # manda los 9 a missing
  dplyr::mutate_at(vars(starts_with("P8_9")), ~ (2 - .x))  # cambia la lógica de la numeraci?n a 2 se le resta el valor. Cambia sentido


labo_8_9 %<>% 
  mutate(index_labo9= rowMeans(across(starts_with("P8_9")), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_labo9)

endireh2021_corta_muj %<>% 
  merge(labo_8_9 ,by=c("ID_VIV", "ID_PER"), all.x=T) %>% 
  mutate(index_labo9=set_label(index_labo9, label="Indice víctima de violencia laboral"))

rm(labo_8_9)
hist(endireh2021_corta_muj$index_labo9)

#### Intensidad ----

labo_8_11 <-  endireh2021_corta_muj %>% 
  filter(P8_4==1) %>% 
  select(ID_VIV, ID_PER, starts_with("P8_11") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P8_11")), ~as.numeric(.x)) %>%  # 
  dplyr::mutate_at(vars(starts_with("P8_11")), ~na_if(.x, 9)) %>%  # manda los 9 a missing
  dplyr::mutate_at(vars(starts_with("P8_11")), ~ (4 - .x))  # cambia la lógica de la numeraci?n a 2 se le resta el valor. Cambia sentido


labo_8_11 %<>% 
  mutate(index_labo11= rowMeans(across(starts_with("P8_11")), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_labo11)

endireh2021_corta_muj %<>% 
  merge(labo_8_11 ,by=c("ID_VIV", "ID_PER"), all.x=T) %>% 
  mutate(index_labo9=set_label(index_labo9, label="Indice frecuencia de violencia laboral en el último año"))


hist(endireh2021_corta_muj$index_labo11)
rm(labo_8_11)

## Indice de violencia pareja ----

# Esta lleva filtro

# C2 - sin novio


pareja_13_1_1 <-  endireh2021_corta_muj %>% 
  filter(!T_INSTRUM=="C2") %>% 
  select(ID_VIV, ID_PER, starts_with("P13_1_1") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P13_1_1")), ~as.numeric(.x)) %>%  # 
  dplyr::mutate_at(vars(starts_with("P13_1_1")), ~na_if(.x, 9)) %>%  # manda los 9  a missing
  dplyr::mutate_at(vars(starts_with("P13_1_1")), ~ (2 - .x))  # cambia la lógica de la numeraci?n a 2 se le resta el valor. Cambia sentido



pareja_13_1_1 %<>% 
  mutate(index_pareja_13_1_1= rowMeans(across(starts_with("P13_1_1")), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_pareja_13_1_1)

endireh2021_corta_muj %<>% 
  merge(pareja_13_1_1 ,by=c("ID_VIV", "ID_PER"), all.x=T) 


endireh2021_corta_muj %>% 
  select(starts_with("index")) %>% 
  summary

pareja_13_1_2 <-  endireh2021_corta_muj %>% 
  filter(!T_INSTRUM=="C2") %>% 
  select(ID_VIV, ID_PER, starts_with("P13_1_2") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P13_1_2")), ~as.numeric(.x)) %>%  # 
  dplyr::mutate_at(vars(starts_with("P13_1_2")), ~na_if(.x, 9)) %>%  # manda los 9  a missing
  dplyr::mutate_at(vars(starts_with("P13_1_2")), ~ (2 - .x))  # cambia la lógica de la numeraci?n a 2 se le resta el valor. Cambia sentido


pareja_13_1_2 %<>% 
  mutate(index_pareja_13_1_2= rowMeans(across(starts_with("P13_1_2")), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_pareja_13_1_2)

endireh2021_corta_muj %<>% 
  merge(pareja_13_1_2 ,by=c("ID_VIV", "ID_PER"), all.x=T) 





pareja_13_1_3 <-  endireh2021_corta_muj %>% 
  filter(!T_INSTRUM=="C2") %>% 
  select(ID_VIV, ID_PER, starts_with("P13_1_3") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P13_1_3")), ~as.numeric(.x)) %>%  # 
  dplyr::mutate_at(vars(starts_with("P13_1_3")), ~na_if(.x, 9)) %>%  # manda los 9  a missing
  dplyr::mutate_at(vars(starts_with("P13_1_3")), ~ (2 - .x))  # cambia la lógica de la numeraci?n a 2 se le resta el valor. Cambia sentido


pareja_13_1_3 %<>% 
  mutate(index_pareja_13_1_3= rowMeans(across(P13_1_3_1:P13_1_3_11), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_pareja_13_1_3)

endireh2021_corta_muj %<>% 
  merge(pareja_13_1_3 ,by=c("ID_VIV", "ID_PER"), all.x=T) 


pareja_13_1_4 <-  endireh2021_corta_muj %>% 
  filter(!T_INSTRUM=="C2") %>% 
  select(ID_VIV, ID_PER, starts_with("P13_1_4") ) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(starts_with("P13_1_4")), ~as.numeric(.x)) %>%  # 
  dplyr::mutate_at(vars(starts_with("P13_1_4")), ~na_if(.x, 9)) %>%  # manda los 9  a missing
  dplyr::mutate_at(vars(starts_with("P13_1_4")), ~ (2 - .x))  # cambia la lógica de la numeraci?n a 2 se le resta el valor. Cambia sentido


pareja_13_1_4 %<>% 
  mutate(index_pareja_13_1_4= rowMeans(across(P13_1_4_1:P13_1_4_11), na.rm = T)) %>% 
  select(ID_VIV, ID_PER, index_pareja_13_1_4)

endireh2021_corta_muj %<>% 
  merge(pareja_13_1_4 ,by=c("ID_VIV", "ID_PER"), all.x=T) 

rm( pareja_13_1_1, pareja_13_1_2, pareja_13_1_3, pareja_13_1_4)

endireh2021_corta_muj %>% 
  select(starts_with("index")) %>% 
  summary()
