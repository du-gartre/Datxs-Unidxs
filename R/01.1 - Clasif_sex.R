

# 01.1 Clasificación de sexos por servidorx públicx
#

#'  ESTA VERSION TIENE UN PASO MAS PARA CLASIFICAR EL SEXO
#'  si no se clasifica el primer nombre en M,H o unisex, usa el segundo nombre para clasificar
#'  
#'  
#' Usaremos un diccionario creado por nosotros para asignar el sexo de los servidores públicos
#' utilizando su nombre como criterio para elegir el sexo.
#' 
#' El diccionario se creó al clasificar manualmente los nombres más repetidos, de tal manera que
#' se pueda clasificar más del 90% de los servidores públicos a partir de su nombre
#' 
#' Hay algunos nombres que clasificamos como UNISEX, es decir que se usan tanto para hombres como para mujeres
#'  - Para las personas que tienen un primer nombre unisex, pero tienen 2do nombre, se usará el 2do nombre
#'    para asignarles un sexo.
#'  - Las personas que solo tienen un nombre y es unisex, no se les asignará sexo y serán descartadas del análisis.
#'  



#*****************************************************************************
# 1. Activar librerías ---------------------------------------------------------------
#*****************************************************************************
library(tidyverse)

#*****************************************************************************
# 2. Importar datos -------------------------------------------------------------
#*****************************************************************************

# Base de sueldos de servidores públicos para agosto de 2021
df_agosto <- read.csv("./data-raw/2021_08agosto_base_remuneraciones.csv")

# Diccionario para asignar sexo por el nombre de los servidores públicos
df_dicc <- read.csv("./data-raw/base_nombres_sexo.csv")


#*****************************************************************************
# 3. Manipular base de Sevidores públicos ------------------------------------
#*****************************************************************************

# Separamos los nombres usando el símbolo de espacio
# Hay personas con hasta 6 palabras separadas por espacio en su nombre
# Usaremos a N1 como el Primer nombre y a N2 como el segundo nombre
df_agosto2 <- df_agosto %>% 
    separate(col = NOMBRE, sep = " ", into = c("N1", "N2", "N3", "N4", "N5", "N6"))



#*****************************************************************************
# 4. Obtener lista de nombres del diccionario --------------------------------
#*****************************************************************************

# Nos quedamos únicamente con los nombres CLASIFICADOS
df_nom_clasif <- df_dicc %>% 
    filter(!is.na(sexo)) %>% 
    select(nombre = Var1, sexo, unisex)

# Nos quedamos únicamente con los nombres de mujeres
df_nom_mujeres <- df_dicc %>% 
    select(nombre = Var1, sexo, unisex) %>% 
    filter(sexo == 1 & unisex == 0)

# Nos quedamos únicamente con los nombres de hombres
df_nom_hombres <- df_dicc %>% 
    select(nombre = Var1, sexo, unisex) %>% 
    filter(sexo == 0 & unisex == 0)

# Nos quedamos únicamente con los nombres unisex
df_nom_unisex <- df_dicc %>% 
    select(nombre = Var1, sexo, unisex) %>% 
    filter(unisex == 1)

# Lista con los nombres de mujeres
v_nom_muj <- df_nom_mujeres$nombre

# Lista con los nombres de hombres
v_nom_hom <- df_nom_hombres$nombre

# Lista con los nombres unisex
v_nom_unisex <- df_nom_unisex$nombre

# Lista con todos los nombres clasificados
v_nom_tot <- df_nom_clasif$nombre



#*****************************************************************************
# 5. Asignación de sexo por nombres ---------------------------------------
#*****************************************************************************

# Creamos una nueva columna llamada sex, la llenamos siguiendo condiciones
df_agosto_sex <- df_agosto2 %>%
    # columna que indica si el primer nombre es unisex (1) o no (0)
    mutate(unisex = if_else(condition = (N1 %in% v_nom_unisex), true = 1, false = 0),
           # Se asigna M o H si el primer nombre está en el diccionario de nombres de Mujeres u hombres, respectivamente.
           sex = case_when((N1 %in% v_nom_muj) ~ "M",
                           (N1 %in% v_nom_hom) ~ "H",
                           # Clasificar a las personas por medio de su segundo nombre (N2)
                           # cuando no se tenga información del primero (N1)
                           (!(N1 %in% v_nom_tot) & (N2 %in% v_nom_muj)) ~ "M",
                           (!(N1 %in% v_nom_tot) & (N2 %in% v_nom_hom)) ~ "H",
                           # Clasificar a las personas por medio de su segundo nombre (N2)
                           # cuando no su primer nombre (N1) sea unisex
                           ((N1 %in% v_nom_unisex) & (N2 %in% v_nom_muj)) ~ "M",
                           ((N1 %in% v_nom_unisex) & (N2 %in% v_nom_hom)) ~ "H")) %>% 
    # Para las personas que tienen primer nombre "DE", se les asigna NA en el sexo
    mutate(sex = if_else(condition = (N1 == "DE"), true = NA_character_, false = sex))


# # Ver clasificación de los que tienen N1 unisex
# df_agosto_sex %>% 
#     filter(unisex == 1) %>% 
#     arrange(N1, N2) %>% 
#     View()


sum(!(is.na(df_agosto_sex$sex)))



df_agosto_sex

#*****************************************************************************
# Guardar base con clasificación ------------------------------------------
#*****************************************************************************

# # Guardamos la base clasificada en formato .csv
# write.csv(x = df_agosto_sex, file = "./data/base_clasificada.csv", fileEncoding = "UTF-8")

