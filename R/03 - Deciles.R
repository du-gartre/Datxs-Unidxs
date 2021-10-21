
# 03 Creación de deciles de ingreso
#

#'  En este código se crean los deciles de ingreso de los servidores públicos 
#'  de la CDMX
#'  Además se indica el número y la proporción de hombres y mujeres por
#'  decil de ingreso

#*****************************************************************************
# 1. Activar librerías ---------------------------------------------------------------
#*****************************************************************************
library(tidyverse)
library(Hmisc)

#*****************************************************************************
# 2. Importar datos -------------------------------------------------------------
#*****************************************************************************

df_base <- read.csv(file = "data/base_clasificada.csv", fileEncoding = "UTF-8")


#*****************************************************************************
# 3. Creación de deciles ------------------------------------
#*****************************************************************************


# Creación de deciles
df_decil <- df_base %>% 
    # Ordenamos la base de datos con base en el sueldo tabular
    arrange(SUELDO_TABULAR_BRUTO) %>% 
    # creamos una columna con el número de renglón
    mutate(num = row_number()) %>% 
    # Hacemos los deciles a partir del número del renglón
    mutate(DECIL = ntile(num, 10))


# Resumen de ingreso por deciles
df_sum_dec <- df_decil %>% 
    group_by(DECIL) %>% 
    summarise(
        min(SUELDO_TABULAR_BRUTO),
        max(SUELDO_TABULAR_BRUTO)) %>% 
    rename(sueldo_min = "min(SUELDO_TABULAR_BRUTO)",
           sueldo_max = "max(SUELDO_TABULAR_BRUTO)") %>% 
    mutate(DECIL = as.factor(DECIL)) %>% 
    as.data.frame()

# Ver número de personas por sexos para cada decil
df_sum_dec_sex <- df_decil %>% 
    group_by(DECIL, sex) %>% 
    summarise(
        n()
    )

# Base con personal por institución, clasificaco por hombres, mujeres y NA
df_dec_sex_wide <- spread(df_sum_dec_sex,
                          key = "sex", 
                          value = "n()") %>% 
    mutate(DECIL = as.factor(DECIL)) %>% 
    mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
    rename(no_ID = "<NA>") %>% 
    # Obtenemos proporción de sexo por decil
    mutate(prop_H = H/TOTAL,
           prop_M = M/TOTAL,
           prop_NO_ID = no_ID/TOTAL) %>% 
    as.data.frame()

# Creación de tabla final de resumen de info por deciles
df_deciles_final <- merge(x = df_sum_dec, 
                    y = df_dec_sex_wide, 
                    by = "DECIL", 
                    all.x = TRUE) %>% 
    arrange(DECIL)


#*****************************************************************************
# 2. Exportar datos ----------------------------------------------------------
#*****************************************************************************

write.csv(x = df_deciles_final, file = "data/df_deciles.csv", fileEncoding = "UTF-8")
