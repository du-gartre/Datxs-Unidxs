
#****************************************************************************
# 1. Activar librerías ------------------------------------------------------
#****************************************************************************

library(tidyverse)


#*****************************************************************************
# 2. Importar datos ----------------------------------------------------------
#*****************************************************************************

# Base de sueldos de servidores públicos para agosto de 2021
df_agosto_sex <- read.csv(file = "./data/base_clasificada.csv", 
                          encoding = "UTF-8", row.names = "X")

#****************************************************************************
# 3. Manipulación de la base de datos -------------------------------------------
#****************************************************************************


## 3.1 categorías de sueldo -----


# # Creamos categorías de ingreso
# # las categorías van de rangos de 10k en 10k 
# df_agosto_sex2 <- df_agosto_sex %>% 
#     mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 10e3)) ~ 1,
#                                   (between(SUELDO_TABULAR_BRUTO, 10e3, 20e3)) ~ 2,
#                                   (between(SUELDO_TABULAR_BRUTO, 20e3, 30e3)) ~ 3,
#                                   (between(SUELDO_TABULAR_BRUTO, 30e3, 40e3)) ~ 4,
#                                   (between(SUELDO_TABULAR_BRUTO, 40e3, 50e3)) ~ 5,
#                                   (between(SUELDO_TABULAR_BRUTO, 50e3, 60e3)) ~ 6,
#                                   (between(SUELDO_TABULAR_BRUTO, 60e3, 70e3)) ~ 7,
#                                   (between(SUELDO_TABULAR_BRUTO, 70e3, 80e3)) ~ 8,
#                                   (between(SUELDO_TABULAR_BRUTO, 80e3, 90e3)) ~ 9,
#                                   (between(SUELDO_TABULAR_BRUTO, 90e3, 100e3)) ~ 10,
#                                   (between(SUELDO_TABULAR_BRUTO, 100e3, 120e3)) ~ 11))

# Para 5 niveles de 25k
df_agosto_sex2 <- df_agosto_sex %>%
    mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 24e3)) ~ 1,
                                  (between(SUELDO_TABULAR_BRUTO, 24e3, 48e3)) ~ 2,
                                  (between(SUELDO_TABULAR_BRUTO, 48e3, 72e3)) ~ 3,
                                  (between(SUELDO_TABULAR_BRUTO, 72e3, 96e3)) ~ 4,
                                  (between(SUELDO_TABULAR_BRUTO, 96e3, 120e3)) ~ 5))

## 3.2 Ingreso diferenciado por sexo y por institución ----
# Sueldo tabular bruto promedio por sexo, por institución
df_ii <- df_agosto_sex2 %>%
    group_by(DESC_UNIDAD_RESPONSABLE, sex) %>%
    summarise(mean(SUELDO_TABULAR_BRUTO))

# Número de hombres, mujeres y no_id por institución
df_ii <- df_agosto_sex2 %>%
    group_by(DESC_UNIDAD_RESPONSABLE, sex) %>%
    summarise(n())

# Número de personas por género, por nivel de ingreso, por institución
df_ii <- df_agosto_sex2 %>%
    group_by(DESC_UNIDAD_RESPONSABLE, sex, niv_sueldo) %>%
    summarise(n()) %>%
    arrange(DESC_UNIDAD_RESPONSABLE,sex, niv_sueldo)

#*****************************************************************************
# 4.  Guardar base ------------------------------------------
#*****************************************************************************

# # Guardamos CSV
# write.csv(x = df_ii, file = "./data-raw/insumo_clusters_3.csv", fileEncoding = "UTF-8")
