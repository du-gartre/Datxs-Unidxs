

 
#' 01.2 Datos_sex
#' 
#' En esta base de datos obtenemos el número de servidores públicos, según su
#' sexo, así como el salario promedio por sexo.

#****************************************************************************
# 1. Activar librerías ------------------------------------------------------
#****************************************************************************

library(dplyr)

#*****************************************************************************
# 2. Importar datos ----------------------------------------------------------
#*****************************************************************************

# Base de sueldos de servidores públicos para agosto de 2021
df_agosto_sex <- read.csv(file = "./data/base_clasificada.csv", 
                          encoding = "UTF-8", row.names = "X")


#****************************************************************************
# 3. Manipulación de la base de datos -------------------------------------------
#****************************************************************************

# Vemos la información de los servidores que tienen información reservada
df_agosto_sex %>% 
    filter(N1 == "RESERVADO") %>%  
    View()

# reemplazamos el nombre de los que no tuvieron asignación de sexo
df_agosto_sex2 <- df_agosto_sex %>% 
    mutate(sex = if_else(condition = is.na(sex), 
                         true = replace_na("no_id"), false = sex))


# Creamos vecotres, con el número y la proporción de personas, según su sexo
v_n_sex <- as.vector(table(df_agosto_sex2$sex))
v_prop_sex <- as.vector(prop.table(table(df_agosto_sex2$sex)))

# creamos un dataframe con base en la información de los vectores anteriores
df_prop_sex <- data.frame(n_sex = v_n_sex, 
                          prop_sex = v_prop_sex, 
                          row.names = c("H", "M", "no_id"))


# Sueldo promedio del total de servidores públicos en la base de datos
mean(df_agosto_sex2$SUELDO_TABULAR_BRUTO)

# Obtenemos el sueldo tabular promedio por sexo
df_w_mean_sex <- df_agosto_sex2 %>% 
    group_by(sex) %>% 
    summarise(
        mean(SUELDO_TABULAR_BRUTO)
    ) %>% 
    as.data.frame()

#*****************************************************************************
# 4. Guardar bases de datos --------------------------------------------------
#*****************************************************************************

# # base de proporción de personas por sexo
# write.csv(x = df_prop_sex, file = "data/df_prop_sex.csv")

# # Base del sueldo tabular promedio, por sexo
# write.csv(x = df_w_mean_sex, file = "data/df_w_mean_sex.csv")
