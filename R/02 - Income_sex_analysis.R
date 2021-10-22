

# 02 Análisis de la distribución de mujeres según ingreso




#****************************************************************************
# 1. Activar librerías ------------------------------------------------------
#****************************************************************************

library(tidyverse)
library(hrbrthemes)
library(tidyselect)
library(reshape2)
library(plotly)adsfads


#*****************************************************************************
# 2. Importar datos ----------------------------------------------------------
#*****************************************************************************

# Base de sueldos de servidores públicos para agosto de 2021
df_agosto_sex <- read.csv(file = "./data/base_clasificada.csv", 
                          encoding = "UTF-8", row.names = "X")


#****************************************************************************
# 3. Manipulación de la base de datos -------------------------------------------
#****************************************************************************


# Creamos categorías de ingreso
# las categorías van de rangos de 10k en 10k 
df_agosto_sex2 <- df_agosto_sex %>% 
    mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 10e3)) ~ 1,
                                  (between(SUELDO_TABULAR_BRUTO, 10e3, 20e3)) ~ 2,
                                  (between(SUELDO_TABULAR_BRUTO, 20e3, 30e3)) ~ 3,
                                  (between(SUELDO_TABULAR_BRUTO, 30e3, 40e3)) ~ 4,
                                  (between(SUELDO_TABULAR_BRUTO, 40e3, 50e3)) ~ 5,
                                  (between(SUELDO_TABULAR_BRUTO, 50e3, 60e3)) ~ 6,
                                  (between(SUELDO_TABULAR_BRUTO, 60e3, 70e3)) ~ 7,
                                  (between(SUELDO_TABULAR_BRUTO, 70e3, 80e3)) ~ 8,
                                  (between(SUELDO_TABULAR_BRUTO, 80e3, 90e3)) ~ 9,
                                  (between(SUELDO_TABULAR_BRUTO, 90e3, 100e3)) ~ 10,
                                  (between(SUELDO_TABULAR_BRUTO, 100e3, 120e3)) ~ 11)
           )

# # 5 categorías
# # de 25k en 25k
# df_agosto_sex2 <- df_agosto_sex %>%
#     mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 24e3)) ~ 1,
#                                   (between(SUELDO_TABULAR_BRUTO, 24e3, 48e3)) ~ 2,
#                                   (between(SUELDO_TABULAR_BRUTO, 48e3, 72e3)) ~ 3,
#                                   (between(SUELDO_TABULAR_BRUTO, 72e3, 96e3)) ~ 4,
#                                   (between(SUELDO_TABULAR_BRUTO, 96e3, 120e3)) ~ 5))


# Número de servidores públicos por nivel salarial
table(df_agosto_sex2$niv_sueldo)

# Proporción del total de servidores públicos por nivel de sueldo
prop.table(table(df_agosto_sex2$niv_sueldo))*100

# Para saber el número de Hombres y mujeres en cada nivel de ingreso
df_clasif <- df_agosto_sex2 %>% 
    group_by(sex, niv_sueldo) %>% 
    summarise(n()) %>% 
    as.data.frame()

# Base con personal por institución, clasificaco por hombres, mujeres y NA
df_niv_sex <- spread(data = df_clasif, 
                  key = "sex" ,
                  value = "n()") %>% 
    replace(is.na(.), 0) %>%
    mutate(niv_sueldo = as.factor(niv_sueldo)) %>% 
    mutate(total = rowSums(across(where(is.numeric))))    

# Tabla con el porcentaje de hombres, mujeres y no_id por nivel de sueldo
df_perc_sex <- df_niv_sex %>% 
    mutate(p_mujeres = M/total,
           p_hombres = H/total,
           p_no_id = `<NA>`/total) %>% 
    select(niv_sueldo, p_mujeres, p_hombres, p_no_id)


# Convertimos de wide a long la tabla con el número de hombres, mujeres y no_id 
# por nivel de sueldo
df_niv_sex_long <- melt(data = df_niv_sex, id.vars = "niv_sueldo") %>% 
    filter(variable != "total")

# Convertimos de wide a long la tabla con el número de hombres, mujeres y no_id 
# por nivel de sueldo
df_perc_sex_long <- melt(data = df_perc_sex, id.vars = "niv_sueldo") %>% 
    filter(variable != "p_total")

# Volvemos factor una columna y damos el orden de los factores
df_perc_sex_long$variable <- factor(x = df_perc_sex_long$variable, 
                                    levels = c("p_no_id", "p_mujeres", "p_hombres"))

#****************************************************************************
# 4. Guardar base de servidores por nivel  ----------------------------------
#****************************************************************************

# write.csv(x = df_niv_sex, file = "data/df_sexo_11niv_cdmx.csv")


#****************************************************************************
# 5. Visualizaciones ---------------------------------------------------------
#****************************************************************************

## 4.1 Gráficos de barras --------------------------------------------------


# Gráfica con la proporción de mujeres, hombres y no_id, por nivel de sueldo
plt_prop <- df_perc_sex_long %>% 
    ggplot(aes(y = value, x = niv_sueldo, fill = variable)) +
    geom_bar(position = "fill", stat = "identity")

# Gráfica con la proporción de mujeres, hombres y no_id, por nivel de sueldo
df_niv_sex_long %>% 
    ggplot(aes(y = niv_sueldo, x = value, fill = variable)) +
    geom_bar(position = "fill", stat = "identity")

# Gráfica con la CANTIDAD de mujeres, hombres y no_id, por nivel de sueldo
plt_niv <- df_niv_sex_long %>% 
    ggplot(aes(y = niv_sueldo, x = value, fill = variable)) +
    geom_bar(position = "stack", stat = "identity")

# Gráfica con la proporción de mujeres, hombres y no_id, por nivel de sueldo
df_perc_sex_long %>% 
    ggplot(aes(y = value, x = niv_sueldo, fill = variable)) +
    geom_bar(position = "dodge", stat = "identity")

# Gráfica con la proporción de mujeres, hombres y no_id, por nivel de sueldo
df_niv_sex_long %>% 
    ggplot(aes(y = value, x = niv_sueldo, fill = variable)) +
    geom_col(position = "dodge")

