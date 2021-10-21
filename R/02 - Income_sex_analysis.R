

# 02 Análisis de la distribución de mujeres según ingreso




#****************************************************************************
# 1. Activar librerías ------------------------------------------------------
#****************************************************************************

library(tidyverse)
library(hrbrthemes)
library(tidyselect)
library(reshape2)
library(plotly)


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
#                                   (between(SUELDO_TABULAR_BRUTO, 100e3, 120e3)) ~ 11)
#            )

# 5 categorías
df_agosto_sex2 <- df_agosto_sex %>%
    mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 24e3)) ~ 1,
                                  (between(SUELDO_TABULAR_BRUTO, 24e3, 48e3)) ~ 2,
                                  (between(SUELDO_TABULAR_BRUTO, 48e3, 72e3)) ~ 3,
                                  (between(SUELDO_TABULAR_BRUTO, 72e3, 96e3)) ~ 4,
                                  (between(SUELDO_TABULAR_BRUTO, 96e3, 120e3)) ~ 5)
    )

# table(df_agosto_sex2$niv_sueldo)
# 
# # 6 categorías
# df_agosto_sex2 <- df_agosto_sex %>% 
#     mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 20e3)) ~ 1,
#                                   (between(SUELDO_TABULAR_BRUTO, 20e3, 40e3)) ~ 2,
#                                   (between(SUELDO_TABULAR_BRUTO, 40e3, 60e3)) ~ 3,
#                                   (between(SUELDO_TABULAR_BRUTO, 60e3, 80e3)) ~ 4,
#                                   (between(SUELDO_TABULAR_BRUTO, 80e3, 100e3)) ~ 5,
#                                   (between(SUELDO_TABULAR_BRUTO, 100e3, 130e3)) ~ 6)
#     )
# 
# table(df_agosto_sex2$niv_sueldo)
# 
# sort(unique(df_agosto_sex2$DESC_UNIDAD_RESPONSABLE))
# 
# df_agosto_sex2 %>% 
#     filter(DESC_UNIDAD_RESPONSABLE =="Universidad de la Salud") %>% 
#     View()


# Número de servidores públicos por nivel salarial
table(df_agosto_sex2$niv_sueldo)

# Proporción del total de servidores públicos por nivel de sueldo
prop.table(table(df_agosto_sex2$niv_sueldo))*100

# Para saber el número de Hombres y mujeres en cada nivel de ingreso
df_clasif <- df_agosto_sex2 %>% 
    group_by(sex, niv_sueldo) %>% 
    summarise(n()) %>% 
    as.data.frame()

# Dataframe del número de mujeres por nivel de sueldo
df_muj_niv <- df_clasif %>% 
    filter(sex == "M") %>% 
    select(niv_sueldo, mujeres = "n()")

# Dataframe del número de hombres por nivel de sueldo
df_hom_niv <- df_clasif %>% 
    filter(sex == "H")%>% 
    select(niv_sueldo, hombres = "n()")

# Dataframe del número de personas no clasificadas por nivel de sueldo
df_unisex_niv <- df_clasif %>% 
    filter(is.na(sex))%>% 
    select(niv_sueldo, no_id = "n()")

# Tabla con el número de hombres, mujeres y no_id por nivel de sueldo
df_niv_sex <- df_muj_niv %>% 
    merge(y = df_hom_niv, by = "niv_sueldo", all.x =TRUE) %>% 
    merge(y = df_unisex_niv, by = "niv_sueldo", all.x =TRUE) %>% 
    mutate(niv_sueldo = as.factor(niv_sueldo)) %>% 
    mutate(total = rowSums(across(where(is.numeric))))

# ver cuantas personas no fueron clasificadas
sum(df_niv_sex$no_id)

# Tabla con el porcentaje de hombres, mujeres y no_id por nivel de sueldo
df_perc_sex <- df_niv_sex %>% 
    mutate(p_mujeres = mujeres/total,
           p_hombres = hombres/total,
           p_no_id = no_id/total) %>% 
    mutate(p_total = rowSums(select(., "p_mujeres", "p_hombres", "p_no_id"))) %>% 
    select(niv_sueldo, p_mujeres, p_hombres, p_no_id, p_total)


# Convertimos de wide a long la tabla con el número de hombres, mujeres y no_id 
# por nivel de sueldo
df_niv_sex_long <- melt(data = df_niv_sex, id.vars = "niv_sueldo") %>% 
    filter(variable != "total")

# Convertimos de wide a long la tabla con el número de hombres, mujeres y no_id 
# por nivel de sueldo
df_perc_sex_long <- melt(data = df_perc_sex, id.vars = "niv_sueldo") %>% 
    filter(variable != "p_total")


# df_hh <- df_agosto_sex2 %>% 
#     filter(sex== "H") %>% 
#     select(sueldo = SUELDO_TABULAR_BRUTO)
# 
# df_mm <- df_agosto_sex2 %>% 
#     filter(sex== "M") %>% 
#     select(sueldo = SUELDO_TABULAR_BRUTO)


# Convertimos de wide a long la tabla con el número de hombres, mujeres y no_id 
# por nivel de sueldo
# df_niv_sex_long <- melt(data = df_agosto_sex2, id.vars = "niv_sueldo") %>% 
#     filter(variable != "total")


#****************************************************************************
# 4. Visualizaciones ---------------------------------------------------------
#****************************************************************************


#****************************************************************************
#* Gráficos de barras
#****************************************************************************

df_perc_sex_long$variable <- factor(x = df_perc_sex_long$variable, 
                                    levels = c("p_no_id", "p_mujeres", "p_hombres"))

# Gráfica con la proporción de mujeres, hombres y no_id, por nivel de sueldo
plt_prop <- df_perc_sex_long %>% 
    ggplot(aes(y = value, x = niv_sueldo, fill = variable)) +
    geom_bar(position = "fill", stat = "identity")


# Para volverla interactiva
ggplotly(plt_prop)

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


#****************************************************************************
#* Histogramas
#****************************************************************************

df_aa %>% 
    filter(!is.na(sex)) %>% 
    ggplot(aes(x = sueldo, fill = sex)) + 
    geom_density()


df_aa %>% 
    filter(!is.na(sex)) %>% 
    ggplot(aes(x = sueldo, fill = sex)) + 
    geom_histogram(position = "dodge")

ggplot() +
    geom_histogram(data = df_mm, aes(x = sueldo, y = ..count..), 
                   fill = "green", color = "gray") + 
    geom_histogram(data = df_hh, aes(x = sueldo, y = -..count..), 
                   fill = "blue", color = "gray") +
    coord_flip()

ggplot()+
    geom_density(data = df_mm, aes(x = sueldo, y = ..density..), fill = "green", color = "gray") + 
    geom_density(data = df_hh, aes(x = sueldo, y = -..density..), fill = "blue", color = "gray") + 
    coord_flip()


## Ingreso diferenciado por sexo y por institución

df_ii <- df_agosto_sex2 %>%
    group_by(DESC_UNIDAD_RESPONSABLE, sex) %>%
    summarise(mean(SUELDO_TABULAR_BRUTO))


df_ii <- df_agosto_sex2 %>%
    group_by(DESC_UNIDAD_RESPONSABLE, sex) %>%
    summarise(n())

df_ii <- df_agosto_sex2 %>%
    group_by(DESC_UNIDAD_RESPONSABLE, sex, niv_sueldo) %>%
    summarise(n()) %>%
    arrange(DESC_UNIDAD_RESPONSABLE,sex, niv_sueldo)


write.csv(x = df_ii, file = "./data-raw/insumo_clusters_3.csv", fileEncoding = "UTF-8")
