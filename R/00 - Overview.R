

# 00 - Overview

#' Este código tiene por objetivo generar un vistazo sobre los datos
#' de la base de datos de remuneraciones de lxs servidorxs públicxs de la 
#' CDMX en agosto de 2021
#' disponible en:
#' https://datos.cdmx.gob.mx/dataset/remuneraciones-al-personal-de-la-ciudad-de-mexico

# Libraries ---------------------------------------------------------------
library(tidyverse)



# Import data -------------------------------------------------------------
df_agosto <- read.csv("./data-raw/2021_08agosto_base_remuneraciones.csv")
df_agosto$NOMBRE


#***************************************************************************
# Análisis por nombres ----------------------------------------------------
#***************************************************************************

# write.csv(x = df_agosto, file = "./data-raw/base_agosto.csv", fileEncoding = "UTF-8")

# Dividimos la columna nombres usando como separador al símbolo de espacio
df_nombre_completo <- df_agosto %>% 
    separate(col = NOMBRE, sep = " ", into = c("N1", "N2", "N3", "N4", "N5", "N6")) %>% 
    select(N1:N6, APELLIDO_1, APELLIDO_2)


# Dividimos la columna nombres usando como separador al símbolo de espacio
df_nombres <- df_agosto %>% 
    separate(col = NOMBRE, sep = " ", into = c("N1", "N2", "N3", "N4", "N5", "N6")) %>% 
    select(N1)

View(df_nombres$N1)

# gender(names = df_nombres$N1, 
#        years = 2021)



# Tabulamos los nombres, ordenamos de mayor a menor y lo guardamos en una tabla
tab_nombres = sort(table(df_nombres$N1), decreasing = TRUE)

# Creamos un data frame con el tabulado de los nombres que:
df_num_nombres <- as.data.frame(tab_nombres) %>% 
    # Tenga la proporción que cada nombre representa del total
    mutate(prop = Freq/sum(Freq),
           # Haga la suma acumulada de las proporciones de los nombres
           cum_prop = cumsum(prop))


# # Guardamos CSV para hacer la clasificación manual de los nombres
# write.csv(x = df_num_nombres, file = "./data-raw/df_freq_nombres.csv", fileEncoding = "UTF-8")

# Filtro para primer nombre:
df_nombre_completo %>% 
    filter(N1 == "ELIUD") %>% 
    view()


df_aa <- df_agosto %>%
    separate(col = NOMBRE, sep = " ", into = c("N1", "N2", "N3", "N4", "N5", "N6")) %>%
    filter(N1 == "NERI") %>% 
    View()
    

sort(table(df_agosto$DESC_UNIDAD_RESPONSABLE))

#***************************************************************************
# Análisis de Sueldo bruto -------------------------------------------------
#***************************************************************************

### Sacar promedio, minimo, máximo de sueldo por sexos
# Ver distribución de sexos por nivel salarial
#       Histograma se salarios, hombres y mujeres empatados 
#       fill bars por nivel salarial
# Graficar Distribución de sexos 
#       por nivel salarial 
#       por Alcaldía


# Tabular sueldos, ver cuál es el monto salarial más común
# 7,341 pesos es el sueldo más común (MODA)
which.max(sort(table(df_agosto$SUELDO_TABULAR_BRUTO), decreasing = TRUE))


# Ver proporción de trabajadores desde el nivel de sueldo más bajo al más alto
# 199,985 personas (79.22%) ganan menos de 20 k brutos al mes
aa <- data.frame(sort(table(df_agosto$SUELDO_TABULAR_BRUTO), decreasing = TRUE)) %>% 
    mutate(Var2 = as.numeric(as.character(Var1))) %>% 
    arrange(Var2) %>% 
    mutate(Freq_acum = cumsum(Freq),
           Prop = Freq/sum(Freq),
           Prop_acum = cumsum(Prop)) %>% 
    relocate(Var2, .after = Var1)


# Hay 785 niveles salariales distintos
length(table(df_agosto$SUELDO_TABULAR_BRUTO))

# El menor es 1,870 pesos mensuales
# El mayor es 111,178 pesos mensuales
# El promedio es de 12,574 pesos mensuales
# La mediana es de 10,444 pesos mensuales
summary(df_agosto$SUELDO_TABULAR_BRUTO)


# Histograma de los salarios
hist(df_agosto$SUELDO_TABULAR_BRUTO)

# Ver todos los sueldos, ordenados de mayor a menor
tab_sueldo <- sort(unique(df_agosto$SUELDO_TABULAR_BRUTO), decreasing = TRUE)

# Crear un df que tenga niveles de sueldo
df_tab_sueldos <- data.frame(tab_sueldo) %>% 
    mutate(niv_sueldo = row_number()) 

length(unique(df_agosto$DESC_UNIDAD_RESPONSABLE))

df_agosto2 %>% 
    filter(DESC_UNIDAD_RESPONSABLE == "Instituto de las Personas con discapacidad de La Ciudad De México") %>% 
    View()

sort(table(df_agosto2$DESC_UNIDAD_RESPONSABLE))


# Asignar nivel salarial a la base original
df_agosto2 <- merge(x = df_agosto, 
                    y = df_tab_sueldos, 
                    by.x = "SUELDO_TABULAR_BRUTO", 
                    by.y = "tab_sueldo",
                    all.x = TRUE) %>% 
    relocate(SUELDO_TABULAR_BRUTO, .before = niv_sueldo)



# Análisis por Organismo --------------------------------------------------
sort(table(df_agosto$DESC_UNIDAD_RESPONSABLE))

