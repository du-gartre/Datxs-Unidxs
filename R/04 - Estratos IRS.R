


# Importamos librerías ----------------------------------------------------
library(tidyverse)
library(stratification)


# Importamos datos --------------------------------------------------------

# Base de Índice de Rezago social 2020 del CONEVAL a nivel municipal
df_IRS <- read.csv(file = "data-raw/IRS_municipios_2020.csv")


# Proceso de estratificación ----------------------------------------------

# Nos quedamos primero solo con datos de CDMX
df_IRS_cdmx <- df_IRS %>% 
    filter(Entidad.federativa == "Ciudad de México") %>% 
    arrange(Índice.de.rezago.social) %>% 
    # ordenamos y pasamos los números a positivo
    # porque strata.cumrootf no puede estratificar números negativos
    mutate(Índice.de.rezago.social = -Índice.de.rezago.social)


# Estratificamos por medio del método de Dalenius y Hodges para minimizar
# la varianza DENTRO de loos grupos y maximizar la varianza ENTRE grupos
# pág 9 
# http://gaia.inegi.org.mx/scince2/documentos/scince/metodo_notaTecnica.pdf
l_strata <- strata.cumrootf(x = df_IRS_cdmx$Índice.de.rezago.social, 
                      n = 16,
                      Ls =2) # dos estratos

# Creamos una nueva columna con los estratos
df_IRS_cdmx$estrato <- l_strata$stratumID

# # Ver los estratos creados
# df_IRS_cdmx %>% 
#     select(Municipio, estrato, Índice.de.rezago.social) %>% 
#     View()


# Guardamos datos ---------------------------------------------------
write.csv(x = df_IRS_cdmx, file = "data/IRS_CDMX_strata.csv")


