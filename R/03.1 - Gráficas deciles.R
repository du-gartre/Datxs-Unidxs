# 03.1 Gráficos de deciles de ingreso
#

#'  A partir de 03, se crean gráficos de deciles de ingreso

#*****************************************************************************
# 1. Activar librerías ---------------------------------------------------------------
#*****************************************************************************
library(tidyverse)

#*****************************************************************************
# 2. Importar datos -------------------------------------------------------------
#*****************************************************************************

df_deciles <- read.csv(file = "data/df_deciles.csv", fileEncoding = "UTF-8")

#*****************************************************************************
# 3. Manipulación de datos ------------------------------------
#*****************************************************************************

# Convertimos de wide a long la tabla con el número de hombres, mujeres y no_id 
# por nivel de sueldo
df_deciles_long <- melt(data = df_deciles, id.vars = "DECIL") %>% 
    filter(variable %in% c("H", "M")) %>%
    mutate(variable = factor(variable, levels = c("M", "H"))) %>% 
    mutate(DECIL = as.factor(DECIL))


#*****************************************************************************
# 3. Creación de gráficos ------------------------------------
#*****************************************************************************



# Gráfico de barras
ggplot(data = df_deciles_long, aes(y = value, x = DECIL, fill = variable)) +
    geom_bar(position = "fill", stat = "identity")

# Gráfico de barras
ggplot(data = df_deciles_long, aes(y = DECIL, x = value, fill = variable)) +
    geom_bar(stat = "identity", position = "fill")








