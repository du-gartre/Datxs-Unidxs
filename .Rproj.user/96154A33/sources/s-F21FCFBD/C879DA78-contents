

#' 01.1 org_staff_by_sex
#' 
#' En este código agrupamos la información para observar el número de hombres
#' y mujeres que están en distintos niveles de sueldo dentro de cada una de
#' las dependencias disponibles en la base de remuneraciones de lxs servidorxs 
#' públicxs de la CDMX en agosto de 2021.

#*****************************************************************************
# 1. Activar librerías ---------------------------------------------------------------
#*****************************************************************************
library(tidyverse)
library(magrittr)


#*****************************************************************************
# 2. Importar datos -------------------------------------------------------------
#*****************************************************************************
# Base de sueldos de servidores públicos para agosto de 2021
df_agosto_sex <- read.csv(file = "./data/base_clasificada.csv", 
                          encoding = "UTF-8", row.names = "X")

#*****************************************************************************
# 3. Manipular base de datos ------------------------------------
#*****************************************************************************

# Creamos categorías de ingreso
# las categorías van de rangos de 25k en 25k
df_agosto_sex2 <- df_agosto_sex %>%
    mutate(niv_sueldo = case_when((between(SUELDO_TABULAR_BRUTO, 0, 25e3)) ~ 1,
                                  (between(SUELDO_TABULAR_BRUTO, 25e3, 50e3)) ~ 2,
                                  (between(SUELDO_TABULAR_BRUTO, 50e3, 75e3)) ~ 3,
                                  (between(SUELDO_TABULAR_BRUTO, 75e3, 100e3)) ~ 4,
                                  (between(SUELDO_TABULAR_BRUTO, 100e3, 125e3)) ~ 5),
           niv_sueldo = as.factor(niv_sueldo)
    )



# Hombres y mujeres por institución
df_personal_sex_orgs <- df_agosto_sex2 %>% 
    group_by(DESC_UNIDAD_RESPONSABLE, sex, niv_sueldo) %>% 
    summarise(
        n()
    )

# Base con personal por institución, clasificaco por hombres, mujeres y NA
df_wide <- spread(data = df_personal_sex_orgs, 
                  key = "sex" ,
                  value = "n()") %>% 
    replace(is.na(.), 0) %>% 
    mutate(total = rowSums(across(where(is.numeric))))

#*****************************************************************************
# Crear base completa -----------------------------------------------------
#*****************************************************************************


v_nom_orgs <- unique(df_wide$DESC_UNIDAD_RESPONSABLE)
n_orgs <- length(v_nom_orgs)

v_nom_orgs_rep <- sort(rep(v_nom_orgs, 5))
v_niv_orgs <- rep(seq(1,5), n_orgs)

df_base <- data.frame(v_nom_orgs_rep, v_niv_orgs) %>% 
    set_colnames(colnames(df_wide)[1:2])

df_niv_wage_sex <- merge(x = df_base, 
      y = df_wide, 
      all.x = TRUE, 
      by = c("DESC_UNIDAD_RESPONSABLE", "niv_sueldo")) %>% 
    mutate(niv_sueldo = as.factor(niv_sueldo)) %>% 
    mutate_all(funs(replace_na(., 0)))


str(df_niv_wage_sex)

#*****************************************************************************
# Guardar base ------------------------------------------
#*****************************************************************************

# Guardamos CSV
write.csv(x = df_niv_wage_sex, file = "data/df_org_sex_niv_sueldo.csv", fileEncoding = "UTF-8")
