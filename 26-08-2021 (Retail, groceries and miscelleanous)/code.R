# Libraries ----
library(tidyverse)
library(foreign) # For stata data bases

# Data ----
## DENUE ()
df1 <- readr::read_csv("./Data/denue_inegi_46111_.csv", locale = readr::locale(encoding = "latin1"))
df1 <- df1 %>% select(
  cve_ent, cve_mun, entidad, municipio, latitud, longitud
) %>% filter(entidad == "CIUDAD DE MÉXICO")

## CENSUS 
df2 <- read_dta("./Data/Censo2020_CA_cdmx_dta/Viviendas09.dta")
df2 <- df2 %>% select(
  mun, nom_mun, cuadorm, numpers
) %>%
  mutate(
    hacinamiento = numpers/cuadorm
  ) %>% group_by(mun, nom_mun) %>%
  summarise(
    hacinamiento = mean(hacinamiento, na.rm = T)
  )
