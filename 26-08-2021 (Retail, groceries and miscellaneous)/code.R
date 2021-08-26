# Libraries ----
library(tidyverse)
library(leaflet) # For Interactive mapping
library(htmlwidgets)
library(webshot)

# Data ----
## DENUE ()
df1 <- readr::read_csv("./Data/denue_inegi_46111_.csv", locale = readr::locale(encoding = "latin1"))
df1 <- df1 %>% select(
  cve_ent, cve_mun, entidad, municipio, latitud, longitud
)

# Map ----

m<- leaflet() %>% setView(-102.552784, 23.634501, zoom = 5) %>% 
  addTiles() %>%
  addCircleMarkers(
    lng         = df1$longitud,
    lat         = df1$latitud,
    radius      = 1,
    color       = "red",
    stroke      = FALSE, 
    fillOpacity = .75
  )
m
