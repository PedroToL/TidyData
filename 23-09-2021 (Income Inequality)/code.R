# Libraries ----
library(tidyverse)
library(foreign)
library(plotly)
library(rjson)
library(RColorBrewer)

# Data ----
hog <- read.dbf("./Data/ENIGH/concentradohogar.dbf", as.is=T)
hog <- hog %>% transmute(
  ID  = paste(folioviv, foliohog, sep ="."),
  ent = substr(ubica_geo, 1, 2),
  mun = substr(ubica_geo, 3, 5),
  factor
)

ing <- read.dbf("./Data/ENIGH/ingresos.dbf", as.is=T)
ing <- ing %>% transmute(
  folio = paste(folioviv, foliohog, numren, sep ="."),
  ID    = paste(folioviv, foliohog, sep ="."),
  ing   = ing_tri
) %>% group_by(folio) %>%
  summarise(
    ID = max(ID), 
    ing = sum(ing)
  )

df <- merge(hog, ing, by = "ID", all = T)
df_ent1 <- df %>% group_by(ent) %>%
  summarise(
    Gini = dineq::gini.wtd(ing, weights = factor),
  ) %>% mutate(
    location_name = c("Aguascalientes", "Baja California", "Baja California Sur",
                      "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango",
                      "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán",
                      "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán",
                      "Zacatecas")
  )

ENT <- fromJSON(file="https://raw.githubusercontent.com/angelnmara/geojson/master/mexicoHigh.json")

# Map ----
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=ENT,
  locations=df_ent$location_name,
  z=df_ent$Gini,
  colors = brewer.pal(9, "YlOrRd"),
  featureidkey="properties.name",
  marker=list(line=list(
    width=0.2, color = "black"))
) 
fig <- fig %>% layout(
  geo = list(
    fitbounds = "locations",
    visible = F,
    showcountries = F, 
    showcoastline = F,
    showland = F)
)
fig <- fig %>% colorbar(title = "Gini")
fig <- fig %>% layout(
  title  = "Household Income Inequality<br>State Level 2020",
  font   = list(size = 20, color = "black"),
  margin = list(l = 0, r = 0, t = 75, b = 25), 
  annotations  = list(x         = 1.06,
                      y         = 0,
                      text      = "Source: ENIGH 2020", 
                      showarrow = F,
                      xref      = 'paper',
                      yref      = 'paper', 
                      xanchor   = 'right',
                      yanchor   = 'auto', 
                      xshift    = 0,
                      yshift    = 0,
                      font = list(size  = 15,
                                  color = "'rgb(100, 100, 100)"))
)
fig
