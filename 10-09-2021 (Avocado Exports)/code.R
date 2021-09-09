# Libraries ----
library(tidyverse)
library(plotly)

# Data ----
df <- read_csv("./avocado_exports.csv")
df <- df %>% filter(Importers != "World") %>%
  transmute(
    importers = recode(Importers,
                       "United States of America" = "USA", 
                       "Canada" = "CAD", 
                       "Japan" = "JPN",
                       .default = "Other"),
    percent   = `Share in value in Mexico's exports, % in 2020`
  ) %>% 
  group_by(importers) %>%
  summarise(
    percent = sum(percent, na.rm = T)
  )

# Plot ----
fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("MEX", "CAD", "JPN", "Other", "USA"),
    x = c(0, 0.9, 0.9, 0.9, 0.9),
    y = c(0.5, 0.75, 0.82, 0.9, 0.35),
    color = c("#006847", "#FF0000", "#ECDCD4", "grey", "#0A3161"),
    pad = 15,
    thickness = 20,
    line = list(
      color = NA,
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,0,0,0),
    target = c(1,2,3,4),
    value =  df$percent, 
    color = "#006847" ,
    alpha = 0.5
  )
) %>% add_annotations(
  x = c(0.96, 0.96, 0.96, 0.97),
  y = c(0.22, 0.15, 0.08, 0.65),
  text = df$percent,
  showarrow = F
) %>%add_annotations(
  x = c(0.95),
  y = c(0),
  text =  "Source: International Trade Center",
  showarrow = F
)
   

fig <- fig %>% layout(
  title = "Mexican Avocado Exports",
  font = list(
    size = 10,
    color = 'black'
  ),
  xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
  yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
  plot_bgcolor = 'lightgrey',
  paper_bgcolor = 'lightgrey'
) 


fig
