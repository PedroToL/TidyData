# Libraries ----
library(tidyverse)
library(plotly)

# Data
df <- read_csv("../12-08-2021 (Inequality in Latin America)/Data/Df.csv") %>% 
  mutate(
    region = recode(region, 
                    'Cono Sur' = "Southern cone",
                    'Andina' = "Andean", 
                    'Centroamerica' = "Central America")
  )

# Graph
fig <- df %>% filter(year == 2018, region != "-") %>%
  plot_ly(x     = ~GDP_PC_2010,
          y     = ~ratio, 
          type  = 'scatter',
          mode  = 'markers',
          color = ~region,
          marker = list(size    = ~density,
                        line    = list(width = 1.2, color = 'rgb(300, 300, 300)'),
                        sizeref = 4,
                        sizemin = 6,
                        opacity = 0.5),
          colors  = c('#2D49BA', '#57AB3C', '#BB3754FF')
  ) %>% add_trace(
    text = ~c_name,
    textposition = "top",
    textfont = list(color = '#000000', size = 10),
    showlegend = F
  )

fig <- fig %>% layout(title  = 'Relation between GDP per Capita,People living in Urban\nRural areas and Population density (Size)\n2018',
                      xaxis  = list(title= "GDP per Capita (Constant US$ 2010)",
                                    gridcolor = 'rgb(215, 215, 215)'),
                      yaxis  = list(title = "Urban/Rural Ratio %",
                                    gridcolor = 'rgb(215, 215, 215)'),
                      margin = list(l = 75, r=20, t=75, b=75),
                      paper_bgcolor = 'rgb(200, 200, 200)',
                      plot_bgcolor  = 'rgb(200, 200, 200)',
                      legend        = list(title        = list(text = '<b> Region <b>'),
                                           x = 0.04, y = .96,
                                           bgcolor     = "#E2E2E2",
                                           bordercolor = "#FFFFFF",
                                           borderwidth = 2),
                      annotations  = list(x = 1, y = -0.12, text = "Source: World Bank Open Data", 
                                          showarrow = F, xref='paper', yref='paper', 
                                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=15, color="'rgb(100, 100, 100)"))
  )

fig

  