# Clear all -----    
rm(list = ls()) 

# Libraries ----
library(tidyverse) # manipulate tabular data
library(plotly)    # graphics

# Data ----
df <- readr::read_csv("./Data/consumption_income_level_2020.csv")

df <- df %>% transmute(
  `Income Level`             = c("Total", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"),
  `Income Level`             = factor(`Income Level`, levels = c("Total", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")),
  `In Pesos`                 = as.double(GASTO_ALCOHOL_CORR),
  `As a % of Current Income` = as.double(GASTO_ALCOHOL_PORC)/100
)

# Plot---
fig <- plot_ly() %>% 
  add_bars(
    x           = df$`Income Level`,
    y           = df$`In Pesos`,
    name        = '<--Mexican Pesos',
    offsetgroup = 1, 
    marker      = list(color = "#A6763C")
    ) %>%
  add_bars(
    x           = df$`Income Level`,
    y           = df$`As a % of Current Income`, 
    name        = 'As a % of Current Income-->',
    yaxis       = "y2", 
    offsetgroup = 2, 
    marker      = list(color = "#F2C53D")
    ) %>%
  layout(
    yaxis      = list(
      tickformat = "$,d"
    ),
    yaxis2     = list(
      overlaying = "y",
      side       = "right",
      title      = "",
      showgrid   = FALSE,
      tickformat = "%"),
    xaxis   = list(title = "Income Level (1-10)"),
    yaxis   = list(title = "Pesos"),
    barmode = 'group',
    legend  = list(x           = 0.3,
                   y           = 0.95,
                   orientation = "h"),
    margin  = list(l = 50, r = 75, t = 50, b = 50),
    title   = "Average Alcohol Houshold Spending<br>By Income Level",
    font    = list(size = 20),
    annotations  = list(x         = 1.02,
                        y         = -0.080,
                        text      = "Source: ENIGH 2020", 
                        showarrow = F,
                        xref      = 'paper',
                        yref      = 'paper', 
                        xanchor   = 'right',
                        yanchor   = 'auto', 
                        xshift    = 0,
                        yshift    = 0,
                        font = list(size  = 15,
                                    color = "'rgb(100, 100, 100)")),
    paper_bgcolor = 'rgb(250, 250, 250)',
    plot_bgcolor  = 'rgb(250, 250, 250)'
    )
fig
