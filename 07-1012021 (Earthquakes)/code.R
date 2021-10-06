# Libraries
library(tidyverse)

# Data ----
df <- read_csv("./Data/SSNMX_catalogo_19000101_20211006.csv", 
               skip = 4) %>% na.omit()

df$Magnitud <- as.double(df$Magnitud)
df$Fecha    <- as.Date(df$Fecha)
df$Month    <- strftime(df$Fecha, format = "%m")

# Plot ----
df %>% filter(Magnitud >= 5) %>%
  group_by(Month) %>%
  summarise(
    n = n()
  ) %>% 
  ggplot() + aes(x    = Month,
                 y    = n,
                 fill = Month) +
  geom_bar(
    stat = "identity",
    show.legend = FALSE,
    width       = 1,
    color = "black"
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL,
       y = NULL, 
       subtitle = "Magnitude greater than 5", 
       caption  = "Source: SSN UNAM") + 
  coord_polar() +
  ggtitle("Monthly Earthquake Frequency since 1900") + 
  theme_minimal()
