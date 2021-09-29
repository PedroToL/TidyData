# Libraries ----
library(tidyverse)
library(foreign)
library(patchwork)

# Data ----
df <- read.dbf(file = "./Data/TB_SEC_VII.dbf", as.is = T)
df <- df %>% transmute(
  FAC_MUJ, 
  
  area     = recode(DOMINIO, 
                    "U" = 1,
                    "C" = 1, 
                    "R" = 0),
  rape_try = recode(P7_9_12,
                    `1` = 1, 
                    `2` = 0),
  rape     = recode(P7_9_13,
                    `1` = 1, 
                    `2` = 0),
  touching = recode(P7_9_14,
                    `1` = 1, 
                    `2` = 0)
) %>% na.omit()

values1 = data.frame(
  name   = c("Try to rape", "Rape", "Touch"), 
  value =c(weighted.mean(df$rape_try, w = df$FAC_MUJ)*100,
             weighted.mean(df$rape, w = df$FAC_MUJ)*100,
             weighted.mean(df$touching, w = df$FAC_MUJ)*100))

values2 = data.frame(
  name  = c("Try to rape", "Try to rape", "Rape",  "Rape", "Touch", "Touch"),
  value = c(weighted.mean(df[df$rape_try == 1, ]$area, w = df[df$rape_try == 1, ]$FAC_MUJ)*100, 
            100 - weighted.mean(df[df$rape_try == 1, ]$area, w = df[df$rape_try == 1, ]$FAC_MUJ)*100,
            weighted.mean(df[df$rape == 1, ]$area, w = df[df$rape == 1, ]$FAC_MUJ)*100, 
            100 - weighted.mean(df[df$rape == 1, ]$area, w = df[df$rape == 1, ]$FAC_MUJ)*100,
            weighted.mean(df[df$touching == 1, ]$area, w = df[df$touching == 1, ]$FAC_MUJ)*100, 
            100 - weighted.mean(df[df$touching == 1, ]$area, w = df[df$touching == 1, ]$FAC_MUJ)*100),
  color = c("Urban Area", "Rural Area"))

# Plots ----
p1 <- values1 %>% ggplot() +
  aes(x      = name, 
      weight = value,
      label  = paste0(round(value, 2), "%")) +
  geom_bar(color = "#AD41DB", fill = "#AD41DB") +
  geom_text(aes(y = value + 0.1)) +
  labs(x = "", y = "") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0),
        panel.grid.major = element_line(size = 0,
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0,
                                        colour = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p2 <- values2 %>% ggplot() +
  aes(x      = name, 
      fill   = color, 
      weight = value) +
  geom_bar() +
  scale_fill_manual(
    values = c(`Rural Area` = "#ED3DF2",
               `Urban Area` = "#AD41DB")
  ) +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0),
        panel.grid.major = element_line(size = 0,
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0,
                                        colour = "white"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top"
  )

layout <- (p1 + p2)

layout +
  plot_annotation(
    title = "In your workplace:\nDid anybody ... you?",
    caption = "Source: ENDIREH 2016 INEGI",
    theme = theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))
    )
