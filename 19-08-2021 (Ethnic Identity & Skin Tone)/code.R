# Libraries -------------------------------------------------------------------
library(tidyverse)
library(ggthemes)

# Data ------------------------------------------------------------------------
data <- readr::read_csv("./Data/MMSI_2016.csv")

## Preprocesing 
data <- data %>% transmute(
  indigenous_language = recode_factor(P10_1,
                                      `1` = "Yes",
                                      `2` = "No"),
  skin_tone           = recode_factor(P10_2,
                                      "01" = "Dark",
                                      "02" = "Dark", 
                                      "03" = "Dark",
                                      "04" = "Dark", 
                                      "05" = "Dark",
                                      "06" = "Dark",
                                      "07" = "Brown",
                                      "08" = "Light Brown",
                                      "09" = "White",
                                      "10" = "White",
                                      "11" = "White"),
  ethnic_identity     = recode_factor(P10_3,
                                      `1` = "Afro-descendant",
                                      `2` = "Indigenous",
                                      `3` = "Mestizo",
                                      `4` = "White",
                                      `5` = "Others",
                                      `9` = "Not Specified"),
  factor              = Factor_Per
)

# Plots ------------------------------------------------------------------------
## Identity & Skin Tone
ggplot(data) + aes(x       = ethnic_identity,
                   fill    = skin_tone, 
                   weights = factor) +
  geom_bar(position = "fill") +
  labs(fill = "Skin Tone",
       subtitle = "Proportion of individuals",
       caption = "Source: Own based on data from MSMSI 2016 (INEGI)") +
  theme_fivethirtyeight() +
  ggtitle("Skin Tone by Ethnic Identity") +
  scale_fill_manual(values = c("#8D5524", "#C68642", "#F1C27D", "#FFDBAC"))
