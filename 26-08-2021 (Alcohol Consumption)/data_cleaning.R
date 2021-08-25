# Libreries ----
library(foreign)   # read different types of data (dbf)
library(survey)    # manipulate census data
library(tidyverse) # manipulate tabular data

options(survey.lonely.psu="adjust")

# Data ---- 
conc <- read.dbf("./Data/ENIGH/concentradohogar.dbf", as.is=T) 
conc <- conc %>% select( c("folioviv", "foliohog", "ing_cor", "factor","upm","est_dis")) %>%
  mutate(
    entidad = substr(folioviv, 1, 2),
    Nhog = 1, 
    factor1 = factor,
    ID = paste(folioviv, foliohog, sep=".")
  ) %>% arrange(
    ing_cor, folioviv, foliohog
  ) 

tot_hogares  <- sum(conc$factor) # Total number of homes
tam_dec      <- trunc(tot_hogares/10) # Number of homes per decil
conc$tam_dec <- tam_dec

df1 <- conc %>% mutate(
  MAXT = ing_cor
) %>% arrange(MAXT) %>% mutate(
  ACUMULA = cumsum(factor)
) # save df in new df to change variables 

for(i in 1:9){ # Generate Income Decile
  a1  <- df1[dim(df1[df1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  df1 <- rbind(df1[1:(dim(df1[df1$ACUMULA<tam_dec*i,])[1]+1),],
               df1[(dim(df1[df1$ACUMULA<tam_dec*i,])[1]+1):dim(df1[1])[1],])
  
  b1  <- tam_dec*i-df1[dim(df1[df1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  
  df1[(dim(df1[df1$ACUMULA<tam_dec*i,])[1]+1),]$factor <- b1
  df1[(dim(df1[df1$ACUMULA<tam_dec*i,])[1]+2),]$factor <- (a1-b1)
}

df1 <- df1 %>% mutate(
  ACUMULA2 = cumsum(df1$factor),
  DECIL    = 0
) 

df1[(df1$ACUMULA2<=tam_dec),]$DECIL<-1

for(i in 1:9){ # Label Decil
  df1[((df1$ACUMULA2>tam_dec*i)&(df1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
df1[df1$DECIL%in%"0",]$DECIL<-10

df1 <- df1 %>% select(
  c("ID", "folioviv", "foliohog", "factor", "factor1", "ing_cor", "upm", "entidad", "DECIL")
) %>% write.dbf(., "./Data/earnings_home_2020.dbf") # Save it as new data base    
    
## Home Spending ----
gh  <- read.dbf("./Data/ENIGH/gastoshogar.dbf")
gh  <- gh %>% select(c("folioviv", "foliohog", "clave", "gasto_tri")) %>%
  filter(clave == "A223" | clave == "A224" | clave == "A225" | clave == "A226" |
           clave == "A227" | clave == "A228" | clave == "A229" | clave == "A230" |
           clave == "A231" | clave == "A232" | clave == "A233" | clave == "A234" |
           clave == "A235" | clave == "A236" | clave == "A237" | clave == "A238") %>% # We only keep those variable related to alcohol
  mutate(
    ID = paste(folioviv, foliohog, sep=".")
  )

df2 <- gh %>% group_by(ID) %>%
  summarise(
    GASTO_ALCOHOL = sum(gasto_tri, na.rm = T)
  ) %>% data.frame(.) %>%
  write.dbf(., "./Data/spendings_home_2020.dbf")

## Joint ----
ing <- read.dbf("./Data/earnings_home_2020.dbf", as.is = T)
gas <- read.dbf("./Data/spendings_home_2020.dbf", as.is = T)

df3 <- merge(ing, gas, by = "ID", all = T) %>% 
  filter(!is.na(GASTO_ALCO), ing_cor>0) %>% 
  mutate(
    gasto_porcentaje = GASTO_ALCO/ing_cor
  ) # Join them by Home identification number

x <- c("Total", round(weighted.mean(df3$GASTO_ALCO, w = df3$factor), 0), 
       round(weighted.mean(df3$gasto_porcentaje, w = df3$factor)*100, 2)) # National mean

prom_gast_alc <- df3 %>% group_by(DECIL) %>%
  summarise(
    GASTO_ALCOHOL_CORR = round(weighted.mean(GASTO_ALCO, w = factor), 2),
    GASTO_ALCOHOL_PORC = round(weighted.mean(gasto_porcentaje, w = factor)*100, 2)
  ) # Income level mean

gast_al <- rbind(x, prom_gast_alc)
write.csv(gast_al, "./Data/consumption_income_level_2020.csv") # Save it as csv

