library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(sf)

greendb = read_csv("E:/учеба/магистрат/2 курс/математическое моделирование/задание 1/greendb.csv")

greendb %>% summary
greendb |> summary()

# ѕосчитать регрессионную зависимость высоты от диаметра ствола дл€ район ‘илевский парк вида Ћипа европейска€

data = greendb %>% 
  filter(species_ru == "Ћипа европейска€", adm_region == "район ‘илевский парк") %>% select(
    height_m, d_trunk_m
  )

model = lm(d_trunk_m ~ height_m, data)

summary(model)

# d_trunk_m = 0.0147201 * heght_m =0.0154717
# p < 0.001

anova(model)
# Analysis of Variance Table
# Response: d_trunk_m
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# height_m     1 4.4425  4.4425  1412.8 < 2.2e-16 ***
#   siduals 1411 4.4368  0.0031                       
#   Signif. codes:  0 С***Т 0.001 С**Т 0.01 С*Т 0.05 С.Т 0.1 С Т 1
# «ависит p < 0.05