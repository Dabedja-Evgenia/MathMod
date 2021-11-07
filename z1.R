library(tidyverse)
library(dplyr)
library(readr)
greendb = read_csv("E:/учеба/магистрат/2 курс/математическое моделирование/задание 1/greendb.csv")
View(greendb)
data = greendb
data$d_trunk_m
rad = data$d_trunk_m / 2
basal = rad * rad * 3.14
greendb$basal = basal
data$height_m
v = greendb$basal * data$height_m
greendb$v = v


#Задание 1: В таблице data создать колонку Vtrunk в которой будет посчитан объем ствола

unique(data$species_ru)
data$species_ru %>% unique
data$species_ru |> unique()
data$species_ru = factor(data$species_ru)
summary(data$species_ru)

sum_table = greendb %>% group_by(species_ru) %>% 
  summarise(
    diam_m = mean(d_trunk_m, na.rm=T),
    num = n(),
    height_m = mean(height_m, na.rm=T)
  )