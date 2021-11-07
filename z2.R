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


# Задание 1: В таблице data создать колонку Vtrunk в которой будет посчитан объем ствола

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
sum_table  = greendb %>% group_by(species_ru) %>%
summarise (
    num = n()
  ) %>% arrange(desc(num))
sum_table = summarise(group_by(greendb, species_ru), 
                         diam_m = mean(d_trunk_m, na.rm=T),
                         num = n(),
                         height_m = mean(height_m, na.rm=T)
   )
   divers = greendb %>% group_by(adm_region, species_ru) %>%
summarise(
       nspecies = n()
     ) %>% select(-nspecies) %>% 
     ungroup() %>% group_by(adm_region) %>%
     summarise(
       nspecies = n()
     )
#Получить сводную таблицу, где для каждого района будет 3 доминирующих вида и количество деревьев в этом районе для каждого из видов.
   divers = greendb %>% group_by(adm_region, species_ru) %>%
     summarise(
       nspecies = n()
     ) %>% group_by(adm_region) %>% 
     arrange(adm_region, desc(nspecies)) %>%
     mutate(order = order(nspecies, decreasing = T)) %>%
     filter( order <= 3) %>% select(-order)
library(tidyr)
   transp = greendb %>% group_by(adm_region, species_ru) %>%
     summarise(
       nspecies = n()
     ) %>% pivot_wider(names_from = species_ru, values_from = nspecies) %>%
     select(starts_with("Липа"))
   ### MAPS
   library(sf)
   library(ggplot2)
   library(ggthemes)
   map = st_read(E:/учеба/магистрат/2 курс/математическое моделирование/задание 1/boundary-polygon-lvl8.geojson,
                 options = "ENCODING=UTF-8"
#Фильтруем по виду
average_trunks = greendb %>% filter(species_ru == "Дуб черешчатый", adm_region != "NA") %>% group_by(adm_region )%>%
  summarise(
    mean_trunk = mean(d_trunk_m, na.rm = TRUE)
  )
# Правим табличку с данными, чтобы сортировка была и плот нормально обработал
average_trunks_data = average_trunks %>% group_by(adm_region, mean_trunk) %>%
  arrange(adm_region, desc(mean_trunk)) %>%
  mutate(order = order(mean_trunk, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -mean_trunk) %>%
  rename(NAME = adm_region)
map = left_join(map, average_trunks_data, by="NAME")
ggplot() + geom_sf(data = map, aes(fill=mean_trunk))+
  theme_foundation() + theme(legend.title = element_blank())
   
