library(tidyverse)
library(dplyr)
library(readr)
greendb = read_csv("E:/�����/���������/2 ����/�������������� �������������/������� 1/greendb.csv")
View(greendb)
data = greendb
data$d_trunk_m
rad = data$d_trunk_m / 2
basal = rad * rad * 3.14
greendb$basal = basal
data$height_m
v = greendb$basal * data$height_m
greendb$v = v


# ������� 1: � ������� data ������� ������� Vtrunk � ������� ����� �������� ����� ������

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
#�������� ������� �������, ��� ��� ������� ������ ����� 3 ������������ ���� � ���������� �������� � ���� ������ ��� ������� �� �����.
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
     select(starts_with("����"))
   ### MAPS
   library(sf)
   library(ggplot2)
   library(ggthemes)
   map = st_read(E:/�����/���������/2 ����/�������������� �������������/������� 1/boundary-polygon-lvl8.geojson,
                 options = "ENCODING=UTF-8"
#��������� �� ����
average_trunks = greendb %>% filter(species_ru == "��� ����������", adm_region != "NA") %>% group_by(adm_region )%>%
  summarise(
    mean_trunk = mean(d_trunk_m, na.rm = TRUE)
  )
# ������ �������� � �������, ����� ���������� ���� � ���� ��������� ���������
average_trunks_data = average_trunks %>% group_by(adm_region, mean_trunk) %>%
  arrange(adm_region, desc(mean_trunk)) %>%
  mutate(order = order(mean_trunk, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -mean_trunk) %>%
  rename(NAME = adm_region)
map = left_join(map, average_trunks_data, by="NAME")
ggplot() + geom_sf(data = map, aes(fill=mean_trunk))+
  theme_foundation() + theme(legend.title = element_blank())
   
