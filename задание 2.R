library(dplyr)

#Показать, что средний диаметр растений зависит от видовой принадлежности

data = read_csv(file = "greendb.csv")

data = data %>% filter(adm_region = "район Филевский парк")
model = lm(d_trunk_m ~ species_ru, data)
