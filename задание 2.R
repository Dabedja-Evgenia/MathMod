library(dplyr)

#��������, ��� ������� ������� �������� ������� �� ������� ��������������

data = read_csv(file = "greendb.csv")

data = data %>% filter(adm_region = "����� ��������� ����")
model = lm(d_trunk_m ~ species_ru, data)
