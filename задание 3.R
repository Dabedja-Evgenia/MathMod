library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(sf)

greendb = read_csv("E:/�����/���������/2 ����/�������������� �������������/������� 1/greendb.csv")

greendb %>% summary
greendb |> summary()

# ��������� ������������� ����������� ������ �� �������� ������ ��� ����� ��������� ���� ���� ���� �����������

data = greendb %>% 
  filter(species_ru == "���� �����������", adm_region == "����� ��������� ����") %>% select(
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
#   Signif. codes:  0 �***� 0.001 �**� 0.01 �*� 0.05 �.� 0.1 � � 1
# ������� p < 0.05