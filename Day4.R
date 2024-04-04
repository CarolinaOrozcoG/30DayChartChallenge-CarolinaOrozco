library(tidyverse)
library(waffle)

day4 <- waffle(c('Claudia Sheinbaum - MORENA-PT-PVEM = 58%' = 58, 
         'Xóchitl Gálvez - PAN-PRI-PRD = 35%' = 35,
         'Jorge Álvarez Máynez - MC = 7%' = 7), rows = 10,
       colors = c("red2","maroon1", "orange"),
       title = 'Intención de voto efectivo a 31 de marzo de 2024',legend_pos="bottom")+
  labs(caption= "Fuente: Barómetro Electoral Bloomberg
       #30DayChartChallenge")

ggsave(day4, filename = "Day4.png", dpi=300,
       width= 7.15, height =  5.46 )  