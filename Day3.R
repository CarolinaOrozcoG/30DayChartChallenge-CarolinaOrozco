library(tidyverse)

base <- readxl::read_xlsx("agrupadoCorrupt1.xlsx")

base %>% glimpse()


day3 <- base %>% na.omit() %>% ungroup() %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(mapping = aes(x = year, y = porcentaje, color = cod, group = cod)) +
  geom_point() +
  geom_line()+
  labs(color= "País", title = "Tolerancia a la Corrupción en América Latina 2017-2020", 
       caption="Fuente: elaboración propia con base en Latinobarómetro
       #30DayChartChallenge",
       y="Porcentaje", x="Año")+
  theme(plot.title=element_text(size=15, color='black', hjust = 0.5))


ggsave(day3, filename = "Day3.png", dpi=300,
       width= 8.36, height =  4.33 )  
