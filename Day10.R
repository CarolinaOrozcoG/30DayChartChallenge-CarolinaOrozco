library(tidyverse)
library(ggtext)


base <- read.csv("alturamedia.csv")

base1 <- base %>% 
  select(Country, Sex, Year, Mean.height) %>% 
  spread(key = Year, value = Mean.height)

base1 %>% 
  ggplot() + 
  geom_point(mapping = aes(x = `1985`, y = reorder(Country, desc(`2019`)), shape = Sex),  color = "dodgerblue", show.legend = F) +
  geom_point(mapping = aes(x = `2019`, y = reorder(Country, desc(`2019`)), shape = Sex), color = "deeppink", show.legend = F) +
  facet_wrap(~Sex) +
  geom_segment(mapping = aes(x = `1985`, 
                             xend = `2019`, 
                             y = reorder(Country, desc(`2019`)),
                             yend = reorder(Country, desc(`2019`))))+
  labs(title= "Evolución de la Estatura Media",
       subtitle=  " Comparación de la estatura media de hombres y mujeres de 19 años entre los años <span style='color:dodgerblue;'>1985</span> y 
    <span style='color:deeppink;'>2019</span>",
       y= "País", x="Estatura",
       caption= "Fuente: NCD Risk Factor Collaboration
       #30DayChartChallenge")+
  theme(plot.subtitle = element_markdown())
  

  