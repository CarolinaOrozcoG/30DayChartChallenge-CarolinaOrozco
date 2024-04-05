library(haven)
library(tidyverse)
library(writexl)
library(extrafont)
library(ggrepel)
library(ggthemes)
library(readxl)
library(dplyr)
library(ggtext)

loadfonts(device ="win", quiet =TRUE)

base <- read.csv("personas-desaparecidas-n.csv")

base <- base %>% 
  mutate(Total= Hombre+Mujer+Indeterminado, Category = as.numeric(base$Category)) %>% 
  filter(Category>2000) %>% filter(Category<2024)


base %>% 
  ggplot()+
  geom_line(aes(x=Category, y= Total),color="red")+
  geom_point(aes(x=Category, y= Total),color="red")+
  geom_line(aes(x=Category, y= Hombre), color= "dodgerblue")+
  geom_point(aes(x=Category, y= Hombre), color= "dodgerblue")+
  geom_line(aes(x=Category, y= Mujer), color= "violet")+
  geom_point(aes(x=Category, y= Mujer), color= "violet")+
  labs(x= NULL, y= "Número de Personas Desaparecidas",
       title = "Total de Personas Desaparecidas por Año",
       subtitle = "Total de personas reportadas como desaparecidas localizadas y no localizadas.",
       caption= "Fuente: elaboración propia con base en datos del Registro Nacional de Personas Desaparecidas y No Localizadas (RNPDNO),
       #30DayChartChallenge")+
  scale_x_continuous(breaks= c(seq(2000,2022,2),2023))+
  scale_y_continuous(breaks = c(seq(0,max(base$Total),2000),30000))+
  annotate(geom = "text",
           x = 2024,
           y = c(max(base$Total), max(base$Hombre), max(base$Mujer)),
           label = c("Total", "Hombre", "Mujer"),
           color = c("red", "dodgerblue", "violet"))+
theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=9, hjust=1, face="italic", color="black"))
