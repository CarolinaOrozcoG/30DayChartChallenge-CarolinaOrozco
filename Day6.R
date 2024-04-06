library(haven)
library(tidyverse)
library(writexl)
library(extrafont)
library(ggrepel)
library(ggthemes)
library(readxl)
library(dplyr)
library(ggtext)

base <- read.csv("EXP_PM2_5_06042024201246950.csv")

paises <- c( "ARG","BOL","BRA","CHL","COL","CRI","ECU",
             "GTM","HND","MEX","NIC","PAN","PER","PRY","URY","VEN",
             "SLV")

LAC <- base %>% filter(COU %in% paises) %>% 
  filter(Year>=2000)

max(LAC$Value)

day6 <- LAC %>% 
  filter(COU== "MEX" | COU=="ECU") %>% 
  ggplot()+
  geom_line(aes(x=Year, y= Value,color= COU))+
  geom_point(aes(x=Year, y= Value ,color= COU))+
  labs(color= "País", x= NULL, y= "Microorganismos por metro cúbico",
       title = "Exposición al Aire Contaminado ",
       subtitle = "Media de exposición al PM2.5 por año ",
       caption= "Fuente: elaboración propia con base en datos de la OECD,
       #30DayChartChallenge")+
  scale_color_manual(values = c("blue","#008f39"))+
  scale_x_continuous(breaks= c(seq(2000,2020,2)))+
  scale_y_continuous(breaks = c(seq(0,max(LAC$Value),5),74))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=9, hjust=1, face="italic", color="black"))
  