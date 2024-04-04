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

base <- read.csv("RS_GBL_01042024204707909.csv")

paises <- c( "ARG","BOL","BRA","CHL","COL","CRI","ECU",
             "GTM","HND","MEX","NIC","PAN","PER","PRY","URY","VEN",
             "SLV")
impuestos <- as.character(seq(from = 1000, to = 6000, 1000))

LAC <- base %>% filter(COU %in% paises) %>% filter(Year== 2021) %>%
  filter(TAX %in% impuestos) %>% filter(Indicator== "Tax revenue as % of total taxation") %>% 
  filter(Level.of.government=="Total")

LAC %>% filter(COU == "CHL") %>% 
  summarise(SUM = sum(Value))

vectorNombres <- unique(LAC$Tax.revenue)

LAC <- LAC %>% 
  mutate(nombreTax = case_when(
    Tax.revenue == vectorNombres[1] ~ "Renta",
    Tax.revenue == vectorNombres[2] ~ "Seguridad Social",
    Tax.revenue == vectorNombres[3] ~ "Salarios",
    Tax.revenue == vectorNombres[4] ~ "Propiedad",
    Tax.revenue == vectorNombres[5] ~ "Bienes y Servicios",
    Tax.revenue == vectorNombres[6] ~ "Otros impuestos"))




graph <- LAC %>% 
  ggplot(mapping = aes(x = as.factor(COU), y = Value, fill = nombreTax))+
  geom_col(col = "white", width = 1, show.legend = F)+
  scale_fill_manual(values = c("#79CDCD","#9A32CD", "#EE1289","#FFB6C1","#CD00CD","#DB7093")) +
  labs(
    title = "Estructura Tributaria de América Latina",
    subtitle = "Porcentaje de recaudo por tipo de impuesto<br><span style='color:#79CDCD;'>Bienes y Servicios</span>, <span style='color:#9A32CD;'>Otros Impuestos</span>, 
    <span style='color:#EE1289;'>Propiedad</span>,
    <span style='color:#FFB6C1;'>Renta</span>,
    <span style='color:#CD00CD;'>Salarios</span>,
    <span style='color:#DB7093;'>Seguridad Social</span>",
    x = NULL,
    y = "Porcentaje",
    caption = "Fuente: OCDE"
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown())
getwd()

ggsave(filename = "day1_ImpuestosLatam.png",
       plot = graph,
       dpi = 300,
       width = 8.37,
       height = 4.84
       )




  geom_text(
    data = map2 %>% filter(country %in% c("Colombia","World")),
    aes(label = etiquetas),
    position = position_stack(vjust = 0.70),
    col = 'white',
    size = 3,
    fontface = 'bold'
  ) +
  labs(title = "B: Participación porcentual por fuente - 2022",
       subtitle = "Comparación entre Colombia y el promedio mundial",
       x = NULL,
       y = "Porcentaje") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

graph2

LAC <- base %>% filter(COU %in% paises)

writexl::write_xlsx(LAC, path = "BaseFiltrada.xlsx")
