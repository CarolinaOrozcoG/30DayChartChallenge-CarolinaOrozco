library(tidyverse)
library(ggspatial)


ipc <- sf::st_read("baseshp.shp", quiet=T) 

mapa <- ipc %>% 
  ggplot()+geom_sf(aes(geometry=geometry))+ 
  geom_sf(aes(geometry = geometry, fill = corrupcn)) +
  scale_fill_continuous("IPC",low = "red3", high = "yellow", limits= c(0,100))+
  xlab("Longitud") + ylab("Latitud") +
  labs(caption= "Fuente: elaboración propia con base en datos de Transparencia Internacional <br> **#30DayChartChallenge**")+
  ggtitle("Índice de Percepción de la Corrupción",
          subtitle = "Realizado por: Carolina Orozco García")+
  theme_test() +
  facet_wrap( ~ year) +
  annotation_scale()+
  annotation_north_arrow(location="tr",height = unit(0.8, 'cm'), width = unit(0.7, 'cm')
)+
  theme(plot.title = element_text(hjust = 0, size = 13, face = "italic"),
        plot.caption = element_markdown(size=7, hjust=0.95, face="italic", color="black"),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.title.y = element_blank(),
        # axis.title.x = element_blank(),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "bottom",
        legend.key.size = unit(0.5,"cm" ),
        legend.key.width = unit(2, "cm"))

ggsave(mapa, filename = "day2.png",dpi = 300,
       width= 7.22, height= 4.93 )
