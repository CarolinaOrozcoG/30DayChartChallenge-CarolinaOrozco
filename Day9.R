library(tidyverse)


base <- readxl::read_xlsx("cutzamala.xlsx")

 base %>%
  ggplot(mapping = aes(x =year, y = almacen))+
  geom_col(col = "white", width = 1, show.legend = F,fill="#79CDCD")+
      labs(
    title = "Evolución del almacenamiento en las presas del Sistema Cutzamala 1991-2024
    Mes: Marzo Día:31",
    subtitle = "Volumen en millones de metros cúbicos",
    x = NULL,
    y = "Volumen",
    caption = "Fuente: CONAGUA") +
   scale_x_continuous(breaks= c(seq(1991,2024,2)))+
   scale_y_continuous(breaks = c(seq(0,max(base$almacen),50)))+
  theme_minimal() 

