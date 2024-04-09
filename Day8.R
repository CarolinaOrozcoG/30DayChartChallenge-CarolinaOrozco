library(tidyverse)

base <- readxl::read_xlsx("Basecompleta.xlsx")

baseG <- base[base$year == "2020", ]

baseG <- baseG %>% 
  group_by(Pais) %>% 
  summarise(educacion = mean(Educacion, na.rm = T))

summary(baseG$educacion)

baseG <- baseG %>% 
  arrange(educacion)

baseG$id <- 1:nrow(baseG)

baseG$etiqueta <- str_c(baseG$Pais, "=", round(baseG$educacion), sep = " ")

# Get the name and the y position of each label
label_data <- baseG

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)


p <- ggplot(data = baseG, aes(x=reorder(Pais,educacion), y=educacion)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(-20,20) +
  labs(title = "Promedio de Años de Educación Cursados en América Latina", 
       caption="Fuente: elaboración propia con base en Latinobarómetro 2020
       #30DayChartChallenge")+
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(axis.text = element_blank(),
     axis.title = element_blank(),
     panel.grid = element_blank()
    #plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=Pais, y=educacion+2, 
                                 label=etiqueta, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, 
            angle= label_data$angle, inherit.aes = FALSE )

ggsave(filename = "Day8.png", plot =p, dpi = 300 )

