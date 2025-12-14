###################################################################################### 
##### Functions and Objects for the European Identity Related to Ukriane Project #####
###################################################################################### 

#### Load Packages ####

pacman::p_load(tidyverse,
               haven,
               labelled,
               readxl, 
               sf, 
               srvyr,
               rnaturalearth,
               rnaturalearthdata,
               foreign,
               ggrepel,
               marginaleffects,
               patchwork,
               ggtext, 
               flextable,
               webshot2,
               ggnewscale,
               knitr,
               kableExtra,
               webshot2,
               gt,
               shiny
)

#### ggplot Custom Theme ####

theme_custom =   theme_minimal() + theme(legend.position = "bottom",
                                         strip.placement = "outside",
                                         legend.spacing.y = unit(0.1, "cm"), # space between rows
                                         legend.key.height = unit(0.3, "cm") ,
                                         plot.title = element_text(hjust = 0.5,
                                                                   face = "bold",
                                                                   size = 10),
                                         plot.subtitle = element_text(hjust = 0.5,
                                                                      face = "bold",
                                                                      size = 8),
                                         axis.title.y = element_text(face = "bold",
                                                                     size = 9),
                                         axis.title.x = element_text(face = "bold",
                                                                     size = 9),
                                         plot.caption = element_text(size = 7,
                                                                     hjust = 1,
                                                                     lineheight = 0.5),
                                         legend.text = element_text(face = "bold", 
                                                                    size = 10),
                                         legend.box = "vertical",
                                         legend.title = element_text(face = "bold",
                                                                     size = 9),
                                         strip.text = element_text(size = 9, face = "bold"),          # smaller text
                                         strip.background = element_rect(fill = "#e6f8d1", colour = NA),                                 # optional
                                         strip.text.y.left = element_text(angle = 0),                  # if y-strips
                                         strip.text.x = element_text(margin = margin(b = 1, t = 1)),   # small top/bottom padding
                                         strip.text.y = element_text(margin = margin(l = 1, r = 1)),
                                         axis.text = element_text(face = "bold", 
                                                                  size = 10),
                                         axis.text.y = element_text(margin = margin(r = 0.05)),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.minor.x = element_blank(),
                                         panel.grid.major.y = element_blank(),
                                         panel.grid.minor.y = element_blank(),
                                         axis.line.x = element_line(color = "grey80"),
                                         axis.line.y = element_line(color = "grey80"),)


#### List of Countries ####


ORIGINAL_COUNTRIES <-  c("Denmark", "Finland", "Germany", 
                         "France", "Lithuania",
                         "Greece", "Italy", "Poland",
                         "Spain", "Sweden", "UK")

NEW_COUNTRIES <- c("UK", "Slovakia", "Netherlands", "Denmark", "Hungary",
                   "Croatia", "Bulgaria", "Lithuania", "Romania", "Greece", 
                   "Poland", "Spain", "Germany", "Sweden", "Italy", "France", "Finland")

NEW_COUNTRIES_2024 <- c("UK", "Slovakia", "Netherlands", "Denmark", "Hungary",
                        "Croatia", "Bulgaria", "Lithuania", "Romania", "Greece", 
                        "Poland", "Spain", "Germany", "Sweden", "Italy", "France", "Finland", "Belgium")


