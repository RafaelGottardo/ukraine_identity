###################################################################################### 
##### Functions and Objects for the European Identity Related to Ukriane Project #####
###################################################################################### 

#### Load Packages ####

pacman::p_load(tidyverse,
               haven,
               estimatr,
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
               shiny,
               psych,
               nnet,
               furrr,
               lme4,
               broom.mixed,
               nnet,
               rgexf,
               igraph,
               ggraph,
               xml2,
               tidygraph
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
                                                                     size = 12),
                                         axis.title.x = element_text(face = "bold",
                                                                     size = 12),
                                         plot.caption = element_text(size = 7,
                                                                     hjust = 1,
                                                                     lineheight = 0.5),
                                         legend.text = element_text(face = "bold", 
                                                                    size = 10),
                                         legend.box = "vertical",
                                         legend.title = element_text(face = "bold",
                                                                     size = 12),
                                         strip.text = element_text(size = 10, face = "bold"),          # smaller text
                                         strip.background = element_rect(fill = "#e6f8d1", colour = NA),                                 # optional
                                         strip.text.y.left = element_text(angle = 0),                  # if y-strips
                                         strip.text.x = element_text(margin = margin(b = 1, t = 1)),   # small top/bottom padding
                                         strip.text.y = element_text(margin = margin(l = 1, r = 1)),
                                         axis.text.x = element_text(face = "bold", 
                                                                  size = 10),
                                         axis.text.y = element_text(face = "bold", 
                                                                    size = 12, margin = margin(r = 0.05)),
                                         # panel.grid.major.x = element_blank(),
                                        #  panel.grid.minor.x = element_blank(),
                                         # panel.grid.major.y = element_blank(),
                                          #panel.grid.minor.y = element_blank(),
                                         axis.line.x = element_line(color = "grey80"),
                                         axis.line.y = element_line(color = "grey80"),)


#### List of Countries ####


ORIGINAL_COUNTRIES <-  c("Denmark", "Finland", "Germany", 
                         "France", "Lithuania",
                         "Greece", "Italy", "Poland",
                         "Spain", "Sweden", "UK")

COUNTRIES_2022 <- c("UK", "Denmark", "Greece", "Hungary", "Lithuania",
                    "Italy", "Poland", "Netherlands", "Romania",  "Slovakia",
                    "Croatia", "Bulgaria", "Spain", "Finland", "France",
                    "Germany", "Sweden")

NEW_COUNTRIES_2024 <- c("UK", "Slovakia", "Netherlands", "Denmark", "Hungary",
                        "Croatia", "Bulgaria", "Lithuania", "Romania", "Greece", 
                        "Poland", "Spain", "Germany", "Sweden", "Italy", "France", "Finland", "Belgium")


CONTROLS <- c("Woman", "Education", "Age", "Urban")

#### Plot Predictions - VOTE CHOICE ####

plot_predictions_vote <- function(data, COLOURS){
data %>% 
ggplot(aes(x = Security_FA, y = estimate, color = group)) +
  geom_line(size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey89") +
    geom_label_repel(
    data = ~ .x %>% 
      filter(str_starts(group, " ", negate = TRUE)) %>% 
      group_by(group) %>% 
      slice_max(Security_FA, n = 1),
    aes(label = group),
    show.legend = FALSE,
    fontface = "bold",
    nudge_x = .1,
    min.segment.length = unit(0, 'lines')
  ) +
  scale_colour_manual(values = COLOURS) +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                        limits = c(-1.5, 1.5), 
                        labels = c("", "Highest Defence Focus",  "", "", "- Relations with Russia -", "", "", "Highest Nomaralization Focus", "")) +
  labs(
    y = "Predicted Probabilty of Supporting Each Party",
    color = "Previous Vote Choice",
    x = "Defence-Normalization Dimension"
  ) +
  theme_custom + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none") 
}

#### Rescale 0 - 1 ####
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}





