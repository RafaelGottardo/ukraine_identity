####### OP-ED #####

europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe")

df_map <- europe %>% 
  left_join(EUI_data_short %>%
              mutate(country = replace(country, country == "UK", "United Kingdom"))  %>% 
             # filter(Year == 2025) %>% 
              group_by(country) %>%
              summarize(Mean = mean(Security_FA, na.rm = TRUE)),
            by = c("name" = "country"))

map_labels <- df_map %>%
  filter(name %in% c(unique(EUI_data_short$country), "United Kingdom")) %>%
  st_point_on_surface() %>%
  mutate(
    coords = st_coordinates(geometry),
    label_text = paste0(name)
  )

palette <- rev(c(
  "#67000d",
  "#a50f15",
  "#cb181d",
  "#ef3b2c",
  "#f7f7f7",
  "#4292c6",
  "#2171b5",
  "#08519c",
  "#08306b"
))

Factor_map <- df_map %>% 
  #filter(!is.na(Mean)) %>% 
  ggplot() +
  geom_sf(aes(fill = Mean), color = "white", linewidth = 0.25) +
  
  scale_fill_stepsn(
    colours = palette,
    breaks = seq(-0.76, 0.59, length.out = 9),
    limits = c(-0.76, 0.59), 
    labels = c("Highest Security Focus", "", "", "", "- Relations with Russia -", "", "", "", "Highest Collaboration Focus"),
    na.value = "#eeeeee",
    guide = guide_colorsteps(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.5, "cm"),
      show.limits = TRUE,
      label.position = "bottom"
    )
  ) +
  geom_label(
    data = map_labels,
    aes(x = coords[,1], y = coords[,2], label = label_text),
    size = 3,
    fontface = "bold",
    fill = "white",
    color = "black",
    label.size = 0
  ) +
  coord_sf(
    xlim = c(-11,38),
    ylim = c(35,67),
    expand = FALSE
  ) +
  
  labs(fill = NULL) +
  
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.2,"cm"),
   # panel.background = element_rect(fill = "#eef3f7", colour = NA),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
   legend.text = element_text(size = 12, face = "bold")
  )

ggsave("plots/Factor_mapLabs.png", Factor_map, width = 9, height = 8)

##### Hungary Vote ####

Hungary_2025 <- EUI_data_short %>% 
  filter(country == "Hungary") %>% 
  filter(Year == 2025)

Hungary_2025 <- Hungary_2025 %>%
  mutate(Security_FA = as.numeric(Security_FA))
mod_hungary <- multinom(reformulate(c("Security_FA", CONTROLS), response = "Past_vote"), data = Hungary_2025)

summary(mod_hungary)

preds <- predictions(
  mod_hungary,
  newdata = datagrid(Security_FA = modelr::seq_range(Hungary_2025$Security_FA, 50)),
  type = "probs"
)

library(ggplot2)

hungary_vote <- preds %>% 
  filter(group != "105") %>% 
  mutate(group = case_match(group, "103" ~ "Fidesz (Orbán)",
                            "200" ~ "United for Hungary")) %>%  
ggplot(aes(x = Security_FA, y = estimate, color = group)) +
  geom_line(size = 1) +
  geom_text(
    data = ~ .x %>% 
      group_by(group) %>% 
      slice_max(Security_FA, n = 1),
    aes(label = group),
    hjust = -0.1,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  scale_colour_manual(values = c("#ff6a00", "#4cb5a1")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = c("", "Most Russia Accommodating","", "", "", "","Most Security Focused", ""), 
                     breaks = seq(-0.8688241, 1.778304, length.out = 8),
                     limits = c(-0.9, 2.5)) +
  labs(
    y = "Predicted Probabilty of Supporting Each Party",
    color = "Vote choice 2022",
    x = "Hungarians' Security Related Orientation"
  ) +
  theme_custom + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none")

ggsave("plots/hungary_vote_graph.png", hungary_vote, width = 7, height = 4, dpi = "retina")
