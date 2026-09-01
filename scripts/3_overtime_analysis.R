
###### Code to perform longitudinal analyses ####

Group_composition_overtime <- EUI_data_long %>% 
  filter(country %in% ORIGINAL_COUNTRIES) %>% 
  group_by(Year) %>% 
  count(Ukraine_groups_long) %>% 
  mutate(prop = n/sum(n),
         Ukraine_groups_long = factor(Ukraine_groups_long,
                                                levels = rev(c("Defence and Security",
                                                               "Trade and Diplomacy",
                                                               "Neither/Don't Know",
                                                               "Normalization Focused")))) %>% 
  ungroup() %>% 
  ggplot(aes(x = Year, y = prop, col = Ukraine_groups_long)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(labels = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
                     breaks = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
                     ) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_vline(xintercept = 2021.75, lty = 4, col = "grey50") +
  annotate("text", x = 2020.65, y = 0.5, label = "Russian Invaison of Ukraine", col = "black" ) +
  labs(x = NULL, y = NULL, col = "Group") + 
  guides(colour = guide_legend(ncol = 2,
                               reverse = TRUE)) +
  scale_colour_manual(values = rev(c("darkblue", "darkred", "forestgreen"))) + 
  theme_custom + 
  theme(panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank())

ggsave("plots/Group_composition_overtime.png", Group_composition_overtime, width = 8, height = 5)



Group_composition_overtime_df <- EUI_data_long %>% 
  filter(country %in% ORIGINAL_COUNTRIES) %>% 
  group_by(Year, country) %>% 
  count(Ukraine_groups_long) %>% 
  mutate(prop = n/sum(n),
         Ukraine_groups_long = factor(Ukraine_groups_long,
                                      levels = rev(c("Defence Focused",
                                                     "Conditional Ukraine Supporters",
                                                     "Domestic/ Distracted",
                                                     "Normalization Focused")))) %>% 
  ungroup() 

country_order <- Group_composition_overtime_df %>% 
  filter(Year == 2025 & Ukraine_groups_long == "Normalization Focused") %>% 
  arrange(-prop) %>% 
  pull(country)

Group_composition_overtime_country <- Group_composition_overtime_df %>% 
  mutate(country = factor(country, levels = country_order)) %>% 
  ggplot(aes(x = Year, y = prop, col = Ukraine_groups_long)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(labels = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
                     breaks = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
  ) + 
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~country) + 
  geom_vline(xintercept = 2021.75, lty = 4, col = "grey50") +
  #annotate("text", x = 2020.65, y = 0.5, label = "Russian Invaison of Ukraine", col = "black" ) +
  labs(x = NULL, y = NULL, col = "Group") + 
  guides(colour = guide_legend(ncol = 2,
                               reverse = TRUE)) +
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  theme_custom + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/Group_composition_overtime_country.png", Group_composition_overtime_country,
       width = 12, height = 10)

