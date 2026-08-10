#### Code to graph the correlates of being pro-Russian ####

Demographics <- EUI_data_short %>% 
  mutate(Education = ifelse(Education == "Tertiary", 1, 0),
         young = ifelse(Age == "18-24", 1, 0),
         young_adult = ifelse(Age == "25-34", 1, 0),
         middle_aged = ifelse(Age == "35-44", 1, 0),
         older_adult = ifelse(Age == "45-54", 1, 0),
         Old = ifelse(Age == "55+", 1, 0),
         Urban = ifelse(Urban == 'Urban/Suburban', 1, 0),
         Right_wing = ifelse(ideology == "Right-wing", 1, 0),
         Left_wing = ifelse(ideology == "Left-wing", 1, 0),
         Centre = ifelse(ideology == "Centre", 1, 0),
         More_defence_focused = ifelse(Security_FA < 0, 1, 0)
         ) %>% 
  group_by(Year) %>% 
  summarise(`% More Defence Focused` = mean(More_defence_focused, na.rm = TRUE),
            `% Degree` = mean(Education, na.rm = TRUE),
            `% 18 to 24` = mean(young, na.rm = TRUE),
            `% 25 to 34` = mean(young_adult, na.rm = TRUE),
            `% 35 to 44` = mean(middle_aged, na.rm = TRUE),
            `% 45 to 54` = mean(older_adult, na.rm = TRUE),
            `% 55+` = mean(Old, na.rm = TRUE),
            `% Left-Wing` = mean(Left_wing, na.rm = TRUE),
            `% Centre` = mean(Centre, na.rm = TRUE),
            `% Right-Wing` = mean(Right_wing, na.rm = TRUE),
            `% Urban/Suburban` = mean(Urban, na.rm = TRUE)
            ) %>% 
  pivot_longer(2:12, names_to = "Variable", values_to = "Percentage") 

demographics_graph <- Demographics %>% 
  mutate(Variable = factor(Variable, levels = rev(c("% More Defence Focused",
                                                "% Degree",
                                                "% Urban/Suburban",
                                                "% 18 to 24",
                                                "% 25 to 34",
                                                "% 35 to 44",
                                                "% 45 to 54",
                                                "% 55+",
                                                "% Left-Wing",
                                                "% Centre",
                                                "% Right-Wing")))) %>% 
  ggplot(aes(x = Percentage, y = Variable, col = as.factor(Year))) +
  geom_point(position = position_dodge(0.6), size = 2) +
  geom_vline(xintercept = 0.5, lty = 4, col = "grey40") +
  scale_colour_manual(values = c("darkgreen", "orange3", "purple4", "hotpink")) + 
  scale_x_continuous(labels = scales::percent) + 
  labs(x = NULL, y = NULL, col = "Year") + 
  theme_custom

ggsave("plots/demographics_graph.png", demographics_graph, width = 8, height = 4)

#### Maps ####

europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe")

df_map <- europe %>% 
  left_join(EUI_data_short %>%
              mutate(country = replace(country, country == "UK", "United Kingdom"))  %>% 
              filter(Year == 2025) %>% 
              group_by(country) %>%
              count(Ukraine_groups) %>% 
              mutate(n = n/sum(n)),
            by = c("name" = "country"))

map_labels <- df_map %>%
  filter(name %in% c(unique(EUI_data_short$country), "United Kingdom")) %>%
  st_point_on_surface() %>%
  mutate(
    coords = st_coordinates(geometry),
    label_text = paste0(abbrev, "\n", round(n * 100, 2), "%")
  )

blue_palette <- c(
  "#e0f3f8",  # very light blue
  "#abd9e9",
  "#74add1",
  "#4575b4",
  "#313695",
  "#1a2c6b",
  "#0d1a44"   # darkest blue
)

Groups_map <- df_map %>% 
  filter(!is.na(Ukraine_groups)) %>% 
  ggplot() +
  geom_sf(aes(fill = n), color = "white", linewidth = 0.25) +
  facet_wrap(~factor(Ukraine_groups,
                     levels = c("Security-focused",
                                "Conditional Ukraine Supporters",
                                "Domestic/ Distracted",
                                "Russia collaboration"))) +
  geom_label(
    data = map_labels,
    aes(x = coords[,1], y = coords[,2], label = label_text),
    size = 2,
    fontface = "bold",
    fill = "white",
    color = "black",
    label.size = 0
  ) +
  
  scale_fill_stepsn(
    colours = blue_palette,
    breaks = seq(0, 0.70, length.out = 7),
    labels = scales::percent,
    limits = c(0, 0.70), 
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
  
  coord_sf(
    xlim = c(-11,38),
    ylim = c(35,67),
    expand = FALSE
  ) +
  
  labs(fill = "Percentage Europeans in Each Group by Country in 2025") +
  
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.2,"cm"),
    panel.background = element_rect(fill = "#eef3f7", colour = NA),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
  )

ggsave("plots/groups_map.png", Groups_map, width = 8, height = 16)

#### Graph over time ####

Proportions_time_groups <- EUI_data_short %>% 
  filter(Year > 2021) %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups,
                                 levels = c("Security-focused",
                                            "Conditional Ukraine Supporters",
                                            "Domestic/ Distracted",
                                            "Russia collaboration"))) %>% 
  ggplot(aes(x = Year, fill = (Ukraine_groups))) +
  geom_bar(position = "fill") + #for frequency, change position to "dodge"
  labs(fill = "Relations to Russia:",
       x = "Year", y = "Proportion") + 
  guides(fill = guide_legend(
                             nrow = 2)) +
  scale_fill_manual(values = c("darkblue", "lightblue", "forestgreen", "darkred")) + 
  theme_custom

ggsave("plots/groups_time.png", Proportions_time_groups, width = 7, height = 5)


countries_order <- EUI_data_short %>% 
  filter(!is.na(country)) %>% 
  group_by(country) %>% 
  count(Ukraine_groups) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(Ukraine_groups == "Russia collaboration") %>% 
  arrange(-prop) %>% 
  pull(country)
  
Proportions_country <- EUI_data_short %>% 
  filter(Year > 2021) %>% 
  filter(!is.na(country)) %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups,
                                 levels = c("Security-focused",
                                            "Conditional Ukraine Supporters",
                                            "Domestic/ Distracted",
                                            "Russia collaboration")),
         country = factor(country, levels = countries_order)) %>% 
  ggplot(aes(x = country, fill = (Ukraine_groups))) +
  geom_bar(position = "fill") + #for frequency, change position to "dodge"
  labs(fill = "Relations to Russia:",
       x = "Country", y = "Proportion") + 
  guides(fill = guide_legend(
    nrow = 2)) +
  scale_fill_manual(values = c("darkblue", "lightblue", "forestgreen", "darkred")) + 
  theme_custom + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust= -0.01))

ggsave("plots/Proportions_country.png", Proportions_country, width = 7, height = 6)

Proportions_time_country_groups <- EUI_data_short %>% 
  filter(Year > 2021) %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups,
                                 levels = c("Security-focused",
                                            "Conditional Ukraine Supporters",
                                            "Domestic/ Distracted",
                                            "Russia collaboration")),
         country = factor(country, levels = countries_order) ) %>% 
  ggplot(aes(x = Year, fill = (Ukraine_groups))) +
  geom_bar(position = "fill") + #for frequency, change position to "dodge"
  labs(fill = "Relations to Russia:",
       x = "Year", y = "Proportion") + 
  facet_wrap(~country) + 
  guides(fill = guide_legend(
    nrow = 2)) +
  scale_fill_manual(values = c("darkblue", "lightblue", "forestgreen", "darkred")) + 
  theme_custom

ggsave("plots/time_country_groups.png", Proportions_time_country_groups, width = 8, height = 8)

#### Quartiles by Year ####

Security_focus_year_plot <- EUI_data_short %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(Year = factor(Year, levels = rev(c("2022", "2023", "2024", "2025")))) %>% 
  ggplot(aes(y = Year, x = Security_FA)) + 
  geom_violin(alpha = 0.6, fill = 'purple') +
  geom_boxplot(width = 0.1, fill = "grey89", outlier.shape = NA) + 
  labs(y = NULL, x = NULL) + 
  scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                      limits = c(-1.5, 1.5), 
                      labels = c("", "Highest Defence Focus (-1.5)",  "", "", "0", "", "", "Highest Normalization Focus (1.5)", "")) +
  theme_custom

ggsave("plots/Security_focus_year_plot.png", Security_focus_year_plot, width = 8, height = 5)

order_countries <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(Mean = median(Security_FA_01, na.rm = TRUE)) %>% 
  arrange(-Mean) %>% 
  pull(country)

Security_focus_country_plot <- EUI_data_short %>% 
  mutate(country = factor(country, levels = order_countries)) %>% 
  filter(!is.na(country)) %>% 
  ggplot(aes(y = country, x = Security_FA)) +
  #geom_violin(alpha = 0.6, fill = 'purple') +
  geom_boxplot(fill = "grey89") + 
  labs(y = NULL, x = NULL) + 
  scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                      limits = c(-1.5, 1.5), 
                      labels = c("", "Highest Defence Focus (-1.5)",  "", "", "- 0 -", "", "", "Highest Normalization Focus (1.5)", "")) +
  theme_custom

ggsave("plots/Security_focus_country_plot.png", Security_focus_country_plot, width = 8, height = 5)

#### Preferred War Outcome ####

preferred_war_outcome <- EUI_data_short %>% 
  group_by(EUI_Ukraine_Outcome) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>% 
  mutate(EUI_Ukraine_Outcome = case_match(EUI_Ukraine_Outcome,
                                          1 ~ "Russia achieves all territorial goals",
                                          2 ~ "Russia takes more territory",
                                          3 ~ "A return to the territorial \n situation before the war",
                                          4 ~ "Ukraine retakes some territory",
                                          5 ~ "Ukraine retakes all territory \n (+ Crimea)"),
         EUI_Ukraine_Outcome = factor(EUI_Ukraine_Outcome,
                                      levels = c("Russia achieves all territorial goals",
                                          "Russia takes more territory",
                                          "A return to the territorial \n situation before the war",
                                          "Ukraine retakes some territory",
                                          "Ukraine retakes all territory \n (+ Crimea)"))) %>% 
  filter(!is.na(EUI_Ukraine_Outcome)) %>% 
  ggplot(aes(x = Mean, y = EUI_Ukraine_Outcome, xmin = conf.low, xmax = conf.high)) + 
  geom_col(fill = "darkblue") +
  geom_linerange(linewidth = 1) + 
  # facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_x_continuous( breaks = seq(-0.7, 1, length.out = 9),
                      limits = c(-0.7, 1), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) +
  labs(x = "Average Security-Collaboration Index Score", colour = "Group",
       y = "Preferred Outcome to the War") + 
  theme_custom

ggsave("plots/preferred_outcome.png", preferred_war_outcome, width = 9, height = 6)

#### Trust US ####

trust_graph <- EUI_data_short %>% 
  mutate(`The US` = ifelse(A5_1 >= 5, 1, 0),
         Russia = ifelse(A5_2 >= 5, 1, 0),
         Ukraine = ifelse(A5_3 >= 5, 1, 0),
         China = ifelse(A5_4 >=5, 1, 0)
  ) %>% 
pivot_longer(cols = c(`The US`, Russia, Ukraine, China),
             names_to = "Country_trust", values_to = "Trust") %>% 
  group_by(Trust, Country_trust) %>% 
  summarize(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)
  ) %>% 
  filter(Trust == 1) %>% 
  ggplot(aes(x = Mean, xmin = conf.low,
             xmax = conf.high, y = Country_trust)) + 
   geom_col(fill = "darkblue") +
  geom_linerange(linewidth = 1) + 
  geom_vline(xintercept = 0, col = "grey30") + 
  scale_x_continuous( breaks = seq(-0.8, 0.4, length.out = 9),
                      limits = c(-0.8, 0.4), 
                      labels = c("", "Highest Collaboration Focus",  "", "", "- Relations with Russia -", "", "", "Highest Security Focus", "")) +
  labs(x = "Average Security-Collaboration Score", y = "Trust in ",
       colour = "Group",
       caption = "Values of 5 or greater on a 0 to 10 scale are considered trusting.") + 
  theme_custom

ggsave("plots/trust_graph_dot.png", trust_graph, width = 8, height = 4)

#### Threat Graph ####

threat_graph <- EUI_data_short %>% 
  group_by(Q68, Year) %>% 
  summarize(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)
  ) %>% 
  mutate(Q68 = case_match(Q68,
                          1 ~ "Terrorism",
                          2 ~ "Russia",
                          3 ~ "The US",
                          4 ~ "China",
                          5 ~ "Nuclear Proliferation",
                          6 ~ "Other",
                          7 ~ "Don't Know"
  ),
  Q68 = factor(Q68, levels = rev(c("Terrorism",
              "Russia",
               "The US",
               "China",
              "Nuclear Proliferation",
                "Other",
               "Don't Know")))) %>% 
  filter(!is.na(Q68)) %>% 
  ggplot(aes(x = Mean, xmin = conf.low, xmax = conf.high, y = Q68)) +
  geom_col(fill = "darkblue") +
  geom_linerange( linewidth = 1) + 
  facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(colour = guide_legend(reverse = TRUE,
                              nrow = 2)) + 
  scale_x_continuous( breaks = seq(-0.7, 0.9, length.out = 9),
                      limits = c(-0.7, 0.9), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) +
  labs(x = "Average Security-Collaboration Index Score", y = "Greatest Threat",
       colour = "Group") + 
  theme_custom

ggsave("plots/threat_graph.png", threat_graph, width = 14, height = 8)

## Bar Grpah 
threat_graph <- EUI_data_short %>% 
  group_by(Ukraine_groups, Year) %>% 
  count(Q68) %>% 
  mutate(Q68 = case_match(Q68,
                          1 ~ "Terrorism",
                          2 ~ "Russia",
                          3 ~ "The US",
                          4 ~ "China",
                          5 ~ "Nuclear Proliferation",
                          6 ~ "Other",
                          7 ~ "Don't Know"
  ),
  Ukraine_groups = factor(Ukraine_groups,
                          levels = rev(c("Security-focused",
                                         "Conditional Ukraine Supporters",
                                         "Domestic/ Distracted",
                                         "Russia collaboration")))) %>% 
  filter(!is.na(Q68)) %>% 
  mutate(prop = n/ sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  ggplot(aes(x = prop, fill = Ukraine_groups, xmin = lower, xmax = upper, y = Q68)) +
  geom_col(position = "fill") +
  # geom_linerange(position = position_dodge(width = 0.6), linewidth = 1) + 
  facet_wrap(~Year) + 
  # geom_hline(yintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), col = "black") + 
  scale_fill_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(fill = guide_legend(reverse = TRUE,
                             nrow = 2)) + 
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Percentage in Each Group", y = "Greatest Threat",
       fill = "Group") + 
  theme_custom

ggsave("plots/threat_graph.png", threat_graph, width = 8, height = 6)


#### Responsibility for the Conflict ####

responsibility_graph <- EUI_data_short %>% 
  group_by(New_Q79) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>% 
  mutate(New_Q79 = case_match(New_Q79,
                              1 ~ "Entirely NATO",
                              2 ~ "More NATO Than Russia",
                              3 ~ "NATO and Russia Equally",
                              4 ~ "More Russia than NATO",
                              5 ~ "Entirely Russia",
                              6 ~ "Don't Know"
  ),
  New_Q79 = factor(New_Q79, levels = rev(c("Entirely NATO",
                   "More NATO Than Russia",
                    "NATO and Russia Equally",
                   "More Russia than NATO",
                  "Entirely Russia",
                  "Don't Know")))) %>% 
  filter(!is.na(New_Q79)) %>% 
  ggplot(aes(x = Mean, xmin = conf.low, xmax = conf.high, y = New_Q79)) +
  geom_col(fill = "darkblue") +
  geom_linerange( linewidth = 1) + 
 # facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(colour = guide_legend(reverse = TRUE,
                              nrow = 2)) + 
  scale_x_continuous( breaks = seq(-0.85, 0.85, length.out = 9),
                      limits = c(-0.85, 0.85), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) + 
  labs(x = "Average Security-Collaboration Index Score", y = "Responsibilty for the Conflict",
       colour = "Group") + 
  theme_custom

ggsave("plots/responsibility_graph.png", responsibility_graph, width = 8, height = 6)

#### Deal with the violations ####

human_rights <- EUI_data_short %>% 
  group_by(Q67_revisions) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>% 
  mutate(Q67_revisions = case_match(Q67_revisions,
                              1 ~ "No Interference",
                              2 ~ "Financial Sanctions",
                              3 ~ "Remove Voting Privileges",
                              4 ~ "Other",
                              5 ~ "Don't Know"
  ),
  Q67_revisions = factor(Q67_revisions, levels = rev(c("No Interference",
                                           "Financial Sanctions",
                                           "Remove Voting Privileges",
                                           "Other", "Don't Know")))) %>% 
  filter(!is.na(Q67_revisions)) %>% 
  ggplot(aes(x = Mean, xmin = conf.low, xmax = conf.high, y = Q67_revisions)) +
  geom_col(fill = "darkblue") +
  geom_linerange( linewidth = 1) + 
  # facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(colour = guide_legend(reverse = TRUE,
                               nrow = 2)) + 
  scale_x_continuous( breaks = seq(-0.85, 0.85, length.out = 9),
                      limits = c(-0.85, 0.85), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) + 
  labs(x = "Average Security-Collaboration Index Score", y = "EU response to human rights violations",
       colour = "Group") + 
  theme_custom

ggsave("plots/human_rights.png", human_rights, width = 8, height = 6)


#### General Trust #####


generalized_trust <- EUI_data_short %>% 
  group_by(Q59) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>% 
  mutate(Q59 = case_match(Q59,
                                    1 ~ "Most People can be trusted",
                                    2 ~ "You can't be too careful",
                                    3 ~ "Don't Know"
  ),
  Q59 = factor(Q59, levels = rev(c( "Most People can be trusted",
                                                         "You can't be too careful",
                                                        "Don't Know")))) %>% 
  filter(!is.na(Q59)) %>% 
  ggplot(aes(x = Mean, xmin = conf.low, xmax = conf.high, y = Q59)) +
  geom_col(fill = "darkblue") +
  geom_linerange( linewidth = 1) + 
  # facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(colour = guide_legend(reverse = TRUE,
                               nrow = 2)) + 
  scale_x_continuous( breaks = seq(-0.85, 0.85, length.out = 9),
                      limits = c(-0.85, 0.85), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) + 
  labs(x = "Average Security-Collaboration Index Score", y = "Generalized Trust",
       colour = "Group") + 
  theme_custom

ggsave("plots/generalized_trust.png", generalized_trust, width = 8, height = 6)

#### Worry about Energy Supply ####

energy_worry <- EUI_data_short %>% 
  group_by(Q7_12, country) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>% 
  mutate(Q7_12 = case_match(Q7_12,
                          0 ~ "Not Worried",
                          1 ~ "Worried"
  ),
  Q7_12 = factor(Q7_12, levels = rev(c( "Not Worried", 
                                    "Worried")))) %>% 
  filter(!is.na(Q7_12)) %>% 
  ggplot(aes(x = Mean, xmin = conf.low, xmax = conf.high, y = Q7_12)) +
  geom_col(fill = "darkblue") +
  geom_linerange( linewidth = 1) + 
  facet_wrap(~ country) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(colour = guide_legend(reverse = TRUE,
                               nrow = 2)) + 
  scale_x_continuous( breaks = seq(-0.85, 0.85, length.out = 9),
                      limits = c(-0.85, 0.85), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) + 
  labs(x = "Average Security-Collaboration Index Score", y = "Worry about Energy Supply",
       colour = "Group") + 
  theme_custom

ggsave("plots/energy_worry.png", energy_worry, width = 8, height = 6)


#### Worried about Inflation #####

inflation_worry <- EUI_data_short %>% 
  group_by(Q7_2) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>% 
  mutate(Q7_2 = case_match(Q7_2,
                            0 ~ "Not Worried",
                            1 ~ "Worried"
  ),
  Q7_2 = factor(Q7_2, levels = rev(c( "Not Worried", 
                                        "Worried")))) %>% 
  filter(!is.na(Q7_2)) %>% 
  ggplot(aes(x = Mean, xmin = conf.low, xmax = conf.high, y = Q7_2)) +
  geom_col(fill = "darkblue") +
  geom_linerange( linewidth = 1) + 
  # facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(colour = guide_legend(reverse = TRUE,
                               nrow = 2)) + 
  scale_x_continuous( breaks = seq(-0.85, 0.85, length.out = 9),
                      limits = c(-0.85, 0.85), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) + 
  labs(x = "Average Security-Collaboration Index Score", y = "Worry about Inflation",
       colour = "Group") + 
  theme_custom

ggsave("plots/inflation_worry.png", inflation_worry, width = 8, height = 6)

#### Correlates of Groups ####



#### ideology non-linear ####

ideology_groups_nonlinear <- lm_robust(reformulate(c("as.factor(Q62)", CONTROLS, "Year", "country"),
                                                   response = "Security_FA"), data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 

preds_ideology_groups_nonlinear <- predictions(ideology_groups_nonlinear,
                                               by = c("Q62")) %>% 
  mutate(country = "Pooled") %>% 
  as.data.frame()


ideology_groups_country_nonlinear <- lm_robust(reformulate(c("as.factor(Q62) * country", CONTROLS, "Year"),
                                                   response = "Security_FA"), data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 

preds_ideology_country_nonlinear <- predictions(ideology_groups_nonlinear,
                                               by = c("Q62", "country")) %>% 
  as.data.frame()


non_linear_ideo <- preds_ideology_groups_nonlinear %>% 
  ggplot(aes(x = Q62, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  theme_custom +
  labs(x = "Ideological Self-Placement",
       y = "Predicted Russian Collaboration Score") + 
scale_x_continuous( breaks = seq(1, 7, length.out = 7),
                                         labels = c("", "Most Left-wing Position",  "",  "", "", "Most Right-wing Position", ""))

ggsave("plots/non_linear_ideo.png", non_linear_ideo, width = 8, height = 5)

non_linear_ideo_country <- preds_ideology_country_nonlinear %>% 
  ggplot(aes(x = Q62, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  facet_wrap(~country) + 
  geom_point() + 
  geom_linerange() + 
  theme_custom +
  labs(x = "Ideological Self-Placement",
       y = "Predicted Russian Collaboration Score") + 
  scale_x_continuous( breaks = seq(1, 7, length.out = 7),
                      labels = c("", "Far-Left",  "",  "", "", "Far-Right", ""))

ggsave("plots/non_linear_ideo_country.png", non_linear_ideo_country, height = 8, width = 8)

ideology_groups <- lm_robust(reformulate(c("Ukraine_groups * Year", CONTROLS, "country"), response = "Q62"), data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 


preds_ideology_groups <- predictions(ideology_groups, by = c("Ukraine_groups", "Year")) %>% 
  mutate(country = "Pooled")

ideology_groups_country <- lm_robust(reformulate(c("Ukraine_groups * Year * country", CONTROLS), response = "Q62"), data = EUI_data_short) 

preds_ideology_groups_country <- predictions(ideology_groups_country, by = c("Ukraine_groups", "Year", "country"))

preds_ideology_groups <- rbind(preds_ideology_groups, preds_ideology_groups_country)

order_countries <- EUI_data_short %>%
  filter(Year == 2025) %>% 
  group_by(country) %>%
  count(Ukraine_groups) %>% 
  mutate(n = n/sum(n)) %>% 
  filter(Ukraine_groups == "Russia collaboration") %>% 
  arrange(-n) %>% 
  pull(country)
         
ideology_graph <- preds_ideology_groups %>% 
  mutate(Year = factor(Year, levels = rev(c("2022", "2023", "2024", "2025"))),
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = c("Pooled", order_countries))
         ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Year, col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 1) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  facet_wrap(~country, nrow = 3) +
  #geom_hline(yintercept = c(1.5, 2.5, 3.5, 4.5, 5.5), col = "black") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  labs(x = "Predicted Ideology \n (1 is the most left-wing and 7 is the most right-wing)",
       y = "Year", col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education.") +
  theme_custom 

ggsave("plots/ideology_plot.png", ideology_graph, width = 12, height = 10)

Ideology_plot_denisty <- EUI_data_short %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups, levels = rev(c("Security-focused", "Conditional Ukraine Supporters",
                                                                "Domestic/ Distracted", "Russia collaboration")))) %>% 
  ggplot(aes(x = Q62)) + 
  geom_density(alpha = 0.8, fill = "orange") + 
  theme_custom + 
  scale_x_continuous(labels = seq(1, 7, 1),
                     breaks = seq(1, 7, 1)) +
  guides(fill = guide_legend(ncol = 2)) + 
  scale_fill_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  labs(x = "Self-Reported Ideology \n (1 indicates the most left-wing position and 7 indiciates the most right-wing position)",
       y = NULL,
       fill = "Relations to Russia:")

ggsave("plots/Ideology_plot_denisty.png", Ideology_plot_denisty, width = 6, height = 4)

Ideology_plot_stacked <- EUI_data_short %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups, levels = rev(c("Security-focused", "Conditional Ukraine Supporters",
                                                          "Domestic/ Distracted", "Russia collaboration")))) %>% 
  ggplot(aes(x = Q62, fill = Ukraine_groups)) + 
  geom_bar(position = "fill") + 
  theme_custom + 
  scale_x_continuous(labels = seq(1, 7, 1),
                     breaks = seq(1, 7, 1)) +
  scale_y_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(ncol = 2)) + 
  scale_fill_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  labs(x = "Self-Reported Ideology \n (1 indicates the most left-wing position and 7 indiciates the most right-wing position)",
       y = NULL,
       fill = "Relations to Russia:")

ggsave("plots/Ideology_plot_stacked.png", Ideology_plot_stacked, width = 6, height = 4)

ideology_graph_bar <- preds_ideology_groups %>% 
  mutate(Year = factor(Year, levels = c("2022", "2023", "2024", "2025")),
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = c("Pooled", order_countries))
  ) %>% 
  ggplot(aes(y = estimate, ymin = conf.low, ymax = conf.high, x = as.factor(Year), fill = Ukraine_groups)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_errorbar(position = position_dodge(width = 1), linewidth = 0.5) + 
  facet_wrap(~country) +
  #geom_hline(yintercept = c(1.5, 2.5, 3.5, 4.5, 5.5), col = "black") + 
  scale_fill_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  labs(y = "Predicted Ideology \n (1 is the most left-wing and 7 is the most right-wing)",
       x = "Year", fill = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education.") +
  theme_custom 

ggsave("plots/ideology_plot_bar.png", ideology_graph_bar, width = 12, height = 10)

#### EU Referendum ####

referendum_groups <- lm_robust(reformulate(c("Ukraine_groups", "Year", CONTROLS , "country"), response = "Q9"), data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 

preds_referendum_groups <- predictions(referendum_groups, by = c("Ukraine_groups")) %>% 
  mutate(country = "Pooled")

EU_Referendum_average <- EUI_data_short %>% 
  group_by(Q9) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>%
  filter(!is.na(Q9)) %>% 
  mutate(Support = case_match(Q9, 0 ~ "No",
                              1 ~ "Yes"),
         Variable = "Support for EU \n Membership") %>% 
  select(-Q9)

referendum_groups_country <- lm_robust(reformulate(c("Ukraine_groups * country", "Year", CONTROLS), response = "Q9"), data = EUI_data_short) 

preds_referendum_groups_country <- predictions(referendum_groups_country, by = c("Ukraine_groups", "country"))

preds_referendum_groups <- rbind(preds_referendum_groups, preds_referendum_groups_country) %>% 
  as.data.frame()

referendum_groups_plot <- preds_referendum_groups %>% 
  mutate(
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = rev(c("Pooled", order_countries)))
  ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = country, col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Predicted percentage of respondents who would vote to remain in the EU",
       y = NULL, col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education.") +
  theme_custom 

ggsave("plots/referendum_groups_plot.png", referendum_groups_plot, width = 10, height = 10)


#### EU Trust ####

EU_trust_groups <- lm_robust(reformulate(c("Ukraine_groups", "as.factor(Year)", CONTROLS, "country"), response = "New_Q43"), data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 

EU_Trust_average <- EUI_data_short %>% 
  group_by(New_Q43) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>%
  filter(!is.na(New_Q43)) %>% 
  mutate(Support = case_match(New_Q43, 0 ~ "No",
                              1 ~ "Yes"),
         Variable = "EU Trust") %>% 
  select(-New_Q43)

preds_EU_trust_groups <- predictions(EU_trust_groups, by = c("Ukraine_groups")) %>% 
  mutate(country = "Pooled")

EU_trust_groups_country <- lm_robust(reformulate(c("Ukraine_groups * country","as.factor(Year)", CONTROLS), response = "New_Q43"), data = EUI_data_short) 

preds_EU_trust_groups_country <- predictions(EU_trust_groups_country, by = c("Ukraine_groups", "country"))

preds_EU_trust_groups <- rbind(preds_EU_trust_groups, preds_EU_trust_groups_country)

EU_trust_plot <- preds_EU_trust_groups %>% 
  mutate(
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = rev(c("Pooled", order_countries)))
  ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = country, col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  #facet_wrap(~country) +
  geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  scale_x_continuous(labels = scales::percent,
                     limits = c(0, 1.1)) + 
  labs(x = "Predicted percentage of respondents who trust the EU",
       y = "Year", col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education.") +
  theme_custom 

ggsave("plots/EU_trust_plot.png", EU_trust_plot, width = 11, height = 8)


#### EU TRUST BAR

EUtrust_plot_stacked <- EUI_data_short %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups, levels = rev(c("Security-focused", "Conditional Ukraine Supporters",
                                                                "Domestic/ Distracted", "Russia collaboration"))),
         New_Q43i = case_match(New_Q43i, 1 ~ 1, 2 ~ 2, 2.5 ~ 3, 3 ~ 4, 4 ~ 5) ) %>% 
  ggplot(aes(x = New_Q43i, fill = Ukraine_groups)) + 
  geom_bar(position = "fill") + 
  theme_custom + 
  scale_x_continuous(labels = c("Do Not Trust at all", "", "Neutral", "", "Trust a Great Deal"),
                     breaks = c(1, 2, 3, 4, 5)) +
  guides(fill = guide_legend(ncol = 2)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  labs(x = "EU Trust",
       y = NULL,
       fill = "Relations to Russia:")

ggsave("plots/EUtrust_plot_stacked.png", EUtrust_plot_stacked, width = 6, height = 4)


#### EU Trust Density

Trust_Denisty <- EUI_data_short %>% 
  mutate(Ukraine_groups = factor(Ukraine_groups, levels = rev(c("Security-focused", "Conditional Ukraine Supporters",
                                                                "Domestic/ Distracted", "Russia collaboration"))),
         New_Q43i = case_match(New_Q43i, 1 ~ 1, 2 ~ 2, 2.5 ~ 3, 3 ~ 4, 4 ~ 5) ) %>% 
  ggplot(aes(x = New_Q43i)) + 
  geom_density(alpha = 0.6, fill = "purple3") + 
  theme_custom + 
  scale_x_continuous(labels = c("Do Not Trust at all", "", "Neutral", "", "Trust a \n Great Deal"),
                     breaks = c(1, 2, 3, 4, 5)) +
  guides(fill = guide_legend(ncol = 2)) + 
  scale_fill_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  labs(x = "EU Trust",
       y = NULL,
       fill = "Relations to Russia:")

ggsave("plots/Trust_Denisty.png", Trust_Denisty, width = 7, height = 4)

#### NATO Support ####
unique(EUI_data_short$Q71)
NATO_support_groups <- lm_robust(reformulate(c("Ukraine_groups", CONTROLS, "Year", "country"), response = "Q71"), data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 

NATO_support_average <- EUI_data_short %>% 
  group_by(Q71) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)) %>%
  filter(!is.na(Q71)) %>% 
  mutate(Support = case_match(Q71, 0 ~ "No",
                              1 ~ "Yes"),
         Variable = "Support for NATO \n Membership") %>% 
  select(-Q71)
  
preds_NATO_support_groups <- predictions(NATO_support_groups, by = c("Ukraine_groups")) %>% 
  mutate(country = "Pooled")

NATO_support_groups_country <- lm_robust(reformulate(c("Ukraine_groups * country", "Year", CONTROLS), response = "Q71"), data = EUI_data_short) 

preds_NATO_support_groups_country <- predictions(NATO_support_groups_country, by = c("Ukraine_groups", "country"))

preds_NATO_support_groups <- rbind(preds_NATO_support_groups, preds_NATO_support_groups_country)

NATO_support_plot <- preds_NATO_support_groups %>% 
  mutate(
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = rev(c("Pooled", order_countries)))
  ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = country, col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Predicted percentage of respondents who think NATO is important",
       y = NULL, col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education.") +
  theme_custom 

ggsave("plots/NATO_support_plot.png", NATO_support_plot, width = 11, height = 8)


#### Pooled Graph ####

Pooled_model_exp3 <- rbind(preds_referendum_groups %>% mutate(Model = "EU Referendum"),
      preds_NATO_support_groups %>% mutate(Model = "NATO Support"),
      preds_EU_trust_groups %>% mutate(Model = "EU Trust"))  %>% 
  filter(country == "Pooled") %>% 
  mutate(
    Ukraine_groups = factor(Ukraine_groups,
                            levels = rev(c("Security-focused",
                                           "Conditional Ukraine Supporters",
                                           "Domestic/ Distracted",
                                           "Russia collaboration")))
  ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Model, col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Predicted percentage of respondents who ...",
       y = NULL, col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education.") +
  theme_custom 

ggsave("plots/Pooled_model_exp3.png",Pooled_model_exp3, width = 8, height = 4)


NATO_EU_df <- rbind(EU_Referendum_average, NATO_support_average, EU_Trust_average)

pooled_vertical_extension <- NATO_EU_df %>% 
  mutate(Support = factor(Support, levels = rev(c("No", "Yes")))) %>% 
  ggplot(aes(x = Mean, y = Variable, xmin = conf.low, xmax = conf.high, fill = Support)) + 
  geom_col(position = position_dodge(width = 0.9)) +
  geom_linerange(position = position_dodge(width = 0.9), linewidth = 1) + 
  # facet_wrap(~Year) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_fill_manual(values = c("darkred", "darkblue") ) + 
  scale_x_continuous( breaks = seq(-0.6, 0.7, length.out = 9),
                      limits = c(-0.6, 0.7), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) +
  labs(x = "Average Security-Collaboration Index Score", fill = NULL,
       y = NULL) + 
  theme_custom

ggsave("plots/pooled_vertical_extension.png", pooled_vertical_extension, width = 8, height = 4)
#### Polarization ####

affective_pol_groups <- lm_robust(reformulate(c("Ukraine_groups * Year", CONTROLS , "country"), response = "Affective_Polarization"), 
                                   data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 


preds_affective_pol_groups <- predictions(affective_pol_groups, by = c("Ukraine_groups", "Year")) %>% 
  mutate(country = "Pooled")

affective_pol_groups_country <- lm_robust(reformulate(c("Ukraine_groups * Year * country", CONTROLS), response = "Affective_Polarization"), data = EUI_data_short) 

preds_affective_pol_groups_country <- predictions(affective_pol_groups_country, by = c("Ukraine_groups", "Year", "country"))

preds_affective_pol_groups <- rbind(preds_affective_pol_groups, preds_affective_pol_groups_country)

affective_pol_plot <- preds_affective_pol_groups %>% 
  mutate(Year = factor(Year, levels = rev(c("2022", "2023", "2024", "2025"))),
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = c("Pooled", order_countries))
  ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = as.factor(Year), col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 1) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  facet_wrap(~country) +
  #geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  labs(x = "Predicted difference in the ranking of respodents' most and least liked parties",
       y = "Year", col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education. \n \n Pooled Model Includes Fixed Effects for Country.") +
  theme_custom 

ggsave("plots/affective_pol_plot.png", affective_pol_plot, width = 11, height = 10)

#### Support Democracy ####

table(EUI_data_short$Support_Democracy, EUI_data_short$Year)


support_democracy_groups1 <- lm_robust(reformulate(c("Security_FA", CONTROLS, "country"), response = "Support_Democracy"), 
                                       data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 




preds_support_democracy_groups1 <- predictions(support_democracy_groups1, by = c("Security_FA")) %>% 
  mutate(Outcome = "Opposition to Executive Aggrandizement") %>% 
  as.data.frame()

support_democracy_groups_country <- lm_robust(reformulate(c("Ukraine_groups * country", CONTROLS), response = "Support_Democracy"), data = EUI_data_short) 

preds_support_democracy_groups_country1 <- predictions(support_democracy_groups_country, by = c("Ukraine_groups", "country"))

# preds_support_democracy_groups1 <- rbind(preds_support_democracy_groups1, preds_support_democracy_groups_country1) %>% 
#   as.data.frame()

support_democracy_plot <- preds_support_democracy_groups1 %>% 
 # filter(country == "Pooled") %>% 
  ggplot(aes(x = Security_FA, ymin = conf.low, ymax = conf.high, y = estimate)) +
  geom_line(colour = "darkblue", linewidth = 1) +
  geom_ribbon(alpha = 0.3, fill = "lightblue") + 
 # geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                      limits = c(-1.5, 1.5), 
                      labels = c("", "Highest Collaboration Focus",  "", "", "- Relations with Russia -", "", "", "Highest Security Focus", "")) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  labs(y = "Predicted level of non-support for executive agrandizement",
       x = "Collaboration-Securtiy Dimension", col = "Group",
       caption = "Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education and include fixed effects for year and country.") +
  theme_custom 

ggsave("plots/support_democracy_plot.png", support_democracy_plot, width = 11, height = 6)

#### Support Democracy ####


support_democracy_groups <- lm_robust(reformulate(c("Security_FA", "Year", CONTROLS, "country"), response = "Q5"), 
                                      data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 

preds_support_democracy_groups <- avg_predictions(support_democracy_groups, by = c("Security_FA")) %>% 
  mutate(Outcome = "Support for Democracy") %>% 
  as.data.frame()

support_democracy_groups_country <- lm_robust(reformulate(c("Ukraine_groups * country", "Year", CONTROLS), response = "Q9"), data = EUI_data_short) 

preds_support_democracy_groups_country <- predictions(support_democracy_groups_country, by = c("Ukraine_groups", "country"))

preds_support_democracy_groups <- rbind(preds_support_democracy_groups, preds_support_democracy_groups_country)

democracy_plot <- preds_support_democracy_groups %>% 
  mutate(
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = rev(c("Security-focused",
                                                "Conditional Ukraine Supporters",
                                                "Domestic/ Distracted",
                                                "Russia collaboration"))),
         country = factor(country, levels = rev(c("Pooled", order_countries)))
  ) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = country, col = Ukraine_groups)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 0.5) + 
  # geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 2)) +
  labs(x = "Predicted level of support for democracy",
       y = NULL, col = "Group",
       caption = "Countries ordered by proportion of Russian collaborators largest to smallest. \n \n Pooled model only includes those from the countries surveyed since 2022. \n \n Models control for gender and education. \n \n Pooled model includes country fixed effects.") +
  theme_custom 

ggsave("plots/democracy_plot.png", democracy_plot, width = 11, height = 8)

#### Pooled Democracy ####

democracy_pooled_plot <- rbind(preds_support_democracy_groups,
preds_support_democracy_groups1
) %>% 
  filter(Security_FA > 1.2 | Security_FA < -1.3) %>% 
  mutate(Security_Focus = case_when(Security_FA > 1.2 ~ "Highest Collaboration Focus",
                                    Security_FA < -1.3 ~ "Highest Security Focus")) %>% 
  ggplot(aes(fill = Security_Focus, xmin = conf.low, xmax = conf.high, x = estimate, y = Outcome)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_linerange(linewidth = 1, position = position_dodge(width = 0.9)) + 
  #facet_wrap(~Model, scales = "free_x") + 
  # geom_vline(xintercept = 0.5, lty = 4, col = "grey40") + 
  scale_fill_manual(values = c("darkred", "darkblue")) + 
  guides(fill = guide_legend(reverse = TRUE,
                               nrow = 2)) +
  labs(x = "Predicted level of support for ...",
       y = NULL, fill = "Security-Collaboration Dimension",
       caption = "Only includes those from the countries surveyed since 2022. \n \n Models control for gender and education. \n \n Model includes country and year fixed effects.") +
  theme_custom 

ggsave("plots/democracy_pooled_plot.png", democracy_pooled_plot, width = 9, height = 4)
EUI_data_short$Support_Democracy

support_EA <- EUI_data_short %>% 
  mutate(No_support_EA = ifelse(Support_Democracy > 2.4, "Does Not Support", "Supports")) %>% 
  group_by(No_support_EA) %>% 
  summarize(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)
  ) %>% 
  filter(No_support_EA %in% c("Does Not Support", "Supports"))  %>% 
  mutate(Variable = "Support for Executive Aggrandizement") %>% 
  rename(Value = No_support_EA)

Support_democracy <- EUI_data_short %>% 
  mutate(Democracy = ifelse(Q5 > 5, "Supports", "Does Not Support")) %>% 
  group_by(Democracy) %>% 
  summarize(Mean = mean(Security_FA, na.rm = TRUE),
            SE = sd(Security_FA, na.rm = TRUE) / sqrt(n()), 
            conf.low = Mean - (1.96 * SE),
            conf.high = Mean + (1.96 * SE)
  ) %>% 
  filter(Democracy %in% c("Supports", "Does Not Support"))  %>% 
  mutate(Variable = "Support for Democracy") %>% 
  rename(Value = Democracy)

democracy_df <- rbind(support_EA, Support_democracy)


democracy_plot_alternative <- democracy_df %>% 
  mutate(Value = factor(Value, levels = c(c("Supports", "Does Not Support")))) %>% 
  ggplot(aes(x = Mean, y = Variable, xmin = conf.low, xmax = conf.high, fill = Value)) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  geom_linerange(position = position_dodge(width = 0.9), linewidth = 1) + 
  scale_fill_manual(values = c("darkgreen", "orange")) + 
  geom_vline(xintercept = 0, col = "black") + 
  scale_x_continuous( breaks = seq(-0.2, 0.6, length.out = 9),
                      limits = c(-0.2, 0.6), 
                      labels = c("", "Highest Security Focus",  "", "", "- Relations with Russia -", "", "", "Highest Collaboration Focus", "")) +
  labs(x = "Average Security-Collaboration Index Score", fill = NULL,
       y = NULL) +
  theme_custom
  
ggsave("plots/democracy_plot_alternative.png",democracy_plot_alternative, width = 9, height = 4)



