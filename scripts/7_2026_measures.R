
##### Examining 2026 Questions ####

France_2026 <- read_sav("data_raw/EUI European Solidarity Wave 8 (June 2026)/P_EUI_EuropeanSolidarity_2026_Master ~France (w).sav") %>% 
  mutate(Country = "France")
Germany_2026 <- read_sav("data_raw/EUI European Solidarity Wave 8 (June 2026)/P_EUI_EuropeanSolidarity_2026_Master ~Germany (w).sav") %>% 
  mutate(Country = "Germany")
Spain_2026 <- read_sav("data_raw/EUI European Solidarity Wave 8 (June 2026)/P_EUI_EuropeanSolidarity_2026_Master ~Spain (w).sav") %>% 
  mutate(Country = "Spain")
UK_2026 <- read_sav("data_raw/EUI European Solidarity Wave 8 (June 2026)/P_EUI_EuropeanSolidarity_2026_Master ~UK (w).sav") %>% 
  mutate(Country = "UK")


EUI_2026 <- bind_rows(France_2026, Germany_2026,
                      Spain_2026, UK_2026)

EUI_2026 <- EUI_2026 %>% 
  filter(AttentionCheck2_23 == 4) %>% 
  mutate(Age = recode_values(age_grp_all,
                             1 ~ "18-24",
                             2 ~ "25-34",
                             3 ~ "35-44",
                             4 ~ "45-54",
                             5 ~ "55+"),
         Woman = ifelse(gender_all == 2, 1, 0),
         Education = recode_values(edu_group, 1 ~ "Less than primary",
                                   2 ~ "Secondary Education",
                                   3 ~ "Tertiary" ),
         Urban = ifelse(Glob_areatype %in% c(1, 2), "Urban/Suburban", "Rural" ))


#### Create the index ####

group_vars <- EUI_2026 %>% 
  select(Q73, New_Q78_4, New_Q78_5)

group_vars  <- group_vars %>% 
  mutate(
    Q73_security = ifelse(Q73 == 1, 1, 0),
    New_Q78_4_security = ifelse(New_Q78_4 %in% c(1, 2), 1, 0),
    New_Q78_5_security = ifelse(New_Q78_5 %in% c(1, 2), 1, 0)
  )

group_vars <- group_vars %>% 
  select(-c(Q73, New_Q78_4, New_Q78_5))


CORS <- tetrachoric(group_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 3 factors with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1, cor = "tet"); Factor_loadings$loading


alpha(group_vars)

EUI_2026$Security_FA <- Factor_loadings$scores
EUI_2026$Security_FA <- as.numeric(EUI_2026$Security_FA) * -1


Russia_ally_plot <- EUI_2026 %>% 
  group_by(Country) %>% 
  count(Russia_allyrival) %>% 
  filter(!is.na(Russia_allyrival)) %>% 
  mutate(prop = n/sum(n),
         Russia_allyrival = recode_values(Russia_allyrival, 1 ~ "An Ally",
                                          2 ~ "A Necessary Partner",
                                          3 ~ "A Rival",
                                          4 ~ "An Adversary", 
                                          5 ~ "Don't Know"),
         Russia_allyrival = factor(Russia_allyrival, levels = rev(c("An Ally",
                                                                "A Necessary Partner",
                                                                 "A Rival",
                                                                 "An Adversary", 
                                                                 "Don't Know")))) %>% 
  ggplot(aes(y = Russia_allyrival, x = prop, fill = Country)) + 
  geom_col(position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept = 0.5, col = "grey50", lty = 4) + 
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) + 
  scale_fill_manual(values = c("darkblue", "darkgreen", "orange3", "darkred")) + 
  labs(x = "Proportion of Respondents", y = "Russia is ...") +
  guides(fill = guide_legend(reverse = TRUE,
                               ncol = 2)) +
  theme_custom

ggsave("plots/Russia_ally_plot.png", Russia_ally_plot, width = 8, height = 4)

Russia_ally_mod <- lmer(reformulate(c("as.factor(Russia_allyrival)", CONTROLS, "(1 | Country)"), response = "Security_FA"),
     data = EUI_2026)

Russia_ally_mod_df <- avg_predictions(Russia_ally_mod, variables = "Russia_allyrival")

Russia_ally_regression <- Russia_ally_mod_df %>% 
  as.data.frame() %>% 
  mutate(Russia_allyrival = recode_values(as.character(Russia_allyrival), "1" ~ "An Ally",
                                          "2" ~ "A Necessary Partner",
                                          "3" ~ "A Rival",
                                          "4" ~ "An Adversary", 
                                          "5" ~ "Don't Know"),
         Russia_allyrival = factor(Russia_allyrival, levels = rev(c("An Ally",
                                                                    "A Necessary Partner",
                                                                    "A Rival",
                                                                    "An Adversary", 
                                                                    "Don't Know")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Russia_allyrival)) + 
  geom_point() + 
  geom_linerange() + 
  scale_x_continuous(breaks = seq(-0.5, 0.8, 0.2)) + 
  labs(x = "Predicted Placement on the Defence-Normalization Scale \n Higher numbers indicate more normalization focused",
       y = "Russia is ...") +
  theme_custom

ggsave("plots/Russia_ally_regression.png", Russia_ally_regression, width = 8, height = 4)

Russia_perspecitve_plot <- EUI_2026 %>% 
  group_by(Country) %>% 
  count(Russia1_2026) %>% 
  filter(!is.na(Russia1_2026)) %>% 
  mutate(prop = n/sum(n),
         Russia1_2026 = recode_values(Russia1_2026, 1 ~ "An Agressor",
                                          2 ~ "A Regional Power",
                                          3 ~ "Justified",
                                          4 ~ "None of these", 
                                          5 ~ "Don't Know"),
         Russia1_2026 = factor(Russia1_2026, levels = rev(c("An Agressor",
                                                                    "A Regional Power",
                                                                    "Justified",
                                                                    "None of these", 
                                                                    "Don't Know")))) %>% 
  ggplot(aes(y = Russia1_2026, x = prop, fill = Country)) + 
  geom_col(position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept = 0.5, col = "grey50", lty = 4) + 
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) + 
  scale_fill_manual(values = c("darkblue", "darkgreen", "orange3", "darkred")) + 
  labs(x = "Proportion of Respondents", y = "Russia is ...") +
  guides(fill = guide_legend(reverse = TRUE,
                             ncol = 2)) +
  theme_custom

ggsave("plots/Russia_perspecitve_plot.png", Russia_perspecitve_plot, width = 8, height = 4)

Russia_war_mod <- lmer(reformulate(c("as.factor(Russia1_2026)", CONTROLS, "(1 | Country)"), response = "Security_FA"),
                        data = EUI_2026)

Russia_war_mod_df <- avg_predictions(Russia_war_mod, variables = "Russia1_2026")

Russia_perspective_regression <- Russia_war_mod_df %>% 
  as.data.frame() %>% 
  mutate(Russia1_2026 = recode_values(Russia1_2026, "1" ~ "An Agressor",
                                      "2" ~ "A Regional Power",
                                      "3" ~ "Justified",
                                      "4" ~ "None of these", 
                                      "5" ~ "Don't Know"),
         Russia1_2026 = factor(Russia1_2026, levels = rev(c("An Agressor",
                                                            "A Regional Power",
                                                            "Justified",
                                                            "None of these", 
                                                            "Don't Know")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Russia1_2026)) + 
  geom_point() + 
  geom_linerange() + 
  scale_x_continuous(breaks = seq(-0.5, 1, 0.2)) + 
  labs(x = "Predicted Placement on the Defence-Normalization Scale \n Higher numbers indicate more normalization focused",
       y = "Russia is ...") +
  theme_custom

ggsave("plots/Russia_perspective_regression.png", Russia_perspective_regression, width = 8, height = 4)

responsibility_plot_2026 <- EUI_2026 %>% 
  group_by(Country) %>% 
  count(Russia2_2026) %>% 
  filter(!is.na(Russia2_2026)) %>% 
  mutate(prop = n/sum(n),
         Russia2_2026 = recode_values(Russia2_2026, 1 ~ "Only Russia",
                                      2 ~ "More Russia than Ukraine",
                                      3 ~ "Russia and Ukraine Equal",
                                      4 ~ "Ukraine is more Responsible", 
                                      5 ~ "Only Ukraine",
                                      6 ~ "Niether", 
                                      7 ~ "Don't Know"),
         Russia2_2026 = factor(Russia2_2026, levels = rev(c("Only Russia",
                                                            "More Russia than Ukraine",
                                                           "Russia and Ukraine Equal",
                                                            "Ukraine is more Responsible", 
                                                            "Only Ukraine",
                                                            "Niether", 
                                                            "Don't Know")))) %>% 
  ggplot(aes(y = Russia2_2026, x = prop, fill = Country)) + 
  geom_col(position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept = 0.5, col = "grey50", lty = 4) + 
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) + 
  scale_fill_manual(values = c("darkblue", "darkgreen", "orange3", "darkred")) + 
  labs(x = "Proportion of Respondents", y = "Primary Responbility for the \n Continuation of the War") +
  guides(fill = guide_legend(reverse = TRUE,
                             ncol = 2)) +
  theme_custom

ggsave("plots/responsibility_plot_2026.png", responsibility_plot_2026, width = 8, height = 4)

responsibility_mod <- lmer(reformulate(c("as.factor(Russia2_2026)", CONTROLS, "(1 | Country)"), response = "Security_FA"),
                       data = EUI_2026)

responsibility_mod_df <- avg_predictions(responsibility_mod, variables = "Russia2_2026")

responsibility_regression_plot <- responsibility_mod_df %>% 
  as.data.frame() %>% 
  mutate(Russia2_2026 = recode_values(Russia2_2026, "1" ~ "Only Russia",
                                      "2" ~ "More Russia than Ukraine",
                                      "3" ~ "Russia and Ukraine Equal",
                                      "4" ~ "Ukraine is more Responsible", 
                                      "5" ~ "Only Ukraine",
                                      "6" ~ "Niether", 
                                      "7" ~ "Don't Know"),
         Russia2_2026 = factor(Russia2_2026, levels = rev(c("Only Russia",
                                                            "More Russia than Ukraine",
                                                            "Russia and Ukraine Equal",
                                                            "Ukraine is more Responsible", 
                                                            "Only Ukraine",
                                                            "Niether", 
                                                            "Don't Know")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Russia2_2026)) + 
  geom_point() + 
  geom_linerange() + 
  scale_x_continuous(breaks = seq(-0.5, 1.2, 0.2)) + 
  labs(x = "Predicted Placement on the Defence-Normalization Scale \n Higher numbers indicate more normalization focused",
       y = "Responsibility for the Conflict") +
  theme_custom

ggsave("plots/responsibility_regression_plot.png", responsibility_regression_plot, width = 8, height = 4)


#### New 2026 Greatest Issue ####

EUI_2026 <- EUI_2026 %>% 
  mutate(Top_Rank = case_when(extrarankq_1 == 1 ~ "Defence against Russia",
                              extrarankq_2 == 1 ~ "Normalize with Russia",
                              extrarankq_3 == 1 ~ "Reduce Greenhouse Gas",
                              extrarankq_4 == 1 ~ "Reduce Immigration",
                              extrarankq_5 == 1 ~ "Address Rising Cost of Living"),
         Defence_against_Russia = ifelse(extrarankq_1 %in% c(1, 2, 3), 1, 0),
         Normalize_with_Russia = ifelse(extrarankq_2 %in% c(1, 2, 3), 1, 0),
         Reduce_Greenhouse_Gas = ifelse(extrarankq_3 %in% c(1, 2, 3), 1, 0),
         Reduce_Immigration = ifelse(extrarankq_3 %in% c(1, 2, 3), 1, 0),
         Address_Rising_Cost_of_Living = ifelse(extrarankq_4 %in% c(1, 2, 3), 1, 0))


top_issues_prop <- EUI_2026 %>% 
  count(Top_Rank) %>% 
  mutate(prop = n / sum(n),
         Variable = "Most Important Issue") %>% 
  bind_rows(EUI_2026 %>% 
              summarise(across(c(Defence_against_Russia, Normalize_with_Russia,
                                 Reduce_Greenhouse_Gas, Reduce_Immigration, 
                                 Address_Rising_Cost_of_Living), \(x)mean(x, na.rm = TRUE))) %>% 
              pivot_longer(c(Defence_against_Russia, Normalize_with_Russia,
                             Reduce_Greenhouse_Gas, Reduce_Immigration, 
                             Address_Rising_Cost_of_Living), 
                           names_to = "Top_Rank",
                           values_to = "prop") %>% 
              mutate(Top_Rank = str_replace_all(Top_Rank, "_", " "),
                     Variable ="Ranked in Top Three")
              ) %>% 
  mutate(Top_Rank = factor(Top_Rank, levels = rev(c("Defence against Russia", "Normalize with Russia",
                                                "Reduce Greenhouse Gas", "Reduce Immigration", 
                                                "Address Rising Cost of Living"))),
         Variable = factor(Variable, levels = rev(c("Most Important Issue", "Ranked in Top Three")))
         ) %>% 
  ggplot(aes(x = prop, y = Top_Rank, fill = Variable)) + 
  geom_col(position = position_dodge(width = 0.8)) + 
  scale_x_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(reverse = TRUE, 
                               ncol = 1)) + 
  scale_fill_manual(values = c("darkgreen", "orange")) + 
  labs(y = NULL, fill = NULL, x = "% of Europeans") + 
  theme_custom

ggsave("plots/top_issues_prop.png", top_issues_prop, width = 8, height = 4)


top_issues_prop_defence_norm <- EUI_2026 %>% 
  mutate(Defence_focused = ifelse(Security_FA < 0, 1, 0)) %>% 
  group_by(Defence_focused) %>% 
  count(Top_Rank) %>% 
  mutate(prop = n / sum(n),
         Variable = "Most Important Issue") %>% 
  bind_rows(EUI_2026 %>% 
              mutate(Defence_focused = ifelse(Security_FA < 0, 1, 0)) %>% 
              group_by(Defence_focused) %>% 
              summarise(across(c(Defence_against_Russia, Normalize_with_Russia,
                                 Reduce_Greenhouse_Gas, Reduce_Immigration, 
                                 Address_Rising_Cost_of_Living), \(x)mean(x, na.rm = TRUE))) %>% 
              pivot_longer(c(Defence_against_Russia, Normalize_with_Russia,
                             Reduce_Greenhouse_Gas, Reduce_Immigration, 
                             Address_Rising_Cost_of_Living), 
                           names_to = "Top_Rank",
                           values_to = "prop") %>% 
              mutate(Top_Rank = str_replace_all(Top_Rank, "_", " "),
                     Variable ="Ranked in Top Three")
  ) %>% 
  mutate(Top_Rank = factor(Top_Rank, levels = rev(c("Defence against Russia", "Normalize with Russia",
                                                    "Reduce Greenhouse Gas", "Reduce Immigration", 
                                                    "Address Rising Cost of Living"))),
         Variable = factor(Variable, levels = c("Most Important Issue", "Ranked in Top Three")),
         Defence_focused = recode_values(Defence_focused, 0 ~ "Normalization Focused",
                                         1 ~ "Defence Focused"),
         Defence_focused = factor(Defence_focused, levels = rev(c("Defence Focused", "Normalization Focused")))
  ) %>% 
  ggplot(aes(x = prop, y = Top_Rank, fill = Defence_focused)) + 
  facet_wrap(~Variable) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  scale_x_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(reverse = TRUE, 
                             ncol = 1)) + 
  scale_fill_manual(values = c("darkred", "darkblue")) + 
  labs(y = NULL, fill = NULL, x = "% of Europeans") + 
  theme_custom

ggsave("plots/top_issues_prop_defence_norm.png", top_issues_prop_defence_norm, width = 8, height = 4)


Important_Issue_mod <- lmer(reformulate(c("Top_Rank", CONTROLS, "(1 | Country)"), response = "Security_FA"),
                                                  data = EUI_2026)

Rank_variables = c("Defence_against_Russia", "Normalize_with_Russia", "Reduce_Greenhouse_Gas", "Reduce_Immigration", "Address_Rising_Cost_of_Living")
Rank_vars_df <- data.frame()
for(i in 1:length(Rank_variables)){
  
  var <- Rank_variables[i]
  mod <- lmer(reformulate(c(var, CONTROLS, "(1 | Country)"), response = "Security_FA"),
              data = EUI_2026)
  
  df <- avg_predictions(mod, variables = var)
  
  df <- df %>% 
    filter(.data[[var]] == 1) %>% 
    mutate(Model = "Ranked in Top Three", 
           Top_Rank = var)
  
  Rank_vars_df <- bind_rows(Rank_vars_df, df)
  }

Rank_vars_df <- Rank_vars_df %>% 
  mutate(Top_Rank = str_replace_all(Top_Rank, "_", " "))

Important_Issue_mod_df <- avg_predictions(Important_Issue_mod, variables = "Top_Rank") %>% 
  mutate(Model = "Top Ranked Issue")

Important_Issue_mod_df <- Important_Issue_mod_df %>% 
  bind_rows(Rank_vars_df)

Important_Issue_plot <- Important_Issue_mod_df %>% 
  mutate(Top_Rank = factor(Top_Rank, levels = rev(c("Defence against Russia",
                                                 "Normalize with Russia",
                                                 "Reduce Greenhouse Gas",
                                                 "Reduce Immigration",
                                                 "Address Rising Cost of Living"))),
         Model = factor(Model, levels = rev(c("Ranked in Top Three", "Top Ranked Issue")))) %>% 
ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Top_Rank, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  scale_x_continuous(breaks = seq(-0.5, 1.2, 0.2)) + 
  guides(colour = guide_legend(reverse = TRUE, 
                               ncol = 1)) +
  scale_colour_manual(values = c("darkgreen", "orange")) + 
  labs(x = "Predicted Placement on the Defence-Normalization Scale \n Higher numbers indicate more normalization-focused",
       y = "Ranked as an Important Issue") +
  theme_custom

ggsave("plots/Important_Issue_plot.png", Important_Issue_plot, width = 8, height = 4)

