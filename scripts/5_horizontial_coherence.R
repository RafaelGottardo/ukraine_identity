###### Horizontial coherence ########

#### Null Model #####

m_NULL <- lmer(reformulate(c("1", "(1|country)"), response = "Security_FA"),
               data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

performance::icc(m_NULL)

coef_df <- coef(m_NULL)$country %>%
  tibble::rownames_to_column("country") %>%
  dplyr::rename(country_mean = `(Intercept)`)

overall_mean <- fixef(m_NULL)[1]

Null_Model <- ggplot(coef_df, aes(x = reorder(country, country_mean),
                                  y = country_mean)) +
  geom_point() +
  coord_flip() +
  geom_hline(yintercept = overall_mean, lty = 4, col ="grey50") + 
  labs(x = "Country", y = "Average Score on the Defence-Normalization Index by Country") + 
  theme_bw()

ggsave("Plots/Null_model.png", Null_Model, width = 8, height = 4)

#### Responsibility for the Conflict ####

m_conflict_responsibility <- lmer(reformulate(c("New_Q79", CONTROLS, "(1|country)"), response = "Security_FA"), 
     data = EUI_data_short %>% filter(country %in% COUNTRIES_2022) %>% mutate(New_Q79 = relevel(factor(New_Q79), "3")))

m_conflict_responsibility_NC <- lmer(reformulate(c("New_Q79", "(1|country)"), response = "Security_FA"), 
                                  data = EUI_data_short %>% filter(country %in% COUNTRIES_2022) %>% mutate(New_Q79 = relevel(factor(New_Q79), "3")))

m_conflict_responsibility_df <- tidy(m_conflict_responsibility, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_conflict_responsibility_df <- tidy(m_conflict_responsibility_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_conflict_responsibility_df)

m_conflict_responsibility_df <- m_conflict_responsibility_df %>% 
  mutate(term = recode_values(term,
                              "New_Q791" ~ "Entirely NATO \n (Ref. NATO and Russia Equally)",
                               "New_Q792" ~ "More NATO than Russia",
                               "New_Q794" ~ "More Russia than NATO",
                               "New_Q795" ~ "Entirely Russia",
                               "New_Q796" ~ "Don't Know"),
         term = factor(term, levels = rev(c("Entirely NATO \n (Ref. NATO and Russia Equally)",
                                            "More NATO than Russia",
                                        "More Russia than NATO",
                                        "Entirely Russia",
                                        "Don't Know"))))


conflict_plot <-  m_conflict_responsibility_df %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom
  
ggsave("plots/conflict_plot.png", conflict_plot, width = 8, height = 4)

##### Trust Model ####

m_trust <- lmer(reformulate(c("The_US", "Russia", "Ukraine", "China", CONTROLS, "(1|country)"), response = "Security_FA"), 
                                  data = EUI_data_short %>%
                                    filter(country %in% COUNTRIES_2022) %>% 
                                    mutate(The_US = ifelse(A5_1 >= 5, 1, 0),
                                  Russia = ifelse(A5_2 >= 5, 1, 0),
                                  Ukraine = ifelse(A5_3 >= 5, 1, 0),
                                  China = ifelse(A5_4 >=5, 1, 0)))


m_trust_NC <- lmer(reformulate(c("The_US", "Russia", "Ukraine", "China", "(1|country)"), response = "Security_FA"), 
                data = EUI_data_short %>%
                  filter(country %in% COUNTRIES_2022) %>% 
                  mutate(The_US = ifelse(A5_1 >= 5, 1, 0),
                         Russia = ifelse(A5_2 >= 5, 1, 0),
                         Ukraine = ifelse(A5_3 >= 5, 1, 0),
                         China = ifelse(A5_4 >=5, 1, 0)))


m_trust_df <- tidy(m_trust, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_trust_df <- tidy(m_trust_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_trust_df)

 m_trust_df <- m_trust_df %>% 
  mutate(term = recode_values(term,
                              "The_US" ~ "The US",
                              "Russia" ~ "Russia",
                              "Ukraine" ~ "Ukraine",
                              "China" ~ "China"),
         term = factor(term,
                       levels = rev(c("The US",
                       "Russia",
                       "Ukraine",
                       "China"))))
  
  Trust_plot <-  m_trust_df %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom

ggsave("plots/Trust_plot.png", Trust_plot, width = 8, height = 4)

#### Threat Graph ####

m_threat <- lmer(reformulate(c("as.factor(Q68)", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_threat_NC <- lmer(reformulate(c("as.factor(Q68)", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short)


m_threat_df <- tidy(m_threat, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_threat_df <- tidy(m_threat_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_threat_df)

m_threat_df <- m_threat_df %>% 
  mutate(term = recode_values(term,
                              "as.factor(Q68)2" ~ "Russia \n (Ref. Terrorism)",
                              "as.factor(Q68)3" ~ "The US",
                              "as.factor(Q68)4" ~ "China",
                              "as.factor(Q68)5" ~ "Nuclear Proliferation",
                              "Q686" ~ "Other",
                              "Q687" ~ "Don't Know"
                              ),
         term = factor(term, levels = rev(c("Russia \n (Ref. Terrorism)", "The US", "China","Nuclear Proliferation",
                                            "Other", "Don't Know")))) 

threat_plot <- m_threat_df %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom

ggsave("plots/threat_plot.png", threat_plot, width = 8, height = 4)

m_threat_year <- lmer(reformulate(c("as.factor(Q68) * as.factor(Year)", CONTROLS,"(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_threat_year_NC <- lmer(reformulate(c("as.factor(Q68) * as.factor(Year)","(1|country)"), response = "Security_FA"), 
                    data = EUI_data_short)


m_threat_year_df <- avg_slopes(m_threat_year, variables = "Q68", by = "Year") %>% 
  mutate(Controls = "Demographic Covariates")

m_threat_year_df <- avg_slopes(m_threat_year_NC, variables = "Q68", by = "Year") %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_threat_year_df)

threat_plot_year <- m_threat_year_df %>% 
  mutate(contrast = recode_values(contrast,
                              "2 - 1" ~ "Russia \n (Ref. Terrorism)",
                              "3 - 1" ~ "The US",
                              "4 - 1" ~ "China",
                              "5 - 1" ~ "Nuclear Proliferation",
                              "6 - 1" ~ "Other",
                              "7 - 1" ~ "Don't Know"
  ),
  contrast = factor(contrast, levels = rev(c("Russia \n (Ref. Terrorism)", "The US", "China","Nuclear Proliferation",
                                     "Other", "Don't Know"))),
  Year = factor(Year, levels = rev(c("2022", "2023", "2024", "2025")))) %>% 
  filter(!is.na(contrast)) %>% 
  filter(Controls == "Demographic Covariates") %>% 
  ggplot(aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, col = Year)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("purple4", "orange2", "seagreen", "skyblue")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL,
       col = NULL) +
  theme_custom

ggsave("plots/threat_plot_year.png", threat_plot_year, width = 8, height = 4)
#### Refugee Support #####

m_Refugee <- lmer(reformulate(c("Refugee_support", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short)

m_Refugee_NC <- lmer(reformulate(c("Refugee_support", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                  data = EUI_data_short)

m_Refugee_df <- tidy(m_Refugee, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_Refugee_df <- tidy(m_Refugee_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_Refugee_df)


#### Ukraine EU Assent ####

m_Ukr_EU <- lmer(reformulate(c("EU_assent", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                  data = EUI_data_short)

m_Ukr_EU_NC <- lmer(reformulate(c("EU_assent", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short)


m_Ukr_EU_df <- tidy(m_Ukr_EU, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_Ukr_EU_df <- tidy(m_Ukr_EU_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_Ukr_EU_df) %>% 
  bind_rows(m_Refugee_df)

m_Ukr_EU_df  <- m_Ukr_EU_df %>% 
  mutate(term = recode_values(term,
                              "EU_assent" ~ "Support Ukrainian EU Membership",
                              "Refugee_support" ~ "Accept Ukrainian Refugees"
                              )) 

Ukraine_plot <- m_Ukr_EU_df %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom
  
ggsave("plots/Ukraine_plot.png", Ukraine_plot, width = 8, height = 4)


Horizontial_coherence_df <- bind_rows(m_conflict_responsibility_df %>% mutate(Model = "Responsibilty for the Conflict (A)"),
                                      m_trust_df %>% mutate(Model = "Trust in Other Countries (B)"),
                                      m_threat_df %>% mutate(Model = "Greatest Threat (C)"),
                                      m_Ukr_EU_df %>% mutate(Model = "Support for Ukraine Aid (D)")
                                      )


horizontial_coherence_plot <- Horizontial_coherence_df %>% 
  filter(!is.na(term)) %>% 
  mutate(Model = factor(Model, levels = c("Responsibilty for the Conflict (A)",
                                          "Trust in Other Countries (B)",
                                          "Greatest Threat (C)",
                                          "Support for Ukraine Aid (D)"))) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  facet_wrap(~Model, scales = "free_y", ncol = 1) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom
  

ggsave("plots/horizontial_coherence_plot.png", horizontial_coherence_plot, width = 8, height = 8)
  

