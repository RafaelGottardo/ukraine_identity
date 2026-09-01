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
     data = EUI_data_short %>% filter(country %in% COUNTRIES_2022) %>% mutate(New_Q79 = relevel(factor(New_Q79), "3")),
     weights = balanced_weights)

m_conflict_responsibility_NC <- lmer(reformulate(c("New_Q79", "(1|country)"), response = "Security_FA"), 
                                  data = EUI_data_short %>% filter(country %in% COUNTRIES_2022) %>% mutate(New_Q79 = relevel(factor(New_Q79), "3")),
                                  weights = balanced_weights)

m_conflict_responsibility_df <- tidy(m_conflict_responsibility, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_conflict_responsibility_df <- tidy(m_conflict_responsibility_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_conflict_responsibility_df)

m_conflict_responsibility_df <- m_conflict_responsibility_df %>% 
  mutate(term = recode_values(term,
                              "New_Q791" ~ "Entirely NATO (A)\n (Ref. NATO and Russia Equally)",
                               "New_Q792" ~ "More NATO than Russia (A)",
                               "New_Q794" ~ "More Russia than NATO (A)",
                               "New_Q795" ~ "Entirely Russia (A)",
                               "New_Q796" ~ "Don't Know (A)"),
         term = factor(term, levels = rev(c("Entirely NATO (A)\n (Ref. NATO and Russia Equally)",
                                            "More NATO than Russia (A)",
                                        "More Russia than NATO (A)",
                                        "Entirely Russia (A)",
                                        "Don't Know (A)"))))


conflict_plot <-  m_conflict_responsibility_df %>% 
  filter(!is.na(term)) %>% 
  filter(Controls == "Demographic Covariates") %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom + 
  theme(legend.position = "none")
  
ggsave("plots/conflict_plot.png", conflict_plot, width = 8, height = 4)

##### Trust Model ####

m_trust <- lmer(reformulate(c("The_US", "Russia", "Ukraine", "China", CONTROLS, "(1|country)"), response = "Security_FA"), 
                                  data = EUI_data_short %>%
                                    filter(country %in% COUNTRIES_2022) %>% 
                                    mutate(The_US = ifelse(A5_1 >= 5, 1, 0),
                                  Russia = ifelse(A5_2 >= 5, 1, 0),
                                  Ukraine = ifelse(A5_3 >= 5, 1, 0),
                                  China = ifelse(A5_4 >=5, 1, 0)),
                weights = balanced_weights)


m_trust_NC <- lmer(reformulate(c("The_US", "Russia", "Ukraine", "China", "(1|country)"), response = "Security_FA"), 
                data = EUI_data_short %>%
                  filter(country %in% COUNTRIES_2022) %>% 
                  mutate(The_US = ifelse(A5_1 >= 5, 1, 0),
                         Russia = ifelse(A5_2 >= 5, 1, 0),
                         Ukraine = ifelse(A5_3 >= 5, 1, 0),
                         China = ifelse(A5_4 >=5, 1, 0)),
                weights = balanced_weights)


m_trust_df <- tidy(m_trust, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_trust_df <- tidy(m_trust_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_trust_df)

 m_trust_df <- m_trust_df %>% 
  mutate(term = recode_values(term,
                              "The_US" ~ "The US (B)",
                              "Russia" ~ "Russia (B)",
                              "Ukraine" ~ "Ukraine (B)",
                              "China" ~ "China (B)"),
         term = factor(term,
                       levels = rev(c("The US (B)",
                       "Russia (B)",
                       "Ukraine (B)",
                       "China (B)"))))
  
  Trust_plot <-  m_trust_df %>% 
  filter(!is.na(term)) %>% 
    filter(Controls == "Demographic Covariates") %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals",
       y = NULL) +
  theme_custom +
    theme(legend.position = "none")

ggsave("plots/Trust_plot.png", Trust_plot, width = 8, height = 4)

#### Threat Graph ####

m_threat <- lmer(reformulate(c("as.factor(Q68)", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
                weights = balanced_weights)

m_threat_NC <- lmer(reformulate(c("as.factor(Q68)", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
                 weights = balanced_weights)


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
  filter(Controls == "Demographic Covariates") %>% 
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
                 data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
                 weights = balanced_weights)

m_threat_year_NC <- lmer(reformulate(c("as.factor(Q68) * as.factor(Year)","(1|country)"), response = "Security_FA"), 
                    data = EUI_data_short,
                    weights = balanced_weights)


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

#### Preferred Outcome ####

m_pref_outcome <- lmer(reformulate(c("EUI_Ukraine_Outcome", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
     data = EUI_data_short %>% filter(country %in% COUNTRIES_2022) %>% mutate(EUI_Ukraine_Outcome = case_when(EUI_Ukraine_Outcome %in% c(1, 2) ~ "Russia Gains Territory",
                                                                                                              EUI_Ukraine_Outcome == 3 ~ "Return to 2022 Stalemate",
                                                                                                              EUI_Ukraine_Outcome %in% c(4, 5) ~ "Ukraine Gains Territory"),
                                                                              EUI_Ukraine_Outcome = factor(EUI_Ukraine_Outcome, levels = c("Return to 2022 Stalemate", "Russia Gains Territory", "Ukraine Gains Territory"))),
     weights = balanced_weights)

m_pref_outcome_df <- tidy(m_pref_outcome, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates") 

m_pref_outcome_df <- m_pref_outcome_df %>% 
  mutate(term = recode_values(term,
                               "EUI_Ukraine_OutcomeRussia Gains Territory" ~ "Russia Gains Territory (A)\n (Ref. Return to 2022 Stalemate)",
                              "EUI_Ukraine_OutcomeUkraine Gains Territory" ~ "Ukraine Gains Territory (A)"
                               ))

#### Refugee Support #####

m_Refugee <- lmer(reformulate(c("Refugee_support", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short,
                 weights = balanced_weights)

m_Refugee_NC <- lmer(reformulate(c("Refugee_support", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                  data = EUI_data_short,
                  weights = balanced_weights)

m_Refugee_df <- tidy(m_Refugee, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_Refugee_df <- tidy(m_Refugee_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_Refugee_df)


#### Ukraine EU Assent ####

m_Ukr_EU <- lmer(reformulate(c("EU_assent", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                  data = EUI_data_short,
                 weights = balanced_weights)

m_Ukr_EU_NC <- lmer(reformulate(c("EU_assent", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short,
                 weights = balanced_weights)


m_Ukr_EU_df <- tidy(m_Ukr_EU, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_Ukr_EU_df <- tidy(m_Ukr_EU_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_Ukr_EU_df) %>% 
  bind_rows(m_Refugee_df)

m_Ukr_EU_df  <- m_Ukr_EU_df %>% 
  mutate(term = recode_values(term,
                              "EU_assent" ~ "Support Ukrainian EU Membership (C)",
                              "Refugee_support" ~ "Accept Ukrainian Refugees (C)"
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


Horizontial_coherence_df <- bind_rows(#m_conflict_responsibility_df %>% mutate(Model = "Responsibilty for the Conflict (A)"),
                                      m_pref_outcome_df %>% mutate(Model = "Preferred Outcome of the Conflict (A)"),
                                      m_trust_df %>% mutate(Model = "Trust in Other Countries (B)"),
                                     # m_threat_df %>% mutate(Model = "Greatest Threat (D)"),
                                      m_Ukr_EU_df %>% mutate(Model = "Support for Ukraine Aid (C)")
                                      
                                      )


horizontial_coherence_plot <- Horizontial_coherence_df %>% 
  filter(!is.na(term)) %>% 
  filter(Controls == "Demographic Covariates") %>% 
  mutate(Model = factor(Model, levels = rev(c(#"Responsibilty for the Conflict (A)",
                                          "Preferred Outcome of the Conflict (A)",
                                          "Trust in Other Countries (B)",
                                          "Support for Ukraine Aid (C)"))),
         term = factor(term, levels = rev(c(#"Entirely NATO (A)\n (Ref. NATO and Russia Equally)",  
                                       # "More NATO than Russia (A)",                             
                                        #"More Russia than NATO (A)",                             
                                        #"Entirely Russia (A)",                                   
                                        #"Don't Know (A)",                                        
           "Russia Gains Territory (A)\n (Ref. Return to 2022 Stalemate)",
           "Ukraine Gains Territory (A)"  ,                        
                                        "Ukraine Partial Victory (A)",                           
                                        "Ukraine Full Victory (A)",                              
                                       # "The US (C)",                                            
                                        "Russia (B)",                                            
                                        "Ukraine (B)",                                           
                                        #"China (C)",                                             
                                        "Support Ukrainian EU Membership (C)",                   
                                        "Accept Ukrainian Refugees (C)"   )))) %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Model)) +
  #facet_wrap(~Model, scales = "free_y", ncol = 1) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_linerange(position = position_dodge(width = 0.6), size = 2) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("orange2", "purple4", "seagreen4")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficents and 95% Confidence Intervals\n(Higher values greater normalization-focus)",
       y = NULL) +
  theme_custom
  

ggsave("plots/horizontial_coherence_plot.png", horizontial_coherence_plot, width = 8, height = 6)
  

