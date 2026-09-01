##### Vertical Extension #####

#### EU Support ####

m_EU_support <- lmer(reformulate(c("Security_FA", CONTROLS, "as.factor(Year)", "(1|country)"), response = "Q9"), 
                                  data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_EU_support_NC <- lmer(reformulate(c("Security_FA", "as.factor(Year)", "(1|country)"), response = "Q9"), 
                     data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_EU_support_df <- tidy(m_EU_support, conf.int = TRUE) %>% 
  mutate(Outcome = "EU Support", Controls = "Demographic Covariates")

m_EU_support_df <- tidy(m_EU_support_NC, conf.int = TRUE) %>%
  mutate(Outcome = "EU Support", Controls = "Fixed Effects Only") %>% 
  bind_rows(m_EU_support_df)

#### NATO Support ####

m_NATO_support <- lmer(reformulate(c("Security_FA", CONTROLS, "as.factor(Year)", "(1|country)"), response = "Q71"), 
                                  data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_NATO_support_NC <- lmer(reformulate(c("Security_FA", "as.factor(Year)", "(1|country)"), response = "Q71"), 
                       data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_NATO_support_df <- tidy(m_NATO_support, conf.int = TRUE) %>% 
  mutate(Outcome = "NATO Support", Controls = "Demographic Covariates")

m_NATO_support_df <- tidy(m_NATO_support_NC, conf.int = TRUE) %>% 
  mutate(Outcome = "NATO Support", Controls = "Fixed Effects Only") %>% 
  bind_rows(m_NATO_support_df)

#### Support for Democracy ####

m_democracy_support <- lmer(reformulate(c("Security_FA", CONTROLS, "as.factor(Year)", "(1|country)"), response = "Q5"), 
                                  data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_democracy_support_NC <- lmer(reformulate(c("Security_FA", "as.factor(Year)", "(1|country)"), response = "Q5"), 
                            data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_democracy_support_df <- tidy(m_democracy_support, conf.int = TRUE) %>% 
  mutate(Outcome = "Support for Democracy", Controls = "Demographic Covariates")

m_democracy_support_df <- tidy(m_democracy_support_NC, conf.int = TRUE) %>% 
  mutate(Outcome = "Support for Democracy", Controls = "Fixed Effects Only") %>% 
  bind_rows(m_democracy_support_df)

#### Support for Aggrandizement #### 

m_aggradizement_support <- lmer(reformulate(c("Security_FA", CONTROLS, "(1|country)"), response = "Support_Aggrandizement"), 
                            data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_aggradizement_support_NC <- lmer(reformulate(c("Security_FA", "(1|country)"), response = "Support_Aggrandizement"), 
                                data = EUI_data_short %>% filter(country %in% COUNTRIES_2022))

m_aggradizement_support_df <- tidy(m_aggradizement_support, conf.int = TRUE) %>% 
  mutate(Outcome = "Support for Aggrandizement", Controls = "Demographic Covariates")

m_aggradizement_support_df <- tidy(m_aggradizement_support_NC, conf.int = TRUE) %>% 
  mutate(Outcome = "Support for Aggrandizement", Controls = "Fixed Effects Only") %>% 
  bind_rows(m_aggradizement_support_df)

#### Graph Models ####

vertical_df <- bind_rows(m_EU_support_df, m_NATO_support_df,
                         m_democracy_support_df, m_aggradizement_support_df)

vertical_extension_plot <- vertical_df %>% 
  filter(term == "Security_FA") %>% 
  ggplot(aes(x = estimate, y = Outcome, xmin = conf.low, xmax = conf.high, col = Controls)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM coefficent and 95% Confidence Intervals \n for the Defence-Normalization Dimenson.",
       y = "Dependent Variable") + 
  theme_custom

 ggsave("plots/vertical_extension.png", vertical_extension_plot, width = 8, height = 4)
 
 
 #### Correlates of the Measure ####
 
EUI_data_short <- EUI_data_short %>% 
   mutate(Immigration_support = (Immigration_types_23_1 + Immigration_types_23_2 + Immigration_types_23_3 + Immigration_types_23_4)/4,
          Nationalist = ifelse(Q4 == 1, 1, 0),
          Radical = ifelse(Radicalized %in% c("Radical Right", "Radical Left"), 1, 0),
          US_threat = ifelse(Q68 == 3, 1, 0),
          Trust_US = ifelse(A5_1 > 5, 1, 0)
           )

RIVAL_EXPLANATIONS <- c("Support_Aggrandizement", "Immigration_support", "Nationalist", "Radical", "GAL_TAN", "US_threat", "Trust_US", "Q5", "Q18", "Q71", "Q9")
rival_explanations_df <- data.frame()
for(i in 1:length(RIVAL_EXPLANATIONS)){
  mod <- lmer(reformulate(c(RIVAL_EXPLANATIONS[i], CONTROLS, "(1 | country)"), response = "Security_FA"), 
              weights = balanced_weights,
              data = EUI_data_short)
  
  df <- tidy(mod, conf.int = TRUE) %>% 
    mutate(Variable = RIVAL_EXPLANATIONS[i])
  
  rival_explanations_df <- bind_rows(rival_explanations_df, df)
  
}

rival_explanations_df <- rival_explanations_df %>% 
  filter(term %in% RIVAL_EXPLANATIONS)
  


rival_explanations_plot <- rival_explanations_df %>% 
  mutate(term = recode_values(term,
                              "Support_Aggrandizement" ~ "Support for Executive Aggrandizement",
                              "Q5" ~ "Support for Democracy",
                              "Immigration_support" ~ "Support for Immigration",
                              "Nationalist" ~ "Nationalist",
                              "Radical" ~ "Ideologically Radical",
                              "GAL_TAN" ~ "GAL",
                              "US_threat" ~ "Threatened by the US",
                              "Trust_US" ~ "Trust the US",
                              "Q18" ~ "Solidarity with the EU",
                              "Q71" ~ "Importance of NATO",
                              "Q9" ~ "Support for EU Membership"
                              ),
         term = factor(term, levels = c("Support for Executive Aggrandizement",
                                        "Support for Democracy",
                                        "GAL",
                                        "Support for Immigration",
                                        "Nationalist",
                                        "Ideologically Radical",
                                        "Threatened by the US",
                                        "Trust the US",
                                        "Importance of NATO",
                                        "Support for EU Membership",
                                        "Solidarity with the EU"
         ))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term)) +
  geom_point() + 
  geom_linerange() + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_x_continuous(limits = c(-1, 1), 
                     breaks = c(-1, -0.6, -0.3, 0, 0.3, 0.6, 0.8),
                     labels = c("-1 (Defence focused)", "", "-0.3", "0.0", "", "0.6 (Normalization focused)", "")) + 
  labs(x = "MLM Coefficents and 95% Confidence Intervals", 
       y = NULL) + 
  theme_custom

ggsave("plots/rival_explanations_plot.png", rival_explanations_plot, width = 8, height = 5)
