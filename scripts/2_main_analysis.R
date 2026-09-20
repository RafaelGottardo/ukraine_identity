##### Descriptive Statistics for the Factor Score ####

if (!exists("EUI_data_short")) source("scripts/2_group_variables.R")

Factor_graph_data <- EUI_data_short %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  select(Security_FA, Year) %>% 
  rbind(EUI_data_short %>%
          select(Security_FA) %>% 
          mutate(Year = "Pooled")) %>% 
  mutate(Mode = "Tetrachoric")

graph_by_year <- Factor_graph_data %>% 
  mutate(Year = factor(Year, levels = c("Pooled", "2025", "2024", "2023", "2022"))) %>% 
  ggplot(aes(x = Security_FA, y = Year, group = Year
  )) +
  #geom_density_ridges2() + 
  geom_boxplot(fill = "lightgrey") +
  labs(y = "Year",
       x = NULL) + 
  scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                      limits = c(-1.5, 1.5), 
                      labels = c("", "",  "", "", "", "", "", "", ""))+ 
  theme_custom 



factor_graph <- EUI_data_short %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  ggplot(aes(x = Security_FA)) +
  geom_density(fill = "lightgrey", alpha = 0.4) +
  labs(x = "Defence-Normalization Dimension\n(Higher valued indicate more normalization-focused)",
       y = "Density") + 
  scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                      limits = c(-1.5, 1.5), 
                      labels = c("", "(-1.3) Defence-Focused",  "", "", "- 0.0 -", "", "", "Normalization-Focused (1.3)", "")) +
  theme_custom

Security_focus_country_plot <- EUI_data_short %>% 
  mutate(country = factor(country, levels = country_order)) %>% 
  filter(!is.na(country)) %>% 
  ggplot(aes(y = country, x = Security_FA)) +
  #geom_violin(alpha = 0.6, fill = 'purple') +
  geom_boxplot(fill = "grey89") + 
  labs(y = "Country", x = NULL) + 
  scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                      limits = c(-1.5, 1.5), 
                      labels = c("", "",  "", "", "", "", "", "", "")) +
  theme_custom


ggarrange(graph_by_year, Security_focus_country_plot, factor_graph, ncol = 1, align = "v") %>% 
  ggsave(file.path(GLOBAL_DIR, "figures", "factor_descriptives.png"), ., width = 8, height = 10)

##### Social Bases of the Dimension ####


#### Fit Model 

Social_group <- lmer(reformulate(c("( Econ_comparison + Employed + Woman + Education + Age + income_ppp_usd_log + Employed)", "as.factor(Year)", "(1 | country)"),
                                 response = "Security_FA"),
                     data = EUI_data_short %>%
                       filter(country %in% COUNTRIES_2022) %>% 
                       # filter(Year %in% c(2024, 2025)) %>% 
                       mutate(country = relevel(factor(country), "Italy"),
                              Radicalized = relevel(factor(Radicalized), "Moderate"),
                              Education = relevel(factor(Education), "Higher Secondary"),
                              Econ_comparison = relevel(factor(Econ_comparison), "The same"),
                              Industry = relevel(factor(Industry), "White Collar"),
                              religion = relevel(factor(religion), "Christian"),
                              Employed = relevel(factor(Employed), "Employed"),
                              Generalized_trust = relevel(factor(Generalized_trust), "Untrusting"),
                              Age = recode_values(Age, "18-24" ~ "Gen Z",
                                                  "25-34" ~ "Young Millennials",
                                                  "35-44" ~ "Transition Generation",
                                                  "45-54" ~ "Cold War Children",
                                                  "55+" ~ " Cold War Adults"),
                              Former_soviet = ifelse(country %in% c("Bulgaria","Czech Republic", "Slovakia", 
                                                                    "Hungary", "Poland", "Romania", "Estonia",
                                                                    "Latvia", "Lithuania"), 1, 0)),
                     weights = balanced_weights
)

##### Create Graph

#### Graph Data frame

Social_group_df <- Social_group %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!term %in% c("(Intercept)", "as.factor(Year)2023", "as.factor(Year)2024", "as.factor(Year)2025", "IndustryUnemployed")) %>% 
  filter(str_starts(term, "country", negate = TRUE)) %>% 
  mutate(term = case_match(term,
                           "AgeGen Z" ~ "Age: Gen Z (Ref. Cold War Adults)",
                           "AgeYoung Millennials" ~ "Age: Young Millennials",
                           "AgeTransition Generation" ~ "Age: Transition Generation",
                           "AgeCold War Children" ~ "Age: Cold War Children",
                           "Former_soviet" ~ "Location: Former Eastern Bloc Countries",
                           "WomanWoman" ~ "Gender: Woman",
                           "UrbanUrban/Suburban" ~ "Urban: Urban (Ref. Suburban)",
                           "RadicalizedRadical Left" ~ "Ideology: Radical Left (Ref. Moderate)",
                           "RadicalizedRadical Right" ~ "Ideology: Radical Right",
                           "RadicalizedDon't Know" ~ "Ideology: Don't Know",
                           "Econ_comparisonBetter off" ~ "Comparision: Subjectively Better off (Ref. The Same)",
                           "income_ppp_usd_log" ~ "Income: Logged Income in PPP USD",
                           "Econ_comparisonWorse off" ~ "Comparision: Subjectively Worse off",
                           "EmployedUnemployed" ~ "Employment: Unemployed (Ref. Employed)",
                           "EmployedStudent" ~ "Employment: Student",
                           "EmployedRetired" ~ "Employment: Retired",
                           "EducationLess than Primary" ~ "Education: Less than Primary (Ref. Secondary Education)",
                           "EducationTertiary" ~ "Education: Tertiary",
                           "Above_median" ~ "Income: Above Median Income",
                           "Generalized_trustTrusting" ~ "Trust: Trusting (Ref. Untrusting)"
  )
  ) 

##### Figure 3 ####

Social_group_plot <- Social_group_df %>% 
  mutate(group = str_extract(term, "^[^:]+"),
         term = str_remove(term, "^[^:]*:\\s*"),
         term = factor(term, levels = rev(c("Gen Z (Ref. Cold War Adults)",
                                            "Young Millennials",
                                            "Transition Generation",
                                            "Cold War Children",
                                            "Former Eastern Bloc Countries",
                                            "Woman",
                                            "Urban (Ref. Suburban)",
                                            "Radical Left (Ref. Moderate)",
                                            "Radical Right",
                                            "Subjectively Better off (Ref. The Same)",
                                            "Don't Know",
                                            "Subjectively Worse off",
                                            "Logged Income in PPP USD",
                                            "Unemployed (Ref. Employed)",
                                            "Student",
                                            "Retired",
                                            "Other",
                                            "Less than Primary (Ref. Secondary Education)",
                                            "Tertiary",
                                            "Trusting (Ref. Untrusting)")))
  ) %>%
  # filter(!effect %in% c("ran_pars")) %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = group)) + 
  geom_point(size = 2) +
  geom_linerange() + 
  scale_colour_manual(values = group_colors) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey29") + 
  geom_hline(yintercept = c(2.5, 5.5, 6.5, 8.5, 9.5), col = "grey80", lty = "dotted") +
  scale_x_continuous( breaks = seq(-0.3, 0.4, length.out = 8),
                      limits = c(-0.25, 0.25),
                      labels = c("-0.3", "-0.2 (Defence Focused)",  "", "0.0", "", "(Normalization Focused) 0.2" , "", "")) +
  labs(x = "MLM Estimates and 95% Confidence Intervals \n Higher Values Represent the More Normalization Position",
       y = "Demographic Group") + 
  theme_custom + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none",
        plot.margin = margin(1,1,1,1, "cm")) 

ggsave(file.path(GLOBAL_DIR, "figures", "Social_group_plot.png"), Social_group_plot, width = 12, height = 6)


#### Age by Country ####

model_data_countries <- EUI_data_short %>%
  filter(country %in% COUNTRIES_2022) %>%
  mutate(
    country            = relevel(factor(country), "Italy"),
    Radicalized        = relevel(factor(Radicalized), "Moderate"),
    Education          = relevel(factor(Education), "Higher Secondary"),
    Econ_comparison    = relevel(factor(Econ_comparison), "The same"),
    Industry           = relevel(factor(Industry), "White Collar"),
    religion           = relevel(factor(religion), "Christian"),
    Employed           = relevel(factor(Employed), "Employed"),
    Generalized_trust  = relevel(factor(Generalized_trust), "Untrusting")
  )

## income_ppp_usd_log is dropped from this model: it's entirely NA for every UK
## respondent (the UK 2025 wave never asked an income question) and, until the
## Slovak Republic/Slovakia naming fix in scripts/2_group_variables.R, was also
## NA for every Slovak respondent. Since it's interacted with country, lm_robust
## silently drops any country where it's 100% missing, so country's fitted
## factor ends up missing UK (and previously Slovakia), which then makes
## avg_predictions() below fail with "factor country has new levels ..." once
## newdata (which still has those countries) is supplied.
Social_base_countries <- lm_robust(reformulate(c(paste0("(", "Employed +", "Woman +", "Age", ")", "*country"), "as.factor(Year)"),
                                               response = "Security_FA"),
                                   data = model_data_countries
                                     )

## Without an explicit newdata, avg_predictions() falls back to reconstructing
## the fitting data itself, which pulls in all 313 columns of model_data_countries
## (everything from EUI_data_short) rather than just the handful the model
## actually uses - that's what was hitting the 24Gb vector memory limit here.
## Passing only the columns the model needs fixes the memory problem, but the delta
## method still costs ~18 ms per row (~25 min for the ~81k rows). The predictions only
## depend on the (Employed, Woman, Age, country, Year) cell, so each unique cell is
## passed once, weighted by its number of respondents: the weighted averages are the
## same as averaging over every row (estimates identical to numerical precision, CIs to
## ~1e-6 in a check on a sample) and it runs in about a minute.
newdata_countries <- model_data_countries %>%
  drop_na(Security_FA, Employed, Woman, Age, country, Year) %>%
  count(Employed, Woman, Age, country, Year, name = "n_respondents")

Age_country_df <- avg_predictions(Social_base_countries, variables = c("Age", "country"),
                                  newdata = newdata_countries, wts = "n_respondents")


Age_by_country <- Age_country_df %>% 
  mutate(Age = recode_values(Age, "18-24" ~ "Gen Z",
                             "25-34" ~ "Young Millennials",
                             "35-44" ~ "Transition Generation",
                             "45-54" ~ "Cold War Children",
                             "55+" ~ "Cold War Adults"),
         Age = factor(Age, levels = rev(c("Gen Z",
                                          "Young Millennials",
                                          "Transition Generation",
                                          "Cold War Children",
                                          "Cold War Adults"))), 
         country = factor(country, levels = country_order)) %>% 
  ggplot(aes(x = estimate, y = Age, xmin = conf.low, xmax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  facet_wrap(~country) + 
  labs(x = "Predicted Placement on the Defence-Normalization Dimension",
       y = NULL) +
  scale_x_continuous( breaks = seq(-0.6, 0.6, length.out = 9),
                      labels = c("", "Defence",  "", "", "", "", "", "Normalization", "")) + 
  theme_custom

ggsave(file.path(GLOBAL_DIR, "figures", "Age_by_country.png"), Age_by_country, width = 10, height = 6)


#### Network Analysis ####

source("scripts/6_network_analysis.R")

#### Figure 4 ####

ggsave(file.path(GLOBAL_DIR, "figures", "horizontial_coherence_network_plot.png"), horizontial_coherence_network_plot, width = 8, height = 4)

##### Horizontial Coherence Plot ####

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

#### Preferred Outcome ####

m_pref_outcome <- lmer(reformulate(c("EUI_Ukraine_Outcome", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                       data = EUI_data_short %>% filter(country %in% COUNTRIES_2022) %>% mutate(EUI_Ukraine_Outcome = case_when(EUI_Ukraine_Outcome %in% c(1, 2) ~ "Russia Takes Territory",
                                                                                                                                EUI_Ukraine_Outcome == 3 ~ "Return to 2022 Frontline",
                                                                                                                                EUI_Ukraine_Outcome %in% c(4, 5) ~ "Ukraine Re-gains Territory"),
                                                                                                EUI_Ukraine_Outcome = factor(EUI_Ukraine_Outcome, levels = c("Return to 2022 Frontline", "Russia Takes Territory", "Ukraine Re-gains Territory"))),
                       weights = balanced_weights)

m_pref_outcome_df <- tidy(m_pref_outcome, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates") 

m_pref_outcome_df <- m_pref_outcome_df %>% 
  mutate(term = recode_values(term,
                              "EUI_Ukraine_OutcomeRussia Takes Territory" ~ "Russia Takes Territory (A)\n (Ref. Return to 2022 Frontline)",
                              "EUI_Ukraine_OutcomeUkraine Re-gains Territory" ~ "Ukraine Re-gains Territory (A)"
  ))

#### Refugee Support #####

EUI_data_short <- EUI_data_short %>% 
  mutate(Refugees = recode_values(New_Q78_1, 
                                  1 ~ "Oppose",
                                  2 ~ "Oppose", 
                                  3 ~ "Support",
                                  4 ~ "Support",
                                  5 ~ "Don't Know"),
         EU_Ukraine = recode_values(New_Q78_7,
                                    1 ~ "Oppose",
                                    2 ~ "Oppose",
                                    3 ~ "Support",
                                    4 ~ "Support",
                                    5 ~ "Don't Know"))

m_Refugee <- lmer(reformulate(c("Refugees", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                  data = EUI_data_short,
                  weights = balanced_weights)

m_Refugee_NC <- lmer(reformulate(c("Refugees", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                     data = EUI_data_short,
                     weights = balanced_weights)

m_Refugee_df <- tidy(m_Refugee, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates")

m_Refugee_df <- tidy(m_Refugee_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_Refugee_df)


#### Ukraine EU Assent ####

m_Ukr_EU <- lmer(reformulate(c("EU_Ukraine", CONTROLS, "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                 data = EUI_data_short,
                 weights = balanced_weights)

m_Ukr_EU_NC <- lmer(reformulate(c("EU_Ukraine", "as.factor(Year)","(1|country)"), response = "Security_FA"), 
                    data = EUI_data_short,
                    weights = balanced_weights)


m_Ukr_EU_df <- tidy(m_Ukr_EU, conf.int = TRUE) %>% 
  mutate(Controls = "Demographic Covariates") 

m_Ukr_EU_df <- tidy(m_Ukr_EU_NC, conf.int = TRUE) %>% 
  mutate(Controls = "Fixed Effects Only") %>% 
  bind_rows(m_Ukr_EU_df) %>% 
  mutate(Model = "Support for Ukrainian EU Membership (C)") %>% 
  bind_rows(m_Refugee_df %>% mutate(Model = "Support for Ukrainian Refugees (D)")) 

m_Ukr_EU_df  <- m_Ukr_EU_df %>% 
  mutate(term = recode_values(term,
                              "EU_UkraineSupport" ~ "Yes to Ukraine into EU (Ref. DK) (C)",
                              "EU_UkraineOppose" ~ "No to Ukraine into EU (C)",
                              "RefugeesSupport" ~ "Yes to Ukrainian Refugees (Ref. DK) (D)",
                              "RefugeesOppose" ~ "No to Ukrainian Refugees (D)"
  )) 

Horizontial_coherence_df <- bind_rows(
  m_pref_outcome_df %>% mutate(Model = "Preferred Outcome of the Conflict (A)"),
  m_trust_df %>% mutate(Model = "Trust in Other Countries (B)"),
  m_Ukr_EU_df
)


horizontial_coherence_plot <- Horizontial_coherence_df %>% 
  filter(!is.na(term)) %>% 
  filter(Controls == "Demographic Covariates") %>% 
  mutate(Model = factor(Model, levels = rev(c(
    "Preferred Outcome of the Conflict (A)",
    "Trust in Other Countries (B)",
    "Support for Ukrainian EU Membership (C)",
    "Support for Ukrainian Refugees (D)"
    ))),
    term = factor(term, levels = rev(c(                       
      "Russia Takes Territory (A)\n (Ref. Return to 2022 Frontline)",
      "Ukraine Re-gains Territory (A)"  ,                        
      "Ukraine Partial Victory (A)",                           
      "Ukraine Full Victory (A)",                              
      "Russia (B)",                                            
      "Ukraine (B)",                                           
      "Yes to Ukraine into EU (Ref. DK) (C)",
      "No to Ukraine into EU (C)",
       "Yes to Ukrainian Refugees (Ref. DK) (D)",
     "No to Ukrainian Refugees (D)"  )))) %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Model)) +
  #facet_wrap(~Model, scales = "free_y", ncol = 1) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_linerange(position = position_dodge(width = 0.6), size = 2) +
  geom_vline(xintercept = 0, lty = 4, col = "grey45") +
  scale_colour_manual(values = c("orange2", "purple4", "seagreen4", "darkred")) +
  scale_x_continuous(limits = c(-1.5, 1.5),
                     breaks = seq(-1.5, 1.5, 0.25)) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "MLM Coefficients and 95% Confidence Intervals\n(Higher values indicate greater normalization-focus)",
       y = "Independent Variable") +
  theme_custom


ggsave(file.path(GLOBAL_DIR, "figures", "horizontial_coherence_plot.png"), horizontial_coherence_plot, width = 9, height = 6)

##### Vertical Extension Network - Figure X ####

ggsave(file.path(GLOBAL_DIR, "figures", "vertical_extension_network_plot.png"), vertical_extension_network_plot, width = 9, height = 5)

#### Network Position ####

network_hypothesis_plot <- network_hypothesis %>% 
  mutate(Year = factor(Year, levels = rev(c("2022", "2023", "2024", "2025"))),
         Model = factor(Model, levels = rev(c("Horizontial Coherence", "Vertical Coherence")))) %>% 
  ggplot(aes(x = Correlation, y = Year, xmin = xmin, xmax = xmax, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6), size = 3) + 
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 1) + 
  # geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  scale_colour_manual(values = c("black", "grey70")) + 
  guides(colour = guide_legend(reverse = FALSE)) +
  labs(x = "Correlation between being defence-focused and location within the network", 
       y = "Year",
       col = NULL) + 
  theme_custom

ggsave(file.path(GLOBAL_DIR, "figures", "network_hypothesis_plot.png"), network_hypothesis_plot, height = 4, width = 8)

#### Relationship between our dimension and existing cleavages ###

testm1 <- lmer(reformulate(c("scale(Q62)", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
               data =   drop_na(EUI_data_short, any_of(c("Q62", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
                 filter(country %in% COUNTRIES_2022),
               weights = balanced_weights)
testm2 <- lmer(reformulate(c("scale(GAL_TAN)", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
               data = EUI_data_short %>% drop_na(all_of(c("Q62", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
                 filter(country %in% COUNTRIES_2022),
               weights = balanced_weights)
testm3 <- lmer(reformulate(c("scale(GAL_TAN)", "scale(Q62)", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
               data = EUI_data_short %>% drop_na(all_of(c("Q62", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
                 filter(country %in% COUNTRIES_2022),
               weights = balanced_weights)

FA_test_df <- rbind(broom::tidy(testm1, conf.int = TRUE) %>% mutate(Model = paste0("Variable Only")),
                    broom::tidy(testm2, conf.int = TRUE) %>% mutate(Model = paste0("Variable Only")),
                    broom::tidy(testm3, conf.int = TRUE) %>% mutate(Model = "Controlling for Other Cleavages"))


factor_test_plot <- FA_test_df %>% 
  filter(term %in% c("scale(Q62)", "scale(GAL_TAN)")) %>% 
  mutate(term = case_match(term, "scale(Q62)" ~ "Left-Right Placement",
                           "scale(GAL_TAN)" ~ "GAL-TAN"),
         term = factor(term, rev(c("Left-Right Placement", "GAL-TAN"))),
         Model = factor(Model, levels = rev(c("Variable Only", "Controlling for Other Cleavages")))) %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Model)) +
  geom_linerange(linewidth = 1, position = position_dodge(width = 0.6)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = c("grey", "black")) + 
  guides(colour = guide_legend(reverse = TRUE,
                               ncol = 1)) +
  labs(x = "MLM Coefficients and 95% Confidence Intervals \n for the Relationship Between Existing Cleavages \n and the Defence-Normalization Dimension",
       y = "Existing Cleavages\n(Measured at individual level)") + 
  theme_custom

ggsave(file.path(GLOBAL_DIR, "figures", "factor_test_plot.png"), factor_test_plot, width = 8, height = 4)


##### Vote Predictions ####

EUI_data_short <- EUI_data_short %>% 
  mutate(Security_FA = as.numeric(Security_FA),
         family = factor(as.character(family), levels = c("11", "1", "2", "3" , "4",
                                            "5",
                                            "6",
                                            "7",
                                            "8",
                                            "9",
                                            "10"
         ))
  )


EUI_data_short <- EUI_data_short %>% 
  mutate(gal = ifelse(galtan <= 5, 1, 0),
         Pro_Russian_party = ifelse(Securtiy_FA_party > 0, 1, 0))

#### Fit Models 

#### Party Families
mod_families_cleavage_NC <- multinom(reformulate(c("Security_FA", "country", CONTROLS),
                                              response = "family"),
                                  weights = balanced_weights,
                                  data = EUI_data_short,
                                  maxit = 1000)

mod_families_cleavage <- multinom(reformulate(c("Security_FA", "Q62", "GAL_TAN", "country", CONTROLS),
                                              response = "family"),
                                  weights = balanced_weights,
                                  data = EUI_data_short,
                                  maxit = 1000)

families_normalization_NC_df <- avg_slopes(mod_families_cleavage_NC, variables = "Security_FA",
                                                newdata = me_newdata(mod_families_cleavage_NC, EUI_data_short)) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only")

families_normalization_df <- avg_slopes(mod_families_cleavage, variables = "Security_FA",
                                             newdata = me_newdata(mod_families_cleavage, EUI_data_short)) %>% 
mutate(Mod = "Controling for Existing Cleavages")

##### GAL PARTIES

mod_gal_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                          response = "gal"),
                              weights = balanced_weights,
                              data = EUI_data_short %>% filter(Year == 2025)) 

mod_gal_normalization_df <- tidy(mod_gal_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "GAL Parties")

mod_gal_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "(1 | country)", CONTROLS),
                                                   response = "gal"),
                                       weights = balanced_weights,
                                       data = EUI_data_short %>% filter(Year == 2025))

mod_gal_normalization_cleavage_df <- tidy(mod_gal_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Existing Cleavages",
         Outcome = "GAL Parties")

#### PRO-RUSSIAN PARTY

mod_Russia_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", "as.factor(Year)", CONTROLS),
                                             response = "Pro_Russian_party"),
                                 weights = balanced_weights,
                                 data = EUI_data_short) 

mod_Russia_normalization_df <- tidy(mod_Russia_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Pro-Normalization Parties")

mod_Russia_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "(1 | country)", "as.factor(Year)", CONTROLS),
                                                      response = "Pro_Russian_party"),
                                          weights = balanced_weights,
                                          data = EUI_data_short)

mod_Russia_normalization_cleavage_df <- tidy(mod_Russia_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Existing Cleavages",
         Outcome = "Pro-Normalization Parties")

parties_normalization_df <- bind_rows(families_normalization_NC_df %>%
                                        as.data.frame() %>% 
                                        mutate(Model = "(1) Party Family"),
                                      families_normalization_df %>%
                                        as.data.frame() %>% 
                                        mutate(Model = "(1) Party Family"),
                                      mod_Russia_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                             group = "Pro-Russian Parties"),
                                      mod_Russia_normalization_cleavage_df %>% mutate(Model = "(2) Party Positions",
                                                                         group = "Pro-Russian Parties"),
                                      mod_gal_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                          group = "GAL Parties"),
                                      mod_gal_normalization_cleavage_df %>% mutate(Model = "(2) Party Positions",
                                                                          group = "GAL Parties")
)

families_normalization_plot <- parties_normalization_df %>% 
  filter(group %in% c("1", "2", "3", "4", "5", "6", "7") | (term ==  "Security_FA" & is.na(contrast))) %>% 
  mutate(group = recode_values(group,  "1" ~ "Radical Right/TAN",
                               "2" ~ "Conservative",
                               "3" ~ "Liberal",
                               "4" ~ "Christian-Democrat",
                               "5" ~ "Socialist",
                               "6" ~ "Radical Left",
                               "7" ~ "Green",
                               "8" ~ "Regionalist", 
                               "9" ~ "No Family",
                               "10" ~ "Confessional", 
                               "11" ~ "Agrarian/Centre",
                               "Pro-Russian Parties"  ~ "Pro-Normalization Parties",
                               "Pro-EU Parties" ~ "Pro-EU Parties",
                               # "Pro-Ukraine Parties" ~ "Pro-Ukraine Parties",
                               "GAL Parties" ~ "GAL Parties"#,
                               #  "Supports Trade and Diplomacy w/ Russia" ~ "Supports Trade and Diplomacy w/ Russia"
  ),
  group = factor(group, levels = rev(c("Radical Right/TAN",
                                       "Conservative",
                                       "Liberal",
                                       "Christian-Democrat",
                                       "Socialist",
                                       "Radical Left",
                                       "Green",
                                       "Regionalist", 
                                       "Confessional", 
                                       "Agrarian/Centre",
                                       "No Family",
                                       "Pro-Normalization Parties",
                                       # "Pro-EU Parties",
                                       "Pro-Ukraine Parties",
                                       "GAL Parties",
                                       "Supports Trade and Diplomacy w/ Russia"))),
  term = recode_values(term,
                       "GAL_TAN" ~ "GAL-TAN Dimension (A)",
                       #"Q9" ~ "Support for EU Membership (A)",
                       "Q62" ~ "Left-Right Self-Placement (A)",
                       "Security_FA" ~ "Defence-Normalization Dimension (B)"
  ),
  Mod = factor(Mod, levels = rev(c("Defence-Normalization Dimension Only", "Controling for Existing Cleavages")))
  ) %>% 
  filter(!is.na(group)) %>% 
  ggplot(aes(x = estimate, y = group, xmin = conf.low, xmax = conf.high, col = Mod)) + 
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  facet_wrap(~Model, ncol = 1, scale = "free_y") + 
  geom_linerange(position = position_dodge(width = 0.6), linewidth = 1) + 
  scale_x_continuous(breaks = c(-0.15, -0.10, -0.05, 0.00, 0.05, 0.1, 0.15),
                     limits = c(-0.15, 0.15),
                     labels =  c("", "-0.15 (Defence Focused)", "", "0.0", "","(Normalization Focused) 0.15", "")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  scale_colour_manual(values = rev(c("grey80", "black"))) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) +
  labs(x = "Difference in the Predicted Probability of Supporting Each Party Type \nby the Defence-Normalization Dimension",
       y = "Party Type/Party Family",
       col = NULL) +
  theme_custom

ggsave(file.path(GLOBAL_DIR, "figures", "families_normalization_plot.png"), families_normalization_plot, width = 8, height = 6)


#### Country Cleavage Strength ####

Models_Vote_choice <- list()

set.seed(1998)
## The UK is fitted separately in scripts/3_country_vote_plots.R
COUNTRIES <- setdiff(unique(EUI_data_short$country), "UK")
for(i in seq_along(COUNTRIES)){
  
  temp <- EUI_data_short %>% 
    filter(country == COUNTRIES[i]) 
  
  temp <- temp %>%
    mutate(Security_FA = as.numeric(Security_FA))
  
  temp$Year <- droplevels(as.factor(temp$Year))
  
  if(COUNTRIES[i] %in% NEW_COUNTRIES_2024){
    mod <- multinom(reformulate(c("Security_FA * Year", "Q62", "GAL_TAN", CONTROLS), response = "Past_vote"), data = temp, maxit = 1000)
    
  }else{
    mod <- multinom(reformulate(c("Security_FA", "Q62", "GAL_TAN", CONTROLS), response = "Past_vote"), data = temp, maxit = 1000)
  }
  
  nd <- me_newdata(mod, temp, "Year")

  preds <- avg_predictions(
    mod,
    variables = c("Security_FA"),
    type = "probs",
    newdata = nd
  )

  slopes <- avg_slopes(
    mod,
    variables = "Security_FA",
    by = "Year",
    newdata = nd
  )
  
  Models_Vote_choice[[COUNTRIES[i]]]$model <- mod
  Models_Vote_choice[[COUNTRIES[i]]]$predictions <- preds
  Models_Vote_choice[[COUNTRIES[i]]]$slopes <- slopes
}

#### Finland 
Finland_parties <- Models_Vote_choice[["Finland"]]$predictions %>%
  mutate(group = case_match(group, 
                            "82" ~ "Keskusta",
                            "83" ~ "Kokoomus",
                            "84" ~ "SDP",
                            "85" ~ "Vasemmistoliitto",
                            "86" ~ "Vihreä liitto",
                            "87" ~ "Kristillisdemokraatit",
                            "88" ~ "RKP",
                            "89" ~ "Perussuomalaiset",
                            "722" ~ "Korjausliike",
                            "723" ~ "Sininen tulevaisuus"
                            
  ),
  group = factor(group, levels = c("Keskusta",
                                   "Kokoomus",
                                   "SDP",
                                   "Vasemmistoliitto",
                                   "Vihreä liitto",
                                   "Kristillisdemokraatit",
                                   "RKP",
                                   "Perussuomalaiset",
                                   "Korjausliike",
                                   "Sininen tulevaisuus")),
  Country = "Finland") %>% 
  plot_predictions_vote(COLOURS = c(
    "#009A44",  # Keskusta - green
    "#003580",  # Kokoomus - dark blue
    "#E11931",  # SDP - red
    "#B71C1C",  # Vasemmistoliitto - dark red
    "#61BF1A",  # Vihreä liitto - bright green
    "#0033A0",  # Kristillisdemokraatit - blue
    "#FFD500",  # RKP - yellow
    "#1D428A",  # Perussuomalaiset - blue
    "#6C757D",  # Korjausliike - grey (less standardized)
    "#2F4F4F"   # Sininen tulevaisuus - dark blue-grey
  )) + facet_wrap(~Country)

##### Germany 

Germany_parties <- Models_Vote_choice[["Germany"]]$predictions %>%
  mutate(group = case_match(group, 
                            "19" ~ "SPD",
                            "20" ~ "CDU/CSU",
                            "21" ~ "Bündnis 90/Die Grünen",
                            "22" ~ "AfD",
                            "23" ~ "FDP",
                            "24" ~ "Die Linke",
                            "254" ~ "Bündnis Sahra Wagenknecht"),
         group = factor(group, levels = c("SPD",
                                          "CDU/CSU",
                                          "Bündnis 90/Die Grünen",
                                          "AfD",
                                          "FDP",
                                          "Die Linke",
                                          "Bündnis Sahra Wagenknecht")),
         Country = "Germany") %>% 
  filter(group != "Lutte Ouvrière") %>% 
  plot_predictions_vote(COLOURS = c(
    "#E3000F",  # SPD - red
    "#000000",  # CDU/CSU - black
    "#46962B",  # Bündnis 90/Die Grünen - green
    "#009EE0",  # AfD - light blue
    "#FFED00",  # FDP - yellow
    "#BE3075",  # Die Linke - magenta
    "#6A0032"   # Bündnis Sahra Wagenknecht - dark wine red
  )
  ) + facet_wrap(~Country) 

#### Bulgaria 

Bulgaria_parties <- Models_Vote_choice[["Bulgaria"]]$predictions %>%
  mutate(group = case_match(group, 
                            "125" ~ "GERB–SDS",
                            "126" ~ "We Continue the Change / Democratic Bulgaria (PP–DB)",
                            "127" ~ "     ",
                            "128" ~ "Revival (Vazrazhdane)",
                            "271" ~ " ",
                            "319" ~ "  ",
                            "320" ~ "   "
                            
  ),
  group = factor(group, levels = c( 
    "GERB–SDS",
    "We Continue the Change / Democratic Bulgaria (PP–DB)",
    "Revival (Vazrazhdane)",
    " ",
    "  ",
    "   ",
    "     ")),
  Country = "Bulgaria") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#1F4E79",  # GERB–SDS - blue
    "#00AEEF",  # PP–DB - light blue  # Peevski bloc - purple (non-standardised grouping)
    "#B22222",  # Revival (Vazrazhdane) - dark red
    "#CCCCCC",
    "#CCCCCC",  # ITN - orange
    "#CCCCCC",  # BSP - red
    "#CCCCCC"   # MRF (DPS) - green
  )
  ) + facet_wrap(~Country)

ggarrange(Finland_parties, Germany_parties, Bulgaria_parties, ncol = 1) %>% 
  ggsave(file.path(GLOBAL_DIR, "figures", "party_predictions.png"), ., width = 8, height = 10)

#### Party Switching ####

EUI_data_short <- EUI_data_short %>%
  mutate(#Right_wing_pro_russia = ifelse(Party_position == "Right-wing pro-Russia", 1, 0),
    #        Left_wing_pro_russia = ifelse(Party_position == "Left-wing pro-Russia", 1, 0),
    ## voted for a pro-normalization party (party index Securtiy_FA_party > 0),
    ## split by the party's economic (lrecon) or GAL-TAN (galtan) placement
    econ_RW_PR    = as.integer(lrecon > 5  & Securtiy_FA_party > 0),
    econ_LW_PR    = as.integer(lrecon <= 5 & Securtiy_FA_party > 0),
    GAL_TAN_RW_PR = as.integer(galtan > 5  & Securtiy_FA_party > 0),
    GAL_TAN_LW_PR = as.integer(galtan <= 5 & Securtiy_FA_party > 0))

## --- Country-level party supply -------------------------------------------------
## A "pro-normalization" party scores above 0 on the party-level defence-normalization
## index (Securtiy_FA_party > 0). "Economically left-wing" = CHES lrecon <= 5 ;
## "GAL" = CHES galtan <= 5. The two flags mark countries that offer voters NO such
## party to switch to.
party_supply_flags <- EUI_data_short %>%
  filter(country %in% COUNTRIES_2022, !is.na(Past_vote), !is.na(Securtiy_FA_party)) %>%
  distinct(country, Past_vote, Securtiy_FA_party, lrecon, galtan) %>%
  group_by(country) %>%
  summarise(
    no_econ_LW_norm_party = !any(lrecon <= 5 & Securtiy_FA_party > 0, na.rm = TRUE),
    no_GAL_norm_party     = !any(galtan <= 5 & Securtiy_FA_party > 0, na.rm = TRUE),
    no_GAL_norm_party_robust = !any(galtan <= 5 & Securtiy_FA_party >= 0, na.rm = TRUE),
    .groups = "drop"
  )

Party_switch_data <- EUI_data_short %>%
  filter(country %in% COUNTRIES_2022) %>%
  left_join(party_supply_flags, by = "country") %>%
  mutate(
    ## respondent GAL-TAN placement - same cut-offs as gal_tan_countries_plot.png
    GAL_TAN_values = case_when(GAL_TAN < 1.6 ~ "TAN",
                               GAL_TAN >= 1.6 & GAL_TAN < 2.5 ~ "Centre",
                               GAL_TAN >= 2.5 ~ "GAL"),
    GAL_TAN_values = factor(GAL_TAN_values, levels = c("GAL", "Centre", "TAN")),
    ## respondent left-right self-placement (Q62: 1-2 left, 3-5 centre, 6-7 right)
    LR_self = case_when(Q62 %in% c(1, 2) ~ "Left-wing",
                        Q62 %in% c(3, 4, 5) ~ "Centre",
                        Q62 %in% c(6, 7) ~ "Right-wing"),
    LR_self = factor(LR_self, levels = c("Left-wing", "Centre", "Right-wing"))
  )


No_LW_party_df <- bind_rows(
  switch_slopes(filter(Party_switch_data, no_GAL_norm_party),
                "No GAL pro-normalization party", "GAL_TAN_RW_PR")
) %>%
  filter(!is.na(x)) %>%
  mutate(x = factor(x, levels = c("Left-wing", "GAL", "Centre", "Right-wing", "TAN")),
         col = factor(col, levels = c("Left-Right Self-Placement", "GAL-TAN Placement")),
         row = factor(row, levels = c("No economically left-wing\npro-normalization party",
                                      "No GAL pro-normalization party")))

No_LW_party_plot <- No_LW_party_df %>%
  filter(row == "No GAL pro-normalization party" & col == "GAL-TAN Placement") %>% 
  ggplot(aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 4, col = "grey60") +
  geom_point() +
  geom_linerange() +
 # facet_grid(row ~ col, scales = "free_x", switch = "y") +
  labs(x = "GAL-TAN Self-Placement",
       y = "Marginal effect of being more normalization-focused on the probability of voting for\na right-wing pro-normalization party") +
  theme_custom +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90))

ggsave(file.path(GLOBAL_DIR, "figures", "No_LW_party_plot.png"), No_LW_party_plot, width = 8, height = 6)

#### Robustness Check 

No_LW_party_robust_df <- bind_rows(
  switch_slopes(filter(Party_switch_data, no_econ_LW_norm_party),
                "No economically left-wing\npro-normalization party", "econ_RW_PR"),
  switch_slopes(filter(Party_switch_data, no_GAL_norm_party_robust),
                "No GAL pro-normalization party", "GAL_TAN_RW_PR")
) %>%
  filter(!is.na(x)) %>%
  mutate(x = factor(x, levels = c("Left-wing", "GAL", "Centre", "Right-wing", "TAN")),
         col = factor(col, levels = c("Left-Right Self-Placement", "GAL-TAN Placement")),
         row = factor(row, levels = c("No economically left-wing\npro-normalization party",
                                      "No GAL pro-normalization party")))

No_LW_party_robust_plot <- No_LW_party_robust_df %>%
  filter(row == "No GAL pro-normalization party" & col == "GAL-TAN Placement") %>% 
  ggplot(aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 4, col = "grey60") +
  geom_point() +
  geom_linerange() +
  # facet_grid(row ~ col, scales = "free_x", switch = "y") +
  labs(x = "GAL-TAN Self-Placement",
       y = "Marginal effect of being more normalization-focused\non the probability of voting for\na right-wing pro-normalization party") +
  theme_custom +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90))

ggsave(file.path(GLOBAL_DIR, "figures", "No_LW_party_robust_plot.png"), No_LW_party_robust_plot, width = 8, height = 6)

