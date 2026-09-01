############################################################
###### Code to Test the Strength of the new cleavage #######
######     Written by Rafael Campos-Gottardo         #######
############################################################

EUI_data_short <- EUI_data_short %>% 
  mutate(Q62_01 = range01(Q62),
         Security_FA_01 = range01(Security_FA),
         New_Q43i_01 = range01(New_Q43i), 
         GAL_TAN_index_01 = range01(GAL_TAN))

COUNTRIES <- unique(EUI_data_short$country)[-c(16, 23)]

###### Factor Analysis #####

group_vars <- EUI_data_short %>% 
  select(Q73, New_Q78_4, New_Q78_5)

group_vars <- group_vars %>% 
  mutate(
         Q73_security = ifelse(Q73 == "European countries should invest more in defence and security to defend against Russian aggression", 1, 0),
         New_Q78_4_security = ifelse(New_Q78_4 %in% c(3, 4), 1, 0),
         New_Q78_5_security = ifelse(New_Q78_5 %in% c(3, 4), 1, 0)
         )

group_vars <- group_vars %>% 
  select(-c(Q73, New_Q78_4, New_Q78_5))


CORS <- tetrachoric(group_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 3 factors with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1, cor = "tet"); Factor_loadings$loadings


#EUI_data_short$Security_FA <- Factor_loadings$scores



EUI_data_short <- EUI_data_short %>% 
  mutate(Domestic = ifelse(Ukraine_groups == "Domestic/ Distracted", 1, 0),
         Russia_col = ifelse(Ukraine_groups == "Russia collaboration", 1, 0),
         Conditional = ifelse(Ukraine_groups == "Conditional Ukraine Supporters", 1, 0),
         Security = ifelse(Ukraine_groups == "Security-focused", 1, 0))


group_cors <- EUI_data_short %>% 
  select( Domestic, Conditional, Russia_col, Security_FA, Security)

cor(group_cors, use = "pairwise.complete.obs")

##### Graph Factor Loadings ####

Factor_graph_data <- EUI_data_short %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  select(Security_FA, Year) %>% 
  rbind(EUI_data_short %>%
          select(Security_FA) %>% 
          mutate(Year = "Pooled"))

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

ggarrange(graph_by_year, factor_graph, ncol = 1, align = "v") %>% 
ggsave("plots/factor_scores_density.png", ., width = 8, height = 6)

##### Factor Score Left- Right ####
security_mid <- mean(c(max(EUI_data_short$Security_FA, na.rm = TRUE), min(EUI_data_short$Security_FA, na.rm = TRUE)))
ideology_mid <- mean(c(max(EUI_data_short$Q62, na.rm = TRUE), min(EUI_data_short$Q62, na.rm = TRUE)))

fa_data_ideo <- EUI_data_short %>% 
  mutate(quadrant = case_when(Security_FA > security_mid & Q62 > ideology_mid ~ "Q1",
                               Security_FA > security_mid & Q62 <= ideology_mid ~ "Q2",
                               Security_FA <= security_mid & Q62 <= ideology_mid ~ "Q3",
                               Security_FA <= security_mid & Q62 > ideology_mid ~ "Q4"))

ideo_labels <- fa_data_ideo %>% 
  count(quadrant) %>% 
  mutate(x = ifelse(quadrant %in% c("Q1", "Q4"), Inf, -Inf),
         if_else(quadrant %in% c("Q1", "Q4"), Inf, -Inf),
         hjust = if_else(quadrant %in% c("Q1", "Q4"), 1, 0),
         y = if_else(quadrant %in% c("Q1", "Q2"), Inf, -Inf),
         vjust = if_else(quadrant %in% c("Q1", "Q2"), 1, 0)) %>% 
  filter(!is.na(quadrant))


Ideology_quadrant_plot <- EUI_data_short %>% 
  mutate(   Ukraine_groups = factor(Ukraine_groups,
                                    levels = rev(c("Security-focused",
                                                   "Conditional Ukraine Supporters",
                                                   "Domestic/ Distracted",
                                                   "Russia collaboration")))) %>% 
ggplot(aes(x = Q62, y = Security_FA, col = Ukraine_groups)) + 
  geom_point(position = "jitter", alpha = 0.6) +
  geom_vline(xintercept = ideology_mid) +
  geom_hline(yintercept = security_mid) + 
  geom_label(data = ideo_labels,
             aes(label = paste0("n = ", prettyNum(n, big.mark = ",", scientific = FALSE)),
                 x = x, y = y, color = NULL,
                 hjust = hjust, vjust = vjust),
             fill = NA, label.size = 0,
             show.legend = FALSE) +
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  scale_x_continuous(labels = seq(1, 7, 1),
                     breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 2, 0.5),
                     labels = seq(-1, 2, 0.5)) + 
  labs(x = "Self-Reported Ideology \n (1 indicates the most left-wing position and 7 indicates the most right-wing position)",
       y = "Security-Colloboration Dimension", 
       col = "Ukraine") + 
  guides(colour = guide_legend(reverse = TRUE,
                               ncol = 2)) + 
  theme_custom

ggsave("plots/Ideology_quadrant_plot.png", Ideology_quadrant_plot, width = 7, height = 7)
##### Factor Score Pro-EU ####

EU_mid <- mean(c(max(EUI_data_short$New_Q43i, na.rm = TRUE), min(EUI_data_short$New_Q43i, na.rm = TRUE)))

fa_data_EU <- EUI_data_short %>% 
  mutate(quadrant = case_when(Security_FA > security_mid & New_Q43i > EU_mid ~ "Q1",
                              Security_FA > security_mid & New_Q43i <= EU_mid ~ "Q2",
                              Security_FA <= security_mid & New_Q43i <= EU_mid ~ "Q3",
                              Security_FA <= security_mid & New_Q43i > EU_mid ~ "Q4"))

EU_labels <- fa_data_EU %>% 
  count(quadrant) %>% 
  mutate(x = ifelse(quadrant %in% c("Q1", "Q4"), Inf, -Inf),
         if_else(quadrant %in% c("Q1", "Q4"), Inf, -Inf),
         hjust = if_else(quadrant %in% c("Q1", "Q4"), 1, 0),
         y = if_else(quadrant %in% c("Q1", "Q2"), Inf, -Inf),
         vjust = if_else(quadrant %in% c("Q1", "Q2"), 1, 0)) %>% 
  filter(!is.na(quadrant))

EU_quadrant_plot <- EUI_data_short %>% 
  mutate(   Ukraine_groups = factor(Ukraine_groups,
                                    levels = rev(c("Security-focused",
                                                   "Conditional Ukraine Supporters",
                                                   "Domestic/ Distracted",
                                                   "Russia collaboration")))) %>% 
  ggplot(aes(x = New_Q43i, y = Security_FA, col = Ukraine_groups)) + 
  geom_point(position = "jitter", alpha = 0.6) +
  geom_vline(xintercept = EU_mid) +
  geom_hline(yintercept = security_mid) + 
  geom_label(data = EU_labels,
             aes(label = paste0("n = ", prettyNum(n, big.mark = ",", scientific = FALSE)),
                 x = x, y = y, color = NULL,
                 hjust = hjust, vjust = vjust),
             fill = NA, label.size = 0,
             show.legend = FALSE) +
  scale_colour_manual(values = rev(c("darkblue", "lightblue", "forestgreen", "darkred"))) + 
  # scale_x_continuous(labels = seq(1, 7, 1),
  #                    breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 2, 0.5),
                     labels = seq(-1, 2, 0.5)) + 
  guides(colour = guide_legend(reverse = TRUE,
                      ncol = 2)) +
  labs(x = "EU Trust",
       y = "Security-Colloboration Dimension", 
       col = "Group") + 
  theme_custom

ggsave("plots/EU_quadrant_plot.png", EU_quadrant_plot, width = 7, height = 7)

##### Cross cutting to Left Right ####

ideology_groups <- lm_robust(reformulate(c("Ukraine_groups", "as.factor(Year)", CONTROLS, "country"), response = "Q62"),
                             data = EUI_data_short %>% filter(country %in% COUNTRIES_2022)) 


ideology_groups_df <- avg_predictions(ideology_groups, variables = "Ukraine_groups")

ideology_regression_plot <- ideology_groups_df %>% 
  mutate(
         Ukraine_groups = factor(Ukraine_groups, levels = rev(c("Security-focused", "Conditional Ukraine Supporters", "Domestic/ Distracted", "Russia collaboration")))
         ) %>% 
  ggplot(aes(x = estimate, y = Ukraine_groups, xmin = conf.low, xmax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  labs(x = "Predicted Ideological Position of Europeans Based on \n Russia Related Group Memebership",
       y = NULL) + 
  theme_custom


ggsave("plots/ideology_regression.png", ideology_regression_plot, width = 6, height = 3)
#### Test Factor Score ####

testm1 <- lmer(reformulate(c("scale(Q62)", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
             data =   drop_na(EUI_data_short, any_of(c("Q62_01", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
               filter(country %in% COUNTRIES_2022),
             weights = balanced_weights)
 testm2 <- lmer(reformulate(c("Q9", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
             data = EUI_data_short %>% drop_na(all_of(c("Q62_01", "Urban", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
               filter(country %in% COUNTRIES_2022),
             weights = balanced_weights)
testm3 <- lmer(reformulate(c("scale(GAL_TAN)", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
             data = EUI_data_short %>% drop_na(all_of(c("Q62_01", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
               filter(country %in% COUNTRIES_2022),
             weights = balanced_weights)
testm4 <- lmer(reformulate(c("scale(GAL_TAN)", "scale(Q62)", CONTROLS, "as.factor(Year)", "(1 | country)"), response = "Security_FA"),
               data = EUI_data_short %>% drop_na(all_of(c("Q62_01", "GAL_TAN", CONTROLS, "Year", "country"))) %>%
                 filter(country %in% COUNTRIES_2022),
               weights = balanced_weights)

FA_test_df <- rbind(broom::tidy(testm1, conf.int = TRUE) %>% mutate(Model = paste0("Variable Only")),
                    broom::tidy(testm2, conf.int = TRUE) %>% mutate(Model = paste0("Variable Only")),
                    broom::tidy(testm3, conf.int = TRUE) %>% mutate(Model = paste0("Variable Only")),
                    broom::tidy(testm4, conf.int = TRUE) %>% mutate(Model = "Controlling for Other Cleavages"))


factor_test_plot <- FA_test_df %>% 
  filter(term %in% c("scale(Q62)", "scale(GAL_TAN)", "Q9")) %>% 
  mutate(term = case_match(term, "scale(Q62)" ~ "Left-Right Placement",
                           #"Q9" ~ "Support for EU Membership",
                           "scale(GAL_TAN)" ~ "GAL-TAN"),
         term = factor(term, rev(c("Left-Right Placement", "Support for EU Membership", "GAL-TAN"))),
         Model = factor(Model, levels = rev(c("Variable Only", "Controlling for Other Cleavages")))) %>% 
  filter(!is.na(term)) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Model)) +
  geom_linerange(linewidth = 1, position = position_dodge(width = 0.6)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = c("grey", "black")) + 
  guides(colour = guide_legend(reverse = TRUE,
                               ncol = 1)) +
  labs(x = "MLM Coefficients and 95% Confidence Intervals \n for the Relationship Between Other Cleavages and the \n Defence-Normalization Dimension",
      y = NULL) + 
  theme_custom
  
ggsave("plots/factor_test_plot.png", factor_test_plot, width = 8, height = 4)

#### Vote Choice - Predictions ####

Models_Vote_choice <- list()

set.seed(1998)
for(i in 1:21){
  
  temp <- EUI_data_short %>% 
    filter(country == COUNTRIES[i]) 
  
  temp <- temp %>%
    mutate(Security_FA = as.numeric(Security_FA))
  
  temp$Year <- droplevels(as.factor(temp$Year))
  
if(COUNTRIES[i] %in% NEW_COUNTRIES_2024){
  mod <- multinom(reformulate(c("Security_FA * Year", "Q62", "Q9", "GAL_TAN", CONTROLS), response = "Past_vote"), data = temp, maxit = 1000)
  
  }else{
    mod <- multinom(reformulate(c("Security_FA", "Q62", "Q9", "GAL_TAN", CONTROLS), response = "Past_vote"), data = temp, maxit = 1000)
  }
  
  preds <- avg_predictions(
    mod,
    variables = c("Security_FA"),
    type = "probs"
  )
  
  slopes <- avg_slopes(
    mod,
    variables = "Security_FA",
    by = "Year"
  )

Models_Vote_choice[[COUNTRIES[i]]]$model <- mod
Models_Vote_choice[[COUNTRIES[i]]]$predictions <- preds
Models_Vote_choice[[COUNTRIES[i]]]$slopes <- slopes
}

#### Croatia ####

Croatia_Parties <- Models_Vote_choice[[1]]$predictions %>% 
  mutate(group = case_match(group, 
                            "139" ~ "HDZ",
                            "141" ~ "Domovinski pokret",
                            "142" ~ "Most",
                            "278" ~ "Možemo!",
                            "323" ~ "Rijeke pravde",
                            "713" ~ "Fokus-Republika"
                            ),
         group = factor(group, levels = c("HDZ",
                "Domovinski pokret",
                "Most",
                "Možemo!",
                "Rijeke pravde",
                "Fokus-Republika")),
         Country = "Croatia") %>% 
  plot_predictions_vote(COLOURS = c(
    "#005BAA",  # Hrvatska demokratska zajednica (HDZ) - dark blue
    "#7A1E1E",  # Domovinski pokret - dark red / burgundy
    "#F28C00",  # Most - orange
    "#2E8B57",  # Možemo! - green
    "#D7263D",  # Rijeke pravde - red
    "#F2C300"   # Fokus-Republika - yellow/gold
  )) + facet_wrap(~Country)

ggsave("plots/Croatia_Parties.png", Croatia_Parties, width = 7, height = 4, dpi = "retina")

#### Denmark ####

Denmark_parties <- Models_Vote_choice[[2]]$predictions %>% 
  mutate(group = case_match(group, 
                            "61" ~ "Socialdemokratiet",
                            "62" ~ "Radikale Venstre",
                            "63" ~ "Det Konservative Folkeparti",
                            "64" ~ "Nye Borgerlige",
                            "66" ~ "Socialistisk Folkeparti",
                            "67" ~ "Liberal Alliance",
                            "69" ~ "Dansk Folkeparti",
                            "71" ~ "Venstre, Danmarks Liberale Parti",
                            "72" ~ "Enhedslisten - De Rød-Grønne",
                            "73" ~ "Alternativet",
                            "74" ~ "Moderaterna",
                            "75" ~ "Centerpartiet",
                            "76" ~ "Liberalerna",
                            "77" ~ "Kristdemokraterna",
                            "78" ~ "Miljöpartiet",
                            "79" ~ "Socialdemokraterna",
                            "80" ~ "Vänsterpartiet",
                            "162" ~ "Danmarksdemokraterne",
                            "707" ~ "Feministiskt Initiativ",
                            "708" ~ "Piratpartiet"
                            
  ),
  group = factor(group, levels = c( "Socialdemokratiet",
                                    "Radikale Venstre",
                                   "Det Konservative Folkeparti",
                                    "Nye Borgerlige",
                                    "Socialistisk Folkeparti",
                                   "Liberal Alliance",
                                    "Dansk Folkeparti",
                                    "Venstre, Danmarks Liberale Parti",
                                    "Enhedslisten - De Rød-Grønne",
                                    "Alternativet",
                                   "Moderaterne",
                                   "Centerpartiet",
                                   "Liberalerna",
                                    "Kristdemokraterna",
                                   "Miljöpartiet",
                                   "Socialdemokraterna",
                                    "Vänsterpartiet",
                                    "Danmarksdemokraterne",
                                   "Feministiskt Initiativ",
                                   "Piratpartiet")),
  Country = "Denmark") %>% 
  plot_predictions_vote(COLOURS = c(
    c(
      "#E31836",  # Socialdemokratiet (DK) - red
      "#7B3F98",  # Radikale Venstre - purple
      "#006C3C",  # Det Konservative Folkeparti - dark green
      "#1F3A93",  # Nye Borgerlige - dark blue
      "#C4122E",  # Socialistisk Folkeparti - red
      "#00AEEF",  # Liberal Alliance - light blue
      "#FFD100",  # Dansk Folkeparti - yellow
      "#1E5AA8",  # Venstre - blue
      "#C4002F",  # Enhedslisten - red
      "#00A651",  # Alternativet - green
      "#6C757D",  # Moderaterna (SE) - grey
      "#009933",  # Centerpartiet (SE) - green
      "#006AB3",  # Liberalerna (SE) - blue
      "#1B5E20",  # Kristdemokraterna (SE) - dark green
      "#83CF39",  # Miljöpartiet (SE) - light green
      "#E8112D",  # Socialdemokraterna (SE) - red
      "#B31B1B",  # Vänsterpartiet (SE) - dark red
      "#8B0000",  # Danmarksdemokraterne - dark red
      "#FF69B4",  # Feministiskt Initiativ - pink
      "#6F2DA8"   # Piratpartiet - purple
    )
  )) + facet_wrap(~Country)

ggsave("plots/Denmark_parties.png", Denmark_parties, width = 7, height = 4)
#### Finland ####

Finland_parties <- Models_Vote_choice[[3]]$predictions %>% 
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

ggsave("plots/Finland_parties.png", width = 7, height = 4, Finland_parties)
#### France - ISSUES to fix ####

France_parties <- Models_Vote_choice[[4]]$predictions %>% 
  mutate(group = case_match(group, 
                            "14" ~ "Les Républicains",
                            "16" ~ "La France Insoumise",
                            "164" ~ "Rassemblement National",
                            "165" ~ "Reconquête!",
                            "168" ~ "Parti Communiste Français",
                            "169" ~ "Debout La France",
                            "308" ~ "Renaissance"#,
                           # "309" ~ "Les Écologistes (LE)"
                            
  ),
  group = factor(group, levels = c("Les Républicains",
                                   "La France Insoumise",
                                   "Rassemblement National",
                                   "Reconquête!",
                                   "Parti Communiste Français",
                                   "Debout La France",
                                   "Renaissance"#,
                                  # "Les Écologistes (LE)"
                                   )),
  Country = "France") %>% 
  filter(group != "Lutte Ouvrière") %>% 
  plot_predictions_vote(COLOURS = c(
    "#1F3A93",  # Les Républicains - dark blue
    "#E11C2A",  # La France Insoumise - bright red
    "#001F5B",  # Rassemblement National - navy blue
    "#000000",  # Reconquête! - black
    "#B22222",  # Parti Communiste Français - dark red
    "#0055A4",  # Debout La France - blue (French flag tone)
    "#FFD700"#,  # Renaissance - gold/yellow
   # "#3FA535"  # Les Écologistes (LE) - green
    
  )
  ) + facet_wrap(~Country)

ggsave("plots/France_parties.png", width = 7, height = 4, France_parties)

#### Germany ####

Germany_parties <- Models_Vote_choice[[5]]$predictions %>% 
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

ggsave("plots/Germany_parties.png", width = 10, height = 6, Germany_parties)


#### GREECE ####

Greece_parties <- Models_Vote_choice[[6]]$predictions %>% 
  mutate(group = case_match(group, 
                            "118" ~ "New Democracy",
                            "119" ~ "SYRIZA",
                            "120" ~ "PASOK",
                            "121" ~ "Communist Party",
                            "122" ~ "Greek Solution",
                            "123" ~ "MeRA25",
                            "318" ~ "Spartans"),
         group = factor(group, levels = c("New Democracy",
                                          "SYRIZA",
                                          "PASOK",
                                          "Communist Party",
                                          "Greek Solution",
                                          "MeRA25",
                                          "Spartans")),
         Country = "Greece") %>% 
  plot_predictions_vote(COLOURS = c(
    "#1F4E79",  # Νέα Δημοκρατία (ND) - deep blue
    "#E0001B",  # ΣΥΡΙΖΑ - red
    "#00843D",  # ΠΑΣΟΚ - green
    "#D40000",  # ΚΚΕ - communist red
    "#0033A0",  # Ελληνική Λύση - blue
    "#6A1B9A",  # MeRA25 - purple
    "#000000",  # Σπαρτιάτες - black
    "#D4AF37"   # Golden Dawn - gold
  )
  ) + facet_wrap(~Country)

ggsave("plots/Greece_parties.png", width = 7, height = 4, Greece_parties)

#### Hungary ####

Hungary_parties <- Models_Vote_choice[[7]]$predictions %>% 
  mutate(group = case_match(group, 
                            "103" ~ "Fidesz",
                            "200" ~ "Tisza"),
         group = factor(group, levels = c("Fidesz",
                                          "Tisza")),
         Country = "Hungary") %>% 
  plot_predictions_vote(COLOURS = c(
    "#ff6a00", "#4cb5a1"
  )
  ) + facet_wrap(~Country) 

ggsave("plots/Hungary_parties.png", width = 10, height = 6, Hungary_parties)

#### Italy ####

Italy_parties <- Models_Vote_choice[[8]]$predictions %>% 
  mutate(group = case_match(group, 
                            "42" ~ "Movimento 5 Stelle",
                            "43" ~ "Lega",
                            "44" ~ "Forza Italia",
                            "45" ~ "Fratelli d'Italia",
                            "46" ~ "Partito Democratico",
                            "47" ~ "+Europa",
                            "151" ~ "Alleanza Verdi-Sinistra",
                            "256" ~ "Azione",
                            "999" ~ "Italexit"),
         group = factor(group, levels = c( 
            "Movimento 5 Stelle",
           "Lega",
           "Forza Italia",
            "Fratelli d'Italia",
           "Partito Democratico",
            "+Europa",
            "Alleanza Verdi-Sinistra",
            "Azione",
            "Italexit")),
         Country = "Italy") %>% 
  filter(group != "Lutte Ouvrière") %>% 
  plot_predictions_vote(COLOURS =c(
    "#FFD700",  # Movimento 5 Stelle - yellow/gold
    "#00AEEF",  # Lega - light blue
    "#0066CC",  # Forza Italia - blue
    "#1C1C1C",  # Fratelli d'Italia - very dark navy/black
    "#E30613",  # Partito Democratico - red
    "#6A1B9A",  # +Europa - purple
    "#3FA535",  # Alleanza Verdi-Sinistra - green
    "#F28C00",  # Azione - orange
    "#2F4F4F"   # Italexit - dark grey
  )
  ) + facet_wrap(~Country)

ggsave("plots/Italy_parties.png", width = 7, height = 4, Italy_parties)

#### Lithuania ####

Lithuania_parties <- Models_Vote_choice[[9]]$predictions %>% 
  mutate(group = case_match(group, 
                            "110" ~ "TS-LKD",
                            "111" ~ "LVŽS",
                            "113" ~ "LSDP",
                            "116" ~ "LLRA-KŠS",
                            "117" ~ "LRP",
                            "194" ~ "DSVL",
                            "325" ~ "LS",
                            "326" ~ "PPNA",
                            "330" ~ "Taikos koalicija"),
         group = factor(group, levels = c( 
           "TS-LKD",
           "LVŽS",
           "LSDP",
           "LLRA-KŠS",
           "LRP",
           "DSVL",
           "LS",
           "PPNA",
           "Taikos koalicija")),
         Country = "Lithuania") %>% 
  plot_predictions_vote(COLOURS =c(
    "#003DA5",  # TS-LKD (Homeland Union) - conservative blue
    "#2E8B57",  # LVŽS (Greens/Peasants) - green
    "#E30613",  # LSDP (Social Democrats) - red
    "#1F4E79",  # LLRA-KŠS (Electoral Action of Poles) - deep blue
    "#7A7A7A",  # LRP - grey (less standardised branding)
    "#6A1B9A",  # DSVL - purple (movement-style branding)
    "#00AEEF",  # LS (Liberals) - light blue
    "#F4A300",  # PPNA - orange/gold tone (non-standardised)
    "#000000"   # Taikos koalicija - black
  )
  ) + facet_wrap(~Country)

ggsave("plots/Lithuania_parties.png", width = 7, height = 4, Lithuania_parties)


#### Netherlands ######## 

Netherlands_Parties <- Models_Vote_choice[[10]]$predictions %>% 
  mutate(group = case_match(group, 
                            "49" ~ "VVD",
                            "50" ~ "PVV",
                            "51" ~ "CDA",
                            "52" ~ "D66",
                            "54" ~ "SP",
                            "58" ~ "Partij voor de Dieren",
                            "59" ~ "Denk",
                            "60" ~ "Forum voor Democratie",
                            "258" ~ "PvdA",
                            "259" ~ "Nieuw Sociaal Contract",
                            "260" ~ "BoerBurgerBeweging",
                            "334" ~ "SGP"
                            ),
         group = factor(group, levels = c( 
           "VVD",
            "PVV",
          "CDA",
          "D66",
           "SP",
            "Partij voor de Dieren",
           "Denk",
            "Forum voor Democratie",
            "PvdA",
            "Nieuw Sociaal Contract",
            "BoerBurgerBeweging",
            "SGP")),
         Country = "Netherlands") %>% 
  filter(!is.na(group)) %>% 
  plot_predictions_vote(COLOURS =c(
    "#FF6600",  # VVD - orange
    "#0033A0",  # PVV - blue
    "#00A7E1",  # CDA - light blue
    "#00A6D6",  # D66 - teal/light blue
    "#E30613",  # SP - red
    "#3FA535",  # Partij voor de Dieren - green
    "#00A859",  # Denk - green (slightly different hue)
    "#6A1B9A",  # Forum voor Democratie - purple
    "#C6002A",  # PvdA - red
    "#6C757D",  # Nieuw Sociaal Contract - grey (new party, no fixed brand colour)
    "#8B4513",  # BoerBurgerBeweging - brown
    "#1A1A1A"   # SGP - black
  )
  ) 

ggsave("plots/Netherlands_parties.png", width = 7, height = 4, Netherlands_Parties)

#### Poland ####

Poland_parties <- Models_Vote_choice[[11]]$predictions %>% 
  mutate(group = case_match(group, 
                            "92" ~ "PiS",
                            "93" ~ "Lewica",
                            "94" ~ "Konfederacja Wolność I Niepodległość",
                            "95" ~ "KO",
                            "261" ~ "Trzecia Droga"
  ),
  group = factor(group, levels = c( 
    "PiS",
     "Lewica",
    "Konfederacja Wolność I Niepodległość",
    "KO",
    "Trzecia Droga")),
  Country = "Poland") %>% 
  filter(!is.na(group)) %>% 
  plot_predictions_vote(COLOURS =c(
    "#1F4E79",  # PiS - dark blue
    "#E30613",  # Lewica - red
    "#000000",  # Konfederacja Wolność i Niepodległość - black
    "#F28C00",  # KO (Koalicja Obywatelska) - orange
    "#2E8B57"   # Trzecia Droga - green
  )
  ) + facet_wrap(~Country)

ggsave("plots/Poland_parties.png", width = 7, height = 4, Poland_parties)

#### Romania ####

Romania_parties <- Models_Vote_choice[[12]]$predictions %>% 
  mutate(group = case_match(group, 
                            "96" ~ "PSD",
                            "97" ~ "PNL",
                            "99" ~ "AUR",
                            "100" ~ "PMDSZ/UDMR",
                            "199" ~ "USR",
                            "263" ~ "S.O.S. România",
                            "315" ~ "POT"
  ),
  group = factor(group, levels = c( 
    "PSD",
     "PNL",
    "AUR",
    "PMDSZ/UDMR",
    "USR",
    "S.O.S. România",
    "POT")),
  Country = "Romania") %>% 
  filter(!is.na(group)) %>% 
  filter(!group %in% c("S.O.S. România", "POT")) %>% 
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # PSD - red
    "#1F4E79",  # PNL - blue
    "#FFD700",  # AUR - gold/yellow
    "#2E8B57",  # UDMR (RMDSZ) - green
    "#00AEEF",  # USR - light blue
    "#8B0000",  # S.O.S. România - dark red
    "#6A1B9A"   # POT - purple
  )
  ) + facet_wrap(~Country)

ggsave("plots/Romania_parties.png", width = 7, height = 4, Romania_parties)


#### Slovakia ####

Slovakia_parties <- Models_Vote_choice[[13]]$predictions %>% 
  mutate(group = case_match(group, 
                            "132" ~ "SMER-SD",
                            "133" ~ "Sme Rodina",
                            "136" ~ "SaS",
                            "138" ~ "KDH",
                            "196" ~ "HLAS-SD",
                            "197" ~ "SNS",
                            "331" ~ "Progresívne Slovensko",
                            "332" ~ "OĽaNO a priatelia"
  ),
  group = factor(group, levels = c( 
    "SMER-SD",
    "Sme Rodina",
    "SaS",
    "KDH",
    "HLAS-SD",
    "SNS",
     "Progresívne Slovensko",
    "OĽaNO a priatelia")),
  Country = "Slovakia") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # SMER-SD - red
    "#8B4513",  # Sme Rodina - brown
    "#00AEEF",  # SaS - light blue
    "#1F4E79",  # KDH - dark blue
    "#6A1B9A",  # HLAS-SD - purple
    "#000000",  # SNS - black
    "#00A859",  # Progresívne Slovensko - green
    "#FFD100"   # OĽaNO a priatelia - yellow
  )
  ) + facet_wrap(~Country)

ggsave("plots/Slovakia_parties.png", width = 7, height = 4, Slovakia_parties)


#### Spain ####

Spain_parties <- Models_Vote_choice[[14]]$predictions %>% 
  mutate(group = case_match(group, 
                            "25" ~ "PSOE",
                            "26" ~ "PP",
                            "27" ~ "Vox"
  ),
  group = factor(group, levels = c( 
    "PSOE",
    "PP",
    "Vox")),
  Country = "Spain") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # PSOE - red
    "#1F4E79",  # PP - blue
    "#6A0D0D"  # Vox - dark greenish-brown / dark tone (often very dark green/blackish)
  )
  ) +
 facet_wrap(~Country)

ggsave("plots/Spain_parties.png", width = 7, height = 4, Spain_parties)

#### Sweden ####

Sweden_Parties <- Models_Vote_choice[[15]]$predictions %>% 
  mutate(group = case_match(group, 
                            "74" ~ "Moderaterna",
                            "75" ~ "Centerpartiet",
                            "76" ~ "Liberalerna",
                            "77" ~ "Kristdemokraterna",
                            "78" ~ "Miljöpartiet",
                            "79" ~ "Socialdemokraterna",
                            "80" ~ "Vänsterpartiet",
                            "81" ~ "Sverigedemokraterna"
                            
  ),
  group = factor(group, levels = c( 
    "Moderaterna",
    "Centerpartiet",
    "Liberalerna",
    "Kristdemokraterna",
    "Miljöpartiet",
    "Socialdemokraterna",
    "Vänsterpartiet",
    "Sverigedemokraterna")),
  Country = "Sweden")%>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#006AB3",  # Moderaterna - blue
    "#009933",  # Centerpartiet - green
    "#006AB3",  # Liberalerna - blue (slightly lighter in practice, but same family)
    "#1B5E20",  # Kristdemokraterna - dark green
    "#83CF39",  # Miljöpartiet - light green
    "#E8112D",  # Socialdemokraterna - red
    "#B31B1B",  # Vänsterpartiet - dark red
    "#FFCC00"   # Sverigedemokraterna - yellow
  )
  ) + facet_wrap(~Country)

ggsave("plots/Sweden_parties.png", width = 7, height = 4, Sweden_Parties)


#### UK ####

UK_model <- multinom(reformulate(c("as.numeric(Security_FA) * as.factor(Year)", "Q62", "GAL_TAN", CONTROLS), response = "Past_vote"), data = EUI_data_short %>% filter(country == "UK"), maxit = 1000)

UK_model_preds <- avg_predictions(
  UK_model,
  variables = "Security_FA",
  type = "probs"
)


UK_parties <- UK_model_preds %>% 
  mutate(group = case_match(group, 
                            "1" ~ "Conservative",
                            "2" ~ "Labour",
                            "3" ~ "Lib-Dem",
                            "4" ~ "          ",
                            "5" ~ "         ",
                            "7" ~ "        ",
                            "8" ~ "      ",
                            "9" ~ "     ",
                            "10" ~ "    ", 
                            "11" ~ "   ",
                            "12" ~ "  ",
                            "250" ~ "Reform",
                            "305" ~ " "
                            
  ),
  group = factor(group, levels = c( 
    "Conservative",
    "Labour",
    "Lib-Dem",
    "Reform",
    " ",
    "  ",
    "   ",
    "    ",
    "     ", 
    "      ",
    "       ",
    "         ",
    "          ")),
  Country = "United Kingdom") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#0087DC",  # Conservative - blue
    "#E4003B",  # Labour - red
    "#FAA61A",  # Lib-Dem - orange
    "#12B6CF",  # SNP - yellow
    "#CCCCCC",  # Plaid Cymru - green
    "#CCCCCC",  # Green Party - green
    "#CCCCCC",  # DUP - orange-red
    "#CCCCCC",  # Sinn Féin - dark green
    "#CCCCCC",  # SDLP - green
    "#CCCCCC",  # UUP - light blue
    "#CCCCCC",  # Alliance - yellow
    "#CCCCCC",  # Reform - teal
    "#CCCCCC"   # TUV - dark blue
  )
  ) + facet_wrap(~Country) 

ggsave("plots/UK_parties.png", width = 7, height = 4, UK_parties)

#### Austria ####

Austria_parties <- Models_Vote_choice[[16]]$predictions %>% 
  mutate(group = case_match(group, 
                            "339" ~ "FPÖ",
                            "340" ~ "Österreichische Volkspartei",
                            "341" ~ "Sozialdemokratische Partei",
                            "342" ~ "NEOS",
                            "343" ~ "Die Grünen"
                            
  ),
  group = factor(group, levels = c( 
    "FPÖ",
    "Österreichische Volkspartei",
    "Sozialdemokratische Partei",
    "NEOS",
    "Die Grünen")),
  Country = "Austria") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#0057A5",  # FPÖ - blue
    "#63C3D1",  # ÖVP - turquoise
    "#E11931",  # SPÖ - red
    "#E2007A",  # NEOS - pink/magenta
    "#77B82A"   # Die Grünen - green
  )
  ) + facet_wrap(~Country)

ggsave("plots/Austria_parties.png", width = 7, height = 4, Austria_parties)

#### Belgium ####

Belgium_parties <- Models_Vote_choice[[17]]$predictions %>% 
  mutate(group = case_match(group, 
                            "291" ~ "NV-A",
                            "292" ~ "VLAAMS BELANG",
                            "293" ~ "PS",
                            "294" ~ "CD&V",
                            "295" ~ "PVDA/PTB",
                            "296" ~ "Open vld",
                            "297" ~ "MR",
                            "298" ~ "Vooruit",
                            "299" ~ "ECOLO",
                            "300" ~ "GROEN",
                            "301" ~ "LE"
                            
  ),
  group = factor(group, levels = c( 
    "NV-A",
    "VLAAMS BELANG",
     "PS",
     "CD&V",
    "PVDA/PTB",
    "Open vld",
    "MR",
    "Vooruit",
    "ECOLO",
    "GROEN",
    "LE")),
  Country = "Belgium") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#FFD200",  # N-VA - yellow
    "#000000",  # Vlaams Belang - black
    "#E30613",  # PS - red
    "#FF7F00",  # CD&V - orange
    "#B22222",  # PVDA/PTB - dark red
    "#00AEEF",  # Open VLD - light blue
    "#0033A0",  # MR - blue
    "#E6007E",  # Vooruit - pink/red
    "#4DBF3B",  # ECOLO - green
    "#009933",  # GROEN - green
    "#6A1B9A"   # LE (Les Engagés) - purple
  )
  ) + facet_wrap(~Country)

ggsave("plots/Belgium_parties.png", width = 7, height = 4, Belgium_parties)


#### Bulgaria ####

Bulgaria_parties <- Models_Vote_choice[[18]]$predictions %>% 
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

ggsave("plots/Bulgaria_parties.png", width = 7, height = 4, Bulgaria_parties)

#### Czech ####

Czechia_parties <- Models_Vote_choice[[19]]$predictions %>% 
  mutate(group = case_match(group, 
                            "346" ~ "Spolu",
                            "347" ~ "ANO 2011",
                            "348" ~ "Česká pirátská strana",
                            "350" ~ "Svoboda a přímá demokracie",
                            "353" ~ " ",
                            "357" ~ "  "
  ),
  group = factor(group, levels = c( 
    "Spolu",
    "ANO 2011",
    "Česká pirátská strana",
    "Svoboda a přímá demokracie",
    " ",
     "  ")),
  Country = "Czechia") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # Spolu - red (coalition branding)
    "#1F4E79",  # ANO 2011 - blue
    "#00AEEF",  # Česká pirátská strana - light blue
    "#6A1B9A",  # SPD - purple
    "#CCCCCC",  # KSČM - dark red
    "#CCCCCC"   # ČSSD - red
  )
  ) + facet_wrap(~Country)

ggsave("plots/Czechia_parties.png", width = 7, height = 4, Czechia_parties)

#### Ireland ####

Ireland_parties <- Models_Vote_choice[[20]]$predictions %>% 
  mutate(group = case_match(group, 
                            "370" ~ "Fianna Fáil",
                            "371" ~ "Fine Gael",
                            "372" ~ "Sinn Féin",
                            "373" ~ " ",
                            "374" ~ "  ",
                            "375" ~ "   ",
                            "376" ~ "    ",
                            "377" ~ "Green Alliance",
                            "378" ~ "Community Before Profit"
  ),
  group = factor(group, levels = c( 
    "Fianna Fáil",
    "Fine Gael",
    "Sinn Féin",
    " ",
    "  ",
    "   ",
    "    ",
    "Green Alliance",
    "Community Before Profit")),
  Country = "Ireland") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#66BB6A",  # Fianna Fáil - green
    "#0057B7",  # Fine Gael - blue
    "#006400",  # Sinn Féin - dark green
    "#CCCCCC",  # Social Democrats - purple
    "#CCCCCC",  # Labour - red
    "#CCCCCC",  # Aontú - black
    "#CCCCCC",  # Independent Ireland - grey
    "#3FA535",  # Green Alliance - green
    "#B22222"   # People Before Profit / Community Before Profit - red
  )
  ) + facet_wrap(~Country)

ggsave("plots/Ireland_parties.png", width = 7, height = 4, Ireland_parties)

#### Portugal ####

Portugal_parties <- Models_Vote_choice[[21]]$predictions %>% 
  mutate(group = case_match(group, 
                            "361" ~ "Partido Socialista",
                            "362" ~ "Chega",
                            "363" ~ "Iniciativa Liberal",
                            "364" ~ "Bloco de Esquerda",
                            "369" ~ "Alternativa Democrática Nacional",
                            "381" ~ "Aliança Democrática - AD"
  ),
  group = factor(group, levels = c( 
    "Partido Socialista",
    "Chega",
    "Iniciativa Liberal",
    "Bloco de Esquerda",
    "Alternativa Democrática Nacional",
    "Aliança Democrática - AD")),
  Country = "Portugal") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # Partido Socialista - red
    "#1F4E79",  # Chega - dark blue
    "#00AEEF",  # Iniciativa Liberal - light blue
    "#8E44AD",  # Bloco de Esquerda - purple
    "#000000",  # Alternativa Democrática Nacional - black
    "#1E3A8A"   # Aliança Democrática (AD) - deep blue
  )
  ) + facet_wrap(~Country)

ggsave("plots/Portugal_parties.png", width = 7, height = 4, Portugal_parties)

#### Party Family ####

EUI_data_short <- EUI_data_short %>% 
  mutate(Security_FA = as.numeric(Security_FA),
         family = factor(family, levels = c("11", "1", "2", "3" , "4",
                                            "5",
                                            "6",
                                            "7",
                                            "8",
                                            "9",
                                            "10"
                                            ))
         )

EUI_data_short <- EUI_data_short %>% 
  mutate(gal = ifelse(galtan <= 5, 1, 0))
EUI_data_short <- EUI_data_short %>% 
  mutate(Pro_Ukraine = ifelse(UA_EU > 5, 1, 0))
EUI_data_short <- EUI_data_short %>% 
  mutate(Pro_EU = ifelse(eu_position > 4, 1, 0))
EUI_data_short <- EUI_data_short %>% 
  mutate(Pro_Russian_party = ifelse(Securtiy_FA_party > 0, 1, 0),
         Very_Pro_Russian_party = ifelse(Kremlin_ties < 3, 1, 0),
         Very_anti_Russian_party = ifelse(Kremlin_ties > 8, 1, 0))
EUI_data_short <- EUI_data_short %>% 
  mutate(Right_wing_party = ifelse(lrecon > 5, 1, 0),
         Far_Right_party = ifelse(lrecon > 7, 1, 0), 
         Far_Left_party = ifelse(lrecon < 3, 1, 0))
EUI_data_short <- EUI_data_short %>% 
  mutate(Trade_diplomacy = ifelse(EU_Russia > 5, 1, 0))


mod_families_cleavage <- multinom(reformulate(c("Q62", "GAL_TAN", "Q9", "country", CONTROLS),
                                              response = "family"),
                                  weights = balanced_weights,
                                  data = EUI_data_short %>% filter(Year == 2025),
                                  maxit = 1000)


families_cleavage_GALTAN <- avg_slopes(mod_families_cleavage, variables = "GAL_TAN")
families_cleavage_EU <- avg_slopes(mod_families_cleavage, variables = "Q9")
families_cleavage_ideology <- avg_slopes(mod_families_cleavage, variables = "Q62")

families_cleavage_df <- bind_rows(families_cleavage_GALTAN,
                                  families_cleavage_EU,
                                  families_cleavage_ideology) %>% 
  as.data.frame()

families_cleavage_plot <- families_cleavage_df %>% 
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
                               "11" ~ "Agrarian/Centre"),
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
                                          "No Family"))),
         term = recode_values(term,
                              "GAL_TAN" ~ "GAL-TAN Dimension (A)",
                              "Q9" ~ "Support for EU Membership (A)",
                              "Q62" ~ "Left-Right Self-Placement (A)",
                              "Security_FA" ~ "Defence-Normalization Dimension (B)"
                              ),
         term = factor(term, levels = c("GAL-TAN Dimension (A)",
                                        "Support for EU Membership (A)",
                                        "Left-Right Self-Placement (A)",
                                        "Defence-Normalization Dimension (B)"))) %>% 
  ggplot(aes(x = estimate, y = group, xmin = conf.low, xmax = conf.high)) + 
  geom_point() +
  facet_wrap(~term,ncol = 2) + 
  geom_linerange() + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  labs(x = "Marginal effect of each cleavage", y = "Party") +
  theme_custom

ggsave("plots/families_cleavage_plot.png", families_cleavage_plot, width = 8, height = 8)

mod_families_normalization <- multinom(reformulate(c("Security_FA", "country", CONTROLS),
                                                   response = "family"),
                                       weights = balanced_weights,
                                       data = EUI_data_short %>% filter(Year == 2025),
                                       maxit = 1000)

families_normalization_df <- avg_slopes(mod_families_normalization, variables = "Security_FA")

mod_Russia_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", "as.factor(Year)", CONTROLS),
                                                   response = "Pro_Russian_party"),
                                 weights = balanced_weights,
                                       data = EUI_data_short) 

mod_very_Russia_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", "as.factor(Year)", CONTROLS),
                                             response = "Very_Pro_Russian_party"),
                                      weights = balanced_weights,
                                 data = EUI_data_short) 

mod_anti_Russia_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", "as.factor(Year)", CONTROLS),
                                                  response = "Very_anti_Russian_party"),
                                      weights = balanced_weights,
                                      data = EUI_data_short) 

mod_Russia_normalization_df <- tidy(mod_Russia_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Pro-Normalization Parties")

mod_very_Russia_normalization_df <- tidy(mod_very_Russia_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Very Pro-Russian Parties")

mod_anti_Russia_normalization_df <- tidy(mod_anti_Russia_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Very Anti-Russian Parties")

mod_EU_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                             response = "Pro_EU"),
                             weights = balanced_weights,
                                 data = EUI_data_short %>% filter(Year == 2025)) 

mod_EU_normalization_df <- tidy(mod_EU_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Pro-EU Parties")

mod_Ukraine_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                         response = "Pro_Ukraine"),
                                  weights = balanced_weights,
                             data = EUI_data_short %>% filter(Year == 2025)) 

mod_Ukraine_normalization_df <- tidy(mod_Ukraine_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Pro-Ukraine Parties")

mod_gal_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                              response = "gal"),
                              weights = balanced_weights,
                                  data = EUI_data_short %>% filter(Year == 2025)) 

mod_gal_normalization_df <- tidy(mod_gal_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "GAL Parties")

mod_trade_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                          response = "Trade_diplomacy"),
                                weights = balanced_weights,
                              data = EUI_data_short %>% filter(Year == 2025)) 

mod_trade_normalization_df <- tidy(mod_trade_normalization, conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Trade and Diplomacy w/ Russia")


parties_normalization_df <- bind_rows(families_normalization_df %>%
                                        as.data.frame() %>% 
                                       # select(-term) %>% 
                                        mutate(Model = "(1) Party Family"),
                                      mod_Russia_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                             group = "Pro-Russian Parties"),
                                      mod_EU_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                         group = "Pro-EU Parties"),
                                      mod_Ukraine_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                              group = "Pro-Ukraine Parties"),
                                      mod_gal_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                          group = "GAL Parties"),
                                      mod_trade_normalization_df %>% mutate(Model = "(2) Party Positions",
                                                                            group = "Supports Trade and Diplomacy w/ Russia")
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
                                              "Pro-EU Parties",
                                              "Pro-Ukraine Parties",
                                              "GAL Parties",
                                              "Supports Trade and Diplomacy w/ Russia"))),
         term = recode_values(term,
                              "GAL_TAN" ~ "GAL-TAN Dimension (A)",
                              "Q9" ~ "Support for EU Membership (A)",
                              "Q62" ~ "Left-Right Self-Placement (A)",
                              "Security_FA" ~ "Defence-Normalization Dimension (B)"
         )) %>% 
  filter(!is.na(group)) %>% 
  ggplot(aes(x = estimate, y = group, xmin = conf.low, xmax = conf.high)) + 
  geom_point() +
  facet_wrap(~Model, ncol = 1, scale = "free_y") + 
  geom_linerange() + 
  scale_x_continuous(breaks = c(-0.15, -0.10, -0.05, 0.00, 0.05, 0.1, 0.15),
                     limits = c(-0.15, 0.15),
                     labels =  c("", "-0.15 (Defence Focused)", "", "0.0", "","(Normalization Foucsed) 0.15", "")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  labs(x = "Difference in the Predicted Probability of Supporting Each Party Type \nby the Defence-Normalization Dimension", y = NULL) +
  theme_custom

ggsave("plots/families_normalization_plot.png", families_normalization_plot, width = 8, height = 4)

families_marginal_effect <- parties_normalization_df %>% 
  filter(group %in% c("1", "2", "3", "4", "5", "6", "7") | (term ==  "Security_FA" & is.na(contrast))) %>% 
  filter(Model == "(1) Party Family") %>% 
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
                               "Pro-Ukraine Parties" ~ "Pro-Ukraine Parties",
                               "GAL Parties" ~ "GAL Parties",
                               "Supports Trade and Diplomacy w/ Russia" ~ "Supports Trade and Diplomacy w/ Russia"),
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
                                              "Pro-EU Parties",
                                              "Pro-Ukraine Parties",
                                              "GAL Parties",
                                              "Supports Trade and Diplomacy w/ Russia"))),
         term = recode_values(term,
                              "GAL_TAN" ~ "GAL-TAN Dimension (A)",
                              "Q9" ~ "Support for EU Membership (A)",
                              "Q62" ~ "Left-Right Self-Placement (A)",
                              "Security_FA" ~ "Defence-Normalization Dimension (B)"
         )) %>% 
  ggplot(aes(x = estimate, y = group, xmin = conf.low, xmax = conf.high)) + 
  geom_point() +
  facet_wrap(~Model, ncol = 1, scale = "free_y") + 
  geom_linerange() + 
  scale_x_continuous(breaks = c( -0.03, 0.00, 0.03, 0.06, 0.09),
                     labels = c("-0.03 (Defence focused)", "0.0", "0.03", "0.06 (Normalization Focused)", "0.09")
                     ) +
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  labs(x = "Marginal Effect of Voting for Each Party \n Based on the Defence-Normalization Dimension", y = NULL) +
  theme_custom

ggsave("plots/families_marginal_effect.png", families_marginal_effect, width = 8, height = 4)

mod_Russia_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", "as.factor(Year)", CONTROLS),
                                             response = "Pro_Russian_party"),
                                          weights = balanced_weights,
                                 data = EUI_data_short)

mod_very_Russia_normalization_cleavage <- lmer(reformulate(c("Security_FA", "(1 | country)", "Q62", "GAL_TAN", "Q9", "as.factor(Year)", CONTROLS),
                                                  response = "Very_Pro_Russian_party"),
                                               weights = balanced_weights,
                                      data = EUI_data_short) 

mod_anti_Russia_normalization_cleavage <- lmer(reformulate(c("Security_FA", "(1 | country)", "Q62", "GAL_TAN", "Q9", "as.factor(Year)", CONTROLS),
                                                  response = "Very_anti_Russian_party"),
                                               weights = balanced_weights,
                                      data = EUI_data_short) 


mod_Russia_normalization_cleavage_df <- tidy(mod_Russia_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Pro-Normalization Parties")


mod_very_Russia_normalization_cleavage_df <- tidy(mod_very_Russia_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Very Pro-Russian Parties")

mod_anti_Russia_normalization_cleavage_df <- tidy(mod_anti_Russia_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Very Anti-Russian Parties")

mod_EU_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                         response = "Pro_EU"),
                                      weights = balanced_weights,
                             data = EUI_data_short %>% filter(Year == 2025))

mod_EU_normalization_cleavage_df <- tidy(mod_EU_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Pro-EU Parties")

mod_Ukraine_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                              response = "Pro_Ukraine"),
                                           weights = balanced_weights,
                                  data = EUI_data_short %>% filter(Year == 2025)) 

mod_Ukraine_normalization_cleavage_df <- tidy(mod_Ukraine_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Pro-Ukraine Parties")


mod_gal_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                          response = "gal"),
                                       weights = balanced_weights,
                              data = EUI_data_short %>% filter(Year == 2025))

mod_gal_normalization_cleavage_df <- tidy(mod_gal_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "GAL Parties")

mod_lr_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                                  response = "Right_wing_party"),
                             weights = balanced_weights,
                                      data = EUI_data_short %>% filter(Year == 2025))

mod_lr_normalization_df <- tidy(mod_lr_normalization,
                                conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Right-Wing Parties (Econ)")

mod_fr_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                         response = "Far_Right_party"),
                             weights = balanced_weights,
                             data = EUI_data_short %>% filter(Year == 2025))

mod_fr_normalization_df <- tidy(mod_fr_normalization,
                                conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Far-Right Parties (Econ)")

mod_fl_normalization <- lmer(reformulate(c("Security_FA", "(1 | country)", CONTROLS),
                                         response = "Far_Left_party"),
                             weights = balanced_weights,
                             data = EUI_data_short %>% filter(Year == 2025))

mod_fl_normalization_df <- tidy(mod_fl_normalization,
                                conf.int = TRUE) %>% 
  mutate(Mod = "Defence-Normalization Dimension Only",
         Outcome = "Far-Left Parties (Econ)")

mod_lr_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                                   response = "Right_wing_party"),
                                      weights = balanced_weights,
                                       data = EUI_data_short %>% filter(Year == 2025))

mod_lr_normalization_cleavage_df <- tidy(mod_lr_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Right-Wing Parties (Econ)")


mod_fr_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                                  response = "Far_Right_party"),
                                      weights = balanced_weights,
                                      data = EUI_data_short %>% filter(Year == 2025))

mod_fr_normalization_cleavage_df <- tidy(mod_fr_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Far-Right Parties (Econ)")

mod_fl_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                                  response = "Far_Left_party"),
                                      weights = balanced_weights,
                                      data = EUI_data_short %>% filter(Year == 2025))

mod_fl_normalization_cleavage_df <- tidy(mod_fl_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Far-Left Parties (Econ)")

mod_trade_normalization_cleavage <- lmer(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "(1 | country)", CONTROLS),
                                            response = "Trade_diplomacy"),
                                         weights = balanced_weights,
                                data = EUI_data_short %>% filter(Year == 2025))

mod_trade_normalization_cleavage_df <- tidy(mod_trade_normalization_cleavage, conf.int = TRUE) %>% 
  mutate(Mod = "Controling for Other Cleavages",
         Outcome = "Trade and Diplomacy w/ Russia")


Parties_df <- bind_rows(mod_Russia_normalization_df,
                        mod_Russia_normalization_cleavage_df,
                        mod_EU_normalization_df,
                        mod_EU_normalization_cleavage_df,
                        mod_Ukraine_normalization_df,
                        mod_Ukraine_normalization_cleavage_df,
                        mod_gal_normalization_df,
                        mod_gal_normalization_cleavage_df,
                        # mod_trade_normalization_df,
                        # mod_trade_normalization_cleavage_df,
                        mod_lr_normalization_df,
                        mod_lr_normalization_cleavage_df,
                        mod_fr_normalization_df,
                        mod_fr_normalization_cleavage_df,
                        mod_fl_normalization_df,
                        mod_fl_normalization_cleavage_df
                        )

support_for_different_parties_plot <- Parties_df %>% 
  filter(term == "Security_FA") %>% 
  mutate(Outcome = factor(Outcome, levels = rev(c(#"Far-Right Parties (Econ)",
                                                  #"Right-Wing Parties (Econ)",
                                                  #"Far-Left Parties (Econ)",
                                              "Pro-Normalization Parties",
                                              #"Trade and Diplomacy w/ Russia",
                                             # "Pro-EU Parties",
                                              #"Pro-Ukraine Parties",
                                              "GAL Parties"
                                              )))) %>% 
  filter(!is.na(Outcome)) %>% 
  mutate(Mod = factor(Mod, levels = rev(c("Defence-Normalization Dimension Only", "Controling for Other Cleavages")))) %>% 
  ggplot(aes(x = estimate, y = Outcome, xmin = conf.low, xmax = conf.high, col = Mod)) + 
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  scale_colour_manual(values = c("black", "grey70")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) + 
  labs(x = "Multilevel Model Coefficents \n Difference in Support for Each Party Type", y = NULL, col = NULL) +
  scale_x_continuous(breaks = c(-0.15, -0.1, -0.05, 0.00, 0.05, 0.1, 0.15),
                     limits = c(-0.15, 0.15),
                     labels = c("", "-0.15 (Defence Focused)", "", "0.0", "","(Normalization Foucsed) 0.15", "")) + 
  theme_custom
  

ggsave("plots/support_for_different_parties_plot.png", support_for_different_parties_plot, width = 8, height = 4) 


Pro_Russian_parties_df <- bind_rows(mod_Russia_normalization_df,
                                    mod_very_Russia_normalization_df,
                                    mod_anti_Russia_normalization_df,
                                    mod_Russia_normalization_cleavage_df,
                                    mod_very_Russia_normalization_cleavage_df,
                                    mod_anti_Russia_normalization_cleavage_df
                                    )

most_pro_russia_plot <- Pro_Russian_parties_df %>% 
filter(term == "Security_FA") %>% 
  mutate(Outcome = factor(Outcome, levels = rev(c("Very Pro-Russian Parties",
                                                  "Pro-Russian Parties",
                                                  "Very Anti-Russian Parties")))) %>% 
  mutate(Mod = factor(Mod, levels = rev(c("Defence-Normalization Cleavage Only", "Controling for Other Cleavages")))) %>% 
  ggplot(aes(x = estimate, y = Outcome, xmin = conf.low, xmax = conf.high, col = Mod)) + 
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  scale_colour_manual(values = c("purple4", "darkgreen")) +
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 1)) + 
  labs(x = "Difference in % Support for Each Party Type", y = NULL, col = NULL,
       caption = "Very Pro-Russian Parties are those that score more 9 or 10 on favouring kremlim ties. \n \n Very Anti-Russian Parties are those that score 1 or 2 on favouring kremlin ties. \n \n Model includes random intercepts for country and fixed effects for year.") +
  scale_x_continuous(breaks = c(-0.10, -0.05, 0.00, 0.05, 0.09)) + 
  theme_custom

ggsave("plots/most_pro_russia_plot.png", most_pro_russia_plot, width = 8, height = 4)

mod_families_both <- multinom(reformulate(c("Security_FA", "Q62", "GAL_TAN", "Q9", "country" , "as.factor(Year)", CONTROLS),
                                          response = "family"),
                              data = EUI_data_short,
                              maxit = 1000)

families_both_GALTAN <- avg_slopes(mod_families_both, variables = "GAL_TAN")
families_both_EU <- avg_slopes(mod_families_both, variables = "Q9")
families_both_ideology <- avg_slopes(mod_families_both, variables = "Q62")
families_both_normalization <- avg_slopes(mod_families_both, variables = "Security_FA")

families_both_df <- bind_rows(families_both_GALTAN,
                           families_both_EU,
                           families_both_ideology,
                           families_both_normalization)

families_both_plot <- families_both_df %>% 
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
                               "11" ~ "Agrarian/Centre"),
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
                                              "No Family"))),
         term = recode_values(term,
                              "GAL_TAN" ~ "GAL-TAN Dimension (C)",
                              "Q9" ~ "Support for EU Membership (C)",
                              "Q62" ~ "Left-Right Self-Placement (C)",
                              "Security_FA" ~ "Defence-Normalization Dimension (C)"
         ),
         term = factor(term, levels = c("GAL-TAN Dimension (C)",
                                        "Left-Right Self-Placement (C)",
                                        "Support for EU Membership (C)",
                                        "Defence-Normalization Dimension (C)"))) %>% 
  ggplot(aes(x = estimate, y = group, xmin = conf.low, xmax = conf.high)) + 
  geom_point() +
  facet_wrap(~term) + 
  geom_linerange() + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  labs(x = "Marginal effect of each cleavage", y = "Party") +
  theme_custom

ggsave("plots/families_both_plot.png", families_both_plot, width = 8, height = 8)


preds_families <- avg_predictions(
  mod_families_both,
  variables = "Security_FA",
  type = "probs"
)

avg_slopes(
  mod_families,
  variables = "Security_FA"
)

party_family_graph <- preds_families %>% 
  mutate(
    group = as.character(group),
    group = case_match(group, 
                       "1" ~ "Radical Right",
                       "2" ~ "Conservative",
                       "3" ~ "Liberal",
                       "4" ~ " ",
                       "5" ~ "Socialist",
                       "6" ~ "Radical Left",
                       "7" ~ "Green",
                       "8" ~ "  ", 
                       "9" ~ "   ",
                       "10" ~ "    ", 
                       "11" ~ "     "
    ),
    group = factor(group, levels = c( 
      "Radical Right",
      "Conservative",
      "Liberal",
      "Green",
      "Socialist",
      "Radical Left",
      " ",
      "  ", 
      "   ",
       "    ", 
       "     "))
  ) %>% 
  #filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS = c(
    "#000000",  # Radical Right - black
    "#1F4E79",  # Conservative - dark blue
    "#00AEEF",  # Liberal - light blue
    "green4",  # Christian Democratic - navy blue
    "red",   # other - grey
    "orange",
    "#CCCCCC",
    "#CCCCCC",
    "#CCCCCC",
    "#CCCCCC",   
    "#CCCCCC" 
  )) 

ggsave("plots/party_family_graph.png", party_family_graph, width = 9, height = 6, dpi = "retina")

#### Vote choice Cleavage - Test Train ####

EUI_data_cleavage <- EUI_data_short %>% 
  filter(Year %in% c(2023, 2025)) %>% 
  filter(!is.na(Past_vote))
  

AIC_Values <- data.frame(`Country` = NA,
                         `Ideology` = NA, `Group` = NA,
                         `Ideology + Urban` = NA,
                         `Ideology + Urban + Group` = NA)


set.seed(1998)
for(i in 1:length(unique(EUI_data_cleavage$country))){

m1 <- multinom(reformulate(c("Q62", CONTROLS, "Year"), response = "Past_vote"), data = EUI_data_cleavage %>%  filter(country == COUNTRIES[i]))
m2 <- multinom(reformulate(c("Ukraine_groups", CONTROLS, "Year"), response = "Past_vote"), data = EUI_data_cleavage %>%  filter(country == COUNTRIES[i]))
m3 <- multinom(reformulate(c("Q62", "Urban", CONTROLS, "Year"), response = "Past_vote"), data = EUI_data_cleavage %>%  filter(country == COUNTRIES[i]))
m4 <- multinom(reformulate(c("Q62", "Urban", "Ukraine_groups", CONTROLS, "Year"), response = "Past_vote"), data = EUI_data_cleavage %>%  filter(country == COUNTRIES[i]))

AIC_Values[i, 1] <- COUNTRIES[i]
AIC_Values[i, 2] <- m1$AIC
AIC_Values[i, 3] <- m2$AIC
AIC_Values[i, 4] <- m3$AIC
AIC_Values[i, 5] <- m4$AIC
}


AIC_Values_long <- AIC_Values %>% 
  pivot_longer(2:5, names_to = "Model", values_to = "AIC") 

Average_AIC <- AIC_Values_long %>% 
  group_by(Model) %>% 
  summarise(`Average AIC` = mean(AIC),
            Country = "Average") %>% 
  ungroup()


 AIC_plot <- ggplot() + 
  geom_point(aes(x = Model, y = AIC, col = Country, group = Country), data = AIC_Values_long, alpha = 0.2) + 
  geom_line(aes(x = Model, y = AIC, col = Country, group = Country), data = AIC_Values_long, alpha = 0.2) + 
  geom_point(aes(x = Model, y = `Average AIC`, group = Country), data =  Average_AIC, col = "black") + 
  geom_line(aes(x = Model, y = `Average AIC`, group = Country), data = Average_AIC, col = "black") + 
  theme_bw() +
  theme(legend.position = "none")

 ggsave("plots/AIC_plot.png", AIC_plot, width = 6, height = 4)

 
 ###### Predict Party Positions #####
 
 
 #### Ideology 
 
# set.seed(1998)
# train_id <- sample(nrow(EUI_data_cleavage), 0.75 * nrow(EUI_data_cleavage))
#  
# train <- EUI_data_short[train_id, ]
# test  <- EUI_data_short[-train_id, ]

ideology_models <- list()
 ideology_models[["m1"]] <- lm(reformulate(c("Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "lrecon"),
                         data = EUI_data_short)
 ideology_models[["m2"]] <- lm(reformulate(c("Q62_01", CONTROLS, "as.factor(Year)", "country"), response = "lrecon"),
                         data = EUI_data_short)
 ideology_models[["m3"]] <- lm(reformulate(c("GAL_TAN_index_01", CONTROLS, "as.factor(Year)", "country"), response = "lrecon"),
                         data = EUI_data_short)
 # ideology_models[["m4"]] <- lm(reformulate(c("Q62_01", "Urban", CONTROLS, "as.factor(Year)", "country"), response = "lrecon"),
 #                         data = EUI_data_short)
 ideology_models[["m5"]] <- lm(reformulate(c("Q62_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "lrecon"),
                         data = EUI_data_short)
 ideology_models[["m6"]] <- lm(reformulate(c("Q62_01", "GAL_TAN_index_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "lrecon"),
                         data = EUI_data_short)

ideology_models_df <- data.frame()
ideology_models_rsquare <- data.frame()
for(i in 1:length(ideology_models)){
  
  temp <- broom::tidy(ideology_models[[i]], conf.int = TRUE) %>% 
    mutate(Model = names(ideology_models[i]))
  
  ideology_models_df <- rbind(ideology_models_df, temp)
  
  temp2 <- summary(ideology_models[[i]])$adj.r.squared
  
  ideology_models_rsquare[i, "R.Squared"] <- temp2
  ideology_models_rsquare[i, "Model"] <- names(ideology_models[i])
}

party_ideology_plot <- ideology_models_df %>% 
  left_join(ideology_models_rsquare) %>% 
  filter(term %in% c("Security_FA_01", "Q62_01", "GAL_TAN_index_01")) %>% 
  mutate(term = case_match(term,
                           "Security_FA_01" ~ "Security-Collaboration",
                           "Q62_01" ~ "Left-Right",
                           "GAL_TAN_index_01" ~ "GAL-TAN Index",
                           #"UrbanUrban/Suburban" ~ "Urban/Suburban"
  ),
  term = factor(term, levels = rev(c("Security-Collaboration", "Left-Right", "GAL-TAN Index", "Urban/Suburban"))),
  Model = case_match(Model, 
                     "m1" ~ paste0("Security-Collaboration Score \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m2" ~ paste0("Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m3" ~ paste0("GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")"),
                    # "m4" ~ paste0("Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m5" ~ paste0("Security-Collaboration Score + Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m6" ~ paste0("Security-Collaboration Score + Ideology +\n GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")")),
  Model = factor(Model, levels = rev(c("Security-Collaboration Score \n(R Squared = 0.117)" , "Ideology \n(R Squared = 0.315)", "GAL-TAN Index \n(R Squared = 0.133)",
                                        "Security-Collaboration Score + Ideology \n(R Squared = 0.319)", 
                                       "Security-Collaboration Score + Ideology +\n GAL-TAN Index \n(R Squared = 0.321)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey60") + 
  labs(x = "OLS Coefficents and 95% Confidence Intervals for Left-Right Placement of \n Party Respodents Voted For", 
       y = NULL,
       col = NULL) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "darkmagenta", "darkcyan", "darkorange")) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  theme_custom

ggsave("plots/party_ideology_plot.png", party_ideology_plot, width = 8, height = 4)
##### Pro-Russia 
EUI_data_short <- EUI_data_short %>% 
  mutate(Pro_Russian_party = ifelse(Kremlin_ties < 5, 1, 0))

Russia_models <- list()
Russia_models[["m1_Russia"]] <- lm(reformulate(c("Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                    data = EUI_data_short)
Russia_models[["m2_Russia"]] <- lm(reformulate(c("Q62_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                data = EUI_data_short)
Russia_models[["m3_Russia"]] <- lm(reformulate(c("GAL_TAN_index_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)
# Russia_models[["m4_Russia"]] <- lm(reformulate(c("Q62_01", "Urban", CONTROLS, "as.factor(Year)", "country"), response = "Kremlin_ties"),
#                 data = EUI_data_short)
Russia_models[["m5_Russia"]] <- lm(reformulate(c("Q62_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                data = EUI_data_short)
Russia_models[["m6_Russia"]] <- lm(reformulate(c("Q62_01", "GAL_TAN_index_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)

modelsummary::modelsummary(list(m1_Russia, m2_Russia, m3_Russia, m4_Russia), stars = TRUE)


Russia_models_df <- data.frame()
Russia_models_rsquare <- data.frame()
for(i in 1:length(Russia_models)){

temp <- broom::tidy(Russia_models[[i]], conf.int = TRUE) %>% 
  mutate(Model = names(Russia_models[i]))

Russia_models_df <- rbind(Russia_models_df, temp)

temp2 <- summary(Russia_models[[i]])$adj.r.squared

Russia_models_rsquare[i, "R.Squared"] <- temp2
Russia_models_rsquare[i, "Model"] <- names(Russia_models[i])
}

Russia_parties_plot <- Russia_models_df %>% 
  left_join(Russia_models_rsquare) %>% 
  filter(term %in% c("Security_FA_01", "GAL_TAN_index_01", "Q62_01")) %>% 
  mutate(term = case_match(term,
                           "Security_FA_01" ~ "Security-Collaboration",
                           "Q62_01" ~ "Left-Right",
                           "GAL_TAN_index_01" ~ "GAL-TAN Index"#,
                          # "UrbanUrban/Suburban" ~ "Urban/Suburban"
  ),
  term = factor(term, levels = rev(c("Security-Collaboration", "Left-Right", "GAL-TAN Index", "Urban/Suburban"))),
         Model = case_match(Model, 
                            "m1_Russia" ~ paste0("Security Collaboration Score \n", "(R Squared = ", round(R.Squared, 3), ")"),
                            "m2_Russia" ~ paste0("Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                            "m3_Russia" ~ paste0("GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")"),
                            #"m4_Russia" ~ paste0("Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                            "m5_Russia" ~ paste0("Security Collaboration Score + Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                            "m6_Russia" ~ paste0("Security Collaboration Score + Ideology + \n GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")")),
         Model = factor(Model, levels = rev(c("Security Collaboration Score \n(R Squared = 0.293)", "Ideology \n(R Squared = 0.26)", "GAL-TAN Index \n(R Squared = 0.272)",
                                              "Security Collaboration Score + Ideology \n(R Squared = 0.296)", 
                                              "Security Collaboration Score + Ideology + \n GAL-TAN Index \n(R Squared = 0.305)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey60") + 
  labs(x = "Probability of Voting for a Pro-Russian Party", 
       y = NULL,
       col = NULL) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "darkmagenta", "darkcyan", "darkorange")) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  theme_custom

ggsave("plots/Russia_parties_plot.png", Russia_parties_plot, width = 8, height = 4)

#### With Group Variables ####

Russia_models2 <- list()
Russia_models2[["m1_Russia"]] <- lm(reformulate(c("Ukraine_groups", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)
Russia_models2[["m2_Russia"]] <- lm(reformulate(c("Q62_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)
Russia_models2[["m3_Russia"]] <- lm(reformulate(c("GAL_TAN", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)
Russia_models2[["m4_Russia"]] <- lm(reformulate(c("Q62_01", "Urban", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)
Russia_models2[["m5_Russia"]] <- lm(reformulate(c("Q62_01", "Urban", "GAL_TAN", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)
Russia_models2[["m6_Russia"]] <- lm(reformulate(c("Q62_01", "Urban", "GAL_TAN", "Ukraine_groups", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Russian_party"),
                                   data = EUI_data_short)

modelsummary::modelsummary(list(m1_Russia, m2_Russia, m3_Russia, m4_Russia), stars = TRUE)


Russia_models2_df <- data.frame()
Russia_models2_rsquare <- data.frame()
for(i in 1:length(Russia_models2)){
  
  temp <- broom::tidy(Russia_models2[[i]], conf.int = TRUE) %>% 
    mutate(Model = names(Russia_models2[i]))
  
  Russia_models2_df <- rbind(Russia_models2_df, temp)
  
  temp2 <- summary(Russia_models2[[i]])$adj.r.squared
  
  Russia_models2_rsquare[i, "R.Squared"] <- temp2
  Russia_models2_rsquare[i, "Model"] <- names(Russia_models2[i])
}

Russia_parties2_plot <- Russia_models2_df %>% 
  left_join(Russia_models_rsquare) %>% 
  filter(term %in% c("Ukraine_groupsSecurity-focused", "Ukraine_groupsConditional Ukraine Supporters", "Ukraine_groupsRussia collaboration", "GAL_TAN", "Q62_01", "UrbanUrban/Suburban")) %>% 
  mutate(term = case_match(term,
                           "Ukraine_groupsSecurity-focused" ~ "Security-focused \n (Ref. Domestic/Distracted)",
                           "Ukraine_groupsConditional Ukraine Supporters" ~ "Conditional Ukraine Supporters",
                           "Ukraine_groupsRussia collaboration" ~ "Russia Collaboration",
                           "Q62_01" ~ "Left-Right",
                           "GAL_TAN" ~ "GAL-TAN Index",
                           "UrbanUrban/Suburban" ~ "Urban/Suburban"
  ),
  term = factor(term, levels = rev(c("Security-focused \n (Ref. Domestic/Distracted)", "Conditional Ukraine Supporters", "Russia Collaboration", "Left-Right", "GAL-TAN Index", "Urban/Suburban"))),
  Model = case_match(Model, 
                     "m1_Russia" ~ paste0("Security-Colloboration Group \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m2_Russia" ~ paste0("Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m3_Russia" ~ paste0("Pro/Anti-EU Attitudes \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m4_Russia" ~ paste0("Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m5_Russia" ~ paste0("Security-Colloboration Group + Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m6_Russia" ~ paste0("Security-Colloboration Group + Ideology + Urban + \n Pro/Anti-EU Attitudes \n", "(R Squared = ", round(R.Squared, 3), ")")),
  Model = factor(Model, levels = rev(c("Security-Colloboration Group \n(R Squared = 0.293)", "Ideology \n(R Squared = 0.26)", "Pro/Anti-EU Attitudes \n(R Squared = 0.281)",
                                       "Ideology + Urban \n(R Squared = 0.26)", "Security-Colloboration Group + Ideology + Urban \n(R Squared = 0.296)", 
                                       "Security-Colloboration Group + Ideology + Urban + \n Pro/Anti-EU Attitudes \n(R Squared = 0.303)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey60") + 
  labs(x = "Probability of Voting for a Pro-Russian Party and 95% Confidence Intervals", 
       y = NULL,
       col = NULL) +
  scale_colour_manual(values = c("darkblue", "darkred", "darkolivegreen", "darkmagenta", "darkcyan", "darkorange")) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  theme_custom

ggsave("plots/Russia_parties_plot_GROUPVAR.png", Russia_parties2_plot, width = 8, height = 4)

#### Pro-EU Parties 

EUI_data_short <- EUI_data_short %>% 
  mutate(Pro_EU = ifelse(eu_position > 4, 1, 0))


EU_models <- list()
EU_models[["m1"]] <- lm(reformulate(c("Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_EU"),
                                   data = EUI_data_short)
EU_models[["m2"]] <- lm(reformulate(c("Q62_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_EU"),
                                   data = EUI_data_short)
EU_models[["m3"]] <- lm(reformulate(c("GAL_TAN_index_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_EU"),
                                   data = EUI_data_short)
# EU_models[["m4"]] <- lm(reformulate(c("Q62_01", "Urban", CONTROLS, "as.factor(Year)", "country"), response = "eu_position"),
#                                    data = EUI_data_short)
EU_models[["m5"]] <- lm(reformulate(c("Q62_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_EU"),
                                   data = EUI_data_short)
EU_models[["m6"]] <- lm(reformulate(c("Q62_01", "GAL_TAN_index_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_EU"),
                                   data = EUI_data_short)



EU_models_df <- data.frame()
EU_models_rsquare <- data.frame()
for(i in 1:length(EU_models)){
  
  temp <- broom::tidy(EU_models[[i]], conf.int = TRUE) %>% 
    mutate(Model = names(EU_models[i]))
  
  EU_models_df <- rbind(EU_models_df, temp)
  
  temp2 <- summary(EU_models[[i]])$adj.r.squared
  
  EU_models_rsquare[i, "R.Squared"] <- temp2
  EU_models_rsquare[i, "Model"] <- names(EU_models[i])
}

pro_EU_parties <- EU_models_df %>% 
  left_join(EU_models_rsquare) %>% 
  filter(term %in% c("Security_FA_01", "Q62_01", "GAL_TAN_index_01")) %>% 
  mutate(term = case_match(term,
                           "Security_FA_01" ~ "Security-Collaboration",
                           "Q62_01" ~ "Left-Right",
                           "GAL_TAN_index_01" ~ "GAL-TAN Index",
                           #"UrbanUrban/Suburban" ~ "Urban/Suburban"
  ),
  term = factor(term, levels = rev(c("Security-Collaboration", "Left-Right", "GAL-TAN Index", "Urban/Suburban"))),
  Model = case_match(Model, 
                     "m1" ~ paste0("Security-Colloboration Group \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m2" ~ paste0("Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m3" ~ paste0("GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     #"m4" ~ paste0("Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m5" ~ paste0("Security-Colloboration Group + Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m6" ~ paste0("Security-Colloboration Group + Ideology +\n GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")")),
  Model = factor(Model, levels = rev(c("Security-Colloboration Group \n(R Squared = 0.269)", "Ideology \n(R Squared = 0.258)", "GAL-TAN Index \n(R Squared = 0.272)",
                                       "Security-Colloboration Group + Ideology \n(R Squared = 0.286)", 
                                       "Security-Colloboration Group + Ideology +\n GAL-TAN Index \n(R Squared = 0.302)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey60") + 
  labs(x = "Probability of voting for a pro-EU party", 
       y = NULL,
       col = NULL) +
  scale_colour_manual(values = c("darkblue", "darkgreen", "darkmagenta", "darkcyan", "darkorange")) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  theme_custom

ggsave("plots/pro_EU_parties.png", pro_EU_parties, width = 8, height = 4)

#### Pro-Ukraine 

EUI_data_short <- EUI_data_short %>% 
  mutate(Pro_Ukraine = ifelse(UA_EU > 5, 1, 0))


Ukraine_models <- list()
Ukraine_models[["m1"]] <- lm(reformulate(c("Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Ukraine"),
                        data = EUI_data_short)
Ukraine_models[["m2"]] <- lm(reformulate(c("Q62_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Ukraine"),
                        data = EUI_data_short)
Ukraine_models[["m3"]] <- lm(reformulate(c("GAL_TAN_index_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Ukraine"),
                        data = EUI_data_short)
# Ukraine_models[["m4"]] <- lm(reformulate(c("Q62_01", "Urban", CONTROLS, "as.factor(Year)", "country"), response = "UA_EU"),
#                         data = EUI_data_short)
Ukraine_models[["m5"]] <- lm(reformulate(c("Q62_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Ukraine"),
                        data = EUI_data_short)
Ukraine_models[["m6"]] <- lm(reformulate(c("Q62_01", "GAL_TAN_index_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "Pro_Ukraine"),
                        data = EUI_data_short)



Ukraine_models_df <- data.frame()
Ukraine_models_rsquare <- data.frame()
for(i in 1:length(Ukraine_models)){
  
  temp <- broom::tidy(Ukraine_models[[i]], conf.int = TRUE) %>% 
    mutate(Model = names(Ukraine_models[i]))
  
  Ukraine_models_df <- rbind(Ukraine_models_df, temp)
  
  temp2 <- summary(Ukraine_models[[i]])$adj.r.squared
  
  Ukraine_models_rsquare[i, "R.Squared"] <- temp2
  Ukraine_models_rsquare[i, "Model"] <- names(Ukraine_models[i])
}

pro_ukraine_plot <- Ukraine_models_df %>% 
  left_join(Ukraine_models_rsquare) %>% 
  filter(term %in% c("Security_FA_01", "Q62_01", "GAL_TAN_index_01")) %>% 
  mutate(term = case_match(term,
                          "Security_FA_01" ~ "Security-Collaboration",
                          "Q62_01" ~ "Left-Right",
                          "GAL_TAN_index_01" ~ "GAL-TAN Index",
                        #  "UrbanUrban/Suburban" ~ "Urban/Suburban"
  ),
  term = factor(term, levels = rev(c("Security-Collaboration", "Left-Right", "GAL-TAN Index", "Urban/Suburban"))),
  Model = case_match(Model, 
                     "m1" ~ paste0("Security-Colloboration Group \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m2" ~ paste0("Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m3" ~ paste0("GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")"),
                    # "m4" ~ paste0("Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m5" ~ paste0("Security-Colloboration Group + Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m6" ~ paste0("Security-Colloboration Group + Ideology +\n GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")")),
  Model = factor(Model, levels = rev(c("Security-Colloboration Group \n(R Squared = 0.331)", "Ideology \n(R Squared = 0.304)", "GAL-TAN Index \n(R Squared = 0.312)",
                                       "Ideology + Urban \n(R Squared = 0.253)", "Security-Colloboration Group + Ideology \n(R Squared = 0.331)", 
                                       "Security-Colloboration Group + Ideology +\n GAL-TAN Index \n(R Squared = 0.337)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey60") + 
  labs(x = "Probability of Voting for a Pro-Ukraine Party", 
       y = NULL,
       col = NULL) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "darkmagenta", "darkcyan", "darkorange")) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  theme_custom

ggsave("plots/pro_ukraine_plot.png", pro_ukraine_plot, width = 8, height = 4)


##### Look at pastvote ####

EUI_data_short %>% 
  group_by(family) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!is.na(family)) %>% 
  mutate(prop = n/sum(n) * 100,
         family = as.numeric(family),
         family = case_match(family, 
                             1 ~ "Radial Right",
         2 ~ "Conservative",
         3 ~ "Liberal",
         4 ~ "Christain Democratic",
         5 ~ "Socialist",
         6 ~ "Radial Left",
         7 ~ "Green",
         8 ~ "Regional", 
         9 ~ "No Family",
         10 ~ "Confessional", 
         11 ~ "Agrarian/Centre")
         )

#### GAL-TAN Parties ####

EUI_data_short <- EUI_data_short %>% 
  mutate(gal = ifelse(galtan > 5, 1, 0))

GAL_models <- list()
GAL_models[["m1"]] <- lm(reformulate(c("Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "gal"),
                             data = EUI_data_short)
GAL_models[["m2"]] <- lm(reformulate(c("Q62_01", CONTROLS, "as.factor(Year)", "country"), response = "gal"),
                             data = EUI_data_short)
GAL_models[["m3"]] <- lm(reformulate(c("GAL_TAN_index_01", CONTROLS, "as.factor(Year)", "country"), response = "gal"),
                             data = EUI_data_short)
# GAL_models[["m4"]] <- lm(reformulate(c("Q62_01", "Urban", CONTROLS, "as.factor(Year)", "country"), response = "galtan"),
#                              data = EUI_data_short)
GAL_models[["m5"]] <- lm(reformulate(c("Q62_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "gal"),
                             data = EUI_data_short)
GAL_models[["m6"]] <- lm(reformulate(c("Q62_01", "GAL_TAN_index_01", "Security_FA_01", CONTROLS, "as.factor(Year)", "country"), response = "gal"),
                             data = EUI_data_short)



GAL_models_df <- data.frame()
GAL_models_rsquare <- data.frame()
for(i in 1:length(GAL_models)){
  
  temp <- broom::tidy(GAL_models[[i]], conf.int = TRUE) %>% 
    mutate(Model = names(GAL_models[i]))
  
  GAL_models_df <- rbind(GAL_models_df, temp)
  
  temp2 <- summary(GAL_models[[i]])$adj.r.squared
  
  GAL_models_rsquare[i, "R.Squared"] <- temp2
  GAL_models_rsquare[i, "Model"] <- names(GAL_models[i])
}

GAL_TAN_plot <- GAL_models_df %>% 
  left_join(GAL_models_rsquare) %>% 
  filter(term %in% c("Security_FA_01", "Q62_01", "GAL_TAN_index_01")) %>% 
  mutate(term = case_match(term,
                           "Security_FA_01" ~ "Security-Collaboration",
                           "Q62_01" ~ "Left-Right",
                           "GAL_TAN_index_01" ~ "GAL-TAN Index",
                        #   "UrbanUrban/Suburban" ~ "Urban/Suburban"
  ),
  term = factor(term, levels = rev(c("Security-Collaboration", "Left-Right", "GAL-TAN Index", "Urban/Suburban"))),
  Model = case_match(Model, 
                     "m1" ~ paste0("Security-Colloboration Group \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m2" ~ paste0("Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m3" ~ paste0("GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")"),
                   #  "m4" ~ paste0("Ideology + Urban \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m5" ~ paste0("Security-Colloboration Group + Ideology \n", "(R Squared = ", round(R.Squared, 3), ")"),
                     "m6" ~ paste0("Security-Colloboration Group + Ideology +\n GAL-TAN Index \n", "(R Squared = ", round(R.Squared, 3), ")")),
  Model = factor(Model, levels = rev(c("Security-Colloboration Group \n(R Squared = 0.178)", "Ideology \n(R Squared = 0.34)", "GAL-TAN Index \n(R Squared = 0.247)",
                                       "Security-Colloboration Group + Ideology \n(R Squared = 0.35)", 
                                       "Security-Colloboration Group + Ideology +\n GAL-TAN Index \n(R Squared = 0.375)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey60") + 
  labs(x = "Probability of Voting for a GAL Party", 
       y = NULL,
       col = NULL) +
  scale_colour_manual(values = c("darkblue", "darkred", "darkmagenta", "darkcyan", "darkorange")) + 
  guides(colour =  guide_legend(reverse = TRUE,
                                ncol = 2)) +
  theme_custom

ggsave("plots/GAL_TAN_plot.png", GAL_TAN_plot, width = 8, height = 4)


#### Compare Party Positions ####

Party_distribution <- EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(EU_party = mean(eu_position),
            Russia_party = 10 - mean(Kremlin_ties),
            Past_vote = mean(Past_vote)) %>% 
  ungroup() %>% 
  filter(!is.na(EU_party)) %>% 
  mutate(country = factor(country, levels = c( "Belgium", "Bulgaria", "Croatia", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",    
                                               "Italy", "Lithuania", "Netherlands", "Poland", "Romania", "Slovakia", "Spain", "Sweden", "UK")),
         Party = case_when(Past_vote == 319 ~ "BSP, BG",
                           Past_vote == 42 ~ "5 Star, IT",
                           Past_vote == 35 ~ "EH Bildu, ES",
                           Past_vote == 44 ~ "Forza Italia, IT",
                           Past_vote == 1 ~ "Consevative Party, UK", 
                           Past_vote == 128 ~ "Revival, BG",
                           Past_vote == 72 ~ "EL-Ø, DK",
                           Past_vote == 60 ~ "FvD, NL",
                           Past_vote == 110 ~ "TS-LKD, LT",
                           Past_vote == 139 ~ "HDZ, HR",
                           Past_vote == 250 ~ "Reform, UK",
                           Past_vote == 92 ~ "PiS, PL",
                        
                           TRUE ~ NA
                           
                           )) %>% 
  ggplot(aes(x = EU_party, y = Russia_party, col = country)) + 
  geom_point() + 
  geom_vline(xintercept = 4, col = "grey40") + 
  geom_hline(yintercept = 5, col = "grey40") + 
  scale_colour_manual(values = c(
    "#FAE042",  # Belgium
    "#00966E",  # Bulgaria
    "#FF0000",  # Croatia
    "#C60C30",  # Denmark
    "#003580",  # Finland
    "#0055A4",  # France
    "#000000",  # Germany
    "#0D5EAF",  # Greece
    "#CE2939",  # Hungary
    "darkgreen",  # Italy
    "#FDB913",  # Lithuania
    "#AE1C28",  # Netherlands
    "#DC143C",  # Poland
    "#002B7F",  # Romania
    "#EE1C25",  # Slovakia
    "#AA151B",  # Spain
    "#006AA7",  # Sweden
    "#012169"   # UK
  )) + 
  geom_label_repel(aes(label = Party),
                   show.legend = FALSE,
                   fontface = "bold",
                   nudge_x = .1,
                   min.segment.length = unit(0, 'lines')) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  labs(x = "Party's Pro-EU Postion", 
       y = "Party's Pro-Russian Position",
       col = NULL) + 
  theme_custom 

ggsave("plots/Russian_EU_position.png", Party_distribution, width = 8, height = 6)

test <- EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(Pro_Russia_individual = mean(Security_FA),
            Russia_party = 10 - mean(Kremlin_ties),
            Past_vote = mean(Past_vote))

lm(formula = Russia_party ~ Pro_Russia_individual, data = test)

security_party_position <- EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(Pro_Russia_individual = mean(Security_FA),
            Russia_party = mean(Securtiy_FA_party, na.rm = TRUE),
            Past_vote = mean(Past_vote)) %>% 
  ungroup() %>% 
  filter(!is.na(Russia_party)) %>% 
  mutate(country = factor(country, levels = c( "Belgium", "Bulgaria", "Croatia", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",    
                                               "Italy", "Lithuania", "Netherlands", "Poland", "Romania", "Slovakia", "Spain", "Sweden", "UK")),
         Party = case_when(Past_vote == 319 ~ "BSP, BG",
                           Past_vote == 320 ~ "	DPS, BG",
                           Past_vote == 42 ~ "5 Star, IT",
                           Past_vote == 35 ~ "EH Bildu, ES",
                           Past_vote == 44 ~ "Forza Italia, IT",
                           Past_vote == 1 ~ "Consevative Party, UK", 
                           Past_vote == 128 ~ "Revival, BG",
                           Past_vote == 72 ~ "EL-Ø, DK",
                           Past_vote == 60 ~ "FvD, NL",
                           Past_vote == 110 ~ "TS-LKD, LT",
                           Past_vote == 139 ~ "HDZ, HR",
                           Past_vote == 250 ~ "Reform, UK",
                           Past_vote == 59 ~ "Denk, DK",
                           Past_vote == 92 ~ "PiS, PL",
                           Past_vote == 24 ~ "Die Linke, DE",
                           Past_vote == 94 ~ "KWiN, PL",
                           Past_vote == 54 ~ "SP, NL",
                           Past_vote == 271 ~ "ITN, BG",
                    
                           TRUE ~ NA
                           
         )) %>% 
  ggplot(aes(x = Pro_Russia_individual, y = Russia_party, col = country, text = Past_vote)) + 
  geom_point(size = 2) + 
 # geom_hline(yintercept = 5, col = "grey40") + 
  scale_colour_manual(values = c(
   # "#FAE042",  # Belgium
    "#00966E",  # Bulgaria
    "#FF0000",  # Croatia
    "#C60C30",  # Denmark
    "#003580",  # Finland
    "#0055A4",  # France
    "#000000",  # Germany
    "#0D5EAF",  # Greece
    "#CE2939",  # Hungary
    "darkgreen",  # Italy
    "#FDB913",  # Lithuania
    "#AE1C28",  # Netherlands
    "#DC143C",  # Poland
    "#002B7F",  # Romania
    "#EE1C25",  # Slovakia
    "#AA151B",  # Spain
    "#006AA7",  # Sweden
    "#012169"   # UK
  )) + 
  geom_label_repel(aes(label = Party),
                   show.legend = FALSE,
                   fontface = "bold",
                   nudge_x = .1,
                   min.segment.length = unit(0, 'lines')) + 
  scale_x_continuous( breaks = seq(-1, 1.3, length.out = 9),
                      limits = c(-1, 1.3), 
                      labels = c("", "Highest Defence Focus",  "", "", "- Relations with Russia -", "", "", "Highest Normalization Focus", ""))  + 
  labs(x = "Average Defence-Normalization Index Score of Supporters", 
       y = "Party's Position on the Defence Normalization Index",
       col = NULL) + 
  theme_custom + 
  theme(legend.position = "none")

ggsave("plots/security_party_position.png", security_party_position, width = 8, height = 5)
test <- EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(Pro_Russia_individual = mean(Security_FA),
            EU_party = mean(eu_position),
            Past_vote = mean(Past_vote)) %>% 
  ungroup()

lm(EU_party ~ Pro_Russia_individual, data = test)

EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(Pro_Russia_individual = mean(Security_FA),
            EU_party = mean(eu_position),
            Past_vote = mean(Past_vote)) %>% 
  ungroup() %>% 
  filter(!is.na(EU_party)) %>% 
  mutate(country = factor(country, levels = c( "Belgium", "Bulgaria", "Croatia", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",    
                                               "Italy", "Lithuania", "Netherlands", "Poland", "Romania", "Slovakia", "Spain", "Sweden", "UK")),
         Party = case_when(Past_vote == 319 ~ "BSP, BG",
                           Past_vote == 42 ~ "5 Star, IT",
                           Past_vote == 35 ~ "EH Bildu, ES",
                           Past_vote == 44 ~ "Forza Italia, IT",
                           Past_vote == 1 ~ "Consevative Party, UK", 
                           Past_vote == 128 ~ "Revival, BG",
                           Past_vote == 72 ~ "EL-Ø, DK",
                           Past_vote == 60 ~ "FvD, NL",
                           Past_vote == 110 ~ "TS-LKD, LT",
                           Past_vote == 139 ~ "HDZ, HR",
                           Past_vote == 250 ~ "Reform, UK",
                           Past_vote == 59 ~ "Denk, DK",
                           Past_vote == 92 ~ "PiS, PL",
                           TRUE ~ NA
                           
         )) %>% 
  ggplot(aes(x = Pro_Russia_individual, y = EU_party, col = country, text = Past_vote)) + 
  geom_point() + 
  geom_hline(yintercept = 4, col = "grey40") + 
  scale_colour_manual(values = c(
    "#FAE042",  # Belgium
    "#00966E",  # Bulgaria
    "#FF0000",  # Croatia
    "#C60C30",  # Denmark
    "#003580",  # Finland
    "#0055A4",  # France
    "#000000",  # Germany
    "#0D5EAF",  # Greece
    "#CE2939",  # Hungary
    "darkgreen",  # Italy
    "#FDB913",  # Lithuania
    "#AE1C28",  # Netherlands
    "#DC143C",  # Poland
    "#002B7F",  # Romania
    "#EE1C25",  # Slovakia
    "#AA151B",  # Spain
    "#006AA7",  # Sweden
    "#012169"   # UK
  )) + 
  geom_label_repel(aes(label = Party),
                   show.legend = FALSE,
                   fontface = "bold",
                   nudge_x = .1,
                   min.segment.length = unit(0, 'lines')) + 
  geom_smooth(aes(x = Pro_Russia_individual, y = EU_party), method = "lm", inherit.aes = FALSE) + 
  scale_x_continuous( breaks = seq(-1, 1.2, length.out = 9),
                      limits = c(-1, 1.2), 
                      labels = c("", "Highest Defence Focus",  "", "", "- Relations with Russia -", "", "", "Highest Normalization Focus", ""))  + 
  labs(x = "Average Security-Collaboration Index Score of Supporters", 
       y = "Party's Pro-Russian Position",
       col = NULL) + 
  theme_custom 

ggsave("plots/security_party_position.png", security_party_position, width = 8, height = 6)
ggplotly(security_party_position)

# 319 = БСП – обединена левица
# 42 - Movimento 5 Stelle
# 35 = EH Bildu
# 44 = Forza Italia
  # 1 Conservative Party
  #128 Възраждане
  # 72 Enhedslisten - De Rød-Grønne
  # 60 = Forum voor Democratie
  # 110 = TS-LKD
 # Hrvatska demokratska zajednica, HDZ 139
  
range(EUI_data_short$eu_position, na.rm = TRUE)


#### Social Base of Cleavage ####

Social_base <- lm_robust(reformulate(c( CONTROLS, "as.factor(Year)", "country", "ideology"),
                                     response = "Security_FA"),
                         data = EUI_data_short %>%
                           filter(country %in% COUNTRIES_2022) %>% 
                           mutate(country = relevel(factor(country), "Italy"),
                                  Education = relevel(factor(Education), "Higher Secondary"))
                           )


Social_base_df <- Social_base %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!term %in% c("(Intercept)", "as.factor(Year)2023", "as.factor(Year)2024", "as.factor(Year)2025")) %>% 
  mutate(term = case_match(term,
                           "Age25-34" ~ "Age: 25-34 \n (Ref. 18-24)",
                           "Age35-44" ~ "Age: 35-44",
                           "Age45-54" ~ "Age: 45-54",
                           "Age55+" ~ "Age: 55+",
                           "WomanWoman" ~ "Gender: Woman",
                           "ideologyDon't Know" ~ "Ideology: DK",
                           "ideologyLeft-wing" ~ "Ideology: Left-wing \n (Ref. Centre)",
                           "ideologyRight-wing" ~ "Ideology: Right-wing",
                           "EducationLess than Primary" ~ "Education: Less than Primary \n (Ref. Secondary Education)",
                           "EducationTertiary" ~ "Education: Tertiary",
                           "UrbanUrban/Suburban" ~ "Urban/Suburban \n (Ref. Rural)",
                           "countryCroatia" ~ "Country: Croatia",
                           "countryBulgaria" ~ "Country: Bulgaria",
                           "countryDenmark" ~ "Country: Denmark",
                           "countryFinland" ~ "Country: Finland",
                           "countryFrance" ~ "Country: France",
                           "countryGermany" ~ "Country: Germany",
                           "countryGreece" ~ "Country: Greece",
                           "countryHungary" ~ "Country: Hungary",
                           "countryLithuania" ~ "Country: Lithuania",
                           "countryNetherlands" ~ "Country: Netherlands",
                           "countryPoland" ~ "Country: Poland",
                           "countryRomania" ~ "Country: Romania",
                           "countrySlovakia" ~ "Country: Slovakia",
                           "countrySpain" ~ "Country: Spain",
                           "countrySweden" ~ "Country: Sweden",
                           "countryUK" ~ "Country: UK"
                           )
        ) 


country_order <- Social_base_df %>% 
  filter(str_detect(term, "Country")) %>% 
  arrange(-estimate) %>% 
  pull(term)

Social_base_plot <- Social_base_df %>% 
  mutate(term = replace(term, term == "Country: Bulgaria", "Country: Bulgaria \n (Ref. Italy)"),
         term = factor(term, levels = rev(c("Age: 25-34 \n (Ref. 18-24)",
                                            "Age: 35-44",
                                            "Age: 45-54",
                                            "Age: 55+",
                                            "Gender: Woman",
                                            "Ideology: Left-wing \n (Ref. Centre)",
                                            "Ideology: Right-wing",
                                            "Ideology: DK",
                                            "Education: Less than Primary \n (Ref. Secondary Education)",
                                            "Education: Tertiary",
                                            "Urban/Suburban \n (Ref. Rural)",
                                            "Country: Bulgaria \n (Ref. Italy)",
                                            country_order[-1])))) %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) + 
  geom_point() +
  geom_linerange() + 
  geom_vline(xintercept = 0, lty = 4, col = "grey29") + 
  geom_hline(yintercept = c(16.5, 17.5, 19.5, 22.5, 23.5), col = "grey80", lty = "dotted") +
  scale_x_continuous( breaks = seq(-0.6, 0.6, length.out = 9),
                      labels = c("", "Defence",  "", "", "", "", "", "Normalization", "")) +
  labs(x = "OLS Estimates and 95% Confidence Intervals \n Higher Values Represent the more Normalization Position",
       y = NULL) + 
  theme_custom

ggsave("plots/Social_base_plot.png", Social_base_plot, width = 8, height = 8)
  
Social_group_year <- lmer(reformulate(c("( Econ_comparison + Radicalized + Employed + Woman + Education + Age + Urban) * as.factor(Year)", "(1 | country)"),
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
                                  Generalized_trust = relevel(factor(Generalized_trust), "Untrusting"))
)

Social_group <- lmer(reformulate(c("( Econ_comparison + Employed + Woman + Education + Age + median_income + Employed)", "as.factor(Year)", "(1 | country)"),
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
vif(Social_group)

social_group_vars <- c( "Econ_comparison", "Radicalized", "Employed", "Woman", "Education", "Age", "Urban")
Social_group_year_df <- data.frame()
for(i in 1:length(social_group_vars)){
df <- avg_slopes(Social_group_year, variables = social_group_vars[i], by = "Year")
Social_group_year_df <- bind_rows(Social_group_year_df, df)
print(i)
}

Trust_preds <- avg_predictions(Social_group_year, variables = c("Year", "Generalized_trust"))

Trust_preds %>% 
  filter(Generalized_trust != "Don't Know") %>% 
  ggplot(aes(x = estimate, y = Year, xmin = conf.low, xmax = conf.high, col = Generalized_trust)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) +
  theme_custom


group_colors <- c(
  "Age"         = "#004488",  # deep blue
  "Gender"      = "#BB5566",  # muted rose
  "Urban"       = "#6699CC",  # dark maroon (alt if too close to Gender: "#6699CC")
  "Ideology"    = "#117733",  # forest green
  "Comparision" = "#882255",  # plum
  "Employment"  = "#44AA99",  # teal
  "Education"   = "#332288",  # indigo
  "Income"      = "#88CCEE",  # light sky blue
  "Trust"       = "#555555"   # neutral grey
)

social_group_plot_year <- Social_group_year_df %>% 
  mutate(contrast = recode_values(contrast, 
                                  "Better off - The same" ~ "Comparision: Better off (Ref. The Same)",
                                  "Don't Know - The same" ~ "Comparision: Don't Know",
                                  "Worse off - The same" ~ "Comparision: Worse off",
                                  "Radical Left - Moderate" ~ "Ideology: Radical Left (Ref. Moderate)",
                                  "Radical Right - Moderate" ~ "Ideology: Radical Right",
                                  "Don't Know - Moderate" ~ "Ideology: Don't Know",
                                  "Unemployed - Employed" ~ "Employment: Unemployed (Ref. Employed)",
                                  "Student - Employed" ~ "Employment: Student",
                                  "Retired - Employed" ~ "Employment: Retired",
                                  "Other - Employed" ~ "Employment: Other",
                                  "Trusting - Untrusting" ~ "Trust: Trusting (Ref. Untrusting)",
                                  "Don't Know - Untrusting" ~ "Trust: Don't Know",
                                  "Woman - Man" ~ "Gender: Woman",
                                  "Less than Primary - Higher Secondary" ~ "Education: Less than Primary (Ref. Secondary Education)",
                                  "Tertiary - Higher Secondary" ~ "Education: Tertiary",
                                  "25-34 - 18-24" ~ "Age: 25-34 (Ref. 18-24)",
                                  "35-44 - 18-24" ~ "Age: 35-44",
                                  "45-54 - 18-24" ~ "Age: 45-54",
                                  "55+ - 18-24" ~ "Age: 55+",
                                  "Urban/Suburban - Other" ~ "Urban: Urban (Ref. Rural)"
                                  ),
         contrast = factor(contrast, levels = rev(c("Age: 25-34 (Ref. 18-24)",
                                            "Age: 35-44",
                                            "Age: 45-54",
                                            "Age: 55+",
                                            "Gender: Woman",
                                            "Urban: Urban (Ref. Suburban)",
                                            "Education: Less than Primary (Ref. Secondary Education)",
                                            "Education: Tertiary",
                                            "Employment: Unemployed (Ref. Employed)",
                                            "Employment: Student",
                                            "Employment: Retired",
                                            "Income: Above Median Income",
                                            "Comparision: Better off (Ref. The Same)",
                                            "Comparision: Worse off",
                                            "Ideology: Radical Left (Ref. Moderate)",
                                            "Ideology: Radical Right",
                                            "Ideology: Don't Know",
                                            "Trust: Trusting (Ref. Untrusting)"
                                            ))),
         group = str_extract(contrast, "^[^:]+"),
         Year = factor(as.factor(Year), levels = rev(c("2022", "2023", "2024", "2025")))) %>% 
  filter(!is.na(contrast)) %>% 
  ggplot(aes(x = estimate, y = contrast, xmin = conf.low, xmax = conf.high, col = Year)) + 
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) + 
  scale_colour_manual(values = c("darkblue", "darkred", "seagreen", "orange")) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey29") + 
  geom_hline(yintercept = c(1.5, 3.5, 6.5, 8.5, 11.5, 12.5), col = "grey80", lty = "dotted") +
  scale_x_continuous( breaks = seq(-0.3, 0.4, length.out = 8),
                      labels = c("-0.3", "-0.25 (Most Defence Focused)",  "", "0.0", "0.1", "", "", "0.5 (Most Normalization Focused)")) +
  labs(x = "MLM Estimates and 95% Confidence Intervals \n Higher Values Represent the more Normalization Position",
       y = NULL,
       col = "Year") + 
  guides(colour = guide_legend(reverse = TRUE)) + 
  theme_custom + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) 
  
ggsave("plots/social_group_plot_year.png", social_group_plot_year, width = 12, height = 8)

Social_group_df <- Social_group %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!term %in% c("(Intercept)", "as.factor(Year)2023", "as.factor(Year)2024", "as.factor(Year)2025", "IndustryUnemployed")) %>% 
  filter(str_starts(term, "country", negate = TRUE)) %>% 
  mutate(term = case_match(term,
                           "AgeGen Z" ~ "Age: Gen Z (Ref. Cold War+)",
                           "AgeSocialized during the Cold War" ~ "Age: Socialized during the Cold War",
                           "AgeYoung Milenials" ~ "Age: Young Milenials",
                           "AgeTransition Generation" ~ "Age: Transition Generation",
                           "AgeCold War Generation" ~ "Age: Cold War Generation",
                           "Former_soviet" ~ "Location: Former Eastern Bloc Countries",
                           "WomanWoman" ~ "Gender: Woman",
                           "UrbanUrban/Suburban" ~ "Urban: Urban (Ref. Suburban)",
                           "RadicalizedRadical Left" ~ "Ideology: Radical Left (Ref. Moderate)",
                           "RadicalizedRadical Right" ~ "Ideology: Radical Right",
                           "RadicalizedDon't Know" ~ "Ideology: Don't Know",
                           "Econ_comparisonBetter off" ~ "Comparision: Better off (Ref. The Same)",
                           "median_income" ~ "Income: Above Median Income",
                           "Econ_comparisonWorse off" ~ "Comparision: Worse off",
                           "EmployedUnemployed" ~ "Employment: Unemployed (Ref. Employed)",
                           "EmployedStudent" ~ "Employment: Student",
                           "EmployedRetired" ~ "Employment: Retired",
                           #"EmployedOther" ~ "Employment: Other",
                           "EducationLess than Primary" ~ "Education: Less than Primary (Ref. Secondary Education)",
                           "EducationTertiary" ~ "Education: Tertiary",
                           "Above_median" ~ "Income: Above Median Income",
                           "Generalized_trustTrusting" ~ "Trust: Trusting (Ref. Untrusting)",
                          # "Generalized_trustDon't Know" ~ "Trust: Don't Know"
  )
  ) 


Social_group_plot <- Social_group_df %>% 
  mutate(
         term = factor(term, levels = rev(c("Age: Gen Z (Ref. Cold War Adults)",
                                            "Age: Young Milenials",
                                            "Age: Transition Generation",
                                            "Age: Cold War Children",
                                            "Location: Former Eastern Bloc Countries",
                                            "Gender: Woman",
                                            "Urban: Urban (Ref. Suburban)",
                                            "Ideology: Radical Left (Ref. Moderate)",
                                            "Ideology: Radical Right",
                                            "Ideology: Don't Know",
                                            "Comparision: Better off (Ref. The Same)",
                                            "Comparision: Don't Know",
                                            "Comparision: Worse off",
                                            "Income: Above Median Income",
                                            "Employment: Unemployed (Ref. Employed)",
                                            "Employment: Student",
                                            "Employment: Retired",
                                            "Employment: Other",
                                            "Education: Less than Primary (Ref. Secondary Education)",
                                            "Education: Tertiary",
                                            "Trust: Trusting (Ref. Untrusting)",
                                            "Trust: Don't Know"))),
         group = str_extract(term, "^[^:]+"),
         term = str_remove(term, "^[^:]*:\\s*")
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
  labs(x = "MLM Estimates and 95% Confidence Intervals \n Higher Values Represent the more Normalization Position",
       y = NULL) + 
  theme_custom + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none",
        plot.margin = margin(1,1,1,1, "cm")) 
  


ggsave("plots/Social_group_plot.png", Social_group_plot, width = 12, height = 6)

Social_base_countries <- lm_robust(reformulate(c(paste0("(", "Above_median +",  "Econ_comparison +", "Radicalized +", "Employed +", "Generalized_trust +", "Woman +", "Education +", "Age +", "Urban", ")", "*country"), "as.factor(Year)"),
                                     response = "Security_FA"),
                         data = EUI_data_short %>%
                           filter(country %in% COUNTRIES_2022) %>% 
                           mutate(country = relevel(factor(country), "Italy"),
                                  Radicalized = relevel(factor(Radicalized), "Moderate"),
                                  Education = relevel(factor(Education), "Higher Secondary"),
                                  Econ_comparison = relevel(factor(Econ_comparison), "The same"),
                                  Industry = relevel(factor(Industry), "White Collar"),
                                  religion = relevel(factor(religion), "Christian"),
                                  Employed = relevel(factor(Employed), "Employed"),
                                  Generalized_trust = relevel(factor(Generalized_trust), "Untrusting")
                                  )
)

Age_country_df <- avg_predictions(Social_base_countries, variables = c("Age", "country"))
SODEMS <- c( "Above_median",  "Econ_comparison", "Radicalized", "Employed", "Generalized_trust", "Woman", "Education", "Age", "Urban")
social_base_list <- vector("list", length(SODEMS))
social_base_country_df <- data.frame()

country_order <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE)) %>% 
  arrange(-Mean) %>% 
  pull(country)

Age_by_country <- Age_country_df %>% 
  mutate(Age = recode_values(Age, "18-24" ~ "Gen Z",
                             "25-34" ~ "Young Milenials",
                             "35-44" ~ "Transition Generation",
                             "45-54" ~ "Cold War Children",
                             "55+" ~ "Cold War Adults"),
         Age = factor(Age, levels = rev(c("Gen Z",
                                      "Young Milenials",
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

ggsave("plots/Age_by_country.png", Age_by_country, width = 10, height = 6)

for (i in seq_along(SODEMS)) {
  social_base_list[[i]] <- Social_base_countries %>%
    avg_slopes(variables = SODEMS[i], by = "country")
  
  print(paste0("Completed:", SODEMS[i], "(",i, "/", length(SODEMS),")"))
}

social_base_df <- bind_rows(social_base_list)

social_base_country_plot <- social_base_country_df %>% 
  mutate(country = factor(country, country_order),
         contrast = case_match(contrast,
                           "25-34 - 18-24" ~ "Age: 25-34 \n (Ref. 18-24)",
                           "35-44 - 18-24" ~ "Age: 35-44",
                           "45-54 - 18-24" ~ "Age: 45-54",
                           "55+ - 18-24" ~ "Age: 55+",
                           "Woman - Man" ~ "Gender: Woman",
                           "Less than Primary - Higher Secondary" ~ "Education: Primary \n (Ref. Higher Secondary)",
                           "Tertiary - Higher Secondary" ~ "Education: Tertiary",
                           "Urban/Suburban - Other"  ~ "Urban/Suburban \n (Ref. Rural)"),
         contrast = factor(contrast, levels = rev(c("Age: 25-34 \n (Ref. 18-24)",
                                        "Age: 35-44",
                                        "Age: 45-54",
                                        "Age: 55+",
                                        "Gender: Woman",
                                        "Education: Primary \n (Ref. Higher Secondary)",
                                         "Education: Tertiary",
                                        "Urban/Suburban \n (Ref. Rural)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = contrast)) + 
  geom_point() +
  geom_linerange() + 
  geom_vline(xintercept = 0, col = "grey40", lty = 4) + 
  facet_wrap(~country) + 
  labs(x = "OLS Estimates and 95% Confidendence Intervals",
       y = NULL) +
  scale_x_continuous( breaks = seq(-0.6, 0.6, length.out = 9),
                                          labels = c("", "Defence",  "", "", "", "", "", "Normalization", "")) + 
  theme_custom

ggsave("plots/social_base_country_plot.png", width = 15, height = 9)


#### Ideology Country ####

Ideology_model <- lm_robust(reformulate(c(CONTROLS, "Radicalized * country", "as.factor(Year)"), response = "Security_FA"), data = EUI_data_short)

Ideology_model_df <- avg_predictions(Ideology_model, variables = c("Radicalized", "country"))


ideology_country <- Ideology_model_df %>% 
  filter(Radicalized != "Don't Know") %>% 
  mutate(country = factor(country, levels = country_order),
         Radicalized = factor(Radicalized, levels = rev(c("Radical Left", "Moderate", "Radical Right")))) %>% 
  ggplot(aes(x = estimate, y = Radicalized, xmin = conf.low, xmax = conf.high)) + 
  geom_point() + 
  geom_linerange() +
  facet_wrap(~country) + 
  labs(x = "Predicted Value on the Defence-Normalization Dimension \n (Higher values indicate more defence focused)",
       caption = "Countries ordered by average defence-normalization index score",
       y = NULL) + 
  theme_custom

ggsave("plots/ideology_country.png", ideology_country, width = 8, height = 8)

data_social_base <- EUI_data_short %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(
         Radicalized = relevel(factor(Radicalized), "Moderate"),
         Education = relevel(factor(Education), "Higher Secondary"),
         Econ_comparison = relevel(factor(Econ_comparison), "The same"),
         Industry = relevel(factor(Industry), "White Collar"),
         religion = relevel(factor(religion), "Christian"),
         Employed = relevel(factor(Employed), "Employed"),
         Generalized_trust = relevel(factor(Generalized_trust), "Untrusting"),
         Former_soviet = ifelse(country %in% c("Bulgaria","Czech Republic", "Slovakia", 
                                               "Hungary", "Poland", "Romania", "Estonia",
                                               "Latvia", "Lithuania"), 1, 0))
#### Subjective Economic Position by Country #####
econ_position_mod <- lm(reformulate(c("Econ_comparison * country", "( Employed + Woman + Education + Age + median_income)", "as.factor(Year)"),
                                 response = "Security_FA"),
                     data = data_social_base 
                       ,
                     weights = balanced_weights
)
econ_position_df <- avg_predictions(econ_position_mod, variables = c("Econ_comparison", "country"))


econ_position_country <- econ_position_df %>% 
  filter(Econ_comparison != "Don't Know") %>% 
  mutate(country = factor(country, levels = country_order),
         Econ_comparison = factor(Econ_comparison, levels = rev(c("Worse off", "The same", "Better off")))) %>% 
  ggplot(aes(x = estimate, y = Econ_comparison, xmin = conf.low, xmax = conf.high)) + 
  geom_point() + 
  geom_linerange() +
  facet_wrap(~country) + 
  labs(x = "Predicted Value on the Defence-Normalization Dimension \n (Higher values indicate more defence focused)",
       caption = "Countries ordered by average defence-normalization index score",
       y = NULL) + 
  theme_custom +
  theme(plot.margin = margin(1,1,1,1, "cm"))

ggsave("plots/econ_position_country.png", econ_position_country, width = 8, height = 8)
#### Affective Polarization ####

affective_polarization_model <- lm_robust(reformulate(c("as.factor(Affective_Polarization)",
                        CONTROLS, "as.factor(Year)", "country"), response = "Security_FA"), data = EUI_data_short)

affective_polarization_model_df <- affective_polarization_model %>% 
  avg_predictions(variables = "Affective_Polarization") 


affective_polarization_model_plot <- affective_polarization_model_df %>% 
  as.data.frame() %>% 
  mutate(Affective_Polarization = replace_values(as.character(Affective_Polarization),
                                                 "0" ~ "Not Affectively Polarized at All",
                                                 "10" ~ "Very Affectively Polarized"),
         Affective_Polarization = factor(Affective_Polarization, rev(c("Not Affectively Polarized at All",
                                                                       as.character(seq(1, 9, 1)),
                                                                       "Very Affectively Polarized")))) %>% 
  ggplot(aes(y = Affective_Polarization, x = estimate, xmin = conf.low, xmax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  scale_x_continuous(breaks = seq(-0.25, 0, length.out = 9),
                     labels = c("", "Defence",  "", "", "", "", "Normalization", "", "")
                     ) + 
  labs(x = "Predicted values of the defence-normalization index \n by level of affective polarization", y = NULL) + 
  theme_custom

ggsave("plots/affective_polarization.png",affective_polarization_model_plot, width = 8, height = 5)
