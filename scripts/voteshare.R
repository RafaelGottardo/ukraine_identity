#### Vote share overtime ####

polls_df <- read.csv("data_clean/politico_polls.csv")

country_colors <- c(
  "UK"          = "#00247D",  # navy blue
  "Denmark"     = "#C60C30",  # red
  "Greece"      = "#0D5EAF",  # blue
  "Hungary"     = "#436F4D",  # green
  "Lithuania"   = "#FDB913",  # yellow
  "Italy"       = "#008C45",  # green
  "Poland"      = "#DC143C",  # crimson
  "Netherlands" = "#FF7900",  # orange
  "Romania"     = "#002B7F",  # blue
  "Slovakia"    = "#7B2D8B",  # purple
  "Croatia"     = "#FF0000",  # red
  "Bulgaria"    = "#00966E",  # teal green
  "Spain"       = "#AA151B",  # deep red
  "Finland"     = "#003580",  # dark blue
  "France"      = "#8B5A2B",  # brown
  "Germany"     = "#1A1A1A",  # near-black
  "Sweden"      = "#FFD700"   # gold
)

Polls_overtime <- polls_df %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(Normalization_party = ifelse(eu_russia > 5, "Normalization-Focused", "Defence-Focused")) %>% 
  pivot_longer(c("poll_2022_04", "poll_2023_04", "poll_2024_04", "poll_2025_04", "poll_2026_04"),
               names_to = "Poll_time", values_to = "Predicted_vote_share") %>% 
  mutate(Poll_time = case_match(Poll_time, "poll_2022_04" ~ "April 2022",
                                "poll_2023_04" ~ "April 2023",
                                "poll_2024_04" ~ "April 2024",
                                "poll_2025_04" ~ "April 2025",
                                "poll_2026_04" ~ "April 2026")) %>% 
  group_by(country, Poll_time, Normalization_party) %>% 
  summarize(Mean_poll = mean(Predicted_vote_share, na.rm = TRUE)) %>% 
  mutate(Mean_poll = Mean_poll/100) %>%  
  ggplot(aes(x = Poll_time, y = Mean_poll, col = country, group = country)) + 
  geom_point(size = 2) + 
  geom_line(linewidth = 0.8) +
  geom_text_repel(
    data = ~ filter(., Poll_time == "April 2025"),   # only label the last point
    aes(label = country),
    hjust = 0,
    direction = "y",           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.4,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5
  ) +
  labs(x = "Date of Poll", y = "Percentage Polling") + 
    scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = country_colors) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.3))) +  # extra room on right for labels
  guides(color = "none") +   # labels replace the legend now
  theme_custom
  

polling_parties <- polls_df %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(Normalization_party = ifelse(eu_russia > 5, "Normalization-Focused", "Defence-Focused")) %>% 
  pivot_longer(c("poll_2022_04", "poll_2023_04", "poll_2024_04", "poll_2025_04", "poll_2026_04"),
               names_to = "Poll_time", values_to = "Predicted_vote_share") %>% 
  mutate(Poll_time = case_match(Poll_time, "poll_2022_04" ~ "2022",
                                "poll_2023_04" ~ "2023",
                                "poll_2024_04" ~ "2024",
                                "poll_2025_04" ~ "2025",
                                "poll_2026_04" ~ "2026")) %>% 
  group_by(country, Poll_time, Normalization_party) %>% 
  ggplot(aes(x = Poll_time, y = Predicted_vote_share, group = party_name_english, col = Normalization_party)) + 
  geom_point(size = 2) + 
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("darkblue", "darkred")) +
  facet_wrap(~country) +
  theme_custom
  
ggsave("plots/polling_parties.png", polling_parties, width = 12, height = 12)

Difference_kremlin <- polls_df %>% 
 group_by(country) %>% 
  mutate(Diff = as.numeric(Kremlin_ties) - mean(as.numeric(Kremlin_ties), na.rm = TRUE)) %>% 
  summarize(Mean_Diff = mean(abs(Diff), na.rm = TRUE)) 

country_order <- Difference_kremlin %>% 
  arrange(Mean_Diff) %>% 
  pull(country)
  
  
Difference_kremlin %>% 
  filter(Mean_Diff != 0) %>% 
  mutate(country = factor(country, levels = country_order)) %>% 
  ggplot(aes(x = Mean_Diff, y = country)) + 
  geom_point()

Polarization_defence_country <- EUI_data_short %>% 
  group_by(country, Year) %>% 
  summarise(Spread = sd(Security_FA, na.rm = TRUE),
         Kremlin_ties = sd(Kremlin_ties, na.rm = TRUE)) 

polarization_def_kremlin <- Polarization_defence_country %>% 
ggplot(aes(x = Kremlin_ties, y = Spread)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm") +
  # geom_text_repel(
  #   data = ~ filter(., Year == 2025),   # only label the last point
  #   aes(label = country),
  #   hjust = 0,
  #   direction = "y",           # only nudge vertically, keeps labels aligned to their point
  #   nudge_x = 0.4,             # push labels to the right of the last point
  #   xlim = c(-Inf, Inf),
  #   segment.size = 0.3,
  #   segment.color = "grey50",
  #   size = 3.5
  # ) +
  labs(x = "Standard Devaition of Parties Pro-Kremlim Attitudes", y = "Standard Devaition of the Defence-Normalization Index",
       caption = "Each point represents a country year obsevation") + 
 # scale_y_continuous(labels = scales::percent) + 
  #scale_color_manual(values = country_colors) +
  guides(color = "none") +   # labels replace the legend now
  theme_custom
  

ggsave("plots/polarization_def_kremlin.png", polarization_def_kremlin, width = 8, height = 5)


#### Group Parties ####
EUI_data_short$Immigration_types_23_3
EUI_data_short <- EUI_data_short %>% 
  mutate(Party_position = case_when(Kremlin_ties < 5 & galtan < 5 ~ "Left-wing pro-Russia",
                                    Kremlin_ties >= 5 & galtan < 5 ~ "Left-wing anti-Russia",
                                    Kremlin_ties < 5 & galtan >= 5 ~ "Right-wing pro-Russia",
                                    Kremlin_ties >= 5 & galtan >= 5 ~ "Right-wing anti-Russia"),
         Immigration_support = (Immigration_types_23_1 + Immigration_types_23_2 + Immigration_types_23_3 + Immigration_types_23_4)/4,
         Nationalist = ifelse(Q4 == 1, 1, 0))
 
ches_data <- ches_data %>% 
  mutate(Party_position = case_when(Kremlin_ties < 5 & galtan < 5 ~ "Left-wing pro-Russia",
                                    Kremlin_ties >= 5 & galtan < 5 ~ "Left-wing anti-Russia",
                                    Kremlin_ties < 5 & galtan >= 5 ~ "Right-wing pro-Russia",
                                    Kremlin_ties >= 5 & galtan >= 5 ~ "Right-wing anti-Russia"))

party_type_models_df <- data.frame()
PARTY_OUTCOME_VARS <- c("Q71", "Q5", "Immigration_support", "Nationalist")
for(i in 1:length(PARTY_OUTCOME_VARS)){
  mod <- lmer(reformulate(c("Party_position", CONTROLS, "as.factor(Year)", "(1 | country)"), response = PARTY_OUTCOME_VARS[i]),
            data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
            weights = balanced_weights)
  
  df <- tidy(mod, conf.int = TRUE) %>% 
    mutate(Variable = PARTY_OUTCOME_VARS[i])
  
  party_type_models_df <- bind_rows(party_type_models_df, df)
}


party_type_correlates <- party_type_models_df %>% 
  filter(str_starts(term, "Party_position")) %>% 
  mutate(Variable = recode_values(Variable, "Q71" ~ "Importance of NATO",
                                  "Q5" ~ "Support for Democracy",
                                  "Immigration_support" ~ "Support for Immigration",
                                  "Nationalist" ~ "Nationalism"
                                  ),
         Variable = factor(Variable, levels = rev(c("Importance of NATO", "Support for Democracy",
                                                "Support for Immigration", "Nationalism"))),
         term = recode_values(term,
                              "Party_positionRight-wing anti-Russia" ~ "Right-wing Anti-Russia",
                              "Party_positionLeft-wing pro-Russia" ~ "Left-wing Pro-Russia",
                              "Party_positionRight-wing pro-Russia" ~ "Right-wing Pro-Russia"
                              ),
         term = factor(term, levels = c("Right-wing Anti-Russia", "Left-wing Pro-Russia", "Right-wing Pro-Russia"))) %>% 
  ggplot(aes(x = estimate, y = Variable, xmin = conf.low, xmax = conf.high, col = term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = c("green4", "orange3", "red4")) + 
  labs(colour = "Party Supported \n (Ref. Left-wing Anti-Russia)",
       x = "MLM estimate and 95% confidence intervals", 
       y = "Outcome Variable",
       caption = "Model includes only countries surveyed since 2022 and country and year fixed effects."
       ) +
  guides(colour = guide_legend(ncol = 1)) +
  scale_x_continuous(labels = c("-0.75", "-0.5", "-0.25", "0.0", "0.25"),
                     breaks = c(-0.75, -0.5, -0.25, 0, 0.25)) + 
  theme_custom

ggsave("plots/party_type_correlates.png", party_type_correlates, width = 8, height = 4)
#### By individual level attitudes 

EUI_data_short <- EUI_data_short %>% 
  mutate(individual_LW_PR = case_when(ideology == "Left-wing" & Security_FA > 0 ~ "Left-Wing Normalization-focused",
                                      ideology == "Right-wing" & Security_FA > 0 ~ "Right-Wing Normalization-focused",
                                      ideology == "Left-wing" & Security_FA <= 0 ~ "Left-Wing Security-focused",
                                      ideology == "Right-wing" & Security_FA <= 0 ~ "Right-Wing Security-focused",
                                      TRUE ~ "Centrist"
                                    
                                      ))
  
LW_proRussia_attitudes_df <- data.frame()
PARTY_OUTCOME_VARS <- c("Q71", "Q5", "Immigration_support", "Nationalist")
for(i in 1:length(PARTY_OUTCOME_VARS)){
  mod <- lmer(reformulate(c("individual_LW_PR", CONTROLS, "as.factor(Year)", "(1 | country)"), response = PARTY_OUTCOME_VARS[i]),
              data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
              weights = balanced_weights)
  
  df <- tidy(mod, conf.int = TRUE) %>% 
    mutate(Variable = PARTY_OUTCOME_VARS[i])
  
  LW_proRussia_attitudes_df <- bind_rows(LW_proRussia_attitudes_df, df)
}
LW_correlates_correlates <- LW_proRussia_attitudes_df %>% 
  filter(term %in% c("individual_LW_PRLeft-Wing Normalization-focused",
                     "individual_LW_PRLeft-Wing Security-focused",
                     "individual_LW_PRRight-Wing Normalization-focused",
                     "individual_LW_PRRight-Wing Security-focused")) %>% 
  mutate(Variable = recode_values(Variable, "Q71" ~ "Importance of NATO",
                                  "Q5" ~ "Support for Democracy",
                                  "Immigration_support" ~ "Support for Immigration",
                                  "Nationalist" ~ "Nationalism"
  ),
  Variable = factor(Variable, levels = rev(c("Importance of NATO", "Support for Democracy",
                                             "Support for Immigration", "Nationalism"))),
  term = recode_values(term, "individual_LW_PRLeft-Wing Normalization-focused" ~ "Left-wing Normalization-Focused",
                       "individual_LW_PRLeft-Wing Security-focused" ~ "Left-wing Security-Focused",
                       "individual_LW_PRRight-Wing Normalization-focused" ~ "Right-wing Normalizaation-Focused",
                       "individual_LW_PRRight-Wing Security-focused" ~ "Right-wing Security Focused"),
  term = factor(term, levels = c("Left-wing Normalization-Focused", 
                                 "Left-wing Security-Focused",
                                 "Right-wing Normalizaation-Focused",
                                 "Right-wing Security Focused"))) %>% 
  ggplot(aes(x = estimate, y = Variable, xmin = conf.low, xmax = conf.high, col = term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = c("black", "darkred", "purple4", "darkblue")) + 
  labs(colour = "Ideology and Placement on the Defence-Normalization Dimension",
       x = "Marginal effect of the defence-normalization dimension\nRef. Centrist", 
       y = "Outcome Variable",
       caption = "Model includes only countries surveyed since 2022 and country and year fixed effects."
  ) +
  guides(colour = guide_legend(ncol = 1)) +
  scale_x_continuous(labels = c("-0.75", "-0.5", "-0.25", "0.0", "0.25"),
                     breaks = c(-0.75, -0.5, -0.25, 0, 0.25)) + 
  theme_custom

ggsave("plots/LW_correlates_correlates.png", LW_correlates_correlates, width = 8, height = 4)

#### Party Switching ####

EUI_data_short <- EUI_data_short %>% 
  mutate(Right_wing_pro_russia = ifelse(Party_position == "Right-wing pro-Russia", 1, 0),
         Left_wing_pro_russia = ifelse(Party_position == "Left-wing pro-Russia", 1, 0),
         LW_RW_pro_russia = case_when(country %in% c("Bulgaria", "Greece", "Hungary",  "Lithuania",
                                                     "Poland", "Romania", "Slovakia") ~ "No Left-wing Pro Russia Party",
                                      country %in% c("Croatia", "Denmark", "Finland", "Sweden", "UK") ~ "No Pro Russia Party",
                                      TRUE ~ "Left and Right-wing Pro-Russia Party"))


LW_RW_country_pro_Russia_mod <- lm_robust(reformulate(c("(Security_FA * ideology * LW_RW_pro_russia)", CONTROLS, "as.factor(Year)"), 
                                           response = "Right_wing_pro_russia"), data = EUI_data_short)


No_LW_party_df <- avg_slopes(LW_RW_country_pro_Russia_mod, variables = "Security_FA", by = c("ideology", "LW_RW_pro_russia"))


No_LW_party_plot <- RW_pro_Russia_df %>% 
  filter(LW_RW_pro_russia != "No Pro Russia Party") %>% 
  mutate(
         ideology = factor(ideology, levels = c("Don't Know", "Left-wing", "Centre", "Right-wing"))) %>% 
  ggplot(aes(x = ideology, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  geom_hline(yintercept = 0, lty = 4, col = "grey60") + 
  facet_wrap(~LW_RW_pro_russia) + 
  labs(x = NULL, y = "Marginal Effect of being more Normalization focused") + 
  theme_custom

ggsave("plots/No_LW_party_plot.png", No_LW_party_plot, width = 8, height = 5)

RW_pro_Russia_mod <- lm_robust(reformulate(c("(Security_FA * ideology * country)", CONTROLS, "as.factor(Year)"), 
                                           response = "Right_wing_pro_russia"), data = EUI_data_short)


RW_pro_Russia_df <- avg_slopes(RW_pro_Russia_mod, variables = "Security_FA", by = c("ideology", "country"))


Voting_RW_PR_country <- RW_pro_Russia_df %>% 
  mutate(country = replace_values(country,
                                  "Croatia" ~ "Croatia (No PR)",
                                  "Denmark" ~ "Denmark (No PR)",
                                  "Finland" ~ "Finland (No PR)",
                                  "Greece" ~ "Greece (No LW-PR)",
                                  "Hungary" ~ "Hungary (No LW-PR)",
                                  "Lithuania" ~ "Lithuania (No LW-PR)",
                                  "Poland" ~ "Poland (No LW-PR)",
                                  "Romania" ~ "Romania (No LW-PR)",
                                  "Slovakia" ~ "Slovakia (No LW-PR)",
                                  "Sweden" ~ "Sweden (No PR)",
                                  "UK" ~ "UK (No PR)"),
  ideology = factor(ideology, levels = c("Don't Know", "Left-wing", "Centre", "Right-wing"))) %>% 
  ggplot(aes(x = ideology, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~country) + 
  theme_custom + 
  labs(x = NULL, y = "Marginal effect of being more normalization focused \n on the probability of supporting a right-wing \n pro-normalization party") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

  ggsave("plots/Voting_RW_PR_country.png", Voting_RW_PR_country, width = 8, height = 6)

##### Average Threat and Cleavage Strength #####

Defence_normalization_threat_df <- EUI_data_short %>% 
  mutate(Russia_threat = ifelse(Q68 == 2, 1, 0)) %>% 
  group_by(country) %>% 
  summarize(Cleavage_strength = mean(Security_FA, na.rm = TRUE),
            # Cleavage_strength_robust = mean(Security_FA_robust, na.rm = TRUE),
            Average_threat = mean(Russia_threat, na.rm = TRUE))

lm(Cleavage_strength ~ Average_threat, Defence_normalization_threat_df) %>% 
  summary()

lmer(reformulate(c("Russia_threat", CONTROLS, "as.factor(Year)", "(1 | country)"),
                 response = "Security_FA"), data = EUI_data_short %>% mutate(Russia_threat = ifelse(Q68 == 2, 1, 0))) %>% 
  summary()

Defence_normalization_threat <-  Defence_normalization_threat_df %>% 
  ggplot(aes(x = Average_threat, y = Cleavage_strength)) +
  labs(y = "Country Position on \nthe Defence-Normalization Dimension",
       x = "% of Respondents who Report being Threatened",
       col = "Defence-Normalization Measure") + 
  geom_smooth(method = "lm", col = "grey40") + 
  geom_point() +
  geom_text_repel(
    aes(label = country),
    hjust = 0,           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.03,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  ) + 
  scale_x_continuous(labels = scales::percent, limits = c(0.1, 0.8)) + 
  theme_custom

ggsave("plots/Defence_normalization_threat.png", Defence_normalization_threat, width = 8, height = 4)
#### Average Slope and Threat


EUI_data_short %>% 
      mutate(Russia_threat = ifelse(Q68 == 2, 1, 0)) %>% 
     group_by(country) %>% 
     summarize(Average_threat = mean(Security_FA, na.rm = TRUE)) %>% 
     left_join(polarization_slope_df, by = "country") %>% 
     ggplot(aes(x = Average_threat, y = `Average Absolute Slope`)) +
  geom_text_repel(  # only label the last point
    aes(label = country),
    hjust = 0,
    direction = "y",           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.05,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  )   + 
     geom_point() +
     theme_custom


### Party Supply Table ####

cleavage_strength <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(DS_pos = mean(Security_FA, na.rm = TRUE)) %>% 
  arrange(-DS_pos)


number_of_parties <- ches_data %>% 
  mutate(Pro_Normalization = ifelse(Kremlin_ties <= 5, 1, 0)) %>% 
  group_by(country_name) %>% 
  count(Pro_Normalization) %>% 
  filter(Pro_Normalization == 1)

RW_LW_num_parties <- ches_data %>% 
  mutate(Pro_Normalization = ifelse(Kremlin_ties <= 5, 1, 0),
         RW = ifelse(galtan > 5, "RW", "LW")) %>% 
  group_by(country_name, RW) %>% 
  count(Pro_Normalization) %>% 
  filter(Pro_Normalization == 1) %>% 
  pivot_wider(names_from = "RW", values_from = n) %>% 
  mutate(Ideology_parties = paste0("RW:", RW, "; LW:", ifelse(is.na(LW), "0", LW))) %>% 
  ungroup() %>% 
  select(-c(Pro_Normalization, RW, LW))
  


poll_pro_norm <- polls_df %>% 
  mutate(Pro_Normalization = ifelse(eu_russia > 5, 1, 0)) %>% 
  group_by(Pro_Normalization, country) %>% 
  summarise(Total_poll = sum(poll_2025_04, na.rm = TRUE)) %>% 
  filter(Pro_Normalization == 1)

Sd_kremlim_ties <- ches_data %>% 
  group_by(country_name) %>% 
  summarise(SD_krelim_ties = sd(Kremlin_ties, na.rm = TRUE))

Party_supply <-  left_join(cleavage_strength,
                           number_of_parties, by = c("country" = "country_name")) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  rename(`num_parties` = n) %>% 
  select(-Pro_Normalization)


Party_supply <- Party_supply %>% 
  left_join(
            RW_LW_num_parties, by = c("country" = "country_name")) %>% 
  mutate(Ideology_parties = ifelse(is.na(Ideology_parties), "No Parties", Ideology_parties))

Party_supply <- Party_supply %>% 
  left_join(poll_pro_norm, by = "country") %>% 
  mutate(Total_poll = ifelse(is.na(Total_poll), "No Parties", paste0(round(Total_poll, 2), "%"))) %>% 
  select(-Pro_Normalization)

Party_supply <- Party_supply %>% 
  left_join(Sd_kremlim_ties, by = c("country" = "country_name"))  
  

Party_supply %>% 
  kable(digits = 2, col.names = c("Country", "Average Position", "\\# Normalization Parties",
                                  "Ideology", "Polling \\% 2025",
                                  "Spread of Pro-Russia Party Scores"), format = "latex", booktabs = TRUE, linesep = "", align = "lcc",
        caption = "Party Supply by Country for Pro-Normalization Parties. Average position indicates the average score on the defence normalization index of respondents in that country. The number of pro-normalization parties indicates that number of parties that score above a 5 from favouring close ties with Russia in the 2023 CHES dataset. Right-wing parties are those that are more TAN from the CHES dataset. Polling data is drawn from Politico's Poll of Polls in 2025.\\label{tab:party_supply}") %>% 
  save_kable("tables/party_supply.tex")
  

##### Voted and DK ####

EUI_data_short <- EUI_data_short %>% 
  mutate(Voted = ifelse(is.na(Past_vote), "No", "Yes"),
         SW = case_when(New_Q78_4 %in% c(1, 2) ~ "Oppose",
                        New_Q78_4 %in% c(3, 4) ~ "Support",
                        New_Q78_4 == 5 ~ "Don't Know"),
         HE = case_when(New_Q78_5 %in% c(1, 2) ~ "Oppose",
                        New_Q78_5 %in% c(3, 4) ~ "Support",
                        New_Q78_5 == 5 ~ "Don't Know"))

SW_df <- EUI_data_short %>% 
  group_by(Voted) %>% 
  count(SW) %>% 
  ungroup() %>% 
  group_by(SW) %>% 
  mutate(prop = n/sum(n) * 100) %>% 
  filter(Voted == "No") %>% 
  bind_cols(data.frame(Variable = c("Send Weapons", "", "")), .) %>% 
  select(-c(Voted, n)) %>% 
  rename(Value = SW)

HE_df <- EUI_data_short %>% 
  group_by(Voted) %>% 
  count(HE) %>% 
  ungroup() %>%  
  group_by(HE) %>% 
  mutate(prop = n/sum(n) * 100) %>% 
  filter(Voted == "No") %>% 
  bind_cols(data.frame(Variable = c("Accept Higher Energy Costs", "", "")), .) %>% 
  select(-c(Voted, n)) %>% 
  rename(Value = HE)

Q73_df <- EUI_data_short %>% 
  group_by(Voted) %>% 
  count(Q73) %>% 
  ungroup() %>% 
  group_by(Q73) %>% 
  mutate(prop = n/sum(n) * 100) %>% 
  filter(Voted == "No") %>% 
  mutate(Q73 = replace(Q73, Q73 == "DK", "Don't Know"),
         Q73 = replace_values(Q73,
                              "European countries should invest more in defence and security to defend against Russian aggression" ~ "Defence and security",
                              "European countries should invest more in trade and diplomacy with Russia to improve relations" ~ "Trade and Diplomacy")) %>% 
  bind_cols(data.frame(Variable = c("Defence and Security", "", "", "")), .)   %>% 
  select(-c(Voted, n)) %>% 
  rename(Value = Q73) %>% 
  arrange(-prop) 


Dont_know_df <- bind_rows(SW_df, HE_df, Q73_df) 


Dont_know_df %>% 
  mutate(prop = paste0(round(prop, 2), "%")) %>% 
  kable(col.names = c("Variables", "Value", "% respondents who did not vote"), format = "latex", booktabs = TRUE, linesep = "", align = "lll",
        caption = "Proportion of Europeans in each response category who reported low political interest (by not voting).\\label{tab:dont_know}") %>% 
  save_kable("tables/dont_know.tex")

#### Dispersion and Strength ####

slopes_df

Dispersion_df <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(Dispersion = max(Securtiy_FA_party, na.rm = TRUE) - min(Securtiy_FA_party, na.rm = TRUE)) %>% 
  left_join(slopes_df %>% select(country, `Average Absolute Slope`), by = "country")

lm(Average_slope ~ Dispersion, Dispersion_df %>% rename(Average_slope = `Average Absolute Slope`)) %>% 
  summary()

Dispersion_plot <- Dispersion_df  %>% 
  ggplot(aes(x = Dispersion, y = `Average Absolute Slope`)) + 
  geom_smooth(method = "lm", col = "grey50") + 
  geom_point() + 
  geom_text_repel(  # only label the last point
    aes(label = country),
    hjust = 0,
    direction = "x",           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.05,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  ) + 
  scale_x_continuous(limits = c(0, 4)) +
  labs(x = "Difference in the Defence-Normalization Scores of Parties") + 
  theme_custom

ggsave("plots/Dispersion_plot.png", Dispersion_plot, width = 8, height = 4)


#### Country Average Plot ####

average_DN_df <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(Average = mean(Security_FA, na.rm = TRUE)) %>% 
  left_join(slopes_df %>% select(country, `Average Absolute Slope`), by = "country")

lm(Average_slope ~ Average, average_DN_df %>% rename(Average_slope = `Average Absolute Slope`)) %>% 
  summary()

Average_DN_plot <- average_DN_df  %>% 
  ggplot(aes(x = Average, y = `Average Absolute Slope`)) + 
  geom_smooth(method = "lm", col = "grey50") + 
  geom_point() + 
  geom_text_repel(  # only label the last point
    aes(label = country),
    hjust = 0,
   # direction = "x",           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.03,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  ) + 
  scale_x_continuous(limits = c(-0.75, 1)) +
  labs(x = "Average Country Level Defence-Normalization Position") + 
  theme_custom

ggsave("plots/Average_DN_plot.png", Average_DN_plot, width = 8, height = 4)
