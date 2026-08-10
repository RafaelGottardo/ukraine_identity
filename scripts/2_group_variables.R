##### The code to Create Project Specific Variables ####

library(tidyverse)

source("scripts/0_functions.R")
source("scripts/1_clean_data.R")


EUI_data_short <- EUI_data %>%
  filter(Year >= 2022) %>% 
  filter(country %in% COUNTRIES_2022)


EUI_data_short <- EUI_data_short %>% 
  mutate(Ukraine_groups = case_when(Q73 == "European countries should invest more in defence and security to defend against Russian aggression" &
                                      New_Q78_4 %in% c(3, 4) &
                                      New_Q78_5 %in% c(3, 4) ~ "Security-focused",
                                    New_Q78_4 %in% c(3, 4) &
                                      New_Q78_5 %in% c(1, 2) ~ "Conditional Ukraine Supporters",
                                    Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations" & 
                                      New_Q78_4 %in% c(1, 2) ~ "Russia collaboration",
                                    TRUE ~ "Domestic/ Distracted"),
         Ukraine_groups = factor(Ukraine_groups,
                                 levels = c("Domestic/ Distracted", "Security-focused", "Conditional Ukraine Supporters", "Russia collaboration")
                                 ),
         Generalized_trust = recode_values(Q59, 1 ~ "Trusting",
                                           2 ~ "Untrusting",
                                           3 ~ "Don't Know"),
         New_Q43i = case_match(New_Q43i, 1 ~ 4, 2 ~ 3, 5 ~ 2.5, 3 ~ 2, 4 ~ 1),
         ideology = recode_values(Q62, 1 ~ "Left-wing",
                                  2 ~ "Left-wing",
                                  3 ~ "Centre",
                                  4 ~ "Centre",
                                  5 ~ "Centre",
                                  6 ~ "Right-wing",
                                  7 ~ "Right-wing",
                                  8 ~ "Don't Know"),
         Refugee_support = ifelse(New_Q78_1 %in% c(3, 4), 1, 0),
         EU_assent = ifelse(New_Q78_7 %in% c(3, 4), 1, 0),
         Radicalized = case_when(Q62 == 1 ~ "Radical Left",
                                 Q62 %in% c(2, 3, 4, 5, 6) ~ "Moderate",
                                 Q62 == 7 ~ "Radical Right",
                                 Q62 == 8 ~ "Don't Know"))


EUI_data_short <- EUI_data_short %>% 
  group_by(country) %>% 
  mutate(income = replace(income, income %in% c(18, 19, 20, 21), NA),
         median_income = median(income, na.rm = TRUE),
         Above_median = ifelse(income > median_income, 1, 0)) %>% 
  ungroup()

Vote_share_df <- read_xlsx("data_raw/party_vote_share.xlsx")
Vote_share_df <- Vote_share_df %>% 
  filter(!is.na(CHES_ID))

EUI_data_short <- EUI_data_short %>% 
  left_join(Vote_share_df, by = c("Past_vote" = "CHES_ID"))

EUI_data_long <- EUI_data %>% 
  bind_rows(EUI_2018, EUI_2020, EUI_2021) %>% 
  mutate(Ukraine_groups_long = case_when(Q73 == "European countries should invest more in defence and security to defend against Russian aggression" ~ "Defence and Security",
                                         Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations" ~ "Trade and Diplomacy",
                                         TRUE ~ "Domestic-Distracted"))

table(EUI_data_short$Ukraine_groups)
#### Create Factor Version of Group Variables ####

group_vars <- EUI_data_short %>% 
  select(Q73, New_Q78_4, New_Q78_5
         )


group_vars  <- group_vars %>% 
  mutate(
    Q73_security = ifelse(Q73 == "European countries should invest more in defence and security to defend against Russian aggression", 1, 0),
    New_Q78_4_security = ifelse(New_Q78_4 %in% c(3, 4), 1, 0),
    New_Q78_5_security = ifelse(New_Q78_5 %in% c(3, 4), 1, 0)
  )

group_vars <- group_vars %>% 
  select(-c(Q73, New_Q78_4, New_Q78_5
            ))


CORS <- tetrachoric(group_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 2 factors with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1, cor = "tet"); Factor_loadings$loadings


# data.frame(Variable = c("D.S.", "S.W.", "H.E."),
#   Loadings = Factor_loadings$loadings) %>% 
#   kable(digits = 3, col.names = c("Variable", "Loadings"),
#         booktabs = TRUE, linesep = "", align = "lr", format = "latex", position = "H",
#         caption = "\\textbf{D.S. = Defence an Security; S.W. = Support Sending Weapons to Ukraine; H.E. = Willing to Accept Higher Energy Costs as a Result of Sanctions.} Confirmatory factor analysis factor loadings based on 1 factor. 1 represented the defence oriented position. \\label{tab:factor_loadings}") %>% 
#   save_kable("tables/factor_loadings.tex")
# 
# Corr_matrix <- round(CORS$rho, 3)
# Corr_matrix[upper.tri(Corr_matrix, diag = TRUE)] <- "-"
# 
# Corr_matrix %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   mutate(rowname = recode_values(rowname,
#                                  "Q73_security" ~ "D.S.",
#                                  "New_Q78_4_security" ~ "S.W.",
#                                  "New_Q78_5_security" ~ "H.E."
#                                  
#                                  )) %>% 
#   kable(digits = 3, col.names = c("", "D.S.", "S.W.", "H.E."),
#         booktabs = TRUE, linesep = "", align = "lccccc", format = "latex", position = "H",
#         caption = "\\textbf{D.S. = Defence an Security; S.W. = Support Sending Weapons to Ukraine; H.E. = Willing to Accept Higher Energy Costs as a Result of Sanctions.} Tetrachoric Correlations between defence-normalization index items. 1 represented the defence oriented position. \\label{tab:defence_corrs}") %>% 
#   save_kable("tables/tetrachoric_corrs.tex")
#   

alpha(group_vars)

EUI_data_short$Security_FA <- Factor_loadings$scores
EUI_data_short$Security_FA <- as.numeric(EUI_data_short$Security_FA) * -1

#### Robust Factor #####

#### Create Factor Version of Group Variables ####

group_vars <- EUI_data_short %>% 
  select(Q73, New_Q78_4, New_Q78_5, Q68, Q67_revisions, Q71a,
         New_Q78_1, #refugees
         New_Q78_3 # humanitarian aid
  )
VAR <- c("Q73", "New_Q78_4", "New_Q78_5", "Q68", "Q67_revisions", "Q71a",
         "New_Q78_1", #refugees
         "New_Q78_3")

group_data <- group_vars %>% 
  mutate(across(c(New_Q78_4, New_Q78_5, New_Q78_1, New_Q78_3), \(x)case_when(x %in% c(1, 2) ~ "Oppose", x %in% c(3, 4) ~ "Support", x == 5 ~ "Don't Know")),
         Q71a = case_when(Q71a %in% c(1, 2) ~ "Important",
                          Q71a %in% c(3, 4) ~ "Not Important",
                          Q71a == 5 ~ "Don't Know"),
  )

group_descriptives <- data.frame()
for (i in seq_along(VAR)){
  df <- group_data %>% 
    count(across(all_of(VAR[i]))) %>%
    rename(Levels = !!VAR[i]) %>%
    filter(!is.na(Levels)) %>% 
    mutate(Levels = as.character(Levels),
           Variable = VAR[i],
           prop = n / sum(n))
  
  group_descriptives <- bind_rows(group_descriptives, df)
}


group_vars  <- group_vars %>% 
  mutate(
    Q73_security = ifelse(Q73 == "European countries should invest more in defence and security to defend against Russian aggression", 1, 0),
    New_Q78_4_security = ifelse(New_Q78_4 %in% c(3, 4), 1, 0),
    New_Q78_5_security = ifelse(New_Q78_5 %in% c(3, 4), 1, 0),
    New_Q78_1_security = ifelse(New_Q78_1 %in% c(3, 4), 1, 0),
    New_Q78_3_secuirty = ifelse(New_Q78_3 %in% c(3, 4), 1, 0),
    #Q68_china = ifelse(Q68 == 4, 1, 0),
    Q67_punish = ifelse(Q67_revisions %in% c(2, 3), 1, 0),
    Q71_important = ifelse(Q71a %in% c(1, 2), 1, 0)
  )

group_vars <- group_vars %>% 
  select(-c(Q73, New_Q78_4, New_Q78_5, Q68, Q67_revisions, Q71a,
            New_Q78_1, #refugees
            New_Q78_3
  ))


CORS <- tetrachoric(group_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 2 factors with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1, cor = "tet"); Factor_loadings$loadings


# data.frame(Variable = c("D.S.", "S.W.", "H.E."),
#   Loadings = Factor_loadings$loadings) %>% 
#   kable(digits = 3, col.names = c("Variable", "Loadings"),
#         booktabs = TRUE, linesep = "", align = "lr", format = "latex", position = "H",
#         caption = "\\textbf{D.S. = Defence an Security; S.W. = Support Sending Weapons to Ukraine; H.E. = Willing to Accept Higher Energy Costs as a Result of Sanctions.} Confirmatory factor analysis factor loadings based on 1 factor. 1 represented the defence oriented position. \\label{tab:factor_loadings}") %>% 
#   save_kable("tables/factor_loadings.tex")
# 
# Corr_matrix <- round(CORS$rho, 3)
# Corr_matrix[upper.tri(Corr_matrix, diag = TRUE)] <- "-"
# 
# Corr_matrix %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   mutate(rowname = recode_values(rowname,
#                                  "Q73_security" ~ "D.S.",
#                                  "New_Q78_4_security" ~ "S.W.",
#                                  "New_Q78_5_security" ~ "H.E."
#                                  
#                                  )) %>% 
#   kable(digits = 3, col.names = c("", "D.S.", "S.W.", "H.E."),
#         booktabs = TRUE, linesep = "", align = "lccccc", format = "latex", position = "H",
#         caption = "\\textbf{D.S. = Defence an Security; S.W. = Support Sending Weapons to Ukraine; H.E. = Willing to Accept Higher Energy Costs as a Result of Sanctions.} Tetrachoric Correlations between defence-normalization index items. 1 represented the defence oriented position. \\label{tab:defence_corrs}") %>% 
#   save_kable("tables/tetrachoric_corrs.tex")
#   

alpha(group_vars)

EUI_data_short$Security_FA_robust <- Factor_loadings$scores
EUI_data_short$Security_FA_robust <- as.numeric(EUI_data_short$Security_FA_robust) * -1
#### with continuous

# group_vars2 <- EUI_data_short %>% 
#   select(Q73, New_Q78_4, New_Q78_5)
# 
# group_vars2  <- group_vars2 %>% 
#   mutate(Q73 = case_match(Q73, "European countries should invest more in defence and security to defend against Russian aggression" ~ 3,
#                           "European countries should invest more in trade and diplomacy with Russia to improve relations"  ~ 1,
#                           "Neither" ~ 2,
#                           "DK" ~ 2)
#   )
# 
# 
# 
# CORS <- tetrachoric(group_vars)
# EIGNS <- eigen(CORS$rho); EIGNS$values # 3 factors with 1 as a cutoff 
# 
# Factor_loadings2 <- fa(group_vars2, 1); Factor_loadings2$loadings
# 
# alpha(group_vars2)

#EUI_data_short$Security_FA <- Factor_loadings$scores

#### Create Factor for GAL TAN ####

GAL_TAN_vars <- EUI_data_short %>% 
  select( Climate, starts_with("Immigration_types"))

GAL_TAN_vars <- GAL_TAN_vars %>% 
  mutate(across(starts_with("Immigration_types"), \(x)ifelse(x %in% c(3, 4), 1, 0)))

CORS <- tetrachoric(GAL_TAN_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 3 factors with 1 as a cutoff 

Factor_loadings2 <- fa(GAL_TAN_vars, 1, cor = "tet"); Factor_loadings2$loadings

alpha(GAL_TAN_vars)

EUI_data_short <- EUI_data_short %>% 
  mutate(GAL_TAN = ( Climate + Immigration_types_23_1 + Immigration_types_23_2 + Immigration_types_23_3 + Immigration_types_23_4)/6,
         
         GAL_TAN_index = as.numeric(Factor_loadings2$scores))


galtan_plot <- ggplot(EUI_data_short, aes(x = GAL_TAN)) + 
  geom_density(fill = "darkgreen") + 
  labs(x = "GAL-TAN Index",
       y = "Density") + 
  theme_custom

ggsave("plots/galtan_plot.png", galtan_plot, width = 4, height = 4)
#### Create Party Family Variables ####

EUI_data_short <- left_join(EUI_data_short, ches_data %>% filter(!is.na(new_q59)), by = c("Past_vote" = "new_q59"))

# EU_POSITION
# LRGEN
# URBAN_RURAL
# EU_RUSSIA

#### Export Data for Predictions in Python ####

EUI_data_predict <- EUI_data_short %>% 
  filter(Year %in% c(2023, 2025)) %>% 
  select(Past_vote, Year, country, Urban, Q62, Ukraine_groups, all_of(CONTROLS), Security_FA) %>% 
  drop_na()




write.csv(EUI_data_predict, "data_clean/prediction_df.csv")
