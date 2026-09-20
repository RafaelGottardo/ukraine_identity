##### The code to Create Project Specific Variables ####

library(tidyverse)

source("scripts/0_functions.R")
source("scripts/1_clean_data.R")
source("scripts/1_prepare_longitudinal.R")   # EUI_2018/2020/2021, used for EUI_data_long below


EUI_data_short <- EUI_data %>%
  filter(Year >= 2022)

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
         #New_Q43i = case_match(New_Q43i, 1 ~ 4, 2 ~ 3, 5 ~ 2.5, 3 ~ 2, 4 ~ 1),
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

#### Recode income into purchasing-power-parity (PPP) US dollars ####

# `income` is a country-specific band number: each country was asked about annual
# household income in its own currency and its own set of brackets (see the
# income_* variables in scripts/1_clean_data.R). To make it comparable we (1) take
# the midpoint of each band in local currency, then (2) convert with a PPP factor.
#
# Band midpoints (annual, local currency units). "Less than X" bottom bands use
# 0.5 * X; open-ended top bands use 1.5 * lower bound. Codes not listed here
# (Don't know / Prefer not to answer / Skipped) fall through to NA on the join.
income_band_midpoints <- bind_rows(
  # Euro countries on the standard 17-band (EUR 5k-wide) scale
  tidyr::crossing(
    country = c("Belgium", "Croatia", "Finland", "France", "Germany",
                "Italy", "Lithuania", "Netherlands", "Spain"),
    tibble(income = 1:17,
           lcu_midpoint = c(2500, 7500, 12500, 17500, 22500, 27500, 32500, 37500,
                            42500, 47500, 52500, 57500, 65000, 75000, 90000,
                            125000, 225000))
  ),
  # Greece: 18-band scale (extra band at the bottom)
  tibble(country = "Greece", income = 1:18,
         lcu_midpoint = c(1250, 3750, 7500, 12500, 17500, 22500, 27500, 32500,
                          37500, 42500, 47500, 52500, 57500, 65000, 75000, 90000,
                          125000, 225000)),
  # Slovakia: 19-band scale (extra bands at the bottom)
  tibble(country = "Slovakia", income = 1:19,
         lcu_midpoint = c(500, 1750, 3750, 7500, 12500, 17500, 22500, 27500,
                          32500, 37500, 42500, 47500, 52500, 57500, 65000, 75000,
                          90000, 125000, 225000)),
  # Denmark (DKK) and Sweden (SEK): 17-band scales, identical numeric breakpoints
  tidyr::crossing(
    country = c("Denmark", "Sweden"),
    tibble(income = 1:17,
           lcu_midpoint = c(25000, 75000, 125000, 175000, 225000, 275000, 325000,
                            375000, 425000, 475000, 525000, 575000, 650000,
                            750000, 900000, 1250000, 2250000))
  ),
  # Hungary (HUF): 17-band scale
  tibble(country = "Hungary", income = 1:17,
         lcu_midpoint = c(250000, 750000, 1500000, 3000000, 5000000, 7000000,
                          9000000, 11000000, 13000000, 15000000, 17000000,
                          19000000, 21000000, 23000000, 25000000, 28000000,
                          45000000)),
  # Poland (PLN): 16-band scale
  tibble(country = "Poland", income = 1:16,
         lcu_midpoint = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000,
                          85000, 95000, 112500, 137500, 175000, 250000, 400000,
                          750000)),
  # Romania (RON / lei): 18-band scale
  tibble(country = "Romania", income = 1:18,
         lcu_midpoint = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000,
                          85000, 95000, 112500, 137500, 162500, 187500, 225000,
                          275000, 400000, 750000)),
  # Bulgaria (BGN): 14-band scale
  tibble(country = "Bulgaria", income = 1:14,
         lcu_midpoint = c(500, 2000, 4000, 7500, 15000, 25000, 35000, 45000,
                          62500, 87500, 125000, 175000, 225000, 375000))
)

# PPP conversion factors: local currency units per international $ (private
# consumption). Source: World Bank series PA.NUS.PRVT.PP, ~2023 values. These are
# approximate - update from WDI::WDI(indicator = "PA.NUS.PRVT.PP") or OECD as
# needed. Euro-area factors are country-specific (same currency, different price
# levels).

## WDI labels some countries differently from this project's own `country`
## column ("United Kingdom" vs "UK", "Slovak Republic" vs "Slovakia"), so
## without this rename the filter/join below silently drop those countries -
## income_ppp_usd(_log) ends up NA for every UK/Slovakia respondent, which in
## turn makes any model interacting income with country lose those countries'
## factor levels entirely (see scripts/2_main_analysis.R's Social_base_countries).
## Only 2024-2025 are joined below, so only those years are requested (the default
## 1960-2031 request is large and timed out once); the API is retried a few times
## because it is occasionally slow, and a failure stops here with a clear message
## rather than surfacing later as a missing `ppp_conversion` / income_ppp_usd_log.
ppp_raw <- NULL
for (attempt in 1:3) {
  ppp_raw <- tryCatch(WDI::WDI(indicator = "PA.NUS.PRVT.PP", start = 2024, end = 2025),
                      error = function(e) NULL)
  if (!is.null(ppp_raw)) break
  Sys.sleep(5)
}
if (is.null(ppp_raw)) stop("Could not download PA.NUS.PRVT.PP from the World Bank API after 3 attempts - ",
                           "income_ppp_usd_log cannot be built. Re-run when the connection is back.")

ppp_conversion <- ppp_raw %>%
  mutate(country = case_match(country,
                              "United Kingdom" ~ "UK",
                              "Slovak Republic" ~ "Slovakia",
                              .default = country)) %>%
  filter(country %in% NEW_COUNTRIES_2024 & year %in% c(2024, 2025))
  


EUI_data_short <- EUI_data_short %>%
  left_join(income_band_midpoints, by = c("country", "income")) %>%
  left_join(ppp_conversion, by = c("country", "Year" = "year")) %>%
  mutate(income_ppp_usd = lcu_midpoint / PA.NUS.PRVT.PP,
         income_ppp_usd_log = log(income_ppp_usd),
         income_ppp_usd_k = income_ppp_usd / 1000,
         # pooled (cross-country) high-income split, now that incomes are comparable
         Above_median_ppp = ifelse(income_ppp_usd > median(income_ppp_usd, na.rm = TRUE), 1, 0))

Vote_share_df <- read_xlsx("data_raw/party_vote_share.xlsx")
Vote_share_df <- Vote_share_df %>% 
  filter(!is.na(CHES_ID))

EUI_data_short <- EUI_data_short %>% 
  left_join(Vote_share_df, by = c("Past_vote" = "CHES_ID"))

EUI_data_long <- EUI_data %>% 
  bind_rows(EUI_2018, EUI_2020, EUI_2021) %>% 
  mutate(Ukraine_groups_long = case_when(Q73 == "European countries should invest more in defence and security to defend against Russian aggression" ~ "Defence and Security",
                                         Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations" ~ "Trade and Diplomacy",
                                         TRUE ~ "Neither/Don't Know"))

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

item_distribution_plot <- group_vars %>% 
  pivot_longer(cols = c(Q73_security, New_Q78_4_security, New_Q78_5_security),
               names_to = "Variable", values_to = "val") %>% 
  mutate(Variable = replace_values(Variable,
                                   "Q73_security" ~ "Defence and Security",
                                   "New_Q78_4_security" ~ "Send Weapons",
                                   "New_Q78_5_security" ~ "Accept Higher Energy Costs"
                                    ),
         Variable = factor(Variable, levels = rev(c("Defence and Security",
                                                "Send Weapons",
                                                "Accept Higher Energy Costs"))),
         val = replace_values(as.character(val),
                              "0" ~ "Do Not Support",
                              "1" ~ "Support"),
         val = factor(val, levels = rev(c("Do Not Support", "Support"))),
         ) %>% 
  ggplot(aes(y = Variable, fill = val, group = val)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkblue", "darkred")) + 
  guides(fill = guide_legend(reverse = TRUE,
                             ncol = 1)) + 
  labs(fill = NULL,
       x = "Percentage of Respondents",
       y = NULL) + 
  scale_x_continuous(labels = scales::percent) +
  theme_custom

ggsave(file.path(GLOBAL_DIR, "figures", "item_distribution_plot.png"), item_distribution_plot, width = 8, height = 4)

CORS <- tetrachoric(group_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 2 factors with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1, cor = "tet"); Factor_loadings$loadings

EUI_data_short$Security_FA <- Factor_loadings$scores
EUI_data_short$Security_FA <- as.numeric(EUI_data_short$Security_FA) * -1

#### Dichotomous without DK ####

group_vars <- EUI_data_short %>% 
  select(Q73, New_Q78_4, New_Q78_5
  )


group_vars  <- group_vars %>% 
  mutate(
    Q73_security = ifelse(Q73 == "European countries should invest more in defence and security to defend against Russian aggression", 1, ifelse(Q73 %in% c("Neither", "DK"), NA, 0)),
    New_Q78_4_security = ifelse(New_Q78_4 %in% c(3, 4), 1, ifelse(New_Q78_4 %in% 5, NA, 0)),
    New_Q78_5_security = ifelse(New_Q78_5 %in% c(3, 4), 1, ifelse(New_Q78_5 %in% 5, NA, 0))
  )

group_vars <- group_vars %>% 
  select(-c(Q73, New_Q78_4, New_Q78_5
  ))

CORS <- tetrachoric(group_vars)
EIGNS <- eigen(CORS$rho); EIGNS$values # 2 factors with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1, cor = "tet"); Factor_loadings$loadings

EUI_data_short$Security_FA_robust <- Factor_loadings$scores
EUI_data_short$Security_FA_robust  <- as.numeric(EUI_data_short$Security_FA_robust) * -1
#### Continious Factor Loadings ####

group_vars <- EUI_data_short %>%  
  select(Q73, New_Q78_4, New_Q78_5
  )


group_vars  <- group_vars %>% 
  mutate(
    Q73_security = recode_values(Q73,
                                 "European countries should invest more in defence and security to defend against Russian aggression" ~ 3,
                                 "Neither"  ~ 2,
                                 "DK" ~ 2,
                                 "European countries should invest more in trade and diplomacy with Russia to improve relations" ~ 1                                 ),
    New_Q78_4 = recode_values(New_Q78_4,
                              1 ~ 1,
                              2 ~ 2,
                              5 ~ 3,
                              3 ~ 4,
                              4 ~ 5),
    New_Q78_5 = recode_values(New_Q78_5,
                                1 ~ 1,
                                2 ~ 2,
                                5 ~ 3,
                                3 ~ 4,
                                4 ~ 5)
  )


group_vars <- group_vars %>% 
  select(-c(Q73
  ))


CORS <- cor(group_vars)
EIGNS <- eigen(CORS); EIGNS$values # 1 factor with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1); Factor_loadings$loadings
EUI_data_short$Security_FA_cont <- Factor_loadings$scores
EUI_data_short$Security_FA_cont <- as.numeric(EUI_data_short$Security_FA_cont) * -1

## Item distribution plot for the continuous (ordinal) items feeding Security_FA_cont,
## same style as item_distribution_plot but with each item's full 3/5-point scale
## instead of the collapsed 0/1 version - low values (trade/diplomacy-leaning,
## disagreement) shade red, high values (defence-focused, agreement) shade blue.
item_distribution_plot_cont <- group_vars %>%
  mutate(New_Q78_4 = recode_values(New_Q78_4,
                                   1 ~ "Very Normalization Focused",
                                   2 ~ "Normalization Focused",
                                   3 ~ "Neutral",
                                   4 ~ "Defence Focused",
                                   5 ~ "Very Defence Focused"
                                   ),
         New_Q78_5 = recode_values(New_Q78_5,
                                   1 ~ "Very Normalization Focused",
                                   2 ~ "Normalization Focused",
                                   3 ~ "Neutral",
                                   4 ~ "Defence Focused",
                                   5 ~ "Very Defence Focused"
         ),
         Q73_security = recode_values(Q73_security,
                                      1 ~ "Normalization Focused",
                                      2 ~ "Neutral",
                                      3 ~ "Defence Focused")
         ) %>% 
  pivot_longer(cols = c(Q73_security, New_Q78_4, New_Q78_5),
               names_to = "Variable", values_to = "val") %>%
  mutate(Variable = replace_values(Variable,
                                   "Q73_security" ~ "Defence and Security",
                                   "New_Q78_4" ~ "Send Weapons",
                                   "New_Q78_5" ~ "Accept Higher Energy Costs"
                                    ),
         Variable = factor(Variable, levels = rev(c("Defence and Security",
                                                "Send Weapons",
                                                "Accept Higher Energy Costs"))),
         val = factor(val, levels = rev(c("Very Normalization Focused",
                                      "Normalization Focused",
                                      "Neutral",
                                      "Defence Focused",
                                      "Very Defence Focused"))),
         ) %>%
  ggplot(aes(y = Variable, fill = val, group = val)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = rev(c("darkred", "red", "grey80", "blue", "darkblue"))) +
  guides(fill = guide_legend(reverse = TRUE,
                             ncol = 2)) +
  labs(fill = NULL,
       x = "Percentage of Respondents",
       y = NULL) +
  scale_x_continuous(labels = scales::percent) +
  theme_custom

ggsave(file.path(GLOBAL_DIR, "figures", "item_distribution_plot_cont.png"), item_distribution_plot_cont, width = 8, height = 4)

#### Continuous Factor Loading without DK ####

group_vars <- EUI_data_short %>% 
  select(Q73, New_Q78_4, New_Q78_5
  )


group_vars  <- group_vars %>% 
  mutate(
    Q73_security = recode_values(Q73,
                                 "European countries should invest more in defence and security to defend against Russian aggression" ~ 2,
                                 "Neither"  ~ NA,
                                 "DK" ~ NA,
                                 "European countries should invest more in trade and diplomacy with Russia to improve relations" ~ 1                                 ),
    New_Q78_4 = recode_values(New_Q78_4,
                              1 ~ 1,
                              2 ~ 2,
                              5 ~ NA,
                              3 ~ 3,
                              4 ~ 4),
    New_Q78_5 = recode_values(New_Q78_5,
                              1 ~ 1,
                              2 ~ 2,
                              5 ~ NA,
                              3 ~ 3,
                              4 ~ 4)
  )


group_vars <- group_vars %>% 
  select(-c(Q73
  ))


CORS <- cor(group_vars, use = "pairwise.complete.obs")
EIGNS <- eigen(CORS); EIGNS$values # 1 factor with 1 as a cutoff 

Factor_loadings <- fa(group_vars, 1); Factor_loadings$loadings
EUI_data_short$Security_FA_cont_robust <- Factor_loadings$scores
EUI_data_short$Security_FA_cont_robust <- as.numeric(EUI_data_short$Security_FA_cont_robust) * -1

#### Create weights #### 

EUI_data_short <- EUI_data_short %>% 
  group_by(Year, country) %>% 
  mutate(weight_sum = sum(weight),
         balanced_weights = weight * (1000 / weight_sum)
         )  %>% 
  ungroup()

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

#EUI_data_short$Security_FA <- Factor_loadings$scores


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

GAL_TAN_vars <- GAL_TAN_vars %>% 
  mutate(GAL_TAN = ( Climate + Immigration_types_23_1 + Immigration_types_23_2 + Immigration_types_23_3 + Immigration_types_23_4)/5,
         
         GAL_TAN_index = as.numeric(Factor_loadings2$scores)) %>% 
  select(GAL_TAN, GAL_TAN_index)

EUI_data_short <- bind_cols(EUI_data_short, GAL_TAN_vars) 

galtan_plot <- ggplot(EUI_data_short, aes(x = GAL_TAN)) + 
  geom_density(fill = "darkgreen") + 
  labs(x = "GAL-TAN Index",
       y = "Density") + 
  theme_custom

ggsave("plots/galtan_plot.png", galtan_plot, width = 4, height = 4)
#### Create Party Family Variables ####

EUI_data_short <- left_join(EUI_data_short,
                            ches_data %>% filter(!is.na(new_q59)), by = c("Past_vote" = "new_q59"))

# EU_POSITION
# LRGEN
# URBAN_RURAL
# EU_RUSSIA


#### Create Party index ####

index_vars <- EUI_data_short %>% 
  select(EU_Russia, weapons, energy_costs) 

index_vars <- index_vars %>% 
  mutate(EU_Russia = 10 - EU_Russia)

COR <- cor(index_vars, use = "complete.obs")

eigen(COR)$values

alpha(index_vars)

index_loadings <- fa(index_vars, cor = "cor", nfactors = 1); index_loadings$loadings


EUI_data_short$Securtiy_FA_party <- as.numeric(index_loadings$scores) * -1

#### Rescaled (0-1) Versions ####

EUI_data_short <- EUI_data_short %>%
  mutate(Q62_01 = range01(Q62),
         Security_FA_01 = range01(Security_FA),
         New_Q43i_01 = range01(New_Q43i),
         GAL_TAN_index_01 = range01(GAL_TAN))

#### Export Data for Predictions in Python ####
table(EUI_data_short$Past_vote)
EUI_data_predict <- EUI_data_short %>% 
  filter(Year %in% c(2023, 2025)) %>% 
  select(Past_vote, Year, country, Urban, Q62, Ukraine_groups, all_of(CONTROLS), Security_FA) %>% 
  drop_na()


write.csv(EUI_data_predict, "data_clean/prediction_df.csv")

#### Country Order ####

country_order <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE)) %>% 
  arrange(-Mean) %>% 
  pull(country)
