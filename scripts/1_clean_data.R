
##### CODE TO IMPORT DATASETS ####

#### source helper scripts ####

source("scripts/0_functions.R")
source("scripts/1_Import_2025.R")
#### 2018 ####

EUI_2018 <- read_csv("Data_raw/2018 SiE year dataset/2018 SiE dataset_dataset.csv")

EUI_2018 <- EUI_2018 %>% 
  select(attack, country, euresource_interests, peuro_q3, countryissues_1:countryissues_15, weight, russia, nato,
         countrydefence, countrythreat, europeanarmy) %>% 
  mutate(Year = 2018,
         attack = case_when(attack == 5 ~ 3,
                            attack %in% c(1, 2) ~ 1,
                            attack %in% c(3, 4) ~ 2),
         countrythreat = case_match(countrythreat, 1 ~ 1, 2 ~ 8, 3 ~ 2, 4 ~ 3, 
                                    5 ~ 4, 6 ~ 5, 7 ~ 9, 8 ~ 6, 9 ~ 6, 10 ~ 7),
         europeanarmy = case_match(europeanarmy, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         across(countryissues_1:countryissues_15, \(x)case_match(x, 1 ~ 1, 2 ~ 0)),
         countrydefence = case_match(countrydefence, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         euresource_interests = euresource_interests,
         nato = case_match(nato, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         peuro_q3 = case_match(peuro_q3, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         country = case_match(country,
                               1 ~ "UK",
                               2 ~ "France",
                               3 ~ "Germany",
                               4 ~ "Denmark",
                               5 ~ "Sweden",
                               6 ~ "Finland",
                               7 ~ "Spain", 
                               8 ~ "Italy",
                               9 ~ "Poland",
                               10 ~ "Lithuania",
                               11 ~ "Greece"
         ),
         russia = case_match(russia,
                             1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                             2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                             3 ~ NA,
                             4 ~ NA
                             )) %>% 
  rename(
    
    "Q9" = "peuro_q3",
    "Q18" = "euresource_interests",
    "Q69" = "countrydefence",
    "Q70" = "europeanarmy",
    "Q71" = "nato",
    "Q73" = "russia",
    "Q68" = "countrythreat",
    "Q44" = "attack"
    ) %>% 
  rename_with(~paste0("Q7_", c(4, 17, 2, 1, 9, 8, 5, 13, 10, 3, 7, 11, 12, 6, 15)),
              starts_with("countryissues_"))

#### 2019 ####

EUI_2019 <- read_xlsx("Data_raw/2019 SiE year dataset/2019 SiE dataset_dataset.xlsx")

EUI_2019 <- EUI_2019 %>% 
  select(country, attack, euresource_interests, peuro_q3, countryissues__1:countryissues__15, weight, countrythreat, selfidentity, europeanarmy) %>% 
  mutate(Year = 2019,
         peuro_q3 = case_match(peuro_q3, "1" ~ 1, "2" ~ 0, "3" ~ 0, "4" ~ 0),
         attack = case_when(attack == 5 ~ 3,
                            attack %in% c(1, 2) ~ 1,
                            attack %in% c(3, 4) ~ 2),
         euresource_interests = euresource_interests,
         countrythreat = case_match(countrythreat, 1 ~ 1, 2 ~ 8, 3 ~ 2, 4 ~ 3, 
                                    5 ~ 4, 6 ~ 5, 7 ~ 9, 8 ~ 6, 9 ~ 6, 10 ~ 7),
         europeanarmy = case_match(europeanarmy, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         country = case_match(country,
                              1 ~ "Denmark",
                              2 ~ "Finland",
                              3 ~ "France",
                              4 ~ "UK",
                              5 ~ "Germany",
                              6 ~ "Greece",
                              7 ~ "Italy",
                              8 ~ "Lithuania",
                              9 ~ "Poland",
                              10 ~ "Romania",
                              11 ~ "Spain",
                              12 ~ "Sweden")) %>% 
  rename("Q4" = "selfidentity",
    "Q18" = "euresource_interests",
         "Q9" = "peuro_q3",
         "Q68" = "countrythreat",
    "Q70" = "europeanarmy",
    "Q44" = "attack") %>% 
  rename_with(~paste0("Q7_", c(4, 17, 2, 1, 9, 8, 5, 13, 10, 3, 7, 11, 12, 6, 15)),
              starts_with("countryissues__"))

#### 2020 ####

EUI_2020 <- read_csv("Data_raw/2020 SiE year dataset/2020 SiE dataset_dataset.csv")

EUI_2020 <- EUI_2020 %>% 
  select(qcountry, q7_1:q7_17, q4, q5, q9, q18, q21, q24, q26, q30, q37, q40, q44, q47a_3, q69, q70, q71, q73, q68, weight, starts_with("Q47a_")) %>% 
  mutate(Year = 2020, 
         q9 = case_match(q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         across(starts_with("Q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
        # q18 = replace(q18, q18 == 11, 5),
         q69 = case_match(q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         q70 = case_match(q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         q71 = case_match(q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         q68 = case_match(q68, 1 ~ 1, 2 ~ 8, 3 ~ 2, 4 ~ 3, 
                                    5 ~ 4, 6 ~ 5, 7 ~ 9, 8 ~ 6, 9 ~ 7),
         qcountry = case_match(qcountry,
                               1 ~ "UK",
                               2 ~ "Denmark",
                               3 ~ "Finland",
                               4 ~ "France",
                               5 ~ "Germany",
                               6 ~ "Sweden",
                               7 ~ "Greece", 
                               8 ~ "Hungary",
                               9 ~ "Italy",
                               10 ~ "Lithuania",
                               11 ~ "Netherlands",
                               12 ~ "Poland",
                               13 ~ "Romania", 
                               14 ~ "Spain"
                               ),
         q73 = case_match(q73,
                          1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ NA,
                          4 ~ NA)) %>% 
  rename("country" = "qcountry",
         "Q4"  =  "q4",
         "Q5"  =  "q5",
         "Q9"  =  "q9",
         "Q18" = "q18",
         "Q21" = "q21",
         "Q24" = "q24", 
         "Q26" = "q26",
         "Q30" = "q30",
         "Q37" = "q37",
         "Q40" = "q40",
         "Q44" = "q44",
         "Q68" = "q68",
         "Q69" = "q69",
         "Q70" = "q70",
         "Q71" = "q71",
         "Q73" = "q73",
         "Q47a_1" = "q47a_1",
         "Q47a_2" = "q47a_2",
         "Q47a_3" = "q47a_3",
         "Q47a_4" = "q47a_4",
         "Q47a_5" = "q47a_5",
         "Q47a_6" = "q47a_6") %>% 
  rename_with(~paste0("Q7_", c(1, 17, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 11, 12, 13, 19, 14)), starts_with("q7_"))


#### 2021 ####

EUI_2021 <- read_csv("Data_raw/2021 SiE year dataset/2021 SiE dataset_dataset.csv")

EUI_2021 <- EUI_2021 %>% 
  select(Qcountry, Q7_1:Q7_17, Q4, Q5, Q9, Q18, Q21, Q24, Q26, Q30, Q37, Q40, Q44, starts_with("Q47a_"), Q68, Q69, Q70, Q71, Q73, weight) %>% 
  mutate(Year = 2021,
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0, 999 ~ NA),
         across(starts_with("Q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
        # Q18 = replace(Q18, Q18 == 11, 5),
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q68 = case_match(Q68, 1 ~ 1, 2 ~ 8, 3 ~ 2, 4 ~ 3, 
                                    5 ~ 4, 6 ~ 5, 7 ~ 9, 8 ~ 6, 9 ~ 6, 10 ~ 7),
         Qcountry = case_match(Qcountry,
                               1 ~ "UK",
                               2 ~ "Denmark",
                               3 ~ "Finland",
                               4 ~ "France",
                               5 ~ "Germany",
                               6 ~ "Sweden",
                               7 ~ "Greece",
                               8 ~ "Hungary",
                               9 ~ "Italy",
                               10 ~ "Lithuania",
                               11 ~ "Netherlands",
                               12 ~ "Poland",
                               13 ~ "Romania",
                               14 ~ "Spain"),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ NA,
                          4 ~ NA)) %>% 
  rename("country" = "Qcountry") %>% 
  rename_with(~paste0("Q7_", c(1, 17, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 11, 12, 13, 19, 14)), starts_with("q7_"))


#### 2022 ####

EUI_2022 <- read_csv("Data_raw/2022 SiE year dataset/2022 SiE dataset_dataset.csv")

EUI_2022 <- EUI_2022 %>% 
  select(country, Q7_r_1:Q7_r_16, New_Q5_1, New_Q5_6, Q4, Q5, Q9, Q18_r, Q21, Q24, Q26_r, Q30, Q37, Q40, Q44,
         starts_with("Q47a_"), Q62,
         Q68_r, Q69, Q70, Q71, Q73, matches("New_Q78_\\d$"), weight) %>% 
  select(-Q47a_6) %>% 
  mutate(Year = 2022,
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0, 999 ~ NA),
        # Q18_r = replace(Q18_r, Q18_r == 11, 5),
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         across(starts_with("Q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         country = case_match(country,
                              180 ~ "Romania",
                              153 ~ "Netherlands",
                              125 ~ "Lithuania",
                              108 ~ "Italy",
                              60 ~ "Denmark",
                              175 ~ "Poland",
                              74 ~ "Finland",
                              75 ~ "France",
                              210 ~ "Sweden",
                              82 ~ "Germany",
                              1 ~ "UK",
                              85 ~ "Greece",
                              99 ~ "Hungary",
                              204 ~ "Spain",
                              56 ~ "Croatia",
                              197 ~ "Slovakia",
                              35 ~ "Bulgaria"),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ NA,
                          4 ~ NA)) %>% 
  rename("Q18" = "Q18_r",
         "Q26" = "Q26_r",
         "Q68" = "Q68_r") %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("Q7_r_"))


#### 2023 ####

EUI_2023 <- read_spss("Data_raw/Results for EUI, SOU and Solidarity 2023 - OMGLOB033 - Merged - SPSS.sav")

EUI_2023 <- EUI_2023 %>% 
  select(country, Q4, Q5,  New_Q5_1, New_Q5_6, Q9, Q7_revisions_1:Q7_revisions_16, Q18_revisions, Q21, Q24, Q26_revisions, Q30, Q37, Q40,
         New_Q43, Q44, EUI_Ukraine_Outcome, Q62, starts_with("Q47a_"),
         Q68_revisions, Q61, gender_all,education_merged1_101, education_merged1_102 , education_merged1_103,
         New_Q43,
         Q69, Q70, Q71, Q73, , weight, matches("New_Q78_\\d$")) %>% 
  mutate(Year = 2023,
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0, 999 ~ NA),
         across(starts_with("Q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
        # Q18_revisions = replace(Q18_revisions, Q18_revisions == 11, 5),
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         country = case_match(country,
                              180 ~ "Romania",
                              153 ~ "Netherlands",
                              108 ~ "Italy",
                              60 ~ "Denmark",
                              125 ~ "Lithuania",
                              175 ~ "Poland",
                              74 ~ "Finland",
                              75 ~ "France",
                              210 ~ "Sweden",
                              82 ~ "Germany",
                              1 ~ "UK",
                              85 ~ "Greece",
                              99 ~ "Hungary",
                              204 ~ "Spain",
                              56 ~ "Croatia",
                              197 ~ "Slovakia",
                              35 ~ "Bulgaria"),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ NA,
                          4 ~ NA),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x))) %>% 
  rename("Q18" = "Q18_revisions", 
         "Q26" = "Q26_revisions",
         "Q68" = "Q68_revisions") %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("Q7_revisions_"))

#### 2024 ####

EUI_2024 <- read_spss("Data_raw/Results for EUI, SOU and Solidarity 2024 - OMGLOB016 - Merged.sav")


EUI_2024 <- EUI_2024 %>% 
  mutate(New_Q59 = replace(New_Q59, New_Q59 == 14, 414),
         New_Q61 = replace(New_Q61, New_Q61 == 14, 414))

val_label(EUI_2024$New_Q59, 414) <- "Les Républicains/ Union de la droite et du centre"
val_label(EUI_2024$New_Q61, 414) <- "Les Républicains/ Union de la droite et du centre"


EUI_2024 <- EUI_2024 %>% 
  select(caseid, country, Q4,# Q5_DE,
         New_Q6a, Q5, Q7_revisions_1:Q7_revisions_16, Q9, Q18_revisions, Q21, Q24, Q26_revisions, Q30, Q37, Q40, Q44, 
         Q47a_3, Q47a_7, Q47a_8,
         EUI_Ukraine_Outcome, Israel_Palestine_2024, Q68_revisions, US_Elections_2024, Q62, age_grp_gen_edu_6, age_grp_gen_edu_7, Q61, age_grp_gen_edu_18, 
         age_grp_gen_edu_19, age_grp_gen_edu_20, Q62,
         New_Q43, starts_with("Q47a_"),
         Q69, Q70, Q71, Q73, weight, matches("New_Q78_\\d$")) %>% 
  rename("Q18" = "Q18_revisions", 
         Q26 = "Q26_revisions",
         "Q68" = "Q68_revisions") %>% 
  mutate(Year = 2024,
         caseid = as.character(caseid),
        # Q5 = ifelse(country == "Germany", Q5_DE, Q5),
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0, 999 ~ NA),
        # Q18 = replace(Q18, Q18 == 11, 5),
         across(starts_with("Q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         country = case_match(country,
                              180 ~ "Romania",
                              153 ~ "Netherlands",
                              125 ~ "Lithuania",
                              108 ~ "Italy",
                              60 ~ "Denmark",
                              175 ~ "Poland",
                              74 ~ "Finland",
                              75 ~ "France",
                              210 ~ "Sweden",
                              82 ~ "Germany",
                              1 ~ "UK",
                              85 ~ "Greece",
                              99 ~ "Hungary",
                              204 ~ "Spain",
                              56 ~ "Croatia",
                              197 ~ "Slovakia",
                              35 ~ "Bulgaria",
                              23 ~ "Belgium"),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ NA,
                          4 ~ NA),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x))
         ) %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("Q7_revisions_")) 



#### 2025 ####

# EUI_2025 <- read_xlsx("Data_raw/P_EUI_EuropeanSolidarity_2025_Master ~Merged (w) - CODES.xlsx")
# 
# 
# col_names <- EUI_2025 %>% 
#   slice(1)
# 
# EUI_2025 <- EUI_2025 %>% 
#   slice(-1)
#                 
# var_label(EUI_2025) <- col_names      

EUI_2025 <- data_2025 %>% 
  select(caseid, country, Q4, Q5_DE, New_Q5_1, New_Q5_6, New_Q6a, Q7_revisions_1:Q7_revisions_16, Q9, Q18_revisions, Q21, Q24, Q26_revisions, Q30, Q37, Q40, Q44,
         EUI_Ukraine_Outcome, Israel_Palestine_2024, Q68_revisions, US_Elections_2024_TrumpSupport, Q5, New_Q5_1, New_Q5_6,
         Q62, gender_all, edu_group, Q61, Q62, A5_1, A5_2, A5_3, A5_4, ForeignPolicyDecisions2_1, completed2024survey,
         ForeignPolicyDecisions2_2, ForeignPolicyDecisions, New_Q59, Q70_2,
         New_Q43, Tariff_EU_US, Tariff_EU_China, Tariff_EU_US_UK, Tariff_EU_China_UK,
         Q69, Q70, Q70_b, Q71, Q73, weight, matches("New_Q78_\\d$"), starts_with("Q47a")) %>% 
  rename("Q18" = "Q18_revisions",
         "Q26" = "Q26_revisions",
         "Q68" = "Q68_revisions") %>% 
  mutate(Year = "2025",
         across(!country, \(x)as.numeric(x)),
         caseid = as.character(caseid),
         A5_2 = replace(A5_2, A5_2 == 99, NA),
         Q5 = ifelse(country == "Germany", Q5_DE, Q5),
         New_Q5_1 = replace(New_Q5_1, New_Q5_1 == 6, 3),
         New_Q5_6 = replace(New_Q5_6, New_Q5_6 == 6, 3),
         across(starts_with("Q47a"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 4 ~ 0, 3 ~ 0, 999 ~ NA),
         Tariff_EU_US = if_else(country == "UK", Tariff_EU_US_UK, Tariff_EU_US),
         Tariff_EU_China = if_else(country == "UK", Tariff_EU_China_UK, Tariff_EU_China),
         Tariff_EU_US_con = case_match(Tariff_EU_US, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
         Tariff_EU_China_con = case_match(Tariff_EU_China, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
         Tariff_EU_US = case_match(Tariff_EU_US, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0),
         Tariff_EU_China = case_match(Tariff_EU_China, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0),
         Q70_base = Q70,
         Q70_2 = case_match(Q70_2, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q70_b_base = Q70_b, 
         # country = case_match(country,
         #                      180 ~ "Romania",
         #                      153 ~ "Netherlands",
         #                      108 ~ "Italy",
         #                      60 ~ "Denmark",
         #                      175 ~ "Poland",
         #                      125 ~ "Lithuania",
         #                      74 ~ "Finland",
         #                      75 ~ "France",
         #                      210 ~ "Sweden",
         #                      82 ~ "Germany",
         #                      1 ~ "UK",
         #                      85 ~ "Greece",
         #                      99 ~ "Hungary",
         #                      204 ~ "Spain",
         #                      56 ~ "Croatia",
         #                      197 ~ "Slovakia",
         #                      35 ~ "Bulgaria",
         #                      23 ~ "Belgium"),
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q70_b = case_match(Q70_b, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         ForeignPolicyDecisions2_1 = case_match(ForeignPolicyDecisions2_1, 1 ~ 1, 2 ~ 0, 3 ~ 0),
         ForeignPolicyDecisions2_2 = case_match(ForeignPolicyDecisions2_2, 1 ~ 1, 2 ~ 0, 3 ~ 0),
         FP_decisions_IV = case_match(ForeignPolicyDecisions, 1 ~ 5, 2 ~ 4, 3 ~ 3, 4 ~ 2, 5 ~ 1, 6 ~ 3),
         ForeignPolicyDecisions = case_match(ForeignPolicyDecisions, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0, 6 ~ 0),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ NA,
                          4 ~ NA),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x))) %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("Q7_revisions_"))


#### Fall Datasets ####

#### April 2022 ####

EUI_april_2022 <- read_xlsx("Data_raw/Results for EUI, SOU and Solidarity 2022 OMGLOB 041 - Merged - CSV (2).xlsx", 
                            sheet = 3)

EUI_april_2022 <- EUI_april_2022 %>% 
  select(weight, country,  matches("New_Q78_\\d$"), Q62, Q7_revisions_2, Q68_revisions) %>% 
  mutate(country = dplyr::recode(country, `1` = "UK",
                                 `15` = "Australia", `32` = "Brazil", 
                                 `40` = "Canada", `46` = "China", 
                                 `35` = "Bulgaria", `56` = "Croatia",
                                 `60` = "Denmark", `65` = "Egypt", `74` = "Finland",
                                 `75` = "France", `82` = "Germany",
                                 `85` = "Greece", `99` = "Hungary", 
                                 `101` = "India", `102` = "Indonesia", 
                                 `108` = "Italy", `110` = "Japan", 
                                 `114` = "Kenya", `125` = "Lithuania",
                                 `140` = "Mexico", `159` = "Nigeria", 
                                 `153` = "Netherlands", `175` = "Poland",
                                 `180` = "Romania", `181` = "Russia", 
                                 `191` = "Saudi Arabia", `197` = "Slovakia", 
                                 `201` = "South Africa", 
                                 `204` = "Spain", `210` = "Sweden",
                                 `216` = "Thailand", `223` = "Turkey", 
                                 `230` = "United States"),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x))) %>% 
  mutate(Year = "April 2022") %>% 
  rename(Q7_2 = Q7_revisions_2,
         Q68 = Q68_revisions
         )

#### Fall 2022 ####

EUI_sept_2022 <- read.spss("Data_raw/yg_sept.sav", to.data.frame = TRUE)


EUI_sept_2022 <- EUI_sept_2022 %>% 
  mutate(country = dplyr::recode(country_cb, `1` = "UK",
                                 `15` = "Australia", `32` = "Brazil", 
                                 `40` = "Canada", `46` = "China", 
                                 `35` = "Bulgaria", `56` = "Croatia",
                                 `60` = "Denmark", `65` = "Egypt", `74` = "Finland",
                                 `75` = "France", `82` = "Germany",
                                 `85` = "Greece", `99` = "Hungary", 
                                 `101` = "India", `102` = "Indonesia", 
                                 `108` = "Italy", `110` = "Japan", 
                                 `114` = "Kenya", `125` = "Lithuania",
                                 `140` = "Mexico", `159` = "Nigeria", 
                                 `153` = "Netherlands", `175` = "Poland",
                                 `180` = "Romania", `181` = "Russia", 
                                 `191` = "Saudi Arabia", `197` = "Slovakia", 
                                 `201` = "South Africa", 
                                 `204` = "Spain", `210` = "Sweden",
                                 `216` = "Thailand", `223` = "Turkey", 
                                 `230` = "United States"),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x)),
         EUI_Ukraine_Outcome = case_match(EUI_Ukraine_Outcome,
                                                 "Russia achieves all its territorial goals in Ukraine" ~ 1,
                                                 "Russia manages to take more Ukrainian territory than it controlled at the start of the war" ~ 2,
                                                 "A return to the territorial situation of earlier this year before the war started, including Russia holding territory it" ~ 3,
                                                 "Don't know" ~ 99,
                                                 "Ukraine manages to retake some of the territory Russia has occupied this year" ~ 4,
                                                "Ukraine manages to retake all of its original territory (including Crimea)" ~ 5)) %>% 
  select(weight, country,  matches("New_Q78_\\d$"), EUI_Ukraine_Outcome) %>% 
  mutate(Year = "Sept. 2022") 


#### Fall 2023 ####

yg_sept_2023 <- read.spss("Data_raw/euiyg_oct.sav", to.data.frame = TRUE)

yg_sept_2023 <- yg_sept_2023 %>% 
  mutate(country = recode(country_cb, `1` = "UK",
                          `15` = "Australia", `32` = "Brazil", 
                          `40` = "Canada", `46` = "China", 
                          `35` = "Bulgaria", `56` = "Croatia",
                          `60` = "Denmark", `65` = "Egypt", `74` = "Finland",
                          `75` = "France", `82` = "Germany",
                          `85` = "Greece", `99` = "Hungary", 
                          `101` = "India", `102` = "Indonesia", 
                          `108` = "Italy", `110` = "Japan", 
                          `114` = "Kenya", `125` = "Lithuania",
                          `140` = "Mexico", `159` = "Nigeria", 
                          `153` = "Netherlands", `175` = "Poland",
                          `180` = "Romania", `181` = "Russia", 
                          `191` = "Saudi Arabia", `197` = "Slovakia",  
                          `201` = "South Africa", 
                          `204` = "Spain", `210` = "Sweden",
                          `216` = "Thailand", `223` = "Turkey", 
                          `230` = "United States"),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x)),
         EUI_Ukraine_Outcome = case_match(EUI_Ukraine_Outcome,
                                          "Russia achieves all its territorial goals in Ukraine" ~ 1,
                                          "Russia manages to take more Ukrainian territory than it controlled at the start of the war" ~ 2,
                                          "A return to the territorial situation of earlier this year before the war started, including Russia holding territory it" ~ 3,
                                          "Don't know" ~ 99,
                                          "Ukraine manages to retake some of the territory Russia has occupied this year" ~ 4,
                                          "Ukraine manages to retake all of its original territory (including Crimea)" ~ 5)) %>% 
  select(weight, country,  matches("New_Q78_\\d$"), EUI_Ukraine_Outcome) %>% 
  mutate(Year = "Oct. 2023")

#### MERGE datasets ####

EUI_data <- bind_rows(EUI_2025, EUI_2024) %>% 
  bind_rows(EUI_2023) %>% 
  bind_rows(EUI_2022) %>% 
  bind_rows(EUI_2021) %>% 
  bind_rows(EUI_2020) %>% 
  bind_rows(EUI_2019) %>% 
  bind_rows(EUI_2018)


EUI_ukraine <- bind_rows(EUI_april_2022, EUI_sept_2022) %>% 
  bind_rows(yg_sept_2023) %>% 
  bind_rows(EUI_2023 %>% mutate(Year = as.character(Year))) %>% 
  bind_rows(EUI_2024 %>% mutate(Year = as.character(Year))) %>% 
  bind_rows(EUI_2025 %>% mutate(Year = as.character(Year))) %>% 
  select(-New_Q78_6)


EUI_ukraine_2 <- bind_rows(EUI_sept_2022) %>% 
  bind_rows(yg_sept_2023) %>% 
  bind_rows(EUI_2023 %>% mutate(Year = as.character(Year))) %>% 
  bind_rows(EUI_2024 %>% mutate(Year = as.character(Year))) %>% 
  bind_rows(EUI_2025 %>% mutate(Year = as.character(Year))) 


EUI_data <- EUI_data %>% 
  mutate(Q44_base = Q44,
    across(Q21:Q44, \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         across(starts_with("New_Q78"), \(x)case_match(x, 1 ~ 5, 2 ~ 4, 3 ~ 2, 4 ~ 1, 5 ~ 3)))



EUI_data <- EUI_data %>% 
  mutate(ukraine_support = rowMeans(across(c(New_Q78_1, New_Q78_2, New_Q78_3, New_Q78_4, 
                                             New_Q78_5, New_Q78_7, New_Q78_9)), na.rm = TRUE),
         US_threat = ifelse(Q68 == 3, 1, 0),
         Trump_support = case_when(US_Elections_2024 == 2 ~ 1, 
                                   (US_Elections_2024_TrumpSupport >= 5 & US_Elections_2024_TrumpSupport != 11) ~ 1,
                                   TRUE ~ 0))

unique(EUI_data$New_Q78_1)


EUI_ukraine <- EUI_ukraine %>% 
  mutate(across(starts_with("New_Q78"), \(x)case_match(x, 1 ~ 5, 2 ~ 4, 3 ~ 2, 4 ~ 1, 5 ~ 3)),
    ukraine_support = rowMeans(across(c(New_Q78_1, New_Q78_2, New_Q78_3, New_Q78_4, 
                                             New_Q78_5, New_Q78_7, New_Q78_9)), na.rm = TRUE),
         )


cross_walk <- read.csv("Data_raw/new_crosswalk_june_9_2025.csv")

cross_walk <- cross_walk %>% 
  mutate(parlgov_id = as.numeric(parlgov_id),
         ches_id = as.numeric(ches_id)) %>% 
  filter(!is.na(new_q59))

parl_gov_data <- read_csv("Data_raw/view_cabinet.csv")

parl_gov_data <- parl_gov_data %>% 
  filter(start_date < as.Date("2022-04-01")) %>% 
  group_by(country_name) %>% 
  slice_max(order_by = start_date, n = 1) %>% 
  ungroup() %>% 
  filter(country_name %in% NEW_COUNTRIES)

parl_gov_data <- left_join(parl_gov_data, cross_walk, by = c("party_id" = "parlgov_id")) %>% 
  filter(!is.na(vote_variable_2022) | !is.na(vote_values_2022))



ches_data <- read_dta("Data_raw/CHES_Ukraine_March_2024.dta") %>% 
  select(-country)

ches_data <- left_join(ches_data, cross_walk, by = c("party_id" = "ches_id"))


EUI_2025 <- EUI_2025 %>% 
  left_join(ches_data, by = c("New_Q59" = "new_q59"))

EUI_2025 <- EUI_2025 %>% 
  mutate(Kremlin_ties = ifelse(Kremlin_ties <= 5, 1, 0))


Incumbent_parties_2025 <- data.frame(country = c("Bulgaria", "Bulgaria", "Bulgaria", "Croatia", "Croatia",
                                                 "Denmark", "Denmark", "Denmark", "Finland", "Finland", "Finland",
                                                 "Finland", "France", "Germany", "Germany", "Greece", "Hungary",
                                                 "Italy", "Italy", "Italy", "Lithuania", "Lithuania", "Lithuania",
                                                 "Netherlands", "Netherlands", "Netherlands", "Netherlands", "Poland", 
                                                 "Poland", "Poland", "Romania", "Romania", "Romania", "Spain", "Spain", "Sweden",
                                                 "Sweden", "Sweden", "UK", "Slovakia", "Slovakia", "Slovakia" ),
                                     New_Q59 = c(125, 319, 271, 141, 139, 160, 61, 71, 83, 87, 88, 89, 308, 20, 19, 118,
                                                 103, 43, 44, 45, 113, 326, 194, 50, 49, 260, 259, 95, 93, 261,
                                                 96, 97, 100, 25, 255, 74, 77, 76, 2, 132, 196, 197),
                                     Incumbent = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
                                     )

cross_walk <- cross_walk %>% 
  mutate(Incumbent_2022 = case_when(new_q59 %in% c(1, 139, 62, 63, 67, 82, 89, 90, 83, 13, 20, 19, 119, 120, 103, 112, 
                                                   113, 114, 116, 49, 51, 52, 56, 92, 97, 132, 181, 79, 78 ) ~ 1,
                                    TRUE ~ 0),
         Incumbent_2025 = case_when(new_q59 %in% c(125, 319, 271, 141, 139, 160, 61, 71, 83, 87, 88, 89, 308, 20, 19, 118,
                                                   103, 43, 44, 45, 113, 326, 194, 50, 49, 260, 259, 95, 93, 261,
                                                   96, 97, 100, 25, 255, 74, 77, 76, 2, 132, 196, 197) ~ 1,
                                    TRUE ~ 0),
  )

table(cross_walk$vote_variable_2022, cross_walk$country_name
      )


       




EUI_2022_incumbent <- read_spss("Data_raw/yg_april_2022_processed.sav") %>% 
  mutate(country = case_match(country,
                              180 ~ "Romania",
                              153 ~ "Netherlands",
                              125 ~ "Lithuania",
                              108 ~ "Italy",
                              60 ~ "Denmark",
                              175 ~ "Poland",
                              74 ~ "Finland",
                              75 ~ "France",
                              210 ~ "Sweden",
                              82 ~ "Germany",
                              1 ~ "UK",
                              85 ~ "Greece",
                              99 ~ "Hungary",
                              204 ~ "Spain",
                              56 ~ "Croatia",
                              197 ~ "Slovakia",
                              35 ~ "Bulgaria",
                              23 ~ "Belgium"),
         Year = 2022)


EUI_2022_2025 <- bind_rows(EUI_2022_incumbent, EUI_2025) %>% 
  filter(!is.na(New_Q59))

EUI_2022_2025 <- EUI_2022_2025 %>% 
  left_join(parl_gov_data, by = c("New_Q59" = "new_q59"))

EUI_2022_2025 <- EUI_2022_2025 %>% 
  left_join(Incumbent_parties_2025, by = "New_Q59")

test <- read_dta("Data_raw/trendfile_dataset_2023 (pastvote_recoded_parlgov)_v4 (1).dta")



test <- read_xlsx("Data_raw/Results for EUI, SOU and Solidarity 2022 OMGLOB 041 - Merged - CSV (2).xlsx", sheet = 3)

# 
# 
# europe <- europe %>% 
#   filter(!is.na(vote_variable) & !is.na(vote_partycode)) %>% 
#   left_join(cross_walk, by = c("vote_partycode" = "new_q59"))


summer_2023 <- read_spss("Data_raw/data/survey/raw/eui-yg/2023-11/eui-yg-2023-11.sav")

write_sav(EUI_data, "Data_clean/EUI_data.sav")



