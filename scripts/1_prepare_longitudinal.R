##### Code to import additional datasets for longitudinal analysis #####


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
                             3 ~ "Other",
                             4 ~ "Other"
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
                          3 ~ "Other",
                          4 ~ "Other")) %>% 
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
                          3 ~ "Other",
                          4 ~ "Other")) %>% 
  rename("country" = "Qcountry") %>% 
  rename_with(~paste0("Q7_", c(1, 17, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 11, 12, 13, 19, 14)), starts_with("q7_"))





