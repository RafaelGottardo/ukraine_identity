##### The code to Create Project Specific Variables ####

source("scripts/0_functions.R")
source("scripts/1_clean_data.R")

EUI_data <- EUI_data %>% 
  mutate(ukraine_groups =  case_when(Q68 == 2 ~ "Anti-Russia",
                               Q73 == "European countries should invest more in defence and security to defend against Russian aggression" ~ "Anti-Russia",
                               Year < 2024 & Q68 != 2 & Q73 != "European countries should invest more in defence and security to defend against Russian aggression" & Q71a == 4 ~ "Pro-Russia",
                               Year < 2024 & Q68 != 2 & Q73 != "European countries should invest more in defence and security to defend against Russian aggression" & Q71a %in% c(1,2,3) ~ "Russia Critical",
                               Year > 2023 & New_Q78_4 %in% c(1, 2) ~ "Anti-Russia",
                               Year > 2023 & New_Q78_4 %in% c(3, 4) & New_Q78_7 %in% c(1, 2) ~ "Anti-Russia",
                               Year > 2023 & New_Q78_4 %in% c(3, 4) & New_Q78_7 %in% c(3, 4) &
                                 New_Q78_11 %in% c(1, 2) ~ "Russia Critical",
                               Year > 2023 & New_Q78_4 %in% c(3, 4) & New_Q78_7 %in% c(3, 4) &
                                 New_Q78_11 %in% c(3, 4) & New_Q78_5 %in% c(1, 2) ~ "Russia Critical",
                               Year > 2023 & New_Q78_4 %in% c(3, 4) & New_Q78_7 %in% c(3, 4) &
                                 New_Q78_11 %in% c(3, 4) & New_Q78_5 %in% c(3, 4) ~ "Pro-Russia"
                               ))

table(EUI_data$New_Q78_1)

EUI_data <- EUI_data %>% 
  mutate(Pro_russia_scale = case_when(Q68 != 2 ~ 1,
                                       TRUE ~ 0),
        # Pro_russia_scale = Pro_russia_scale + (Year < 2025 & Q68 == 3),
         Pro_russia_scale = Pro_russia_scale + (Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations"),
         Pro_russia_scale = Pro_russia_scale + (Q69i == 3),
         Pro_russia_scale = Pro_russia_scale + (Q5 < 5),
         Pro_russia_scale = Pro_russia_scale + (Q9i %in% c(2, 3)),
         Pro_russia_scale = Pro_russia_scale + (New_Q43 %in% c(3, 4))
         )

table(EUI_data$Pro_russia_scale)
EUI_data <- EUI_data %>% 
  mutate(across(starts_with("New_Q78"), \(x)ifelse(x %in% c(4, 5), 1, 0), .names = "{.col}_binary"),
         Aid_support = rowSums(
           across(New_Q78_1_binary:New_Q78_9_binary),
           na.rm = TRUE
         ))

EUI_data <- EUI_data %>% 
  mutate(
         Pro_russia_scale_short = case_when(Q68 != 2 ~ 1,
                                      TRUE ~ 0),
        # Pro_russia_scale_short = Pro_russia_scale_short + (Year < 2025 & Q68 == 3),
         Pro_russia_scale_short = Pro_russia_scale_short + (Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations"),
         Pro_russia_scale_short = Pro_russia_scale_short + (Q69i == 3),
         Pro_russia_scale_short = Pro_russia_scale_short + (Q5 < 5),
         Pro_russia_scale_short = Pro_russia_scale_short + (Q9i %in% c(2, 3)),
         Pro_russia_scale_short = Pro_russia_scale_short + (New_Q43 %in% c(3, 4)),
         Pro_russia_scale_short = Pro_russia_scale_short + (EUI_Ukraine_Outcome %in% c(1, 2)),
         Pro_russia_scale_short = Pro_russia_scale_short + (Aid_support < 4)
  )

EUI_data$Q69
EUI_data <- EUI_data %>% 
  mutate(appease_russia_scale = case_when(Q68 %in% c(2, 5) ~ 1,
                                      TRUE ~ 0),
         appease_russia_scale = appease_russia_scale + (Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations"),
         appease_russia_scale = appease_russia_scale + (Q69i == 3),
         appease_russia_scale = appease_russia_scale + (Q70i %in% c(3, 4)),
         appease_russia_scale = appease_russia_scale + (Q71a %in% c(3, 4)),
         appease_russia_scale = appease_russia_scale + (Q9i == 1)
  )

EUI_data <- EUI_data %>% 
 mutate(appease_conflict = case_when((New_Q78_1 %in% c(4, 5) | New_Q78_3 %in% c(4, 5) | New_Q78_11 %in% c(4, 5)) &
                                       (New_Q78_4 %in% c(1:3) | New_Q78_5 %in% c(1:3) | New_Q78_7 %in% c(1:3)) ~ 1,
                                     TRUE ~ 0)) 

EUI_data <- EUI_data %>% 
  mutate(appease_russia_scale_short = case_when(Q68 %in% c(2, 5) ~ 1,
                                          TRUE ~ 0),
         appease_russia_scale_short = appease_russia_scale_short + (Q73 == "European countries should invest more in trade and diplomacy with Russia to improve relations"),
         appease_russia_scale_short = appease_russia_scale_short + (Q69i == 3),
         appease_russia_scale_short = appease_russia_scale_short + (Q70i %in% c(3, 4)),
         appease_russia_scale_short = appease_russia_scale_short + (Q71a %in% c(3, 4)),
         appease_russia_scale_short = appease_russia_scale_short + (Q9i == 1),
         appease_russia_scale_short = appease_russia_scale_short + (EUI_Ukraine_Outcome == 3),
         appease_russia_scale_short = appease_russia_scale_short + (appease_conflict == 1)
  )

cor(EUI_data$appease_russia_scale_short, EUI_data$Pro_russia_scale_short, use = "complete.obs")
cor(EUI_data$appease_russia_scale, EUI_data$Pro_russia_scale, use = "complete.obs")

table(EUI_data$appease_russia_scale)
table(EUI_data$appease_russia_scale_short)

table(EUI_data$Pro_russia_scale)
table(EUI_data$Pro_russia_scale_short)
