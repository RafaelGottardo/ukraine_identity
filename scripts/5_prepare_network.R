##### Prepare Data for network analysis ####

for(i in c(2022, 2023, 2024, 2025)){
  
horizontial_df <- EUI_data_short %>% 
  filter(Year == i) %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(
         # Russia_trust = ifelse(A5_2 >= 5, 1, 0),
         # Ukraine_trust = ifelse(A5_3 >= 5, 1, 0),
         # 
         # Russia_distrust = ifelse(A5_2 >= 5, 0, 1),
         # Ukraine_distrust = ifelse(A5_3 >= 5, 0, 1),
         # Terrorism_threat = ifelse(Q68 == 1, 1, 0),
         # Russia_threat = ifelse(Q68 == 2, 1, 0),
         # US_threat = ifelse(Q68 == 3, 1, 0),
         # China_threat = ifelse(Q68 == 4, 1, 0),
         # Nuclear_threat = ifelse(Q68 == 5, 1, 0),
         # Other_threat = ifelse(Q68 == 6, 1, 0),
         # DK_threat = ifelse(Q68 == 7, 1, 0),
         Refugee_A = ifelse(New_Q78_1 %in% c(3, 4), 1, 0),
         #Refugee_A = ifelse(New_Q78_1 == 3, 1, 0),
         Refugee_D = ifelse(New_Q78_1 %in% c(1, 2), 1, 0),
         #Refugee_SD = ifelse(New_Q78_1 == 1, 1, 0),
         Refugee_DK = ifelse(New_Q78_1 == 5, 1, 0),
         #Ukr_EU_SA = ifelse(New_Q78_7 == 4, 1, 0),
         Ukr_EU_A = ifelse(New_Q78_7 %in% c(3, 4), 1, 0),
         Ukr_EU_D = ifelse(New_Q78_7 %in% c(1, 2), 1, 0),
        # Ukr_EU_SD = ifelse(New_Q78_7 == 1, 1, 0),
         Ukr_EU_DK = ifelse(New_Q78_7 == 5, 1, 0),
         Defence_Focused = ifelse(Security_FA < 0, 1, 0),
         Normalizaiton_Focused = ifelse(Security_FA > 0, 1, 0),
         Ter_Russia_goals = ifelse(EUI_Ukraine_Outcome %in% c(1, 2), 1, 0),
         #Ter_Russia_more = ifelse(EUI_Ukraine_Outcome == 2, 1, 0),
         Ter_pre_war = ifelse(EUI_Ukraine_Outcome == 3, 1, 0),
         Ter_Ukr_gain = ifelse(EUI_Ukraine_Outcome %in% c(4, 5), 1, 0)#,
        # Ter_ukr_retake = ifelse(EUI_Ukraine_Outcome == 5, 1, 0)
) %>% 
  select(#US_trust,
         # Russia_trust,
         # Ukraine_trust,
         # #China_trust,
         # #US_distrust,
         # Russia_distrust,
         # Ukraine_distrust,
         #China_distrust,
         # Terrorism_threat,
         # Russia_threat,
         # US_threat,
         # China_threat,
         # Nuclear_threat,
         # Other_threat,
         # DK_threat,
        # Refugee_SA,
         Refugee_A,
         Refugee_D,
         #Refugee_SD,
         Refugee_DK,
         #Ukr_EU_SA,
         Ukr_EU_A,
         Ukr_EU_D,
         #Ukr_EU_SD,
         Ukr_EU_DK,
         Defence_Focused,
         Normalizaiton_Focused,
         Ter_Russia_goals,
        # Ter_Russia_more,
         Ter_pre_war,
         Ter_Ukr_gain#,
        # Ter_ukr_retake
         ) %>% 
  drop_na()

write.csv(file = paste0("data_clean/horizontial_df", i, ".csv"), horizontial_df)
}

horizontial_df <- EUI_data_short %>% 
  filter(Year == 2025) %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(
     Russia_trust = ifelse(A5_2 >= 5, 1, 0),
     Ukraine_trust = ifelse(A5_3 >= 5, 1, 0),
    # 
     Russia_distrust = ifelse(A5_2 >= 5, 0, 1),
     Ukraine_distrust = ifelse(A5_3 >= 5, 0, 1),
    # Terrorism_threat = ifelse(Q68 == 1, 1, 0),
    # Russia_threat = ifelse(Q68 == 2, 1, 0),
    # US_threat = ifelse(Q68 == 3, 1, 0),
    # China_threat = ifelse(Q68 == 4, 1, 0),
    # Nuclear_threat = ifelse(Q68 == 5, 1, 0),
    # Other_threat = ifelse(Q68 == 6, 1, 0),
    # DK_threat = ifelse(Q68 == 7, 1, 0),
    Refugee_A = ifelse(New_Q78_1 %in% c(3, 4), 1, 0),
    #Refugee_A = ifelse(New_Q78_1 == 3, 1, 0),
    Refugee_D = ifelse(New_Q78_1 %in% c(1, 2), 1, 0),
    #Refugee_SD = ifelse(New_Q78_1 == 1, 1, 0),
    Refugee_DK = ifelse(New_Q78_1 == 5, 1, 0),
    #Ukr_EU_SA = ifelse(New_Q78_7 == 4, 1, 0),
    Ukr_EU_A = ifelse(New_Q78_7 %in% c(3, 4), 1, 0),
    Ukr_EU_D = ifelse(New_Q78_7 %in% c(1, 2), 1, 0),
    # Ukr_EU_SD = ifelse(New_Q78_7 == 1, 1, 0),
    Ukr_EU_DK = ifelse(New_Q78_7 == 5, 1, 0),
    Defence_Focused = ifelse(Security_FA < 0, 1, 0),
    Normalizaiton_Focused = ifelse(Security_FA > 0, 1, 0),
    Ter_Russia_goals = ifelse(EUI_Ukraine_Outcome %in% c(1, 2), 1, 0),
    #Ter_Russia_more = ifelse(EUI_Ukraine_Outcome == 2, 1, 0),
    Ter_pre_war = ifelse(EUI_Ukraine_Outcome == 3, 1, 0),
    Ter_Ukr_gain = ifelse(EUI_Ukraine_Outcome %in% c(4, 5), 1, 0)#,
    # Ter_ukr_retake = ifelse(EUI_Ukraine_Outcome == 5, 1, 0)
  ) %>% 
  select(#US_trust,
     Russia_trust,
     Ukraine_trust,
    # #China_trust,
    # #US_distrust,
     Russia_distrust,
     Ukraine_distrust,
    #China_distrust,
    # Terrorism_threat,
    # Russia_threat,
    # US_threat,
    # China_threat,
    # Nuclear_threat,
    # Other_threat,
    # DK_threat,
    # Refugee_SA,
    Refugee_A,
    Refugee_D,
    #Refugee_SD,
    Refugee_DK,
    #Ukr_EU_SA,
    Ukr_EU_A,
    Ukr_EU_D,
    #Ukr_EU_SD,
    Ukr_EU_DK,
    Defence_Focused,
    Normalizaiton_Focused,
    Ter_Russia_goals,
    # Ter_Russia_more,
    Ter_pre_war,
    Ter_Ukr_gain#,
    # Ter_ukr_retake
  ) %>% 
  drop_na()

write.csv(file = paste0("data_clean/horizontial_df", ".csv"), horizontial_df)

for(i in c(2022, 2023, 2024, 2025)){
vertical_extension_df <- EUI_data_short %>% 
  filter(Year == i) %>% 
  filter(country %in% COUNTRIES_2022) %>% 
   mutate(#Support_aggrandizement = ifelse(Support_Aggrandizement >= 4, 1, 0),
  #        Neutral_aggrandizement = ifelse(Support_Aggrandizement > 2 & Support_Aggrandizement < 4, 1, 0),
  #        Oppose_aggrandizement = ifelse(Support_Aggrandizement <= 2, 1, 0),
         Low_democracy = ifelse(Q5 < 4, 1, 0),
         Middle_democracy = ifelse(Q5 >= 4 & Q5 <= 7, 1, 0),
         High_democracy = ifelse(Q5 > 7, 1, 0),
         # Ideology_Right = ifelse(ideology == "Right-wing", 1, 0),
         # Ideology_Left = ifelse(ideology == "Left-wing", 1, 0),
         # Ideology_Centre = ifelse(ideology == "Centre", 1, 0),
         # Ideology_DK = ifelse(ideology == "Don't Know", 1, 0),
         #Climate = Climate,
         # Terrorism_threat = ifelse(Q68 == 1, 1, 0),
         Russia_threat = ifelse(Q68 == 2, 1, 0),
         # US_threat = ifelse(Q68 == 3, 1, 0),
         # China_threat = ifelse(Q68 == 4, 1, 0),
         # Nuclear_threat = ifelse(Q68 == 5, 1, 0),
         Other_threat = ifelse(Q68 %in% c(1, 3, 4, 5, 6, 7), 1, 0),
        # DK_threat = ifelse(Q68 == 7, 1, 0),
         Defence_Focused = ifelse(Security_FA < 0, 1, 0),
         Normalizaiton_Focused = ifelse(Security_FA > 0, 1, 0),
        Trust_EU = ifelse(New_Q43i %in% c(1, 2), 1, 0),
        Do_not_trust_EU = ifelse(New_Q43i %in% c(3, 4), 1, 0),
        Trust_Gov = ifelse(New_Q41 %in% c(1, 2), 1, 0),
        Do_not_trust_Gov = ifelse(New_Q41 %in% c(3, 4), 1, 0),
         # Immigration_Support = Immigration_types_23_1 + Immigration_types_23_2 + Immigration_types_23_3 + Immigration_types_23_4,
         # Immigration_Support_Low = ifelse(Immigration_Support < 8, 1, 0),
         # Immigration_Support_Mid = ifelse(Immigration_Support >= 8 & Immigration_Support <= 12, 1, 0),
         # Immigration_Support_High = ifelse(Immigration_Support > 12, 1, 0),
         Support_NATO = Q71,
         Support_EU = Q9,
         Energy = Q7_12,
         Inflation = Q7_2,
         EU_punish_nointerference = ifelse(Q67_revisions == 1, 1, 0),
         EU_punish_financial = ifelse(Q67_revisions == 2, 1, 0),
         EU_punish_voting = ifelse(Q67_revisions == 3, 1, 0),
         EU_punish_other = ifelse(Q67_revisions %in% c(4, 5), 1, 0),
         General_trust_trust = ifelse(Q59 == 1, 1, 0),
         General_trust_notrust = ifelse(Q59 == 2, 1, 0),
         Econ_better_off = ifelse(Econ_comparison == "Better off", 1, 0),
         Econ_the_same = ifelse(Econ_comparison == "The same", 1, 0),
         Econ_worse_off = ifelse(Econ_comparison == "Worse off", 1, 0),
         Econ_DK = ifelse(Econ_comparison == "Don't Know", 1, 0)
         
  ) %>% 
  select(Support_NATO,
         Support_EU,
         Defence_Focused,
         Normalizaiton_Focused,
         # Support_aggrandizement,
         # Neutral_aggrandizement,
         # Oppose_aggrandizement,
         Low_democracy,
         Middle_democracy,
         High_democracy,
         # Ideology_Right,
         # Ideology_Left,
         # Ideology_Centre,
         # Ideology_DK,
         # Climate,
         Russia_threat,
         #US_threat,
         #China_threat,
         #Nuclear_threat,
         Other_threat,
         #DK_threat,
         #Inflation,
         # Immigration_Support_Low,
         # Immigration_Support_Mid,
         # Immigration_Support_High,
         # Energy,
         EU_punish_nointerference,
         EU_punish_financial,
         EU_punish_voting,
         EU_punish_other,
         Trust_EU,
         Do_not_trust_EU,
         Trust_Gov,
         Do_not_trust_Gov,
          General_trust_trust,
          General_trust_notrust
         # Econ_better_off,
         # Econ_the_same,
         # Econ_worse_off,
         # Econ_DK
         ) %>% 
  drop_na()





write.csv(file = paste0("data_clean/vertical_extension", i, ".csv"), vertical_extension_df)


}

vertical_extension_df <- EUI_data_short %>% 
  filter(Year == 2025) %>% 
  filter(country %in% COUNTRIES_2022) %>% 
  mutate(Support_aggrandizement = ifelse(Support_Aggrandizement >= 4, 1, 0),
           Neutral_aggrandizement = ifelse(Support_Aggrandizement > 2 & Support_Aggrandizement < 4, 1, 0),
            Oppose_aggrandizement = ifelse(Support_Aggrandizement <= 2, 1, 0),
    Low_democracy = ifelse(Q5 < 4, 1, 0),
    Middle_democracy = ifelse(Q5 >= 4 & Q5 <= 7, 1, 0),
    High_democracy = ifelse(Q5 > 7, 1, 0),
    # Ideology_Right = ifelse(ideology == "Right-wing", 1, 0),
    # Ideology_Left = ifelse(ideology == "Left-wing", 1, 0),
    # Ideology_Centre = ifelse(ideology == "Centre", 1, 0),
    # Ideology_DK = ifelse(ideology == "Don't Know", 1, 0),
    #Climate = Climate,
    # Terrorism_threat = ifelse(Q68 == 1, 1, 0),
    Russia_threat = ifelse(Q68 == 2, 1, 0),
    # US_threat = ifelse(Q68 == 3, 1, 0),
    # China_threat = ifelse(Q68 == 4, 1, 0),
    # Nuclear_threat = ifelse(Q68 == 5, 1, 0),
    Other_threat = ifelse(Q68 %in% c(1, 3, 4, 5, 6, 7), 1, 0),
    # DK_threat = ifelse(Q68 == 7, 1, 0),
    Defence_Focused = ifelse(Security_FA < 0, 1, 0),
    Normalizaiton_Focused = ifelse(Security_FA > 0, 1, 0),
    Trust_EU = ifelse(New_Q43i %in% c(1, 2), 1, 0),
    Do_not_trust_EU = ifelse(New_Q43i %in% c(3, 4), 1, 0),
    Trust_Gov = ifelse(New_Q41 %in% c(1, 2), 1, 0),
    Do_not_trust_Gov = ifelse(New_Q41 %in% c(3, 4), 1, 0),
    # Immigration_Support = Immigration_types_23_1 + Immigration_types_23_2 + Immigration_types_23_3 + Immigration_types_23_4,
    # Immigration_Support_Low = ifelse(Immigration_Support < 8, 1, 0),
    # Immigration_Support_Mid = ifelse(Immigration_Support >= 8 & Immigration_Support <= 12, 1, 0),
    # Immigration_Support_High = ifelse(Immigration_Support > 12, 1, 0),
    Support_NATO = Q71,
    Support_EU = Q9,
    Energy = Q7_12,
    Inflation = Q7_2,
    EU_punish_nointerference = ifelse(Q67_revisions == 1, 1, 0),
    EU_punish_financial = ifelse(Q67_revisions == 2, 1, 0),
    EU_punish_voting = ifelse(Q67_revisions == 3, 1, 0),
    EU_punish_other = ifelse(Q67_revisions %in% c(4, 5), 1, 0),
    General_trust_trust = ifelse(Q59 == 1, 1, 0),
    General_trust_notrust = ifelse(Q59 == 2, 1, 0),
    Econ_better_off = ifelse(Econ_comparison == "Better off", 1, 0),
    Econ_the_same = ifelse(Econ_comparison == "The same", 1, 0),
    Econ_worse_off = ifelse(Econ_comparison == "Worse off", 1, 0),
    Econ_DK = ifelse(Econ_comparison == "Don't Know", 1, 0)
    
  ) %>% 
  select(Support_NATO,
         Support_EU,
         Defence_Focused,
         Normalizaiton_Focused,
          Support_aggrandizement,
          Neutral_aggrandizement,
          Oppose_aggrandizement,
         Low_democracy,
         Middle_democracy,
         High_democracy,
         # Ideology_Right,
         # Ideology_Left,
         # Ideology_Centre,
         # Ideology_DK,
         # Climate,
         Russia_threat,
         #US_threat,
         #China_threat,
         #Nuclear_threat,
         Other_threat,
         #DK_threat,
         #Inflation,
         # Immigration_Support_Low,
         # Immigration_Support_Mid,
         # Immigration_Support_High,
         # Energy,
         EU_punish_nointerference,
         EU_punish_financial,
         EU_punish_voting,
         EU_punish_other,
         Trust_EU,
         Do_not_trust_EU,
         Trust_Gov,
         Do_not_trust_Gov,
         General_trust_trust,
         General_trust_notrust
         # Econ_better_off,
         # Econ_the_same,
         # Econ_worse_off,
         # Econ_DK
  ) %>% 
  drop_na()





write.csv(file = paste0("data_clean/vertical_extension", ".csv"), vertical_extension_df)
  