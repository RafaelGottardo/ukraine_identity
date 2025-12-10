

data_2025 = read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Croatia (w).sav") %>%
  # rename_with(tolower)%>%
  mutate(country = "Croatia",
         Year = "2025") %>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Denmark (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Denmark",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Finland (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Finland",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~France (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "France",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Germany (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Germany",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Greece (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Greece",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Hungary (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Hungary",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Italy (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Italy",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Lithuania (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Lithuania",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Netherlands (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Netherlands",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Poland (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Poland",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Romania (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Romania",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Slovakia (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Slovakia",
                     Year = "2025"))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Spain (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Spain",
                     Year = "2025"))%>%
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Sweden (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Sweden",
                     Year = "2025"))%>%
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~UK (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "UK",
                     Year = "2025")) %>%
  
  # mutate(region =
  #          case_when(country_name == "UK" & region_uk_grouped == 8 ~ "Northern Ireland",
  #                    country_name == "Spain" & region_grouped_es == 1 ~ "Catalonia",
  #                    .default = "Other")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Austria (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Austria",
                     Year = "2025")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Belgium (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Belgium",
                     Year = "2025")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Bulgaria (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Bulgaria",
                     year = "2025")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Czech_Republic (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Czech Republic",
                     Year = "2025")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Ireland (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Ireland",
                     Year = "2025")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Portugal (w).sav") %>%
             # rename_with(tolower) %>%
              mutate(country = "Portugal",
                     Year = "2025")) 

# data_2025 = data_2025 %>%
#   left_join(crosswalk_close_to)
# 
# 
# 
# data_2025_vote = data_2025 %>%
#   filter(is.na(kremlin_ties)) %>%
#   select(-kremlin_ties, -party_note) %>%
#   left_join(crosswalk_vote_2025)
# 
# data_2025 = data_2025 %>%
#   filter(!is.na(kremlin_ties)) %>%
#   bind_rows(data_2025_vote) %>%
#   mutate(year = "2025")

