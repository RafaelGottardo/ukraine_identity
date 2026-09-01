
data_2025 = read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Croatia (w).sav") %>%
  # rename_with(tolower)%>%
  mutate(country = "Croatia",
         Year = "2025",
         HR_2024vote_recode = case_match(HR_2024vote_recode, 
                                         1 ~ 139,
                                         2 ~ 323,
                                         3 ~ 141,
                                         4 ~ 278,
                                         5 ~ 142,
                                         6 ~ 285)) %>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Denmark (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Denmark",
                     Year = "2025",
                     FT22_DK = case_match(FT22_DK, 
                                         1 ~ 61,
                                         2 ~ 62,
                                         3 ~ 63,
                                         17 ~ 64,
                                         4 ~ 66,
                                         5 ~ 67,
                                         6 ~ 77,
                                         7 ~ 69,
                                         8 ~ 71,
                                         23 ~ 162,
                                         9 ~ 72,
                                         16 ~ 73)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Finland (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Finland",
                     Year = "2025",
                     FT23 = case_match(FT23,
                                       1 ~ 82,
                                       2 ~ 83,
                                       3 ~ 84,
                                       4 ~ 85,
                                       5 ~ 86,
                                       6 ~ 87,
                                       7 ~ 88,
                                       8 ~ 89
                                       )))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~France (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "France",
                     Year = "2025", 
                     fr_pastvote_legislative24_round1 = case_match(fr_pastvote_legislative24_round1, 
                                                                   1 ~ 308,
                                                                   2 ~ 14,
                                                                   3 ~ 168,
                                                                   4 ~ 164
                                                                   )))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Germany (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Germany",
                     Year = "2025", 
                     q_BTW25_Quote = case_match(q_BTW25_Quote, 
                                                1 ~ 19,
                                                2 ~ 20,
                                                3 ~ 21,
                                                4 ~ 22, 
                                                5 ~ 23,
                                                6 ~ 24,
                                                7 ~ 254)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Greece (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Greece",
                     Year = "2025",
                     GR_2023vote_recode = case_match(GR_2023vote_recode, 
                                                     1 ~ 118,
                                                     2 ~ 119,
                                                     3 ~ 120,
                                                     4 ~ 121,
                                                     5 ~ 318,
                                                     6 ~ 122
                                                     )))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Hungary (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Hungary",
                     Year = "2025",
                     HU_2022vote_recode = case_match(HU_2022vote_recode,
                                                1 ~ 103,
                                                3 ~ 105,
                                                2 ~ 200
                                                )))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Italy (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Italy",
                     Year = "2025",
                     IT_pastvote_2022 = case_match(IT_pastvote_2022,
                                                   1 ~ 42,
                                                   2 ~ 43,
                                                   3 ~ 44,
                                                   4 ~ 45,
                                                   5 ~ 46,
                                                   6 ~ 47,
                                                   7 ~ 456,
                                                   8 ~ 151,
                                                   9 ~ 999
                                                   )))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Lithuania (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Lithuania",
                     Year = "2025", 
                     pastvote2024LT = case_match(pastvote2024LT,
                                                         1 ~ 113,
                                                         2 ~ 110,
                                                         3 ~ 326,
                                                         4 ~ 194,
                                                         5 ~ 325,
                                                         6 ~ 111)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Netherlands (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Netherlands",
                     Year = "2025",
                     NL_2023vote_recode = case_match(NL_2023vote_recode,
                                                      1 ~ 50,
                                                     2 ~ 258,
                                                     3 ~ 49,
                                                     4 ~ 259,
                                                     5 ~ 52,
                                                     6 ~ 260,
                                                     7 ~ 51,
                                                     8 ~ 54,
                                                     9 ~ 59,
                                                     10 ~ 58,
                                                     11 ~ 60,
                                                     12 ~ 312,
                                                     13 ~ 56)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Poland (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Poland",
                     Year = "2025",
                     PL_2023vote_recode = case_match(PL_2023vote_recode, 
                                                      1 ~ 92,
                                                      2 ~ 95,
                                                      3 ~ 93,
                                                      4 ~ 261,
                                                      5 ~ 94)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Romania (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Romania",
                     Year = "2025",
                     RO_2024vote_recode = case_match(RO_2024vote_recode, 
                                                     1 ~ 96,
                                                     2 ~ NA,
                                                     3 ~ 97,
                                                     4 ~ 199,
                                                     5 ~ 263,
                                                     6 ~ 315,
                                                     7 ~ 100)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Slovakia (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Slovakia",
                     Year = "2025",
                     SK_2023vote_recode = case_match(SK_2023vote_recode,
                                                     1 ~ 132,
                                                     2 ~ 331,
                                                     3 ~ 196,
                                                     4 ~ 332,
                                                     5 ~ 138,
                                                     6 ~ 136,
                                                     7 ~ 197)))%>%
  
  bind_rows(read_spss("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Spain (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Spain",
                     Year = "2025",
                     ES_pastvote_July2023_recoded = case_match(ES_pastvote_July2023_recoded,
                                                      1 ~ 26,
                                                      2 ~ 25,
                                                      3 ~ 27,
                                                      4 ~ 255,
                                                      5 ~ 182,
                                                      6 ~ 32,
                                                      7 ~ 35,
                                                      8 ~ 34,
                                                      9 ~ NA,
                                                      10 ~ NA,
                                                      11 ~ NA,
                                                      12 ~ NA,
                                                      13 ~ 36
                                                      )))%>%
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Sweden (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Sweden",
                     Year = "2025",
                     FT22_SE = case_match(FT22_SE, 
                                       1 ~ 74,
                                       2 ~ 75,
                                       3 ~ 76,
                                       4 ~ 77,
                                       6 ~ 79,
                                       7 ~ 80,
                                       10 ~ 81)))%>%
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~UK (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "UK",
                     Year = "2025",
                     Vote2024_combo = case_when(Vote2024_combo == 1 ~ 1,
                                                Vote2024_combo ==  2 ~ 2,
                                                Vote2024_combo ==  3 ~ 3,
                                                Vote2024_combo ==  4 ~ 4,
                                                Vote2024_combo ==  5 ~ 5,
                                                Vote2024_combo == 6 ~ 250,
                                                Vote2024_combo ==  7 ~ 7,
                                                pastvote_ge_2024_ni == 1 ~ 8,
                                                pastvote_ge_2024_ni == 2 ~ 9,
                                                pastvote_ge_2024_ni == 3 ~ 10,
                                                pastvote_ge_2024_ni == 4 ~ 11,
                                                pastvote_ge_2024_ni == 5 ~ 12,
                                                pastvote_ge_2024_ni == 6 ~ 305))) %>%
  
  # mutate(region =
  #          case_when(country_name == "UK" & region_uk_grouped == 8 ~ "Northern Ireland",
  #                    country_name == "Spain" & region_grouped_es == 1 ~ "Catalonia",
  #                    .default = "Other")) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Austria (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Austria",
                     Year = "2025",
                     AT_2024vote_recode = case_match(AT_2024vote_recode,
                                                     1 ~ 340,
                                                     2 ~ 341,
                                                     3 ~ 339,
                                                     4 ~ 343,
                                                     5 ~ 342))) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Belgium (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Belgium",
                     Year = "2025",
                     BE_2024vote_recode = case_match(BE_2024vote_recode, 
                                                     1 ~ 291,
                                                     2 ~ 292,
                                                     3 ~ 297,
                                                     4 ~ 295,
                                                     5 ~ 298,
                                                     6 ~ 293,
                                                     7 ~ 294,
                                                     8 ~ 301,
                                                     9 ~ 296,
                                                     10 ~ 300))) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Bulgaria (w).sav") %>%
            #  rename_with(tolower)%>%
              mutate(country = "Bulgaria",
                     year = "2025",
                     BG_2024vote_recode = case_match(BG_2024vote_recode,
                                                     1 ~ 125,
                                                     2 ~ 126,
                                                     3 ~ 128,
                                                     4 ~ 127,
                                                     5 ~ 319,
                                                     6 ~ 320,
                                                     7 ~ 271))) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Czech_Republic (w).sav") %>%
             # rename_with(tolower)%>%
              mutate(country = "Czech Republic",
                     Year = "2025",
                     CZ_2021vote_recode = case_match(CZ_2021vote_recode,
                                                     1 ~ 346,
                                                     2 ~ 347,
                                                     3 ~ 348,
                                                     4 ~ 350,
                                                     5 ~ 351,
                                                     6 ~ 357,
                                                     7 ~ 353 ))) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Ireland (w).sav") %>%
              # rename_with(tolower)%>%
              mutate(country = "Ireland",
                     Year = "2025",
                     pastvote2024IE = case_match(pastvote2024IE, 
                                                       1 ~ 370,
                                                       2 ~ 372,
                                                       3 ~ 371,
                                                       4 ~ 373,
                                                       5 ~ 374,
                                                       6 ~ 375,
                                                       7 ~ 376,
                                                       9 ~ 377,
                                                       10 ~ 378))) %>% 
  
  bind_rows(read_sav("Data_raw/2025_country_data-sav/P_EUI_EuropeanSolidarity_2025_Master ~Portugal (w).sav") %>%
             # rename_with(tolower) %>%
              mutate(country = "Portugal",
                     Year = "2025",
                     PT_2024vote_recode = case_match(PT_2024vote_recode,
                                                     1 ~ 381,
                                                     2 ~ 361,
                                                     3 ~ 364,
                                                     4 ~ 369,
                                                     5 ~ 362,
                                                     6 ~ 363,
                                                     7 ~ 367
                                                     
                                                
                                                     ))) 

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

