
##### CODE TO IMPORT DATASETS ####

#### source helper scripts ####

source("scripts/0_functions.R")
source("scripts/1_Import_2025.R")


crosswalk = read_csv("data_raw/new_crosswalk_june_9_2025.csv") 

#### 2023 ####

EUI_2023 <- read_dta("data_raw/EUI YouGov 2023/SiE dataset_2023_merged_all countries.dta")

EUI_2023 <- EUI_2023 %>% 
  filter(attentioncheck1_23_1 == 4 & attentioncheck2_23 == 4) %>% 
  select(country, q2, q4, q5,  q67_revisions, new_q5_1, new_q5_6, q9, q7_revisions_1:q7_revisions_16, q18_revisions, q21, q24, q26_revisions, q30, q37, q40,
         new_q43, q44, eui_ukraine_outcome, q62, starts_with("q47a_"), new_q79, new_q62a,  q59, age_grp_all,
         work_industry_shortlist, profile_work_stat, 
         q61, q63,  q4, new_q41, new_q5_1, new_q5_6, 
         q68_revisions, gender_all, education_merged1_101, education_merged1_102, education_merged1_103,
          q62, country_birth, new_q60a, glob_areatype,immigration_types_23_1, immigration_types_23_2, immigration_types_23_3, immigration_types_23_4,
         q69, q70, q71, q73, pastvote, weight, matches("new_q78_\\d{1,2}$")) %>% 
  mutate(Year = 2023,
         Q9i = q9,
         Q9 = case_match(q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0, 999 ~ NA),
         across(starts_with("q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         Q62 = case_when(q62 == 8 ~ 8, 
                         q62 == 100 ~ 8,
                         TRUE ~ q62),
        # Q18_revisions = replace(Q18_revisions, Q18_revisions == 11, 5),
        Q69i = q69,
         Q69 = case_match(q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
        Q70i = q70,
         Q70 = case_match(q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71a = q71,
         Q71 = case_match(q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
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
         Q73 = case_match(q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ "Neither",
                          4 ~ "DK"),
        Past_vote = case_match(pastvote, 
                              "Conservatives" ~ 1,
                              "Labour" ~ 2,
                              "Greens" ~ 3,
                              "Plaid Cymru" ~ 5,
                              "Brexit Party" ~ 250,
                              "SNP" ~ 4,
                              "Liberalerna" ~ 76,
                              "Moderaterna" ~ 74,
                              "Æ Danmarksdemokraterne - Inger Støjberg" ~ 162,
                              "Miljöpartiet" ~ 78,
                              "Kristdemokraterna" ~ 77,
                              "Feministiskt Initiativ" ~ 707,
                              "Centerpartiet" ~ 75,
                              "Socialdemokraterna" ~ 79,
                              "Piratpartiet" ~ 708,
                              "Moderaterne" ~ 160,
                              "Vänsterpartiet" ~ 80, 
                              "Q Frie Grønne" ~ 708,
                              "New Democracy ND" ~ 118,
                              "Communist Party of Greece, Kommounistikó Kómma Elládas, KKE" ~ 121,
                              "SYRIZA" ~ 119,
                              "European Realistic Disobedience Front" ~ 123,
                              "Greek Solution" ~ 122,
                              "Movement for Change KINAL" ~ 120,
                              "Golden Dawn"  ~ NA,
                              "United for Hungary" ~ 200,
                              "Fidesz–KDNP Party Alliance" ~ 103,
                              "Our Homeland Movement" ~ 105,
                              "Labour Party" ~ 330,
                              "Homeland Union – Lithuanian Christian Democrats"  ~ 110,
                              "Lithuanian Farmers and Greens Union" ~ 111,
                              "Liberal Movement of the Republic of Lithuania" ~ 325,
                              "Lithuanian Regions Party" ~ 117,
                              "Electoral Action of Poles in Lithuania – Christian Families Alliance or EAPL–CFA" ~ 116,
                              "Movimento 5 Stelle" ~ 42,
                              "Plus Europe" ~ 47,
                              "Fratelli d'Italia'" ~ 45,
                              "Forza Italia" ~ 44,
                              "Azione-Italia Viva" ~ 256,
                              "Partito Democratico" ~ 46,
                              "Lega" ~ 43,
                              "Alleanza Verdi-Sinistra" ~ 151,
                              "Prawo i Sprawiedliwość, PiS" ~ 92,
                              "Koalicja Polska"  ~ 95,
                              "Konfederacja Wolność i Niepodległość" ~ 94,
                              "Forum voor Democratie" ~ 60,
                              "Democraten 66 (D66)" ~ 52,
                              "Overige" ~ NA,
                              "Volkspartij voor Vrijheid en Democratie (VVD)" ~ 49,
                              "GroenLinks" ~ 258,
                              "Christen Democratisch Appèl (CDA)" ~ 51,
                              "Partij van de Arbeid (PvdA)"  ~ 258,
                              "Socialistische Partij (SP)" ~ 54,
                              "Partij voor de Dieren" ~ 58,
                              "Partij voor de Vrijheid (PVV)"  ~ 50,
                              "50PLUS" ~ NA,
                              "ChristenUnie/SGP" ~ NA,
                              "Denk" ~ 59,
                              "Alianța pentru Unirea Românilor (AUR)" ~ 99,
                              "Partidul Național Liberal (PNL)"  ~ 97,
                              "Partidul Social Democrat (PSD)"  ~ 96,
                              "Alianța 2020 USR-PLUS"  ~ 199,
                              "PRO România (PRO)" ~ NA,
                              "Romániai Magyar Demokrata Szövetség (RMDSZ)/ Uniunea Democrată Maghiară din România (UDMR)" ~ 100,
                              "Partidul Mișcarea Populară (PMP)"  ~ NA,
                              "Direction – Slovak Social Democracy, formerly and legally called Direction – Social Democracy" ~ 132,
                              "Ordinary People and Independent Personalities (OĽANO), NOVA, Christian Union (KÚ), ZMENA ZDOLA" ~ 332,
                              "People's Party Our Slovakia" ~ 197,
                              "We Are Family" ~ 133,
                              "Christian Democratic Movement" ~ 138,
                              "Freedom and Solidarity"  ~ 136,
                              "Restart koalicija" ~ 323,
                              "Zeleno–lijeva koalicija"  ~ 278,
                              "Most" ~ 142,
                              "Hrvatska demokratska zajednica, HDZ" ~ 139,
                              "Domovinski pokret Miroslava Škore, DPMŠ" ~ 141,
                              "Centar" ~ 713,
                              "Възраждане" ~ 128,
                              "Продължаваме промяната" ~ 126,
                              "ГЕРБ-СДС" ~ 125,
                              "Движение за права и свободи, ДПС" ~ 127,
                              "Български възход" ~ NA,
                              "Демократична България, ДБ" ~ 126,
                              "БСП за България" ~ 319,
                              "Más País-Equo" ~ 255,
                              "Unidas Podemos+Podemos EU" ~ 28,
                              "ERC-Sobiranistes" ~ 182,
                              "En Comú Podem" ~ 28,
                              "PSOE"   ~ 25,
                              "PRC" ~ NA,
                              "Ciudadanos" ~ NA,
                              "PP" ~ 26,
                              "EAJ-PNV" ~ 34,
                              "Vox" ~ 27,
                              "CUP-PR" ~ 36,
                              "PACMA" ~ NA,
                              "Junts-JuntsxCat" ~ 32,
                              "BNG" ~ NA,
                              "EH Bildu" ~ 35,
                              "Més Compromís" ~ NA,
                              "Kokoomus" ~ 83,
                              "Perussuomalaiset" ~ 89,
                              "Vihreä liitto" ~ 86,
                              "SDP, Sosialidemokraattinen Puolue" ~ 84,
                              "Change the party" ~ 722,
                              "Vasemmistoliitto" ~ 85,
                              "Keskusta" ~ 82,
                              "Kristillisdemokraatit" ~ 87,
                              "Sininen tulevaisuus" ~ 723,
                              "RKP, Suomen ruotsalainen kansanpuolue" ~ 88,
                              "Marine Le Pen (Rassemblement National)" ~ 164,
                              "Emmanuel Macron (La République En Marche)"  ~ 308,
                              "Valérie Pécresse (Les Républicains)" ~ 14,
                              "Eric Zemmour (Reconquête!)" ~ 165,
                              "Jean-Luc Mélenchon (La France Insoumise)" ~ 16,
                              #"Yannick Jadot (Europe Écologie Les Verts)"   ~ 309,
                              "Jean Lassalle (Résistons!)" ~ 165,
                              "Nicolas Dupont-Aignan (Debout La France)"  ~ 169,
                              "Philippe Poutou (Nouveau Parti Anticapitaliste)" ~ NA,
                              "Fabien Roussel (Parti Communiste Français)" ~ 168,
                              "Anne Hidalgo (Parti Socialiste)"    ~ NA,
                              "Nathalie Arthaud (Lutte Ouvrière)" ~ NA,
                              "SPD" ~ 19,
                              "FDP" ~ 23,
                              "AfD" ~ 22,
                              "Bündnis 90/Die Grünen" ~ 21,
                              "CDU/CSU" ~ 20,
                              "Die Linke" ~ 24,
                          "Sverigedemokraterna" ~ 81
                              ),
        Urban = case_when(glob_areatype == 1 ~ "Urban/Suburban",
                          glob_areatype == 2 ~ "Urban/Suburban",
                          glob_areatype %in% c(3, 4, 5) ~ "Other",
                          TRUE ~ "Other"),
         across(matches("new_q78_\\d$"),\(x)as.numeric(x)),
        New_Q43i = new_q43,
        New_Q43 = ifelse(new_q43 %in% c(1, 2), 1, 0)) %>% 
  rename("Q18" = "q18_revisions", 
         "Q26" = "q26_revisions",
         "Q68" = "q68_revisions",
         "Q59" = "q59", 
         "Q61" = "q61",
         "Q5" = "q5",
         "New_Q5_1" = "new_q5_1",
         "New_Q5_6" = "new_q5_6",
         "Q67_revisions" = "q67_revisions",
         "EUI_Ukraine_Outcome" = "eui_ukraine_outcome",
         "New_Q78_4" = "new_q78_4",
         "New_Q78_5" = "new_q78_5",
         "New_Q78_1" = "new_q78_1",
         "New_Q78_7" = "new_q78_7",
         "Q2" = "q2",
         "New_Q41" = "new_q41",
         "New_Q60a" = "new_q60a",
         "New_Q62a" = "new_q62a",
         "Glob_areatype" = "glob_areatype",
         "Immigration_types_23_1" = "immigration_types_23_1", 
         "Immigration_types_23_2" = "immigration_types_23_2", 
         "Immigration_types_23_3" = "immigration_types_23_3",
         "Immigration_types_23_4" = "immigration_types_23_4") %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("q7_revisions_"))

#### 2024 ####

vote_2024 = read_csv("data_raw/yg_2024_vote_values.csv")

vote_2024 <- vote_2024 %>% 
  mutate(vote_values_2024 = replace(vote_values_2024, vote_values_2024 %in% c(98, 99), NA),
         caseid = as.character(caseid)) %>% 
  left_join(crosswalk %>% select(vote_values_2024, new_q59, country_name) %>% filter(!is.na(vote_values_2024)), by = c( "country_r" = "country_name", "vote_values_2024")) %>% 
  rename(Past_vote = new_q59)

EUI_2024 <- read_spss("Data_raw/Results for EUI, SOU and Solidarity 2024 - OMGLOB016 - Merged.sav")

EUI_2024 <- EUI_2024 %>% 
  filter(AttentionCheck2_23 == 4)

EUI_2024 <- EUI_2024 %>% 
  mutate(New_Q59 = replace(New_Q59, New_Q59 == 14, 414),
         New_Q61 = replace(New_Q61, New_Q61 == 14, 414))

val_label(EUI_2024$New_Q59, 414) <- "Les Républicains/ Union de la droite et du centre"
val_label(EUI_2024$New_Q61, 414) <- "Les Républicains/ Union de la droite et du centre"


EUI_2024 <- EUI_2024 %>% 
  select(caseid, country, Q4, Q2,# Q5_DE, 
         age_grp_gen_edu_1, age_grp_gen_edu_2, age_grp_gen_edu_3, age_grp_gen_edu_4, age_grp_gen_edu_5,
         New_Q6a, Q5, Q7_revisions_1:Q7_revisions_16, Q9, Q18_revisions, Q21, Q24, Q26_revisions, Q30, Q37, Q40, Q44, 
         Q47a_3, Q47a_7, Q47a_8, Q62, country_birth, New_Q60a, New_Q62a, Q59,
         Q61, Q63,  Q4, profile_work_stat, starts_with("income"), New_Q41,
         EUI_Ukraine_Outcome, Israel_Palestine_2024, Q68_revisions, US_Elections_2024, Q62, age_grp_gen_edu_6, age_grp_gen_edu_7, Q61, age_grp_gen_edu_18, 
         age_grp_gen_edu_19, age_grp_gen_edu_20, Q62, Glob_areatype, Q67_revisions,
         New_Q43, starts_with("Q47a_"), Immigration_types_23_1, Immigration_types_23_2, Immigration_types_23_3, Immigration_types_23_4,
         Q69, Q70, Q71, Q73, weight, matches("New_Q78_\\d{1,2}$")) %>% 
  rename("Q18" = "Q18_revisions", 
         Q26 = "Q26_revisions",
         "Q68" = "Q68_revisions") %>% 
  mutate(Year = 2024,
         caseid = as.character(caseid),
        # Q5 = ifelse(country == "Germany", Q5_DE, Q5),
        Q9i = Q9,
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0, 999 ~ NA),
        # Q18 = replace(Q18, Q18 == 11, 5),
         across(starts_with("Q47a_"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
        Q69i = Q69,
        Q62 = case_when(Q62 == 8 ~ 8, 
                        Q62 == 100 ~ 8,
                        TRUE ~ Q62),
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
        Q70i = Q70,
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71a = Q71,
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
        New_Q43i = New_Q43,
        New_Q43 = ifelse(New_Q43 %in% c(1, 2), 1, 0),
         country = recode_values(country,
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
                          3 ~ "Neither",
                          4 ~ "DK"),
        Urban = case_when(Glob_areatype == 1 ~ "Urban/Suburban",
                          Glob_areatype == 2 ~ "Urban/Suburban",
                          Glob_areatype %in% c(3, 4, 5) ~ "Other",
                          TRUE ~ "Other"),
        income = case_when(country == "Croatia" ~ income_hr,
                           country == "Denmark" ~ income_Denmark,
                           country == "Finland" ~ income_Finland,
                           country == "France" ~ income_France,
                           country == "Germany" ~ income_Germany,
                           country == "Greece" ~ income_GR,
                           country == "Hungary" ~ income_HU,
                           country == "Italy" ~ income_italy,
                           country == "Lithuania" ~ income_Lithuania,
                           country == "Netherlands" ~ income_NL,
                           country == "Poland" ~ income_Poland,
                           country == "Romania" ~ income_ro,
                           country == "Slovakia" ~ income_sk,
                           country == "Spain" ~ income_Spain,
                           country == "Sweden" ~ income_SE,
                           country == "Belgium" ~ income_BE,
                           country == "Bulgaria" ~ income_bg),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x))
         ) %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("Q7_revisions_")) 

EUI_2024 <- EUI_2024 %>% 
  left_join(vote_2024, by = "caseid")

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
  filter(AttentionCheck2_23 == 4) %>% 
  select(caseid, country, Q2, Q4, Q5_DE, New_Q5_1, New_Q5_6, New_Q6a, New_Q62a, Q7_revisions_1:Q7_revisions_16, Q9, Q18_revisions, Q21, Q24, Q26_revisions, Q30, Q37, Q40, Q44,
         EUI_Ukraine_Outcome, Israel_Palestine_2024, Q68_revisions, US_Elections_2024_TrumpSupport, Q5, New_Q5_1, New_Q5_6,
         Q62, gender_all, edu_group, Q61, Q62, A5_1, A5_2, A5_3, A5_4, ForeignPolicyDecisions2_1, completed2024survey,
         ForeignPolicyDecisions2_2, ForeignPolicyDecisions, New_Q59, Q70_2, Q62, country_birth, Q67_revisions,
         work_industry_shortlist, starts_with("income"), 
         Q61, Q63,  Q4, profile_work_stat, New_Q41,
         New_Q43, Tariff_EU_US, Tariff_EU_China, Tariff_EU_US_UK, Tariff_EU_China_UK, New_Q60a, Q59, age_grp_all,
         Vote2024_combo, q_BTW25_Quote, FT23, Glob_areatype, Immigration_types_23_1,   Immigration_types_23_2, Immigration_types_23_3, Immigration_types_23_4,
         pastvote2024IE, pastvote2024LT, ES_pastvote_July2023_recoded, fr_pastvote_legislative24_round1, IT_pastvote_2022, AT_2024vote_recode,
         BE_2024vote_recode, BG_2024vote_recode, CZ_2021vote_recode, GR_2023vote_recode, HR_2024vote_recode, HU_2022vote_recode,
         NL_2023vote_recode, PL_2023vote_recode, PT_2024vote_recode, RO_2024vote_recode, SK_2023vote_recode, FT22_SE, FT22_DK,
         Q69, Q70, Q70_b, Q71, Q73, weight, matches("New_Q78_\\d{1,2}$"), starts_with("Q47a")) %>% 
  rename("Q18" = "Q18_revisions",
         "Q26" = "Q26_revisions",
         "Q68" = "Q68_revisions") %>% 
  mutate(Year = "2025",
         Past_vote = case_when(country == "UK" ~ Vote2024_combo,
                               country == "Ireland" ~ pastvote2024IE,
                               country == "Lithuania" ~ pastvote2024LT,
                               country == "Spain" ~ ES_pastvote_July2023_recoded,
                               country == "France" ~ fr_pastvote_legislative24_round1,
                               country == "Italy" ~ IT_pastvote_2022,
                               country == "Austria" ~ AT_2024vote_recode,
                               country == "Belgium" ~ BE_2024vote_recode, 
                               country == "Bulgaria" ~ BG_2024vote_recode,
                               country == "Czech Republic" ~ CZ_2021vote_recode,
                               country == "Greece" ~ GR_2023vote_recode,
                               country == "Germany" ~ q_BTW25_Quote,
                               country == "Croatia" ~ HR_2024vote_recode,
                               country == "Hungary" ~ HU_2022vote_recode,
                               country == "Netherlands" ~ NL_2023vote_recode,
                               country == "Poland" ~ PL_2023vote_recode,
                               country == "Portugal" ~ PT_2024vote_recode,
                               country == "Romania" ~ RO_2024vote_recode,
                               country == "Slovakia" ~ SK_2023vote_recode,
                               country == "Sweden" ~ FT22_SE,
                               country == "Denmark" ~ FT22_DK,
                               country == "Finland" ~ FT23
                               ),
         income = case_when(country == "Croatia" ~ income_hr,
                            country == "Denmark" ~ income_Denmark,
                            country == "Finland" ~ income_Finland,
                            country == "France" ~ income_France,
                            country == "Germany" ~ income_Germany,
                            country == "Greece" ~ income_GR,
                            country == "Hungary" ~ income_HU,
                            country == "Italy" ~ income_italy,
                            country == "Lithuania" ~ income_Lithuania,
                            country == "Netherlands" ~ income_NL,
                            country == "Poland" ~ income_Poland,
                            country == "Romania" ~ income_ro,
                            country == "Slovakia" ~ income_sk,
                            country == "Spain" ~ income_Spain,
                            country == "Sweden" ~ income_SE,
                            country == "Austria" ~ income_Austria,
                            country == "Belgium" ~ income_BE,
                            country == "Bulgaria" ~ income_bg,
                            country == "Czech Republic" ~ income_CZ,
                            country == "Ireland" ~ income_Ireland,
                            country == "Portugal" ~ income_Portugal),
         Urban = case_when(Glob_areatype == 1 ~ "Urban/Suburban",
                          Glob_areatype == 2 ~ "Urban/Suburban",
                          Glob_areatype %in% c(3, 4, 5) ~ "Other",
                          TRUE ~ "Other"),
         across(!c(country, Urban), \(x)as.numeric(x)),
         caseid = as.character(caseid),
         A5_2 = replace(A5_2, A5_2 == 99, NA),
         Q5 = ifelse(country == "Germany", Q5_DE, Q5),
         New_Q5_1 = replace(New_Q5_1, New_Q5_1 == 6, 3),
         New_Q5_6 = replace(New_Q5_6, New_Q5_6 == 6, 3),
         Q62 = case_when(Q62 == 8 ~ 8, 
                         Q62 == 100 ~ 8,
                         TRUE ~ Q62),
         New_Q43i = New_Q43,
         New_Q43 = ifelse(New_Q43 %in% c(1, 2), 1, 0),
         across(starts_with("Q47a"), \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         Q9i = Q9,
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 4 ~ 0, 3 ~ 0, 999 ~ NA),
         Tariff_EU_US = if_else(country == "UK", Tariff_EU_US_UK, Tariff_EU_US),
         Tariff_EU_China = if_else(country == "UK", Tariff_EU_China_UK, Tariff_EU_China),
         Tariff_EU_US_con = case_match(Tariff_EU_US, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
         Tariff_EU_China_con = case_match(Tariff_EU_China, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1),
         Tariff_EU_US = case_match(Tariff_EU_US, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0),
         Tariff_EU_China = case_match(Tariff_EU_China, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0),
         Q70i = Q70,
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
         Q69i = Q69,
         Q69 = case_match(Q69, 1 ~ 1, 2 ~ 0, 3 ~ 0, 4 ~ 0),
         Q70 = case_match(Q70, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q70_b = case_match(Q70_b, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q71a = Q71,
         Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         ForeignPolicyDecisions2_1 = case_match(ForeignPolicyDecisions2_1, 1 ~ 1, 2 ~ 0, 3 ~ 0),
         ForeignPolicyDecisions2_2 = case_match(ForeignPolicyDecisions2_2, 1 ~ 1, 2 ~ 0, 3 ~ 0),
         FP_decisions_IV = case_match(ForeignPolicyDecisions, 1 ~ 5, 2 ~ 4, 3 ~ 3, 4 ~ 2, 5 ~ 1, 6 ~ 3),
         ForeignPolicyDecisions = case_match(ForeignPolicyDecisions, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0, 6 ~ 0),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ "Neither",
                          4 ~ "DK"),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x))) %>% 
  rename_with(~paste0("Q7_", 1:16), starts_with("Q7_revisions_"))



#### April 2022 ####

EUI_april_2022 <- read_sav("Data_raw/eui-yg-2022-04.sav")

EUI_2022 <- EUI_april_2022 %>% 
  select(weight, country,  matches("New_Q78_\\d$"), Q2, Q62, Q7_revisions_2, Q7_revisions_12, New_Q60a, Q9, Q5, New_Q43, Glob_areatype, age_grp_all, work_industry_shortlist, 
         Q61, Q63,  Q4, profile_work_stat, Q67_revisions, contains("pastvote"), FT19_dk, FT18, FT19,
         PDL_Vote_18_Quote_IT, Presidential_vote17,
         q_BTW21_Quote, New_Q41, New_Q43, New_Q5_1, New_Q5_6,
         New_Q62a, country_birth, Q68_revisions, Q71, Q73, New_Q79, Q59, gender_all, starts_with("education_UK_All"), starts_with("Immigration_types")) %>% 
  mutate( country = case_match(country,
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
         vote_values_2022 = case_when(
           country == "UK" ~ pastvote_ge_2019,
           country == "France" ~ Presidential_vote17,
           country == "Germany" ~ q_BTW21_Quote,
           country == "Denmark" ~ FT19_dk,
           country == "Sweden" ~ FT18,
           country == "Finland" ~ FT19,
           country == "Italy" ~ PDL_Vote_18_Quote_IT,
           country == "Spain" ~ ES_pastvoteNov_2019,
           country == "Poland" ~ pastvoteSejm_2019,
           country == "Greece" ~ pastvote2019EL,
           country == "Bulgaria" ~ pastvote21BG_we,
           country == "Croatia" ~ pastvote2020HR,
           country == "Hungary" ~ pastvote2018HU,
           country == "Lithuania" ~ pastvote2020LT,
           country == "Netherlands" ~ pastvote2020NL,
           country == "Romania" ~ pastvote2020RO,
           country == "Slovakia" ~ pastvote20SK
         ),
         across(matches("New_Q78_\\d$"),\(x)as.numeric(x)),
        Q71 = case_match(Q71, 1 ~ 1, 2 ~ 1, 3 ~ 0, 4 ~ 0, 5 ~ 0),
         Q73 = case_match(Q73, 1 ~ "European countries should invest more in defence and security to defend against Russian aggression",
                          2 ~ "European countries should invest more in trade and diplomacy with Russia to improve relations",
                          3 ~ "Neither",
                          4 ~ "DK"),
         Urban = case_when(Glob_areatype == 1 ~ "Urban/Suburban",
                           Glob_areatype == 2 ~ "Urban/Suburban",
                           Glob_areatype %in% c(3, 4, 5) ~ "Other",
                           TRUE ~ "Other"),
         New_Q43i = New_Q43,
         Q9 = case_match(Q9, 1 ~ 1, 2 ~ 0, 4 ~ 0, 3 ~ 0, 999 ~ NA),
         New_Q43 = ifelse(New_Q43 %in% c(1, 2), 1, 0),
         #across(starts_with("New_Q78"), \(x)case_match(x, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1, 5 ~ 2.5)),
         Q62 = case_when(Q62 == 8 ~ 8, 
                         Q62 == 100 ~ 8,
                         TRUE ~ Q62)) %>% 
  mutate(Year = 2022) %>% 
  rename(Q7_2 = Q7_revisions_2,
         Q7_12 = Q7_revisions_12,
         Q68 = Q68_revisions
         )

EUI_2022 <- EUI_2022 %>% 
 left_join(crosswalk %>% select(new_q59, vote_values_2022, country_name) %>% filter(!is.na(vote_values_2022)), by = c("vote_values_2022", "country" = "country_name")) %>% 
 rename(Past_vote = new_q59)

#### MERGE datasets ####

EUI_data <- bind_rows(EUI_2025, EUI_2024) %>% 
  bind_rows(EUI_2023) %>% 
  bind_rows(EUI_2022) 



EUI_data <- EUI_data %>% 
  mutate(Q44_base = Q44,
         Q5 = replace(Q5, Q5 == 11, 5),
    across(Q21:Q44, \(x)case_match(x, 1 ~ 1, 2 ~ 0, 3 ~ 0)),
         across(starts_with("New_Q78"), \(x)case_match(x, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1, 5 ~ 5)),
    across(c(New_Q60a, New_Q62a), \(x)replace(x, x == 12, 6)),
    Diff = New_Q60a - New_Q62a,
    Affective_Polarization = ifelse(Diff >= 0, Diff, 0),
    across(c(New_Q5_6), \(x)case_match(x, 1 ~ 5, 2 ~ 4, 3 ~ 3, 4 ~ 2, 5 ~ 1)),
    Support_Aggrandizement = (New_Q5_1 + New_Q5_6) / 2,
    Woman = case_when(age_grp_gen_edu_6 == 1 | gender_all == 1 ~ "Man",
                      age_grp_gen_edu_7 == 1 | gender_all == 2 ~ "Woman"
                      ),
    Econ_comparison = recode_values(Q61, 1 ~ "Better off",
                                    2 ~ "Better off",
                                    3 ~ "The same",
                                    4 ~ "Worse off",
                                    5 ~ "Worse off", 
                                    6 ~ "Don't Know"),
    religion = recode_values(Q63, 1 ~ "Not religious",
                             2 ~ "Christian",
                             10 ~ "Christian",
                             11 ~ "Christian",
                             12 ~ "Catholic",
                             13 ~ "Orthodox",
                             3 ~ "Jewish",
                             4 ~ "Other",
                             5 ~ "Muslim",
                             6 ~ "Other",
                             7 ~ "Other",
                             8 ~ "Other",
                             9 ~ "Other"
                             ),
    Industry = recode_values(work_industry_shortlist, 
                             1 ~ "Manual Labour",
                             2 ~ "Manual Labour",
                             3 ~ "Retail/Hospitality",
                             4 ~ "White Collar",
                             5 ~ "Retail/Hospitality",
                             6 ~ "White Collar",
                             7 ~ "Knowledge Economy",
                             8 ~ "Technology",
                             9 ~ "White Collar",
                             10 ~ "Knowledge Economy",
                             12 ~ "Manual Labour",
                             13 ~ "Retail/Hospitality",
                             97 ~ "Other",
                             98 ~ "DK",
                             99 ~ "Unemployed",
                             NA ~ "Unemployed"
                             ),
    Employed = recode_values(profile_work_stat, 1 ~ "Employed",
                             2 ~ "Employed",
                             3 ~ "Employed",
                             4 ~ "Student",
                             5 ~ "Retired",
                             6 ~ "Unemployed",
                             7 ~ "Unemployed",
                             8 ~ "Other"),
    Living_Area = recode_values(Glob_areatype, 1 ~ "City Centre",
                                2 ~ "Suburb",
                                3 ~ "Town/Village",
                                4 ~ "Town/Village",
                                5 ~ "Rural Area",
                                6 ~ "DK"),
    Age = case_when(age_grp_all == 1 | age_grp_gen_edu_1 == 1 ~ "18-24",
                age_grp_all == 2 | age_grp_gen_edu_2 == 1 ~ "25-34",
                age_grp_all == 3 | age_grp_gen_edu_3 == 1~ "35-44",
                age_grp_all == 4 | age_grp_gen_edu_4 == 1~ "45-54",
                age_grp_all == 5 | age_grp_gen_edu_5 == 1~ "55+"),
    Education = case_when(age_grp_gen_edu_18 == 1 | edu_group == 1 | education_merged1_101 == 1 | education_UK_All_1 == 1 |  education_UK_All_2 == 1 | education_UK_All_3 == 1 | education_UK_All_101 == 1 ~ "Less than Primary",
                          age_grp_gen_edu_19 == 1 | edu_group == 2 | education_UK_All_5 == 1 | education_merged1_102 == 1 | education_UK_All_102 == 1 ~ "Higher Secondary",
                          age_grp_gen_edu_20 == 1 | edu_group == 3 | education_UK_All_6 == 1 | education_merged1_103 == 1 | education_UK_All_7 == 1 | education_UK_All_8 == 1 | education_UK_All_9 == 1 | education_UK_All_10 == 1 | education_UK_All_103 == 1 ~ "Tertiary"),
    across(starts_with("Immigration_types"), \(x)case_match(x, 1 ~ 1, 2 ~ 2, 3 ~ 3, 4 ~ 4, 995 ~ 0, 994 ~ 0)),
    Climate = Q7_11
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

# EUI_data <- EUI_data %>% 
#   left_join(ches_data %>% filter(!is.na(new_q59)), by = c("Past_vote" = "new_q59"))

table(ches_data$Kremlin_ties, ches_data$party)
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

#write_sav(EUI_data, "data_raw/data//EUI_data.sav")
