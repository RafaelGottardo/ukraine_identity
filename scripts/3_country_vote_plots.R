##### Predicted Vote Choice by Country #####

## One plot per country of each party's predicted vote share across the
## Defence-Normalization dimension, built from the country-level multinomial models in
## Models_Vote_choice (scripts/2_main_analysis.R; UK: fitted below). Each plot carries
## its own party-colour palette via scale_colour_manual(). The plots are combined into
## one grid in scripts/3_appendix.R, and each is also saved individually to plots/.
## Note: Finland_parties, Germany_parties and Bulgaria_parties are also built (with
## slightly different labelling) for the main-text figure in scripts/2_main_analysis.R.

if (!exists("Models_Vote_choice")) source("scripts/2_main_analysis.R")

#### Croatia ####

Croatia_Parties <- Models_Vote_choice[["Croatia"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "139" ~ "HDZ",
                            "141" ~ "Domovinski pokret",
                            "142" ~ "Most",
                            "278" ~ "Možemo!",
                            "323" ~ "Rijeke pravde",
                            "713" ~ "Fokus-Republika"
                            ),
         group = factor(group, levels = c("HDZ",
                "Domovinski pokret",
                "Most",
                "Možemo!",
                "Rijeke pravde",
                "Fokus-Republika")),
         Country = "Croatia") %>% 
  plot_predictions_vote(COLOURS = c(
    "#005BAA",  # Hrvatska demokratska zajednica (HDZ) - dark blue
    "#7A1E1E",  # Domovinski pokret - dark red / burgundy
    "#F28C00",  # Most - orange
    "#2E8B57",  # Možemo! - green
    "#D7263D",  # Rijeke pravde - red
    "#F2C300"   # Fokus-Republika - yellow/gold
  )) + facet_wrap(~Country)

ggsave("plots/Croatia_Parties.png", Croatia_Parties, width = 7, height = 4, dpi = "retina")

#### Denmark ####

Denmark_parties <- Models_Vote_choice[["Denmark"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "61" ~ "Socialdemokratiet",
                            "62" ~ "Radikale Venstre",
                            "63" ~ "Det Konservative Folkeparti",
                            "64" ~ "Nye Borgerlige",
                            "66" ~ "Socialistisk Folkeparti",
                            "67" ~ "Liberal Alliance",
                            "69" ~ "Dansk Folkeparti",
                            "71" ~ "Venstre, Danmarks Liberale Parti",
                            "72" ~ "Enhedslisten - De Rød-Grønne",
                            "73" ~ "Alternativet",
                            "74" ~ "Moderaterna",
                            "75" ~ "Centerpartiet",
                            "76" ~ "Liberalerna",
                            "77" ~ "Kristdemokraterna",
                            "78" ~ "Miljöpartiet",
                            "79" ~ "Socialdemokraterna",
                            "80" ~ "Vänsterpartiet",
                            "162" ~ "Danmarksdemokraterne",
                            "707" ~ "Feministiskt Initiativ",
                            "708" ~ "Piratpartiet"
                            
  ),
  group = factor(group, levels = c( "Socialdemokratiet",
                                    "Radikale Venstre",
                                   "Det Konservative Folkeparti",
                                    "Nye Borgerlige",
                                    "Socialistisk Folkeparti",
                                   "Liberal Alliance",
                                    "Dansk Folkeparti",
                                    "Venstre, Danmarks Liberale Parti",
                                    "Enhedslisten - De Rød-Grønne",
                                    "Alternativet",
                                   "Moderaterne",
                                   "Centerpartiet",
                                   "Liberalerna",
                                    "Kristdemokraterna",
                                   "Miljöpartiet",
                                   "Socialdemokraterna",
                                    "Vänsterpartiet",
                                    "Danmarksdemokraterne",
                                   "Feministiskt Initiativ",
                                   "Piratpartiet")),
  Country = "Denmark") %>% 
  plot_predictions_vote(COLOURS = c(
    c(
      "#E31836",  # Socialdemokratiet (DK) - red
      "#7B3F98",  # Radikale Venstre - purple
      "#006C3C",  # Det Konservative Folkeparti - dark green
      "#1F3A93",  # Nye Borgerlige - dark blue
      "#C4122E",  # Socialistisk Folkeparti - red
      "#00AEEF",  # Liberal Alliance - light blue
      "#FFD100",  # Dansk Folkeparti - yellow
      "#1E5AA8",  # Venstre - blue
      "#C4002F",  # Enhedslisten - red
      "#00A651",  # Alternativet - green
      "#6C757D",  # Moderaterna (SE) - grey
      "#009933",  # Centerpartiet (SE) - green
      "#006AB3",  # Liberalerna (SE) - blue
      "#1B5E20",  # Kristdemokraterna (SE) - dark green
      "#83CF39",  # Miljöpartiet (SE) - light green
      "#E8112D",  # Socialdemokraterna (SE) - red
      "#B31B1B",  # Vänsterpartiet (SE) - dark red
      "#8B0000",  # Danmarksdemokraterne - dark red
      "#FF69B4",  # Feministiskt Initiativ - pink
      "#6F2DA8"   # Piratpartiet - purple
    )
  )) + facet_wrap(~Country)

ggsave("plots/Denmark_parties.png", Denmark_parties, width = 7, height = 4)
#### Finland ####

Finland_parties <- Models_Vote_choice[["Finland"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "82" ~ "Keskusta",
                            "83" ~ "Kokoomus",
                            "84" ~ "SDP",
                            "85" ~ "Vasemmistoliitto",
                            "86" ~ "Vihreä liitto",
                            "87" ~ "Kristillisdemokraatit",
                            "88" ~ "RKP",
                            "89" ~ "Perussuomalaiset",
                            "722" ~ "Korjausliike",
                            "723" ~ "Sininen tulevaisuus"
                            
  ),
  group = factor(group, levels = c("Keskusta",
                                   "Kokoomus",
                                    "SDP",
                                    "Vasemmistoliitto",
                                    "Vihreä liitto",
                                    "Kristillisdemokraatit",
                                    "RKP",
                                    "Perussuomalaiset",
                                     "Korjausliike",
                                     "Sininen tulevaisuus")),
  Country = "Finland") %>% 
  plot_predictions_vote(COLOURS = c(
    "#009A44",  # Keskusta - green
    "#003580",  # Kokoomus - dark blue
    "#E11931",  # SDP - red
    "#B71C1C",  # Vasemmistoliitto - dark red
    "#61BF1A",  # Vihreä liitto - bright green
    "#0033A0",  # Kristillisdemokraatit - blue
    "#FFD500",  # RKP - yellow
    "#1D428A",  # Perussuomalaiset - blue
    "#6C757D",  # Korjausliike - grey (less standardized)
    "#2F4F4F"   # Sininen tulevaisuus - dark blue-grey
  )) + facet_wrap(~Country)

ggsave("plots/Finland_parties.png", width = 7, height = 4, Finland_parties)
#### France - ISSUES to fix ####

France_parties <- Models_Vote_choice[["France"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "14" ~ "Les Républicains",
                            "16" ~ "La France Insoumise",
                            "164" ~ "Rassemblement National",
                            "165" ~ "Reconquête!",
                            "168" ~ "Parti Communiste Français",
                            "169" ~ "Debout La France",
                            "308" ~ "Renaissance"#,
                           # "309" ~ "Les Écologistes (LE)"
                            
  ),
  group = factor(group, levels = c("Les Républicains",
                                   "La France Insoumise",
                                   "Rassemblement National",
                                   "Reconquête!",
                                   "Parti Communiste Français",
                                   "Debout La France",
                                   "Renaissance"#,
                                  # "Les Écologistes (LE)"
                                   )),
  Country = "France") %>% 
  filter(group != "Lutte Ouvrière") %>% 
  plot_predictions_vote(COLOURS = c(
    "#1F3A93",  # Les Républicains - dark blue
    "#E11C2A",  # La France Insoumise - bright red
    "#001F5B",  # Rassemblement National - navy blue
    "#000000",  # Reconquête! - black
    "#B22222",  # Parti Communiste Français - dark red
    "#0055A4",  # Debout La France - blue (French flag tone)
    "#FFD700"#,  # Renaissance - gold/yellow
   # "#3FA535"  # Les Écologistes (LE) - green
    
  )
  ) + facet_wrap(~Country)

ggsave("plots/France_parties.png", width = 7, height = 4, France_parties)

#### Germany ####

Germany_parties <- Models_Vote_choice[["Germany"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "19" ~ "SPD",
                            "20" ~ "CDU/CSU",
                            "21" ~ "Bündnis 90/Die Grünen",
                            "22" ~ "AfD",
                            "23" ~ "FDP",
                            "24" ~ "Die Linke",
                            "254" ~ "Bündnis Sahra Wagenknecht"),
  group = factor(group, levels = c("SPD",
                                   "CDU/CSU",
                                  "Bündnis 90/Die Grünen",
                                   "AfD",
                                  "FDP",
                                   "Die Linke",
                                   "Bündnis Sahra Wagenknecht")),
  Country = "Germany") %>% 
  filter(group != "Lutte Ouvrière") %>% 
  plot_predictions_vote(COLOURS = c(
    "#E3000F",  # SPD - red
    "#000000",  # CDU/CSU - black
    "#46962B",  # Bündnis 90/Die Grünen - green
    "#009EE0",  # AfD - light blue
    "#FFED00",  # FDP - yellow
    "#BE3075",  # Die Linke - magenta
    "#6A0032"   # Bündnis Sahra Wagenknecht - dark wine red
  )
  ) + facet_wrap(~Country) 

ggsave("plots/Germany_parties.png", width = 10, height = 6, Germany_parties)


#### GREECE ####

Greece_parties <- Models_Vote_choice[["Greece"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "118" ~ "New Democracy",
                            "119" ~ "SYRIZA",
                            "120" ~ "PASOK",
                            "121" ~ "Communist Party",
                            "122" ~ "Greek Solution",
                            "123" ~ "MeRA25",
                            "318" ~ "Spartans"),
         group = factor(group, levels = c("New Democracy",
                                          "SYRIZA",
                                          "PASOK",
                                          "Communist Party",
                                          "Greek Solution",
                                          "MeRA25",
                                          "Spartans")),
         Country = "Greece") %>% 
  plot_predictions_vote(COLOURS = c(
    "#1F4E79",  # Νέα Δημοκρατία (ND) - deep blue
    "#E0001B",  # ΣΥΡΙΖΑ - red
    "#00843D",  # ΠΑΣΟΚ - green
    "#D40000",  # ΚΚΕ - communist red
    "#0033A0",  # Ελληνική Λύση - blue
    "#6A1B9A",  # MeRA25 - purple
    "#000000",  # Σπαρτιάτες - black
    "#D4AF37"   # Golden Dawn - gold
  )
  ) + facet_wrap(~Country)

ggsave("plots/Greece_parties.png", width = 7, height = 4, Greece_parties)

#### Hungary ####

Hungary_parties <- Models_Vote_choice[["Hungary"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "103" ~ "Fidesz",
                            "200" ~ "Tisza"),
         group = factor(group, levels = c("Fidesz",
                                          "Tisza")),
         Country = "Hungary") %>% 
  plot_predictions_vote(COLOURS = c(
    "#ff6a00", "#4cb5a1"
  )
  ) + facet_wrap(~Country) 

ggsave("plots/Hungary_parties.png", width = 10, height = 6, Hungary_parties)

#### Italy ####

Italy_parties <- Models_Vote_choice[["Italy"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "42" ~ "Movimento 5 Stelle",
                            "43" ~ "Lega",
                            "44" ~ "Forza Italia",
                            "45" ~ "Fratelli d'Italia",
                            "46" ~ "Partito Democratico",
                            "47" ~ "+Europa",
                            "151" ~ "Alleanza Verdi-Sinistra",
                            "256" ~ "Azione",
                            "999" ~ "Italexit"),
         group = factor(group, levels = c( 
            "Movimento 5 Stelle",
           "Lega",
           "Forza Italia",
            "Fratelli d'Italia",
           "Partito Democratico",
            "+Europa",
            "Alleanza Verdi-Sinistra",
            "Azione",
            "Italexit")),
         Country = "Italy") %>% 
  filter(group != "Lutte Ouvrière") %>% 
  plot_predictions_vote(COLOURS =c(
    "#FFD700",  # Movimento 5 Stelle - yellow/gold
    "#00AEEF",  # Lega - light blue
    "#0066CC",  # Forza Italia - blue
    "#1C1C1C",  # Fratelli d'Italia - very dark navy/black
    "#E30613",  # Partito Democratico - red
    "#6A1B9A",  # +Europa - purple
    "#3FA535",  # Alleanza Verdi-Sinistra - green
    "#F28C00",  # Azione - orange
    "#2F4F4F"   # Italexit - dark grey
  )
  ) + facet_wrap(~Country)

ggsave("plots/Italy_parties.png", width = 7, height = 4, Italy_parties)

#### Lithuania ####

Lithuania_parties <- Models_Vote_choice[["Lithuania"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "110" ~ "TS-LKD",
                            "111" ~ "LVŽS",
                            "113" ~ "LSDP",
                            "116" ~ "LLRA-KŠS",
                            "117" ~ "LRP",
                            "194" ~ "DSVL",
                            "325" ~ "LS",
                            "326" ~ "PPNA",
                            "330" ~ "Taikos koalicija"),
         group = factor(group, levels = c( 
           "TS-LKD",
           "LVŽS",
           "LSDP",
           "LLRA-KŠS",
           "LRP",
           "DSVL",
           "LS",
           "PPNA",
           "Taikos koalicija")),
         Country = "Lithuania") %>% 
  plot_predictions_vote(COLOURS =c(
    "#003DA5",  # TS-LKD (Homeland Union) - conservative blue
    "#2E8B57",  # LVŽS (Greens/Peasants) - green
    "#E30613",  # LSDP (Social Democrats) - red
    "#1F4E79",  # LLRA-KŠS (Electoral Action of Poles) - deep blue
    "#7A7A7A",  # LRP - grey (less standardised branding)
    "#6A1B9A",  # DSVL - purple (movement-style branding)
    "#00AEEF",  # LS (Liberals) - light blue
    "#F4A300",  # PPNA - orange/gold tone (non-standardised)
    "#000000"   # Taikos koalicija - black
  )
  ) + facet_wrap(~Country)

ggsave("plots/Lithuania_parties.png", width = 7, height = 4, Lithuania_parties)


#### Netherlands ######## 

Netherlands_Parties <- Models_Vote_choice[["Netherlands"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "49" ~ "VVD",
                            "50" ~ "PVV",
                            "51" ~ "CDA",
                            "52" ~ "D66",
                            "54" ~ "SP",
                            "58" ~ "Partij voor de Dieren",
                            "59" ~ "Denk",
                            "60" ~ "Forum voor Democratie",
                            "258" ~ "PvdA",
                            "259" ~ "Nieuw Sociaal Contract",
                            "260" ~ "BoerBurgerBeweging",
                            "334" ~ "SGP"
                            ),
         group = factor(group, levels = c( 
           "VVD",
            "PVV",
          "CDA",
          "D66",
           "SP",
            "Partij voor de Dieren",
           "Denk",
            "Forum voor Democratie",
            "PvdA",
            "Nieuw Sociaal Contract",
            "BoerBurgerBeweging",
            "SGP")),
         Country = "Netherlands") %>% 
  filter(!is.na(group)) %>% 
  plot_predictions_vote(COLOURS =c(
    "#FF6600",  # VVD - orange
    "#0033A0",  # PVV - blue
    "#00A7E1",  # CDA - light blue
    "#00A6D6",  # D66 - teal/light blue
    "#E30613",  # SP - red
    "#3FA535",  # Partij voor de Dieren - green
    "#00A859",  # Denk - green (slightly different hue)
    "#6A1B9A",  # Forum voor Democratie - purple
    "#C6002A",  # PvdA - red
    "#6C757D",  # Nieuw Sociaal Contract - grey (new party, no fixed brand colour)
    "#8B4513",  # BoerBurgerBeweging - brown
    "#1A1A1A"   # SGP - black
  )
  ) 

ggsave("plots/Netherlands_parties.png", width = 7, height = 4, Netherlands_Parties)

#### Poland ####

Poland_parties <- Models_Vote_choice[["Poland"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "92" ~ "PiS",
                            "93" ~ "Lewica",
                            "94" ~ "Konfederacja Wolność I Niepodległość",
                            "95" ~ "KO",
                            "261" ~ "Trzecia Droga"
  ),
  group = factor(group, levels = c( 
    "PiS",
     "Lewica",
    "Konfederacja Wolność I Niepodległość",
    "KO",
    "Trzecia Droga")),
  Country = "Poland") %>% 
  filter(!is.na(group)) %>% 
  plot_predictions_vote(COLOURS =c(
    "#1F4E79",  # PiS - dark blue
    "#E30613",  # Lewica - red
    "#000000",  # Konfederacja Wolność i Niepodległość - black
    "#F28C00",  # KO (Koalicja Obywatelska) - orange
    "#2E8B57"   # Trzecia Droga - green
  )
  ) + facet_wrap(~Country)

ggsave("plots/Poland_parties.png", width = 7, height = 4, Poland_parties)

#### Romania ####

Romania_parties <- Models_Vote_choice[["Romania"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "96" ~ "PSD",
                            "97" ~ "PNL",
                            "99" ~ "AUR",
                            "100" ~ "PMDSZ/UDMR",
                            "199" ~ "USR",
                            "263" ~ "S.O.S. România",
                            "315" ~ "POT"
  ),
  group = factor(group, levels = c( 
    "PSD",
     "PNL",
    "AUR",
    "PMDSZ/UDMR",
    "USR",
    "S.O.S. România",
    "POT")),
  Country = "Romania") %>% 
  filter(!is.na(group)) %>% 
  filter(!group %in% c("S.O.S. România", "POT")) %>% 
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # PSD - red
    "#1F4E79",  # PNL - blue
    "#FFD700",  # AUR - gold/yellow
    "#2E8B57",  # UDMR (RMDSZ) - green
    "#00AEEF",  # USR - light blue
    "#8B0000",  # S.O.S. România - dark red
    "#6A1B9A"   # POT - purple
  )
  ) + facet_wrap(~Country)

ggsave("plots/Romania_parties.png", width = 7, height = 4, Romania_parties)


#### Slovakia ####

Slovakia_parties <- Models_Vote_choice[["Slovakia"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "132" ~ "SMER-SD",
                            "133" ~ "Sme Rodina",
                            "136" ~ "SaS",
                            "138" ~ "KDH",
                            "196" ~ "HLAS-SD",
                            "197" ~ "SNS",
                            "331" ~ "Progresívne Slovensko",
                            "332" ~ "OĽaNO a priatelia"
  ),
  group = factor(group, levels = c( 
    "SMER-SD",
    "Sme Rodina",
    "SaS",
    "KDH",
    "HLAS-SD",
    "SNS",
     "Progresívne Slovensko",
    "OĽaNO a priatelia")),
  Country = "Slovakia") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # SMER-SD - red
    "#8B4513",  # Sme Rodina - brown
    "#00AEEF",  # SaS - light blue
    "#1F4E79",  # KDH - dark blue
    "#6A1B9A",  # HLAS-SD - purple
    "#000000",  # SNS - black
    "#00A859",  # Progresívne Slovensko - green
    "#FFD100"   # OĽaNO a priatelia - yellow
  )
  ) + facet_wrap(~Country)

ggsave("plots/Slovakia_parties.png", width = 7, height = 4, Slovakia_parties)


#### Spain ####

Spain_parties <- Models_Vote_choice[["Spain"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "25" ~ "PSOE",
                            "26" ~ "PP",
                            "27" ~ "Vox"
  ),
  group = factor(group, levels = c( 
    "PSOE",
    "PP",
    "Vox")),
  Country = "Spain") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # PSOE - red
    "#1F4E79",  # PP - blue
    "#6A0D0D"  # Vox - dark greenish-brown / dark tone (often very dark green/blackish)
  )
  ) +
 facet_wrap(~Country)

ggsave("plots/Spain_parties.png", width = 7, height = 4, Spain_parties)

#### Sweden ####

Sweden_Parties <- Models_Vote_choice[["Sweden"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "74" ~ "Moderaterna",
                            "75" ~ "Centerpartiet",
                            "76" ~ "Liberalerna",
                            "77" ~ "Kristdemokraterna",
                            "78" ~ "Miljöpartiet",
                            "79" ~ "Socialdemokraterna",
                            "80" ~ "Vänsterpartiet",
                            "81" ~ "Sverigedemokraterna"
                            
  ),
  group = factor(group, levels = c( 
    "Moderaterna",
    "Centerpartiet",
    "Liberalerna",
    "Kristdemokraterna",
    "Miljöpartiet",
    "Socialdemokraterna",
    "Vänsterpartiet",
    "Sverigedemokraterna")),
  Country = "Sweden")%>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#006AB3",  # Moderaterna - blue
    "#009933",  # Centerpartiet - green
    "#006AB3",  # Liberalerna - blue (slightly lighter in practice, but same family)
    "#1B5E20",  # Kristdemokraterna - dark green
    "#83CF39",  # Miljöpartiet - light green
    "#E8112D",  # Socialdemokraterna - red
    "#B31B1B",  # Vänsterpartiet - dark red
    "#FFCC00"   # Sverigedemokraterna - yellow
  )
  ) + facet_wrap(~Country)

ggsave("plots/Sweden_parties.png", width = 7, height = 4, Sweden_Parties)


#### UK ####

## The UK is not part of the country loop in scripts/2_main_analysis.R, so its model
## (Security_FA by year) is fitted here; scripts/5_prepare_social_media.R uses it too.
## Security_FA is already numeric, so it enters the formula as-is - marginaleffects
## (0.32) rejects a wrapped term like as.numeric(Security_FA) as `variables`.
UK_data <- EUI_data_short %>% filter(country == "UK")

UK_model <- multinom(reformulate(c("Security_FA * as.factor(Year)", "Q62", "GAL_TAN", CONTROLS), response = "Past_vote"),
                     data = UK_data, maxit = 1000)

UK_model_preds <- avg_predictions(
  UK_model,
  variables = "Security_FA",
  type = "probs",
  newdata = me_newdata(UK_model, UK_data)
)


UK_parties <- UK_model_preds %>% 
  mutate(group = case_match(group, 
                            "1" ~ "Conservative",
                            "2" ~ "Labour",
                            "3" ~ "Lib-Dem",
                            "4" ~ "          ",
                            "5" ~ "         ",
                            "7" ~ "        ",
                            "8" ~ "      ",
                            "9" ~ "     ",
                            "10" ~ "    ", 
                            "11" ~ "   ",
                            "12" ~ "  ",
                            "250" ~ "Reform",
                            "305" ~ " "
                            
  ),
  group = factor(group, levels = c( 
    "Conservative",
    "Labour",
    "Lib-Dem",
    "Reform",
    " ",
    "  ",
    "   ",
    "    ",
    "     ", 
    "      ",
    "       ",
    "         ",
    "          ")),
  Country = "United Kingdom") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#0087DC",  # Conservative - blue
    "#E4003B",  # Labour - red
    "#FAA61A",  # Lib-Dem - orange
    "#12B6CF",  # SNP - yellow
    "#CCCCCC",  # Plaid Cymru - green
    "#CCCCCC",  # Green Party - green
    "#CCCCCC",  # DUP - orange-red
    "#CCCCCC",  # Sinn Féin - dark green
    "#CCCCCC",  # SDLP - green
    "#CCCCCC",  # UUP - light blue
    "#CCCCCC",  # Alliance - yellow
    "#CCCCCC",  # Reform - teal
    "#CCCCCC"   # TUV - dark blue
  )
  ) + facet_wrap(~Country) 

ggsave("plots/UK_parties.png", width = 7, height = 4, UK_parties)

#### Austria ####

Austria_parties <- Models_Vote_choice[["Austria"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "339" ~ "FPÖ",
                            "340" ~ "Österreichische Volkspartei",
                            "341" ~ "Sozialdemokratische Partei",
                            "342" ~ "NEOS",
                            "343" ~ "Die Grünen"
                            
  ),
  group = factor(group, levels = c( 
    "FPÖ",
    "Österreichische Volkspartei",
    "Sozialdemokratische Partei",
    "NEOS",
    "Die Grünen")),
  Country = "Austria") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#0057A5",  # FPÖ - blue
    "#63C3D1",  # ÖVP - turquoise
    "#E11931",  # SPÖ - red
    "#E2007A",  # NEOS - pink/magenta
    "#77B82A"   # Die Grünen - green
  )
  ) + facet_wrap(~Country)

ggsave("plots/Austria_parties.png", width = 7, height = 4, Austria_parties)

#### Belgium ####

Belgium_parties <- Models_Vote_choice[["Belgium"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "291" ~ "NV-A",
                            "292" ~ "VLAAMS BELANG",
                            "293" ~ "PS",
                            "294" ~ "CD&V",
                            "295" ~ "PVDA/PTB",
                            "296" ~ "Open vld",
                            "297" ~ "MR",
                            "298" ~ "Vooruit",
                            "299" ~ "ECOLO",
                            "300" ~ "GROEN",
                            "301" ~ "LE"
                            
  ),
  group = factor(group, levels = c( 
    "NV-A",
    "VLAAMS BELANG",
     "PS",
     "CD&V",
    "PVDA/PTB",
    "Open vld",
    "MR",
    "Vooruit",
    "ECOLO",
    "GROEN",
    "LE")),
  Country = "Belgium") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#FFD200",  # N-VA - yellow
    "#000000",  # Vlaams Belang - black
    "#E30613",  # PS - red
    "#FF7F00",  # CD&V - orange
    "#B22222",  # PVDA/PTB - dark red
    "#00AEEF",  # Open VLD - light blue
    "#0033A0",  # MR - blue
    "#E6007E",  # Vooruit - pink/red
    "#4DBF3B",  # ECOLO - green
    "#009933",  # GROEN - green
    "#6A1B9A"   # LE (Les Engagés) - purple
  )
  ) + facet_wrap(~Country)

ggsave("plots/Belgium_parties.png", width = 7, height = 4, Belgium_parties)


#### Bulgaria ####

Bulgaria_parties <- Models_Vote_choice[["Bulgaria"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "125" ~ "GERB–SDS",
                            "126" ~ "We Continue the Change / Democratic Bulgaria (PP–DB)",
                            "127" ~ "     ",
                            "128" ~ "Revival (Vazrazhdane)",
                            "271" ~ " ",
                            "319" ~ "  ",
                            "320" ~ "   "
                            
  ),
  group = factor(group, levels = c( 
    "GERB–SDS",
    "We Continue the Change / Democratic Bulgaria (PP–DB)",
    "Revival (Vazrazhdane)",
    " ",
    "  ",
    "   ",
    "     ")),
  Country = "Bulgaria") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#1F4E79",  # GERB–SDS - blue
    "#00AEEF",  # PP–DB - light blue  # Peevski bloc - purple (non-standardised grouping)
    "#B22222",  # Revival (Vazrazhdane) - dark red
    "#CCCCCC",
    "#CCCCCC",  # ITN - orange
    "#CCCCCC",  # BSP - red
    "#CCCCCC"   # MRF (DPS) - green
  )
  ) + facet_wrap(~Country)

ggsave("plots/Bulgaria_parties.png", width = 7, height = 4, Bulgaria_parties)

#### Czech ####

Czechia_parties <- Models_Vote_choice[["Czech Republic"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "346" ~ "Spolu",
                            "347" ~ "ANO 2011",
                            "348" ~ "Česká pirátská strana",
                            "350" ~ "Svoboda a přímá demokracie",
                            "353" ~ " ",
                            "357" ~ "  "
  ),
  group = factor(group, levels = c( 
    "Spolu",
    "ANO 2011",
    "Česká pirátská strana",
    "Svoboda a přímá demokracie",
    " ",
     "  ")),
  Country = "Czechia") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # Spolu - red (coalition branding)
    "#1F4E79",  # ANO 2011 - blue
    "#00AEEF",  # Česká pirátská strana - light blue
    "#6A1B9A",  # SPD - purple
    "#CCCCCC",  # KSČM - dark red
    "#CCCCCC"   # ČSSD - red
  )
  ) + facet_wrap(~Country)

ggsave("plots/Czechia_parties.png", width = 7, height = 4, Czechia_parties)

#### Ireland ####

Ireland_parties <- Models_Vote_choice[["Ireland"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "370" ~ "Fianna Fáil",
                            "371" ~ "Fine Gael",
                            "372" ~ "Sinn Féin",
                            "373" ~ " ",
                            "374" ~ "  ",
                            "375" ~ "   ",
                            "376" ~ "    ",
                            "377" ~ "Green Alliance",
                            "378" ~ "Community Before Profit"
  ),
  group = factor(group, levels = c( 
    "Fianna Fáil",
    "Fine Gael",
    "Sinn Féin",
    " ",
    "  ",
    "   ",
    "    ",
    "Green Alliance",
    "Community Before Profit")),
  Country = "Ireland") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#66BB6A",  # Fianna Fáil - green
    "#0057B7",  # Fine Gael - blue
    "#006400",  # Sinn Féin - dark green
    "#CCCCCC",  # Social Democrats - purple
    "#CCCCCC",  # Labour - red
    "#CCCCCC",  # Aontú - black
    "#CCCCCC",  # Independent Ireland - grey
    "#3FA535",  # Green Alliance - green
    "#B22222"   # People Before Profit / Community Before Profit - red
  )
  ) + facet_wrap(~Country)

ggsave("plots/Ireland_parties.png", width = 7, height = 4, Ireland_parties)

#### Portugal ####

Portugal_parties <- Models_Vote_choice[["Portugal"]]$predictions %>% 
  mutate(group = case_match(group, 
                            "361" ~ "Partido Socialista",
                            "362" ~ "Chega",
                            "363" ~ "Iniciativa Liberal",
                            "364" ~ "Bloco de Esquerda",
                            "369" ~ "Alternativa Democrática Nacional",
                            "381" ~ "Aliança Democrática - AD"
  ),
  group = factor(group, levels = c( 
    "Partido Socialista",
    "Chega",
    "Iniciativa Liberal",
    "Bloco de Esquerda",
    "Alternativa Democrática Nacional",
    "Aliança Democrática - AD")),
  Country = "Portugal") %>% 
  filter(!is.na(group)) %>%  
  plot_predictions_vote(COLOURS =c(
    "#E30613",  # Partido Socialista - red
    "#1F4E79",  # Chega - dark blue
    "#00AEEF",  # Iniciativa Liberal - light blue
    "#8E44AD",  # Bloco de Esquerda - purple
    "#000000",  # Alternativa Democrática Nacional - black
    "#1E3A8A"   # Aliança Democrática (AD) - deep blue
  )
  ) + facet_wrap(~Country)

ggsave("plots/Portugal_parties.png", width = 7, height = 4, Portugal_parties)
