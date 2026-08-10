
Models_Vote_choice_df <- data.frame()
for(i in 1:length(Models_Vote_choice)){
  
  coef_df <- as.data.frame(Models_Vote_choice[[i]]$slopes) %>% 
    mutate(country = names(Models_Vote_choice[i]))
  
  Models_Vote_choice_df <- bind_rows(Models_Vote_choice_df, coef_df)
  
}

UK_model_df <- avg_slopes(UK_model, variables = "Security_FA", by = "Year") 
Models_Vote_choice_df <- bind_rows(Models_Vote_choice_df,
                                   UK_model_df %>% 
                                     mutate(country = "UK",
                                            Year = as.factor(Year)))


Models_Vote_choice_df %>% 
  arrange(-Security_FA) %>% 
  select(Security_FA, country) %>% 
  kable(format = "latex", col.names = c("Party Number", "Slope", "Country"),
        booktabs = TRUE, longtable = TRUE) %>% 
  save_kable("tables/slopes_parties.tex")

country_average_2025 <- EUI_data_short %>% 
  filter(Year == 2025) %>% 
  group_by(country) %>% 
  summarise(`Average Index Score` = round(mean(Security_FA, na.rm = TRUE), 3))

slopes_df <- Models_Vote_choice_df %>% 
  left_join(Vote_share_df %>% mutate(CHES_ID = as.character(CHES_ID))
             , by = c("group" = "CHES_ID")) %>% 
  mutate(country = replace_values(country, 
                                  "Croatia" ~ " *** Croatia",
                                  "Denmark" ~ "* Denmark",
                                  "France" ~ "*** France",
                                  "Germany" ~ "*** Germany",
                                  "Greece" ~ "*** Greece",
                                  "Hungary" ~ "*** Hungary",
                                  "Italy" ~ "*** Italy",
                                  "Lithuania" ~ "*** Lithuania",
                                  "Netherlands" ~ "*** Netherlands",
                                  "Poland" ~ "*** Poland",
                                  "Romania" ~ "*** Romania",
                                  "Slovakia" ~ "*** Slovakia",
                                  "Sweden" ~ "*** Sweden",
                                  "Austria" ~ "** Austria",
                                  "Belgium" ~ "** Belgium",
                                  "Bulgaria" ~ "*** Bulgaria",
                                  "Czech Republic" ~ "*** Czechia",
                                  "Portugal"  ~ "** Portugal"
                                  )) %>% 
  group_by(country) %>% 
  summarise(`Average Absolute Slope` = round(mean(abs(estimate)), 3),
            `Wieghted Slope` = round(mean(abs(estimate * Vote_share), na.rm = TRUE), 3),
            `Standard Deviation` = round(sd(estimate), 3),
            `Minimum and Maximum` = paste0(round(min(estimate), 3), "; ", round(max(estimate), 3))) %>% 
  left_join(country_average_2025, by = "country") %>% 
  group_by(country) %>% 
  arrange(-`Average Absolute Slope`)

country_order <- slopes_df %>% 
  arrange(`Average Absolute Slope`) %>% 
  pull(country)

slopes_by_year <- slopes_df %>% 
  mutate(country = factor(country, levels = country_order),
         #Year = factor(Year, levels = rev(c("2023", "2024", "2025")))
         ) %>% 
  ggplot(aes(x = `Average Absolute Slope`, y = country)) + 
  geom_col(fill = "magenta4") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = NULL) +
  
  #scale_fill_manual(values = c("orange", "purple3", "darkred")) + 
  theme_custom

ggsave("plots/slopes_by_year.png", slopes_by_year, width = 8, height = 4)

slopes_df %>% 
  kable(format = "latex", booktabs = TRUE, linesep = "", align = "lccccc",
        caption = "Average absolute slope for the defence-normalization cleavage by country\\label{tab:average_slopes} ") %>% 
  save_kable("tables/average_slope_country.tex")
  
quantile(Models_Vote_choice_df$estimate, 0.66)


EUI_data_short %>% 
  filter(Year == 2025) %>% 
  filter(Country %in% c("UK", "Sweden", "Germany", "Poland", "Italy", "Austria", "France", "Bulgaria")) %>% 
  group_by(Country, Past_vote) %>% 
  count() %>% 
  mutate(Past_vote = recode_values(as.character(Past_vote), 
                                   "339" ~ "Freiheitliche Partei Österreichs",
                                   "340" ~ "Österreichische Volkspartei",
                                   "341" ~ "Sozialdemokratische Partei",
                                   "342" ~ "NEOS – Das Neue Österreich",
                                   "343" ~ "Die Grünen – Die Grüne",
                                   "125" ~ "GERB–SDS",
                                   "126" ~ "We Continue the Change / Democratic Bulgaria (PP–DB)",
                                   "127" ~ "Delyan Slavchev Peevski",
                                   "128" ~ "Revival (Vazrazhdane)",
                                   "271" ~ "There Is Such a People",
                                   "320" ~ "Movement for Rights and Freedoms",
                                    "14" ~ "Les Républicains",
                                   "164" ~ "Rassemblement National",
                                   "168" ~ "Parti Communiste",
                                   "308" ~ "Renaissance",
                                   "19" ~ "SPD",
                                   "20" ~ "CDU",
                                   "21" ~ "Bündnis 90/Die Grünen",
                                   "22" ~ "AfD",
                                   "23" ~ "FDP",
                                   "24" ~ "Die Linke",
                                   "254" ~ "Bündnis Sahra Wagenknecht",
                                   "42" ~ "Movimento 5 Stelle",
                                   "43" ~ "Lega",
                                   "44" ~ "Forza Italia",
                                   "45" ~ "Fratelli d'Italia",
                                   "46" ~ "Partito Democratico",
                                   "47" ~ "+ Europa",
                                   "151" ~ "Alleanza Verdi-Sinistra",
                                   "92" ~ "PiS",
                                   "93" ~ "Lewica",
                                   "94" ~ "Konfederacja Wolność I Niepodległość",
                                   "95" ~ "Koalicja Obywatelska, KO",
                                   "75" ~ "Centerpartiet",
                                   "76" ~ "Liberalerna",
                                   "77" ~ "Kristdemokraterna",
                                   "78" ~ "Mijiöpartiet",
                                   "79" ~ "Socialdemokraterna",
                                   "80" ~ "Vänsterpartiet",
                                   "81" ~ "Sverigedemokraterna",
                                   "1" ~ "Conservative Party",
                                   "2" ~ "Labour Party",
                                   "3" ~ "Liberal Democrats",
                                   "4" ~ "SNP",
                                   "5" ~ "Plaid Cymru",
                                   "7" ~ "Green Party",
                                   "250" ~ "Reform Party"
                                   )) %>% 
  arrange(Country, -n) %>% 
  kable(format = "latex", booktabs = TRUE, linesep = "", align = "llr",
        col.names = c("Country", "Past Vote", "Number of Respondents"),
        caption = "Parties respondents reported voting for in their country's last election 2025 Dataset.\\label{tab:countries_vote} ") %>% 
  save_kable("tables/past_vote.tex")


Polarization_df <- Models_Vote_choice_df %>% 
  left_join(ches_data %>% select(new_q59, Kremlin_ties) %>% mutate(new_q59 = as.character(new_q59)), by = c("group" = "new_q59"))

lm(Avg_slope ~ sd, data = Polarization_df %>% 
     group_by(country, Year) %>% 
     summarize(Avg_slope = round(mean(abs(estimate)), 3), 
               sd = sd(Kremlin_ties, na.rm = TRUE))) %>% 
  summary()
polarization_slope <- Polarization_df %>% 
  group_by(country, Year) %>% 
  summarize(Avg_slope = round(mean(abs(estimate)), 3), 
            sd = sd(Kremlin_ties, na.rm = TRUE)) %>% 
  ggplot(aes(x = sd, y = Avg_slope)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "Standard Deviation of Kremlin Ties",
        y= "Average Absolute Slope",
       caption = "Each point represents a country-year average \n \n Signficant correlation between the two axises (beta = 0.007***)") + 
  theme_custom


ggsave("plots/polarization_slope.png", polarization_slope, width = 8, height = 5)
  
  