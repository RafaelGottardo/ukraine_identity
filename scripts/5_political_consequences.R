#### Script to test the effect of the defence-collaboration dimension on vote choice ####

vote_choice_hypothesis <- data.frame()
for(i in 1:length(Models_Vote_choice)){
  
  
  
  coef_df <- as.data.frame(Models_Vote_choice[[i]]$slopes) %>% 
    mutate(country = names(Models_Vote_choice[i]))
  
  Models_Vote_choice_df <- bind_rows(Models_Vote_choice_df, coef_df)
  
}

chi_squred_test <- list()
Chi_squared_test_df <- data.frame(Country = NA,
                                  Df = NA,
                                  Pvalue = NA)

for(i in 1:21){
  
  temp <- EUI_data_short %>% 
    filter(country == COUNTRIES[i]) 
  temp <- temp %>%
    mutate(Security_FA = as.numeric(Security_FA)) %>% 
    drop_na(all_of(c("Security_FA", "Q62", "Q9", "GAL_TAN", CONTROLS)))
  
if(COUNTRIES[i] %in% COUNTRIES_2023){
full_mod <- multinom(reformulate(c("Security_FA", "Q62", "Q9", "GAL_TAN", "as.factor(Year)", CONTROLS), response = "Past_vote"),
                     data = temp)
reduced_mod <- multinom(reformulate(c("Q62", "Q9", "GAL_TAN", "as.factor(Year)", CONTROLS), response = "Past_vote"),
                        data = temp)
}else{
  full_mod <- multinom(reformulate(c("Security_FA", "Q62", "Q9", "GAL_TAN", CONTROLS), response = "Past_vote"),
                       data = temp)
  reduced_mod <- multinom(reformulate(c("Q62", "Q9", "GAL_TAN", CONTROLS), response = "Past_vote"),
                          data = temp)
}
chi_squred_test[[i]] <- anova(reduced_mod, full_mod, test = "Chisq")
Chi_squared_test_df[i, 1] = COUNTRIES[i] 
Chi_squared_test_df[i, 2] = chi_squred_test[[i]]$`   Df`[2]
Chi_squared_test_df[i, 3] = chi_squred_test[[i]]$`Pr(Chi)`[2]

}

Chi_squared_test_df %>% 
  kable(format = "latex", digits = 3, col.names = c("Country", "Df", "P Value"),  booktabs = TRUE, linesep = "", align = "lcc",
        caption = "Chi-Squared test for signficance of the defence-normalization index on vote choice for each country\\label{tab:chi_square}") %>% 
  save_kable("tables/vote_choice_chi_sq.tex")


###### D-N GAL-TAN #####

EUI_data_short <- EUI_data_short %>% 
  mutate(GAL_TAN_values = case_when(GAL_TAN < 1.6 ~ "TAN",
                                    GAL_TAN > 1.6 & GAL_TAN < 2.5 ~ "Centre",
                                    GAL_TAN >= 2.5 ~ "GAL"))
  
gal_tan_countries <- lm(reformulate(c("country * GAL_TAN_values", "as.factor(Year)", CONTROLS),
                                      response = "Security_FA"),
                          data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
                          weights = balanced_weights)

gal_tan_countries_df <- avg_predictions(gal_tan_countries, variables = c("GAL_TAN_values", "country"))

gal_tan_countries_plot <- gal_tan_countries_df %>% 
  mutate(GAL_TAN_values = factor(GAL_TAN_values, levels = c("GAL", "Centre", "TAN")),
         country = factor(country, levels = country_order)) %>% 
  ggplot(aes(x = GAL_TAN_values, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  geom_linerange() + 
  facet_wrap(~country) + 
  labs(x = "GAL-TAN Position", y = "Predicted position on the defence normalization dimension\n(Higher numbers indicate more defence focused)") +
  theme_custom

ggsave("plots/gal_tan_countries_plot.png", gal_tan_countries_plot, width = 8, height = 8)



EU_countries <- lm(reformulate(c("country * as.factor(Q9)", "as.factor(Year)", CONTROLS),
                                    response = "Security_FA"),
                        data = EUI_data_short %>% filter(country %in% COUNTRIES_2022),
                        weights = balanced_weights)

EU_countries_df <- avg_predictions(EU_countries, variables = c("Q9", "country"))


EU_countries_plot <- EU_countries_df %>% 
  mutate(Q9 = recode_values(Q9, "0" ~ "Leave",
                            "1" ~ "Remain")) %>% 
  ggplot(aes(x = Q9, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point() +
  geom_linerange() +
  facet_wrap(~country) +
  labs(x = "Voting intention in an EU Exit Referendum",
       y = "Predicted position on the defence normalization dimension\n(Higher numbers indicate more defence focused)") + 
  theme_custom
  
ggsave("plots/EU_countries_plot.png", EU_countries_plot, width = 8, height = 8)
