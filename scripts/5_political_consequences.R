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
