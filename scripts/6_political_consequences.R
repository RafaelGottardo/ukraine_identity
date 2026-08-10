##### Polarization Slope Graph ####

polarization_slope_df <- EUI_data_short %>% 
  group_by(country, Year) %>% 
  summarise(`Average Polarization` = mean(Affective_Polarization, na.rm = TRUE)) %>% 
  filter(Year == 2025) %>% 
  left_join(slopes_df, by = "country")

lm(`Average Absolute Slope` ~ `Average Polarization`, data = polarization_slope_df) %>% 
  summary()

polarization_slope_plot <- polarization_slope_df %>% 
  ggplot(aes(x = `Average Polarization`, y = `Average Absolute Slope`)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", alpha = 0.3) + 
  geom_text_repel(aes(label = country)) + 
  theme_custom

ggsave("plots/polarization_slope_plot.png", polarization_slope_plot, width = 8, height = 4)
