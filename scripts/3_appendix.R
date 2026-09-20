###### Appendix #####

## Dependencies, each sourced here if it is not already in the session: the models from
## scripts/2_main_analysis.R (Social_group, Models_Vote_choice), the per-country plots
## from scripts/3_country_vote_plots.R, and slopes_df from scripts/5_prepare_social_media.R.
if (!exists("Social_group")) source("scripts/2_main_analysis.R")
if (!exists("Portugal_parties")) source("scripts/3_country_vote_plots.R")
if (!exists("slopes_df")) source("scripts/5_prepare_social_media.R")

## Countries ordered by average defence-normalization score (highest first). Recomputed
## here because scripts/5_prepare_social_media.R overwrites the global country_order with
## an ordering by average absolute slope, and factor(Country, levels = country_order)
## below would otherwise use that one.
country_order <- EUI_data_short %>% 
  group_by(country) %>% 
  summarise(Mean = mean(Security_FA, na.rm = TRUE)) %>% 
  arrange(-Mean) %>% 
  pull(country)

#### Alternative Factor Loadings ####

#### Dichotomous, continuous and continuous-without-DK loadings, side by side

variant_labels <- c(Security_FA_robust = "Dichotomous\nWithout DK",
                     Security_FA_cont = "Continuous",
                     Security_FA_cont_robust = "Continuous\nWithout DK")
variant_levels <- unname(variant_labels)
year_levels <- c("Pooled", "2025", "2024", "2023", "2022")

## Row 1 data: by year (pooled + yearly)
year_data_long <- EUI_data_short %>%
  filter(country %in% COUNTRIES_2022) %>%
  select(Security_FA_robust, Security_FA_cont, Security_FA_cont_robust, Year) %>%
  mutate(Year = as.character(Year)) %>%
  bind_rows(EUI_data_short %>%
              select(Security_FA_robust, Security_FA_cont, Security_FA_cont_robust) %>%
              mutate(Year = "Pooled")) %>%
  pivot_longer(cols = starts_with("Security_FA"), names_to = "Variant", values_to = "Security_FA") %>%
  transmute(Variant = variant_labels[Variant], Type = "Year", Group = Year, Security_FA)

## Row 2 data: by country
country_data_long <- EUI_data_short %>%
  filter(!is.na(country)) %>%
  select(country, Security_FA_robust, Security_FA_cont, Security_FA_cont_robust) %>%
  pivot_longer(cols = starts_with("Security_FA"), names_to = "Variant", values_to = "Security_FA") %>%
  transmute(Variant = variant_labels[Variant], Type = "Country", Group = as.character(country), Security_FA)

## Row 3 data: pooled density (no grouping)
density_data_long <- EUI_data_short %>%
  filter(country %in% COUNTRIES_2022) %>%
  select(Security_FA_robust, Security_FA_cont, Security_FA_cont_robust) %>%
  pivot_longer(cols = everything(), names_to = "Variant", values_to = "Security_FA") %>%
  transmute(Variant = variant_labels[Variant], Type = "Density", Group = NA_character_, Security_FA)

## Combined into one long data frame so the whole figure is a single facet
## grid (rows = plot type, columns = loading variant) instead of three
## separately built plots stitched together with ggarrange. Boxplot rows use
## a discrete y (Group); the density row has none, so each row is given its
## own scale below via ggh4x::facetted_pos_scales() to let that row's
## continuous (density) axis coexist with the two discrete (Year/Country)
## axes above it.
appendix_long <- bind_rows(year_data_long, country_data_long, density_data_long) %>%
  mutate(Variant = factor(Variant, levels = variant_levels),
         Type = factor(Type, levels = c("Year", "Country", "Density")),
         Group = factor(Group, levels = c(year_levels, country_order)))

factor_descriptives_grid <- appendix_long %>%
  ggplot(aes(x = Security_FA)) +
  geom_boxplot(data = ~ filter(.x, Type == "Year"), aes(y = Group), fill = "lightgrey") +
  geom_boxplot(data = ~ filter(.x, Type == "Country"), aes(y = Group), fill = "grey89") +
  geom_density(data = ~ filter(.x, Type == "Density"), fill = "lightgrey", alpha = 0.4) +
  facet_grid(Type ~ Variant, scales = "free_y", switch = "y") +
  ## facet_grid()'s own free_y only allows one shared scale TYPE across the whole
  ## plot, which is why each row is given its own explicit scale here (discrete
  ## for Year/Country, continuous for Density) via ggh4x::facetted_pos_scales() -
  ## this keeps axis labels shown only once per row (left-most column), unlike
  ## ggh4x::facet_grid2(independent = "y"), which forces every panel to be labelled.
  ggh4x::facetted_pos_scales(y = list(
    Type == "Year" ~ scale_y_discrete(),
    Type == "Country" ~ scale_y_discrete(),
    Type == "Density" ~ scale_y_continuous()
  )) +
  labs(x = "Defence-Normalization Dimension\n(Higher valued indicate more normalization-focused)",
       y = NULL) +
  scale_x_continuous( breaks = seq(-1.5, 1.55, length.out = 9),
                      limits = c(-1.5, 1.55),
                      labels = c("", "Defence\n(-1.3)", "", "", "0.0", "", "", "Normalization\n(1.5)", "")) +
  theme_custom +
  theme(strip.text.y.left = element_text(angle = 90))

ggsave(file.path(GLOBAL_DIR, "figures", "factor_descriptives_continious.png"), factor_descriptives_grid, width = 10, height = 10)

#### Country Vote-Choice Prediction Grid ####

## Combines the 22 per-country party vote-choice prediction plots built in
## scripts/3_country_vote_plots.R (Croatia_Parties ... Portugal_parties - one ggplot
## per country, each showing every party's predicted vote share as a function of the
## Defence-Normalization dimension) into a single large facet_wrap(~Country) grid,
## with panels ordered by country_order (average defence-normalization index score).
##
## Each country plot already carries its own bespoke party-colour palette via
## scale_colour_manual(); rather than re-deriving 22 sets of party names/hex codes
## here, each plot's already-resolved group -> colour mapping is read back off the
## built plot object and attached to its data as a literal hex colour column, so the
## combined plot can use scale_colour_identity() while every party keeps its colour.
##
## Two plots use a Country label that doesn't match the country variable's actual
## value elsewhere in the data (UK_parties: "United Kingdom"; Czechia_parties:
## "Czechia" vs. "Czech Republic") - both are corrected below so their panels line
## up with country_order.


country_vote_plots <- list(
  Croatia_Parties, Denmark_parties, Finland_parties, France_parties, Germany_parties,
  Greece_parties, Hungary_parties, Italy_parties, Lithuania_parties, Netherlands_Parties,
  Poland_parties, Romania_parties, Slovakia_parties, Spain_parties, Sweden_Parties,
  UK_parties, Austria_parties, Belgium_parties, Bulgaria_parties, Czechia_parties,
  Ireland_parties, Portugal_parties
)

extract_vote_plot_data <- function(p) {
  colour_scale <- p$scales$get_scales("colour")
  colour_lookup <- setNames(colour_scale$palette(nlevels(p$data$group)), levels(p$data$group))
  p$data %>%
    mutate(colour = unname(colour_lookup[as.character(group)]))
}

country_vote_long <- map_dfr(country_vote_plots, extract_vote_plot_data) %>%
  mutate(Country = replace_values(Country,
                                  "United Kingdom" ~ "UK",
                                  "Czechia" ~ "Czech Republic"),
         Country = factor(Country, levels = country_order))

## Fail loudly rather than silently plotting a single "NA" facet if any
## country label still doesn't match country_order (e.g. a new/renamed country).
if (anyNA(country_vote_long$Country)) {
  warning("Country Vote-Choice Grid: dropping ",
          sum(is.na(country_vote_long$Country)),
          " rows whose Country doesn't match country_order - check for label mismatches.")
  country_vote_long <- country_vote_long %>% filter(!is.na(Country))
}

country_vote_choice_grid <- country_vote_long %>%
  ggplot(aes(x = Security_FA, y = estimate, colour = colour, group = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey89", colour = NA) +
  geom_label_repel(
    data = ~ .x %>%
      filter(str_starts(group, " ", negate = TRUE)) %>%
      group_by(Country, group) %>%
      slice_max(Security_FA, n = 1),
    aes(label = group),
    show.legend = FALSE,
    fontface = "bold",
    size = 2,
    nudge_x = .1,
    min.segment.length = unit(0, 'lines')
  ) +
  scale_colour_identity() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7)) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, length.out = 9),
                     limits = c(-1.5, 1.5),
                     labels = c("", "Defence", "", "", "", "", "", "Normalization", "")) +
  facet_wrap(~Country, ncol = 4) +
  labs(x = "Defence-Normalization Dimension",
       y = "Predicted Probability of Supporting Each Party",
       caption = "Countries ordered by average defence-normalization index score.") +
  theme_custom +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none")

ggsave(file.path(GLOBAL_DIR, "figures", "country_vote_choice_grid.png"), country_vote_choice_grid, width = 20, height = 24)

#### VIF of Social Base ####

vif_raw <- vif(Social_group)

vif_tbl <- if (is.matrix(vif_raw)) {
  as.data.frame(vif_raw) %>%
    tibble::rownames_to_column("Variable")
} else {
  tibble(Variable = names(vif_raw),
         GVIF = as.numeric(vif_raw),
         Df = 1,
         `GVIF^(1/(2*Df))` = sqrt(as.numeric(vif_raw)))
}

vif_tbl <- vif_tbl %>%
  mutate(Variable = case_match(Variable,
                               "Econ_comparison" ~ "Economic comparison",
                               "Employed" ~ "Employment status",
                               "Woman" ~ "Gender",
                               "Education" ~ "Education",
                               "Age" ~ "Age (generation)",
                               "income_ppp_usd_log" ~ "Household income (log PPP USD)",
                               "as.factor(Year)" ~ "Survey year",
                               .default = Variable),
         across(where(is.numeric), ~ round(.x, 2)))

##### Table X-X ####
vif_tbl %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Generalized variance inflation factors for the social bases model.",
        col.names = c("Variable", "GVIF", "Df", "GVIF$^{1/(2\\cdot\\text{Df})}$"),
        align = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable(file.path(GLOBAL_DIR, "tables", "social_bases_vif.tex"))

#### Table: Parties by Family ####

## cross_walk's own new_q59_label/country_name columns are blank (not always
## literal "NA") for a number of codes, which produced entries like " (Greece)"
## or a bare "/" - so the party label falls back through several name columns,
## and the country always comes from each respondent's own reported `country`
## (always populated) rather than cross_walk's often-blank country_name.
## A handful of codes are blank across every cross_walk column too; those are
## filled in below from the hand-coded Past_vote -> name lookups already used
## in scripts/5_prepare_social_media.R and the Party case_when block of the
## security_party_position plot below. Anything still unmatched after that is dropped.
manual_party_names <- c(
  "308" = "Renaissance",
  "319" = "Bulgarian Socialist Party",
  "320" = "Movement for Rights and Freedoms",
  "339" = "Freiheitliche Partei Österreichs",
  "340" = "Österreichische Volkspartei",
  "130" = "Democratic Bulgaria",
  "175" = "National Creation"
)

## Bulgarian/Greek party names are stored in Cyrillic/Greek script in
## new_q59_label and party_name (e.g. "Движение за права и свободи, ДПС"),
## while party_name_english/parlgov_english carry the English translation
## ("Movement for Rights and Freedoms") - so any native-script label matching
## the Cyrillic or Greek unicode blocks is swapped for its English translation
## when one is available, instead of always taking new_q59_label first.
crosswalk_names <- cross_walk %>%
  mutate(across(c(new_q59_label, party_name_english, party_name,
                  ches_party, parlgov_english, parlgov_party),
                ~ na_if(trimws(.x), ""))) %>%
  mutate(Native_label = coalesce(new_q59_label, party_name, parlgov_party, ches_party),
         English_label = coalesce(party_name_english, parlgov_english),
         Party_label = if_else(!is.na(Native_label) &
                                 grepl("[Ͱ-ϿЀ-ӿ]", Native_label, perl = TRUE),
                               coalesce(English_label, Native_label),
                               coalesce(Native_label, English_label))) %>%
  transmute(Past_vote = as.character(new_q59), Party_label) %>%
  filter(!is.na(Past_vote)) %>%
  distinct(Past_vote, .keep_all = TRUE) %>%
  mutate(Party_label = coalesce(unname(manual_party_names[Past_vote]), Party_label))

family_party_table <- EUI_data_short %>%
  select(family, Past_vote, country) %>%
  filter(!is.na(family), !is.na(Past_vote), !is.na(country)) %>%
  mutate(Past_vote = as.character(Past_vote)) %>%
  distinct() %>%
  left_join(crosswalk_names, by = "Past_vote") %>%
  filter(!is.na(Party_label)) %>%
  mutate(Party = paste0(Party_label, " (", country, ")"),
         family = case_match(as.character(family),
                             "1" ~ "Radical Right/TAN",
                             "2" ~ "Conservative",
                             "3" ~ "Liberal",
                             "4" ~ "Christian-Democrat",
                             "5" ~ "Socialist",
                             "6" ~ "Radical Left",
                             "7" ~ "Green",
                             "8" ~ "Regionalist",
                             "9" ~ "No Family",
                             "10" ~ "Confessional",
                             "11" ~ "Agrarian/Centre"),
         family = factor(family, levels = c("Radical Right/TAN",
                                            "Conservative",
                                            "Liberal",
                                            "Christian-Democrat",
                                            "Socialist",
                                            "Radical Left",
                                            "Green",
                                            "Regionalist",
                                            "Confessional",
                                            "Agrarian/Centre",
                                            "No Family"))) %>%
  arrange(family, Party) %>%
  distinct(family, Party) %>%
  ## kable's usual auto-escaping is turned off below (escape = FALSE) so the
  ## \textbf{} markup on each family's first party survives - so every party
  ## name has to be LaTeX-escaped by hand here first.
  mutate(Party = str_replace_all(Party, "([&%$#_{}])", "\\\\\\1")) %>%
  group_by(family) %>%
  mutate(Party = if_else(row_number() == 1, paste0("\\textbf{", Party, "}"), Party)) %>%
  summarise(Parties = paste(Party, collapse = ", "), .groups = "drop")

family_party_table %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", longtable = TRUE, escape = FALSE,
        col.names = c("Party Family", "Parties"),
        caption = "Parties by CHES Party Family.\\label{tab:family_parties}") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
  column_spec(2, width = "10cm") %>%
  save_kable(file.path(GLOBAL_DIR, "tables", "family_parties.tex"))

#### Party Positions on the Dimension #####

test <- EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(Pro_Russia_individual = mean(Security_FA),
            Russia_party = mean(Securtiy_FA_party),
            Past_vote = mean(Past_vote))

lm(formula = Russia_party ~ Pro_Russia_individual, data = test)

security_party_position <- EUI_data_short %>% 
  group_by(country, Past_vote) %>% 
  summarise(Pro_Russia_individual = mean(Security_FA),
            Russia_party = mean(Securtiy_FA_party, na.rm = TRUE),
            Past_vote = mean(Past_vote)) %>% 
  ungroup() %>% 
  filter(!is.na(Russia_party)) %>% 
  mutate(country = factor(country, levels = c( "Belgium", "Bulgaria", "Croatia", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",    
                                               "Italy", "Lithuania", "Netherlands", "Poland", "Romania", "Slovakia", "Spain", "Sweden", "UK")),
         Party = case_when(Past_vote == 319 ~ "BSP, BG",
                           Past_vote == 320 ~ "	DPS, BG",
                           Past_vote == 42 ~ "5 Star, IT",
                           Past_vote == 35 ~ "EH Bildu, ES",
                           Past_vote == 44 ~ "Forza Italia, IT",
                           Past_vote == 1 ~ "Consevative Party, UK", 
                           Past_vote == 128 ~ "Revival, BG",
                           Past_vote == 72 ~ "EL-Ø, DK",
                           Past_vote == 60 ~ "FvD, NL",
                           Past_vote == 110 ~ "TS-LKD, LT",
                           Past_vote == 139 ~ "HDZ, HR",
                           Past_vote == 250 ~ "Reform, UK",
                           Past_vote == 59 ~ "Denk, DK",
                           Past_vote == 92 ~ "PiS, PL",
                           Past_vote == 24 ~ "Die Linke, DE",
                           Past_vote == 94 ~ "KWiN, PL",
                           Past_vote == 54 ~ "SP, NL",
                           Past_vote == 271 ~ "ITN, BG",
                           
                           TRUE ~ NA
                           
         )) %>% 
  ggplot(aes(x = Pro_Russia_individual, y = Russia_party, col = country, text = Past_vote)) + 
  geom_point(size = 2) + 
  # geom_hline(yintercept = 5, col = "grey40") + 
  scale_colour_manual(values = c(
    "#FAE042",  # Belgium
    "#00966E",  # Bulgaria
    "#FF0000",  # Croatia
    "#C60C30",  # Denmark
    "#003580",  # Finland
    "#0055A4",  # France
    "#000000",  # Germany
    "#0D5EAF",  # Greece
    "#CE2939",  # Hungary
    "darkgreen",  # Italy
    "#FDB913",  # Lithuania
    "#AE1C28",  # Netherlands
    "#DC143C",  # Poland
    "#002B7F",  # Romania
    "#EE1C25",  # Slovakia
    "#AA151B",  # Spain
    "#006AA7",  # Sweden
    "#012169"   # UK
  )) + 
  geom_label_repel(aes(label = Party),
                   show.legend = FALSE,
                   fontface = "bold",
                   nudge_x = .1,
                   min.segment.length = unit(0, 'lines')) + 
  scale_x_continuous( breaks = seq(-1, 1.3, length.out = 9),
                      limits = c(-1, 1.3), 
                      labels = c("", "Highest Defence Focus",  "", "", "- Relations with Russia -", "", "", "Highest Normalization Focus", ""))  + 
  labs(x = "Average Defence-Normalization Index Score of Supporters", 
       y = "Party's Position on the Defence Normalization Index",
       col = NULL) + 
  theme_custom + 
  theme(legend.position = "none")

ggsave(file.path(GLOBAL_DIR, "figures", "security_party_position.png"), security_party_position, width = 8, height = 5)

##### Vote Share Plots ####

Defence_normalization_threat_df <- EUI_data_short %>% 
  mutate(Russia_threat = ifelse(Q68 == 2, 1, 0)) %>% 
  group_by(country) %>% 
  summarize(Cleavage_strength = mean(Security_FA, na.rm = TRUE),
            # Cleavage_strength_robust = mean(Security_FA_robust, na.rm = TRUE),
            Average_threat = mean(Russia_threat, na.rm = TRUE)) %>% 
  mutate(Model = "(1) Average Individual Level Threat by Country")

lm(Cleavage_strength ~ Average_threat, Defence_normalization_threat_df) %>% 
  summary() 

lmer(reformulate(c("Russia_threat", CONTROLS, "as.factor(Year)", "(1 | country)"),
                 response = "Security_FA"), data = EUI_data_short %>% mutate(Russia_threat = ifelse(Q68 == 2, 1, 0))) %>% 
  summary()

Defence_normalization_threat <-  Defence_normalization_threat_df %>% 
  ggplot(aes(x = Average_threat, y = Cleavage_strength)) +
  facet_wrap(~Model) +
  labs(y = "Country Position on \nthe Defence-Normalization Dimension",
       x = "% of Respondents who Report being Threatened",
       col = "Defence-Normalization Measure") + 
  geom_smooth(method = "lm", col = "grey40") + 
  geom_point() +
  geom_text_repel(
    aes(label = country),
    hjust = 0,           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.03,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  ) + 
  scale_x_continuous(labels = scales::percent, limits = c(0.1, 0.8)) + 
  theme_custom

ggsave("plots/Defence_normalization_threat.png", Defence_normalization_threat, width = 8, height = 4)

Dispersion_df <- EUI_data_short %>%
  group_by(country) %>%
  summarise(Dispersion = sd(Securtiy_FA_party, na.rm = TRUE)) %>%
  left_join(slopes_df %>%
              mutate(country = str_remove(country, "^\\s*\\*+\\s*")) %>%
              select(country, `Average Absolute Slope`, `Wieghted Slope`), by = "country") %>% 
  mutate(Model = "(2) Average Dispersion by Country")

lm(Average_slope ~ Dispersion, Dispersion_df %>% rename(Average_slope = `Average Absolute Slope`)) %>% 
  summary()

Dispersion_plot <- Dispersion_df  %>% 
  ggplot(aes(x = Dispersion, y = `Average Absolute Slope`)) + 
  geom_smooth(method = "lm", col = "grey50") + 
  facet_wrap(~Model) +
  geom_point() + 
  geom_text_repel(  # only label the last point
    aes(label = country),
    hjust = 0,
    direction = "x",           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.05,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  ) + 
  scale_x_continuous(limits = c(0.1, 1.7)) +
  labs(x = "Standard Deviation of the Defence-Normalization Scores of Parties") + 
  theme_custom

ggsave("plots/Dispersion_plot.png", Dispersion_plot, width = 8, height = 4)

average_DN_df <- EUI_data_short %>%
  group_by(country) %>%
  summarise(Average = mean(Security_FA, na.rm = TRUE)) %>%
  left_join(slopes_df %>%
              mutate(country = str_remove(country, "^\\s*\\*+\\s*")) %>%
              select(country, `Average Absolute Slope`, `Wieghted Slope`), by = "country") %>% 
  mutate(Model = "(3) Average Position on the Defence-Normalization Dimension")

lm(Average_slope ~ Average, average_DN_df %>% rename(Average_slope = `Average Absolute Slope`)) %>% 
  summary()

Average_DN_plot <- average_DN_df  %>% 
  ggplot(aes(x = Average, y = `Average Absolute Slope`)) + 
  geom_smooth(method = "lm", col = "grey50") + 
  facet_wrap(~Model) +
  geom_point() + 
  geom_text_repel(  # only label the last point
    aes(label = country),
    hjust = 0,
    # direction = "x",           # only nudge vertically, keeps labels aligned to their point
    nudge_x = 0.03,             # push labels to the right of the last point
    xlim = c(-Inf, Inf),
    segment.size = 0.3,
    segment.color = "grey50",
    size = 3.5,
    col = "black"
  ) + 
  scale_x_continuous(limits = c(-0.75, 1)) +
  labs(x = "Average Country Level Defence-Normalization Position") + 
  theme_custom

ggarrange(Defence_normalization_threat, Dispersion_plot, Average_DN_plot, ncol = 1) %>% 
ggsave(file.path(GLOBAL_DIR, "figures", "Country_level_plots.png"), ., width = 10, height = 10)

#### Economic 
