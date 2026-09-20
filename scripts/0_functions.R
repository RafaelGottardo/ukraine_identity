###################################################################################### 
##### Functions and Objects for the European Identity Related to Ukraine Project #####
###################################################################################### 

#### Set File Directory ####

GLOBAL_DIR <<- "~/Library/CloudStorage/Dropbox/Apps/Overleaf/Security_collaboration"

#### Load Packages ####

pacman::p_load(tidyverse,
               haven,
               estimatr,
               labelled,
               readxl, 
               car,
               sf, 
               srvyr,
               rnaturalearth,
               rnaturalearthdata,
               foreign,
               ggrepel,
               marginaleffects,
               patchwork,
               ggtext, 
               flextable,
               webshot2,
               ggnewscale,
               knitr,
               kableExtra,
               webshot2,
               gt,
               shiny,
               psych,
               nnet,
               furrr,
               lme4,
               broom.mixed,
               nnet,
               rgexf,
               igraph,
               ggraph,
               xml2,
               tidygraph,
               ggridges,
               ggpubr
)

#### ggplot Custom Theme ####

theme_custom =   theme_minimal() + theme(legend.position = "bottom",
                                         strip.placement = "outside",
                                         legend.spacing.y = unit(0.1, "cm"), # space between rows
                                         legend.key.height = unit(0.3, "cm") ,
                                         plot.title = element_text(hjust = 0.5,
                                                                   face = "bold",
                                                                   size = 10),
                                         plot.subtitle = element_text(hjust = 0.5,
                                                                      face = "bold",
                                                                      size = 8),
                                         axis.title.y = element_text(face = "bold",
                                                                     size = 12),
                                         axis.title.x = element_text(face = "bold",
                                                                     size = 12),
                                         plot.caption = element_text(size = 7,
                                                                     hjust = 1,
                                                                     lineheight = 0.5),
                                         legend.text = element_text(face = "bold", 
                                                                    size = 10),
                                         legend.box = "vertical",
                                         legend.title = element_text(face = "bold",
                                                                     size = 12),
                                         strip.text = element_text(size = 10, face = "bold"),          # smaller text
                                         strip.background = element_rect(fill = "#e6f8d1", colour = NA),                                 # optional
                                         strip.text.y.left = element_text(angle = 0),                  # if y-strips
                                         strip.text.x = element_text(margin = margin(b = 1, t = 1)),   # small top/bottom padding
                                         strip.text.y = element_text(margin = margin(l = 1, r = 1)),
                                         axis.text.x = element_text(face = "bold", 
                                                                  size = 10),
                                         axis.text.y = element_text(face = "bold", 
                                                                    size = 12, margin = margin(r = 0.05)),
                                         # panel.grid.major.x = element_blank(),
                                        #  panel.grid.minor.x = element_blank(),
                                         # panel.grid.major.y = element_blank(),
                                          #panel.grid.minor.y = element_blank(),
                                         axis.line.x = element_line(color = "grey80"),
                                         axis.line.y = element_line(color = "grey80"),)

#### Colours for Social Bases ####

group_colors <- c(
  "Age"         = "#004488",  # deep blue
  "Gender"      = "#BB5566",  # muted rose
  "Urban"       = "#6699CC",  # dark maroon (alt if too close to Gender: "#6699CC")
  "Ideology"    = "#117733",  # forest green
  "Comparision" = "#882255",  # plum
  "Employment"  = "#44AA99",  # teal
  "Education"   = "#332288",  # indigo
  "Income"      = "#88CCEE",  # light sky blue
  "Trust"       = "#555555"   # neutral grey
)

#### List of Countries ####


ORIGINAL_COUNTRIES <-  c("Denmark", "Finland", "Germany", 
                         "France", "Lithuania",
                         "Greece", "Italy", "Poland",
                         "Spain", "Sweden", "UK")

COUNTRIES_2022 <- c("UK", "Denmark", "Greece", "Hungary", "Lithuania",
                    "Italy", "Poland", "Netherlands", "Romania",  "Slovakia",
                    "Croatia", "Bulgaria", "Spain", "Finland", "France",
                    "Germany", "Sweden")

NEW_COUNTRIES_2024 <- c("Slovakia", "Netherlands", "Denmark", "Hungary",
                        "Croatia", "Bulgaria", "Lithuania", "Romania", "Greece", 
                        "Poland", "Spain", "Germany", "Sweden", "Italy", "France",
                        "Finland", "Belgium", "UK")


CONTROLS <- c("Woman", "Education", "Age", "Urban")

#### Plot Predictions - VOTE CHOICE ####

plot_predictions_vote <- function(data, COLOURS){
data %>% 
ggplot(aes(x = Security_FA, y = estimate, color = group)) +
  geom_line(size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey89") +
    geom_label_repel(
    data = ~ .x %>% 
      filter(str_starts(group, " ", negate = TRUE)) %>% 
      group_by(group) %>% 
      slice_max(Security_FA, n = 1),
    aes(label = group),
    show.legend = FALSE,
    fontface = "bold",
    nudge_x = .1,
    min.segment.length = unit(0, 'lines')
  ) +
  scale_colour_manual(values = COLOURS) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.7)) +
    scale_x_continuous( breaks = seq(-1.5, 1.5, length.out = 9),
                        limits = c(-1.5, 1.5), 
                        labels = c("", "Highest Defence Focus",  "", "", "- Relations with Russia -", "", "", "Highest Normalization Focus", "")) +
  labs(
    y = "Predicted Probabilty of Supporting Each Party",
    color = "Previous Vote Choice",
    x = "Defence-Normalization Dimension"
  ) +
  theme_custom + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none") 
}

#### Rescale 0 - 1 ####
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

#### marginaleffects newdata ####

## Without an explicit `newdata`, avg_slopes()/avg_predictions() rebuild it from the
## model call's full data frame - all 300+ columns of EUI_data_short - which makes
## them ~15x slower on the multinom fits (and memory-hungry). This returns the rows
## the model was actually fitted on (complete cases on every variable in its formula),
## restricted to the columns it uses plus any `extra` ones (e.g. a `by` variable).
## The estimates are identical to the default; keeping rows with a missing outcome
## would NOT be, so the complete-case step matters.
me_newdata <- function(model, data, extra = NULL){
  model_vars <- all.vars(formula(model))
  data %>%
    select(all_of(unique(c(model_vars, extra)))) %>%
    drop_na(all_of(model_vars))
}


#### Party Switch Functions ####

## One model per row facet x column facet (4 fits): each column is a standalone
## moderation of the Security_FA effect by that placement measure, within the
## countries that lack the relevant pro-normalization party. The outcome matches
## the row - voting an economic-right pro-normalization party where no economic-left
## one exists, voting a TAN pro-normalization party where no GAL one exists.
## `dat` here is already restricted to the (sometimes very few) countries where
## a given party-supply flag holds, so `moderator` can end up with only one
## level actually present once NAs are dropped - `Security_FA * moderator`
## can't build contrasts for that, so this skips the panel (with a warning)
## instead of letting the whole script crash on "contrasts need 2 or more levels".
fit_switch <- function(dat, moderator, response){
  n_levels <- dplyr::n_distinct(dat[[moderator]], na.rm = TRUE)
  if (n_levels < 2) {
    warning("fit_switch(): '", moderator, "' has only ", n_levels,
            " level(s) among countries ", paste(unique(dat$country), collapse = ", "),
            " - skipping this panel.", call. = FALSE)
    return(NULL)
  }
  lm_robust(reformulate(c(paste0("Security_FA * ", moderator), CONTROLS, "as.factor(Year)"),
                        response = response),
            data = dat)
}

switch_slopes <- function(dat, row_lab, response){
  m_lr <- fit_switch(dat, "LR_self", response)
  m_galtan <- fit_switch(dat, "GAL_TAN_values", response)
  bind_rows(
    if (!is.null(m_lr)) as.data.frame(avg_slopes(m_lr, variables = "Security_FA", by = "LR_self",
                                                 newdata = me_newdata(m_lr, dat, "LR_self"))) %>%
      transmute(row = row_lab, col = "Left-Right Self-Placement",
                x = as.character(LR_self), estimate, conf.low, conf.high),
    if (!is.null(m_galtan)) as.data.frame(avg_slopes(m_galtan, variables = "Security_FA", by = "GAL_TAN_values",
                                                     newdata = me_newdata(m_galtan, dat, "GAL_TAN_values"))) %>%
      transmute(row = row_lab, col = "GAL-TAN Placement",
                x = as.character(GAL_TAN_values), estimate, conf.low, conf.high)
  )
}


