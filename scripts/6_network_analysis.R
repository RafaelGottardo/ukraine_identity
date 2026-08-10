#### Code to Graph Networks imported from Python ####



pacman::p_load(igraph,
               xml2,
               rgexf,
               ggraph,
               ggrepel,
               ggplot2,
               tidyverse)

# ---- Parse a node attribute directly from the GEXF XML ---------------------
parse_attribute <- function(gexf_file, attr_title) {
  doc   <- xml2::read_xml(gexf_file)
  ns    <- c(d = "http://www.gexf.net/1.2draft")
  attr_def <- xml2::xml_find_first(
    doc, sprintf("//d:attributes[@class='node']/d:attribute[@title='%s']", attr_title), ns)
  if (inherits(attr_def, "xml_missing")) {
    stop("No <attribute title='", attr_title, "'> found in ", gexf_file)
  }
  attr_id <- xml2::xml_attr(attr_def, "id")
  nodes <- xml2::xml_find_all(doc, "//d:node", ns)
  ids   <- xml2::xml_attr(nodes, "id")
  vals  <- vapply(nodes, function(nd) {
    av <- xml2::xml_find_first(
      nd, sprintf("./d:attvalues/d:attvalue[@for='%s']", attr_id), ns)
    if (inherits(av, "xml_missing")) NA_character_
    else xml2::xml_attr(av, "value")
  }, character(1))
  setNames(as.numeric(vals), ids)
}

# ---- Load graph ----------------------------------------------------------
gexf_path <- "data_clean/horizontial_coherence_network.gexf"
gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  Refugee_A         = "Ukrainian Refugees:Agree",
  Refugee_D         = "Ukrainian Refugees:Disagree",
  Refugee_DK        = "Ukrainian Refugees:Don't Know",
  Russia_distrust   = "Distrust Russia",
  Russia_trust      = "Trust Russia",
  Ter_pre_war       = "Return to 2022 Stalemate",
  Ter_Russia_goals  = "Russia full Victory",
  Ter_Russia_more   = "Russia Partial Victory",
  Ter_Ukr_gain      = "Ukraine Partial Victory",
  Ter_ukr_retake    = "Ukraine Full Victory",
  Ukr_EU_A          = "Ukrainian EU Membership:Agree",
  Ukr_EU_D          = "Ukrainian EU Membership:Disagree",
  Ukr_EU_DK         = "Ukrainian EU Membership:Don't Know",
  Ukraine_distrust  = "Distrust Ukraine",
  Ukraine_trust     = "Trust Ukraine",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]
#coords_pca[,1] <- -coords_pca[,1]

# Reuse the same FR layout coordinates for consistency


horizontial_coherence_network_plot <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  labs(title = "Network by Defence-Focused vs Normalization-Focused") +
  theme(legend.position = "bottom")

ggsave("plots/horizontial_coherence_network_plot.png", horizontial_coherence_network_plot, width = 8, height = 4)

#### Node and respondent x-coordinates + H1 (horizontial coherence) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
horizontial_df <- read.csv("data_clean/horizontial_df.csv", row.names = 1)
dummy_mat <- as.matrix(horizontial_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
horizontial_df <- horizontial_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

horizontial_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_horizontial <- cor.test(horizontial_df$x_cor, horizontial_df$Defence_Focused)
h1_horizontial

#### Horizontial Coherence 2023 ####

# ---- Load graph ----------------------------------------------------------
gexf_path <- "data_clean/horizontial_coherence_network2023.gexf"
gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  Refugee_A         = "Ukrainian Refugees:Agree",
  Refugee_D         = "Ukrainian Refugees:Disagree",
  Refugee_DK        = "Ukrainian Refugees:Don't Know",
  Russia_distrust   = "Distrust Russia",
  Russia_trust      = "Trust Russia",
  Ter_pre_war       = "Return to 2022 Stalemate",
  Ter_Russia_goals  = "Russia full Victory",
  Ter_Russia_more   = "Russia Partial Victory",
  Ter_Ukr_gain      = "Ukraine Partial Victory",
  Ter_ukr_retake    = "Ukraine Full Victory",
  Ukr_EU_A          = "Ukrainian EU Membership:Agree",
  Ukr_EU_D          = "Ukrainian EU Membership:Disagree",
  Ukr_EU_DK         = "Ukrainian EU Membership:Don't Know",
  Ukraine_distrust  = "Distrust Ukraine",
  Ukraine_trust     = "Trust Ukraine",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]
coords_pca[,1] <- -coords_pca[,1]

# Reuse the same FR layout coordinates for consistency


horizontial_coherence_network_plot_2023 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 4,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  labs(title = "Network by Defence-Focused vs Normalization-Focused") +
  theme(legend.position = "bottom")

ggsave("plots/horizontial_coherence_network_plot_2023.png", horizontial_coherence_network_plot_2023, width = 8, height = 6)

#### Node and respondent x-coordinates + H1 (horizontial coherence) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
horizontial_df <- read.csv("data_clean/horizontial_df2023.csv", row.names = 1)
dummy_mat <- as.matrix(horizontial_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
horizontial_df <- horizontial_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

horizontial_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_horizontial_2023 <- cor.test(horizontial_df$x_cor, horizontial_df$Defence_Focused)
h1_horizontial_2023

#### Horizontial Coherence 2024 ####

# ---- Load graph ----------------------------------------------------------
gexf_path <- "data_clean/horizontial_coherence_network2024.gexf"
gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  Refugee_A         = "Ukrainian Refugees:Agree",
  Refugee_D         = "Ukrainian Refugees:Disagree",
  Refugee_DK        = "Ukrainian Refugees:Don't Know",
  Russia_distrust   = "Distrust Russia",
  Russia_trust      = "Trust Russia",
  Ter_pre_war       = "Return to 2022 Stalemate",
  Ter_Russia_goals  = "Russia full Victory",
  Ter_Russia_more   = "Russia Partial Victory",
  Ter_Ukr_gain      = "Ukraine Partial Victory",
  Ter_ukr_retake    = "Ukraine Full Victory",
  Ukr_EU_A          = "Ukrainian EU Membership:Agree",
  Ukr_EU_D          = "Ukrainian EU Membership:Disagree",
  Ukr_EU_DK         = "Ukrainian EU Membership:Don't Know",
  Ukraine_distrust  = "Distrust Ukraine",
  Ukraine_trust     = "Trust Ukraine",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]
coords_pca[,1] <- -coords_pca[,1]

# Reuse the same FR layout coordinates for consistency


horizontial_coherence_network_plot_2024 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  labs(title = "Network by Defence-Focused vs Normalization-Focused") +
  theme(legend.position = "bottom")

ggsave("plots/horizontial_coherence_network_plot_2024.png", horizontial_coherence_network_plot_2024, width = 8, height = 6)

#### Node and respondent x-coordinates + H1 (horizontial coherence) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
horizontial_df <- read.csv("data_clean/horizontial_df2024.csv", row.names = 1)
dummy_mat <- as.matrix(horizontial_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
horizontial_df <- horizontial_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

horizontial_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_horizontial_2024 <- cor.test(horizontial_df$x_cor, horizontial_df$Defence_Focused)

h1_horizontial_2024

#### Horizontial Coherence 2025 ####

# ---- Load graph ----------------------------------------------------------
gexf_path <- "data_clean/horizontial_coherence_network2025.gexf"
gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  Refugee_A         = "Ukrainian Refugees:Agree",
  Refugee_D         = "Ukrainian Refugees:Disagree",
  Refugee_DK        = "Ukrainian Refugees:Don't Know",
  Russia_distrust   = "Distrust Russia",
  Russia_trust      = "Trust Russia",
  Ter_pre_war       = "Return to 2022 Stalemate",
  Ter_Russia_goals  = "Russia full Victory",
  Ter_Russia_more   = "Russia Partial Victory",
  Ter_Ukr_gain      = "Ukraine Partial Victory",
  Ter_ukr_retake    = "Ukraine Full Victory",
  Ukr_EU_A          = "Ukrainian EU Membership:Agree",
  Ukr_EU_D          = "Ukrainian EU Membership:Disagree",
  Ukr_EU_DK         = "Ukrainian EU Membership:Don't Know",
  Ukraine_distrust  = "Distrust Ukraine",
  Ukraine_trust     = "Trust Ukraine",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)


# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]
#coords_pca[,1] <- -coords_pca[,1]

# Reuse the same FR layout coordinates for consistency


horizontial_coherence_network_plot_2025 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 4,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  labs() +
  theme(legend.position = "bottom")

ggsave("plots/horizontial_coherence_network_plot_2025.png", horizontial_coherence_network_plot_2025, width = 8, height = 4)

#### Node and respondent x-coordinates + H1 (horizontial coherence) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
horizontial_df <- read.csv("data_clean/horizontial_df2025.csv", row.names = 1)
dummy_mat <- as.matrix(horizontial_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
horizontial_df <- horizontial_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

horizontial_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_horizontial_2025 <- cor.test(horizontial_df$x_cor, horizontial_df$Defence_Focused)
h1_horizontial_2025

#### Horizontial Coherence Graph ####

ggpubr::ggarrange(horizontial_coherence_network_plot_2023,
                  horizontial_coherence_network_plot_2024, horizontial_coherence_network_plot_2025, common.legend = TRUE,
                  legend = "bottom") %>% 
  ggsave( "plots/horizontial_over_time.png", ., width = 12, height = 9)

#### Horizontial Coherence Hypothesis ####

network_hypothesis_horizontial <- data.frame(Year = c("2023", "2024", "2025"),
                                 Correlation = c(abs(h1_horizontial_2023$estimate), 
                                                 abs(h1_horizontial_2024$estimate),
                                                 abs(h1_horizontial_2025$estimate)),
                                 xmin = c(abs(h1_horizontial_2023$conf.int[1]), 
                                          abs(h1_horizontial_2024$conf.int[1]),
                                          abs(h1_horizontial_2025$conf.int[1])),
                                 xmax = c(abs(h1_horizontial_2023$conf.int[2]),
                                          abs(h1_horizontial_2024$conf.int[2]),
                                          abs(h1_horizontial_2025$conf.int[2])))      

#### Vertical extension 2022 ####

gexf_path <- "data_clean/vertical_extension_network_2022.gexf"

gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  Econ_better_off = "Economically Better Off",
  Econ_DK = "Economically Don't Know",
  Econ_the_same = "Economically The Same",
  Econ_worse_off = "Economically Worse Off",
  Energy = "Worried about Energy Prices",
  EU_punish_financial = "EU Punishes Violation of Law: Financial Sanctions",
  EU_punish_nointerference  = "EU Punishes Violation of Law: No punishment",
  EU_punish_other   = "EU Punishes Violation of Law: Other",
  EU_punish_voting = "EU Punishes Violation of Law: Remove Voting Rights",
  # General_trust_notrust      = "Generalized Trust: Low Trust",
  # General_trust_trust = "Generalized Trust: High Trust",
  High_democracy       = "High Support Democracy",
  Low_democracy  = "Low Support Democracy",
  Middle_democracy   = "Middle Support Democracy",
  Inflation      = "Worried about Inflation",
  Neutral_aggrandizement = "E. Aggrandizement: Neutral",
  Oppose_aggrandizement = "E. Aggrandizement: Oppose",
  Support_aggrandizement  = "E. Aggrandizement: Support",
  Support_EU  = "Pro EU Membership",
  Support_NATO     = "NATO is Important",
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US",
  Russia_threat = "Threatened by Russia",
  US_threat = "Threatened by the US",
  China_threat = "Threated by China",
  Nuclear_threat = "Threated by Nuclear War",
  Other_threat = "Another Threat",
  DK_threat = "Don't Know Biggest Threat"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]

coords_pca[, 1] <- -coords_pca[, 1]


# Reuse the same FR layout coordinates for consistency

vertical_extension_network_plot_2022 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight, alpha = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  facet_wrap(~"2022") + 
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  theme(strip.background = element_rect(fill = "#e6f8d1", colour = NA)) + 
  labs() +
  theme(legend.position = "bottom")

ggsave("plots/vertical_extension_network_plot_2022.png", vertical_extension_network_plot_2022, width = 8, height = 6)

#### Node and respondent x-coordinates + H1 (vertical extension) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
vertical_extension_df <- read.csv("data_clean/vertical_extension2022.csv", row.names = 1)
dummy_mat <- as.matrix(vertical_extension_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
vertical_extension_df <- vertical_extension_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

vertical_extension_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_vertical_extension_2022 <- cor.test(vertical_extension_df$x_cor, vertical_extension_df$Defence_Focused)
h1_vertical_extension_2022

#### Vertical extension 2023 ####

gexf_path <- "data_clean/vertical_extension_network_2023.gexf"

gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  Econ_better_off = "Economically Better Off",
  Econ_DK = "Economically Don't Know",
  Econ_the_same = "Economically The Same",
  Econ_worse_off = "Economically Worse Off",
  Energy = "Worried about Energy Prices",
  EU_punish_financial = "EU Punishes Violation of Law: Financial Sanctions",
  EU_punish_nointerference  = "EU Punishes Violation of Law: No punishment",
  EU_punish_other   = "EU Punishes Violation of Law: Other",
  EU_punish_voting = "EU Punishes Violation of Law: Remove Voting Rights",
  # General_trust_notrust      = "Generalized Trust: Low Trust",
  # General_trust_trust = "Generalized Trust: High Trust",
  High_democracy       = "High Support Democracy",
  Low_democracy  = "Low Support Democracy",
  Middle_democracy   = "Middle Support Democracy",
  Inflation      = "Worried about Inflation",
  Neutral_aggrandizement = "E. Aggrandizement: Neutral",
  Oppose_aggrandizement = "E. Aggrandizement: Oppose",
  Support_aggrandizement  = "E. Aggrandizement: Support",
  Support_EU  = "Pro EU Membership",
  Support_NATO     = "NATO is Important",
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US",
  Russia_threat = "Threatened by Russia",
  US_threat = "Threatened by the US",
  China_threat = "Threated by China",
  Nuclear_threat = "Threated by Nuclear War",
  Other_threat = "Another Threat",
  DK_threat = "Don't Know Biggest Threat"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]

coords_pca[, 1] <- -coords_pca[, 1]


# Reuse the same FR layout coordinates for consistency

vertical_extension_network_plot_2023 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight, alpha = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  facet_wrap(~"2023") + 
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  theme(strip.background = element_rect(fill = "#e6f8d1", colour = NA)) + 
  theme(legend.position = "bottom")

ggsave("plots/vertical_extension_network_plot_2023.png", vertical_extension_network_plot_2023, width = 8, height = 6)

#### Node and respondent x-coordinates + H1 (vertical extension) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
vertical_extension_df <- read.csv("data_clean/vertical_extension2023.csv", row.names = 1)
dummy_mat <- as.matrix(vertical_extension_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
vertical_extension_df <- vertical_extension_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

vertical_extension_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_vertical_extension_2023 <- cor.test(vertical_extension_df$x_cor, vertical_extension_df$Defence_Focused)
h1_vertical_extension_2023

#### Vertical extension 2024 ####

gexf_path <- "data_clean/vertical_extension_network_2024.gexf"

gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  Econ_better_off = "Economically Better Off",
  Econ_DK = "Economically Don't Know",
  Econ_the_same = "Economically The Same",
  Econ_worse_off = "Economically Worse Off",
  Energy = "Worried about Energy Prices",
  EU_punish_financial = "EU Punishes Violation of Law: Financial Sanctions",
  EU_punish_nointerference  = "EU Punishes Violation of Law: No punishment",
  EU_punish_other   = "EU Punishes Violation of Law: Other",
  EU_punish_voting = "EU Punishes Violation of Law: Remove Voting Rights",
  # General_trust_notrust      = "Generalized Trust: Low Trust",
  # General_trust_trust = "Generalized Trust: High Trust",
  High_democracy       = "High Support Democracy",
  Low_democracy  = "Low Support Democracy",
  Middle_democracy   = "Middle Support Democracy",
  Inflation      = "Worried about Inflation",
  Neutral_aggrandizement = "E. Aggrandizement: Neutral",
  Oppose_aggrandizement = "E. Aggrandizement: Oppose",
  Support_aggrandizement  = "E. Aggrandizement: Support",
  Support_EU  = "Pro EU Membership",
  Support_NATO     = "NATO is Important",
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US",
  Russia_threat = "Threatened by Russia",
  US_threat = "Threatened by the US",
  China_threat = "Threated by China",
  Nuclear_threat = "Threated by Nuclear War",
  Other_threat = "Another Threat",
  DK_threat = "Don't Know Biggest Threat"
)

stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]

#coords_pca[, 1] <- -coords_pca[, 1]


# Reuse the same FR layout coordinates for consistency

vertical_extension_network_plot_2024 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight, alpha = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  facet_wrap(~"2024") + 
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  theme(strip.background = element_rect(fill = "#e6f8d1", colour = NA)) + 
  labs() +
  theme(legend.position = "bottom")

ggsave("plots/vertical_extension_network_plot_2024.png", vertical_extension_network_plot_2024, width = 8, height = 6)

#### Node and respondent x-coordinates + H1 (vertical extension) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
vertical_extension_df <- read.csv("data_clean/vertical_extension2024.csv", row.names = 1)
dummy_mat <- as.matrix(vertical_extension_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
vertical_extension_df <- vertical_extension_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

vertical_extension_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_vertical_extension_2024 <- cor.test(vertical_extension_df$x_cor, vertical_extension_df$Defence_Focused)
h1_vertical_extension_2024

#### Vertical extension 2025 ####

gexf_path <- "data_clean/vertical_extension_network_2025.gexf"

gexf_obj <- rgexf::read.gexf(gexf_path)
g        <- rgexf::gexf.to.igraph(gexf_obj)

# ---- Explicit id -> human-readable label mapping -------------------------
label_map <- c(
  Econ_better_off = "Economically Better Off",
  Econ_DK = "Economically Don't Know",
  Econ_the_same = "Economically The Same",
  Econ_worse_off = "Economically Worse Off",
  Energy = "Worried about Energy Prices",
  EU_punish_financial = "EU Punishes Violation of Law: Financial Sanctions",
  EU_punish_nointerference  = "EU Punishes Violation of Law: No punishment",
  EU_punish_other   = "EU Punishes Violation of Law: Other",
  EU_punish_voting = "EU Punishes Violation of Law: Remove Voting Rights",
  # General_trust_notrust      = "Generalized Trust: Low Trust",
  # General_trust_trust = "Generalized Trust: High Trust",
  High_democracy       = "High Support Democracy",
  Low_democracy  = "Low Support Democracy",
  Middle_democracy   = "Middle Support Democracy",
  Inflation      = "Worried about Inflation",
  Neutral_aggrandizement = "E. Aggrandizement: Neutral",
  Oppose_aggrandizement = "E. Aggrandizement: Oppose",
  Support_aggrandizement  = "E. Aggrandizement: Support",
  Support_EU  = "Pro EU Membership",
  Support_NATO     = "NATO is Important",
  China_distrust    = "Distrust China",
  China_trust       = "Trust China",
  US_distrust       = "Distrust US",
  US_trust          = "Trust US",
  Russia_threat = "Threatened by Russia",
  US_threat = "Threatened by the US",
  China_threat = "Threated by China",
  Nuclear_threat = "Threated by Nuclear War",
  Other_threat = "Another Threat",
  DK_threat = "Don't Know Biggest Threat"
)


stopifnot(all(V(g)$name %in% names(label_map)))
V(g)$label <- label_map[V(g)$name]

# ---- Pull attribute, keyed by node id -------------------------------------
defence_map <- parse_attribute(gexf_path, "Defence_Focused")
V(g)$Defence_Focused <- defence_map[V(g)$name]

# ---- Two discrete colors based on sign ------------------------------------
col_defence <- "steelblue3"
col_norm    <- "firebrick3"

V(g)$color <- ifelse(V(g)$Defence_Focused >= 0, col_defence, col_norm)

# ---- Plot -------------------------------------------------------------
set.seed(1998)
E(g)$weight <- as.numeric(E(g)$weight)
g <- delete_edges(g, E(g)[E(g)$weight <= 0])

layout_g <- layout_with_fr(g, weights = E(g)$weight,  niter = 2000)
pca  <- prcomp(layout_g, center = TRUE, scale. = FALSE)
coords_pca <- pca$x[, 1:2]

coords_pca[, 1] <- -coords_pca[, 1]


# Reuse the same FR layout coordinates for consistency

vertical_extension_network_plot_2025 <- ggraph(g, layout = coords_pca) +
  geom_edge_link(aes(width = weight, alpha = weight), color = "gray60", alpha = 0.4) +
  scale_edge_width(range = c(0.2, 2), guide = "none") +
  geom_node_point(aes(color = ifelse(Defence_Focused >= 0,
                                     "Defence-Focused",
                                     "Normalization-Focused")),
                  size = 5) +
  facet_wrap(~"2025") +
  geom_text_repel(aes(x = x, y = y, label = label),
                  size = 4,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  segment.color = "gray40",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Defence-Focused" = "steelblue2",
                                "Normalization-Focused" = "firebrick2"),
                     name = NULL) +
  theme_void() +
  theme(strip.background = element_rect(fill = "#e6f8d1", colour = NA)) + 
  labs() +
  theme(legend.position = "bottom")

ggsave("plots/vertical_extension_network_plot_2025.png", vertical_extension_network_plot_2025, width = 8, height = 4)

#### Node and respondent x-coordinates + H1 (vertical extension) ####

# ---- x-coordinate of each attitude node, reusing the FR + PCA layout above ----
xcor_att <- setNames(coords_pca[, 1], V(g)$name)

# renormalize x between -1 and 1
xcor_att <- ((xcor_att - min(xcor_att)) / (max(xcor_att) - min(xcor_att))) * 2 - 1

# ---- x-coordinate of each respondent: mean position of the attitudes they endorse ----
vertical_extension_df <- read.csv("data_clean/vertical_extension2025.csv", row.names = 1)
dummy_mat <- as.matrix(vertical_extension_df[, names(xcor_att)])
keep <- rowSums(dummy_mat) > 0
vertical_extension_df <- vertical_extension_df[keep, ]
dummy_mat <- dummy_mat[keep, ]

vertical_extension_df$x_cor <- as.numeric(dummy_mat %*% xcor_att) / rowSums(dummy_mat)

# ---- H1: respondents' network position correlates with their Defence-Focused identity ----
h1_vertical_extension_2025 <- cor.test(vertical_extension_df$x_cor, vertical_extension_df$Defence_Focused)
h1_vertical_extension_2025

ggpubr::ggarrange(vertical_extension_network_plot_2022, vertical_extension_network_plot_2023,
                  vertical_extension_network_plot_2024, vertical_extension_network_plot_2025, common.legend = TRUE,
                  legend = "bottom") %>% 
  ggsave( "plots/network_over_time.png", ., width = 12, height = 9)
   

network_hypothesis <- data.frame(Year = c("2022", "2023", "2024", "2025"),
                                 Correlation = c(abs(h1_vertical_extension_2022$estimate), 
                                                 abs(h1_vertical_extension_2023$estimate),
                                                 abs(h1_vertical_extension_2024$estimate),
                                                 abs(h1_vertical_extension_2025$estimate)),
                                 xmin = c(abs(h1_vertical_extension_2022$conf.int[1]), 
                                          abs(h1_vertical_extension_2023$conf.int[1]),
                                          abs(h1_vertical_extension_2024$conf.int[1]),
                                          abs(h1_vertical_extension_2025$conf.int[1])),
                                 xmax = c(abs(h1_vertical_extension_2022$conf.int[2]), 
                                                abs(h1_vertical_extension_2023$conf.int[2]),
                                                 abs(h1_vertical_extension_2024$conf.int[2]),
                                                 abs(h1_vertical_extension_2025$conf.int[2])))               

network_hypothesis <- network_hypothesis %>% 
  mutate(Model = "Vertical Coherence")

network_hypothesis_horizontial <- network_hypothesis_horizontial %>% 
  mutate(Model = "Horizontial Coherence")

network_hypothesis <- network_hypothesis %>% 
  bind_rows(network_hypothesis_horizontial)
  
network_hypothesis_plot <- network_hypothesis %>% 
  mutate(Year = factor(Year, levels = rev(c("2022", "2023", "2024", "2025"))),
         Model = factor(Model, levels = rev(c("Horizontial Coherence", "Vertical Coherence")))) %>% 
  ggplot(aes(x = Correlation, y = Year, xmin = xmin, xmax = xmax, col = Model)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
 # geom_vline(xintercept = 0, lty = 4, col = "grey40") +
  scale_colour_manual(values = c("darkgreen", "magenta4")) + 
  guides(colour = guide_legend(reverse = FALSE)) +
  labs(x = "Correlation between being defence-focused and location within the network", 
       y = NULL,
       col = NULL) + 
  theme_custom

ggsave("plots/network_hypothesis_plot.png", network_hypothesis_plot, height = 4, width = 8)


