########################################################################
##### Network Statistics: Horizontal Coherence & Vertical Extension #####
########################################################################
#
# Computes the same family of network-structure statistics used in
# network-statistcs-anes.R (density, degree, weighted transitivity,
# average path length, assortativity, community detection, and a
# subgroup/cluster comparison) for the attitude networks built in
# network_analysis.ipynb and visualized in 6_network_analysis.R.
#
# Instead of Democrat/Republican "evaluation", the grouping variable
# here is Defence-Focused vs. Normalization-Focused (the sign of each
# node's correlation with the Security_FA factor, exactly as used to
# color nodes in 6_network_analysis.R).
#
# Inputs (produced by network_analysis.ipynb):
#   data_clean/horizontial_coherence_network.gexf        (pooled)
#   data_clean/horizontial_coherence_network2023.gexf
#   data_clean/horizontial_coherence_network2024.gexf
#   data_clean/horizontial_coherence_network2025.gexf
#   data_clean/vertical_extension_network.gexf            (pooled)
#   data_clean/vertical_extension_network_2022.gexf
#   data_clean/vertical_extension_network_2023.gexf
#   data_clean/vertical_extension_network_2024.gexf
#   data_clean/vertical_extension_network_2025.gexf
#
# Respondent counts come from the matching data_clean/*.csv files.
#
# Outputs (written to tables/):
#   table_network_structure_horizontal.{html, png, tex, docx}
#   table_network_structure_vertical.{html, png, tex, docx}
#   table_coherence_clusters.{html, png, tex, docx}
########################################################################

pacman::p_load(igraph,
               rgexf,
               xml2,
               readr,
               dplyr,
               tidyr,
               purrr,
               tibble,
               gt)


# ---- 1. File locations -----------------------------------------------------
horizontal_gexf <- list(
  "2023"   = "data_clean/horizontial_coherence_network2023.gexf",
  "2024"   = "data_clean/horizontial_coherence_network2024.gexf",
  "2025"   = "data_clean/horizontial_coherence_network2025.gexf",
  "Pooled" = "data_clean/horizontial_coherence_network.gexf"
)
horizontal_csv <- list(
  "2023"   = "data_clean/horizontial_df2023.csv",
  "2024"   = "data_clean/horizontial_df2024.csv",
  "2025"   = "data_clean/horizontial_df2025.csv",
  "Pooled" = "data_clean/horizontial_df.csv"
)

vertical_gexf <- list(
  "2022"   = "data_clean/vertical_extension_network_2022.gexf",
  "2023"   = "data_clean/vertical_extension_network_2023.gexf",
  "2024"   = "data_clean/vertical_extension_network_2024.gexf",
  "2025"   = "data_clean/vertical_extension_network_2025.gexf",
  "Pooled" = "data_clean/vertical_extension_network.gexf"
)
vertical_csv <- list(
  "2022"   = "data_clean/vertical_extension2022.csv",
  "2023"   = "data_clean/vertical_extension2023.csv",
  "2024"   = "data_clean/vertical_extension2024.csv",
  "2025"   = "data_clean/vertical_extension2025.csv",
  "Pooled" = "data_clean/vertical_extension.csv"
)

for (f in c(unlist(horizontal_gexf), unlist(horizontal_csv),
            unlist(vertical_gexf),   unlist(vertical_csv))) {
  if (!file.exists(f)) stop("Missing input file: '", f, "'")
}


# ---- 2. GEXF loader (keeps Defence_Focused node attribute) ----------------
# Same XML parse as parse_attribute() in 6_network_analysis.R.
parse_attribute <- function(gexf_file, attr_title) {
  doc <- xml2::read_xml(gexf_file)
  ns  <- c(d = "http://www.gexf.net/1.2draft")

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

load_attitude_network <- function(gexf_file) {
  gx <- rgexf::read.gexf(gexf_file)
  g  <- rgexf::gexf.to.igraph(gx)
  V(g)$label <- V(g)$name

  defence_map <- parse_attribute(gexf_file, "Defence_Focused")
  V(g)$Defence_Focused <- defence_map[V(g)$name]
  V(g)$group <- ifelse(V(g)$Defence_Focused >= 0,
                       "Defence-Focused", "Normalization-Focused")

  E(g)$weight <- as.numeric(E(g)$weight)
  g <- igraph::delete_edges(g, E(g)[E(g)$weight <= 0])
  g
}


# ---- 3. Compute every network-structure statistic --------------------------
network_stats <- function(g, n_respondents, label) {
  ww <- E(g)$weight

  set.seed(42)
  wt <- igraph::cluster_walktrap(g, weights = ww)

  set.seed(42)
  lv <- igraph::cluster_louvain(g, weights = ww)
  louvain_k <- length(unique(igraph::membership(lv)))

  spin_k <- NA_integer_
  tryCatch({
    comps <- igraph::components(g)
    g_spin <- if (comps$no == 1) {
      g
    } else {
      igraph::induced_subgraph(
        g, V(g)[comps$membership == which.max(comps$csize)])
    }
    set.seed(42)
    sg     <- igraph::cluster_spinglass(g_spin, weights = E(g_spin)$weight)
    spin_k <- length(unique(igraph::membership(sg)))
  }, error = function(e) {
    cat("    [spinglass] failed for", label, ":", conditionMessage(e), "\n")
  })

  list(
    network               = label,
    sample_N              = n_respondents,
    n_nodes               = igraph::vcount(g),
    n_edges               = igraph::ecount(g),
    density               = igraph::edge_density(g),
    mean_degree           = mean(igraph::degree(g)),
    mean_strength         = mean(igraph::strength(g, weights = ww)),
    mean_edge_weight      = mean(ww),
    weighted_transitivity = mean(
      igraph::transitivity(g, type = "weighted",
                           weights = ww, isolates = "zero"),
      na.rm = TRUE),
    avg_local_cc          = mean(
      igraph::transitivity(g, type = "local", isolates = "zero"),
      na.rm = TRUE),
    avg_path_length       = igraph::mean_distance(
      g, weights = 1 / ww, directed = FALSE),
    diameter              = igraph::diameter(
      g, weights = 1 / ww, directed = FALSE),
    assortativity_group   = igraph::assortativity_nominal(
      g, types = as.integer(factor(V(g)$group)), directed = FALSE),
    walktrap_k            = length(unique(igraph::membership(wt))),
    louvain_k             = louvain_k,
    spinglass_k           = spin_k
  )
}

run_family <- function(gexf_map, csv_map, family_label) {
  cat("Loading", family_label, "networks...\n")
  stats_list <- list()
  for (nm in names(gexf_map)) {
    g <- load_attitude_network(gexf_map[[nm]])
    n_respondents <- nrow(readr::read_csv(csv_map[[nm]], show_col_types = FALSE))
    cat("  ", nm, ":", vcount(g), "nodes,", ecount(g), "edges,",
        n_respondents, "respondents\n")
    stats_list[[nm]] <- network_stats(g, n_respondents, nm)
  }
  stats_list
}

horizontal_stats <- run_family(horizontal_gexf, horizontal_csv, "horizontal coherence")
vertical_stats   <- run_family(vertical_gexf,   vertical_csv,   "vertical extension")


# ---- 4. Helper formatters -------------------------------------------------
fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_3   <- function(x) formatC(x, format = "f", digits = 3)
fmt_2   <- function(x) formatC(x, format = "f", digits = 2)
fmt_1   <- function(x) formatC(x, format = "f", digits = 1)

# ---- 5. Build a network-structure table (rows = stats, cols = networks) ---
STAT_ROWS <- tribble(
  ~field,                   ~label,                           ~fmt,
  "sample_N",               "Respondent sample size",         fmt_int,
  "n_nodes",                "Nodes",                          fmt_int,
  "n_edges",                "Edges",                          fmt_int,
  "density",                "Density",                        fmt_3,
  "mean_degree",            "Mean degree",                    fmt_2,
  "mean_edge_weight",       "Mean edge weight",                fmt_3,
  "weighted_transitivity",  "Weighted transitivity",           fmt_3,
  "avg_local_cc",           "Avg. local clustering coeff.",    fmt_3,
  "avg_path_length",        "Average path length",             fmt_1,
  "diameter",               "Diameter",                        fmt_1,
  "assortativity_group",    "Assortativity (Defence vs. Norm.)", fmt_3,
  "walktrap_k",             "Walktrap communities",             fmt_int,
  "louvain_k",              "Louvain communities",              fmt_int,
  "spinglass_k",            "Spinglass communities",            fmt_int
)

build_structure_table <- function(stats_list) {
  cols <- lapply(stats_list, function(s) {
    vapply(seq_len(nrow(STAT_ROWS)), function(i) {
      val <- s[[STAT_ROWS$field[i]]]
      if (is.na(val)) "-" else STAT_ROWS$fmt[[i]](val)
    }, character(1))
  })
  out <- tibble(`Network statistic` = STAT_ROWS$label)
  for (nm in names(cols)) out[[nm]] <- cols[[nm]]
  out
}

style_gt <- function(gt_tbl) {
  gt_tbl |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_options(
      table.font.size        = px(13),
      data_row.padding       = px(4),
      column_labels.padding  = px(6),
      table.border.top.style = "solid",
      table.border.top.width = px(1),
      table.border.top.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(1),
      table.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      table_body.hlines.style = "solid",
      table_body.hlines.width = px(1),
      table_body.hlines.color = "#d3d3d3"
    )
}

save_gt <- function(gt_tbl, out_dir, basename) {
  gtsave(gt_tbl, file.path(out_dir, paste0(basename, ".html")))
  if (requireNamespace("webshot2", quietly = TRUE)) {
    try(gtsave(gt_tbl, file.path(out_dir, paste0(basename, ".png"))), silent = TRUE)
  }
  try(gtsave(gt_tbl, file.path(out_dir, paste0(basename, ".tex"))), silent = TRUE)
  try(gtsave(gt_tbl, file.path(out_dir, paste0(basename, ".docx"))), silent = TRUE)
}

out_dir <- "tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tbl_horizontal <- build_structure_table(horizontal_stats)
gt_horizontal  <- tbl_horizontal |>
  gt() |>
  tab_header(title = "Horizontal coherence network structure across waves") |>
  cols_align(align = "left",   columns = `Network statistic`) |>
  cols_align(align = "center", columns = -`Network statistic`) |>
  style_gt()
save_gt(gt_horizontal, out_dir, "table_network_structure_horizontal")

tbl_vertical <- build_structure_table(vertical_stats)
gt_vertical  <- tbl_vertical |>
  gt() |>
  tab_header(title = "Vertical extension network structure across waves") |>
  cols_align(align = "left",   columns = `Network statistic`) |>
  cols_align(align = "center", columns = -`Network statistic`) |>
  style_gt()
save_gt(gt_vertical, out_dir, "table_network_structure_vertical")


# ---- 6. Defence-Focused vs. Normalization-Focused subgraph analysis -------
# Analogous to the Democrat/Republican partisan subgraphs in
# network-statistcs-anes.R: split each network by the sign of
# Defence_Focused and compare basic structural properties.

cluster_stats <- function(g, group_value, network_label) {
  nodes <- V(g)[V(g)$group == group_value]
  sg    <- igraph::induced_subgraph(g, nodes)
  sg    <- igraph::delete_edges(sg, E(sg)[E(sg)$weight <= 0])
  ww    <- E(sg)$weight

  lcc_vals <- igraph::transitivity(sg, type = "local", isolates = "zero")

  if (igraph::ecount(sg) > 0) {
    inv_w <- 1 / ww
    diam  <- igraph::diameter(sg, weights = inv_w, directed = FALSE)
    apl   <- igraph::mean_distance(sg, weights = inv_w, directed = FALSE)
  } else {
    diam <- NA_real_
    apl  <- NA_real_
  }

  tibble(
    network  = network_label,
    cluster  = group_value,
    n_nodes  = igraph::vcount(sg),
    n_edges  = igraph::ecount(sg),
    density  = igraph::edge_density(sg),
    diameter = diam,
    avg_path = apl,
    avg_lcc  = mean(lcc_vals, na.rm = TRUE)
  )
}

build_cluster_rows <- function(gexf_map, family_label) {
  purrr::map_dfr(names(gexf_map), function(nm) {
    g <- load_attitude_network(gexf_map[[nm]])
    dplyr::bind_rows(
      cluster_stats(g, "Defence-Focused",       paste(family_label, nm)),
      cluster_stats(g, "Normalization-Focused",  paste(family_label, nm))
    ) |>
      mutate(family = family_label, wave = nm, .before = 1)
  })
}

cluster_df <- dplyr::bind_rows(
  build_cluster_rows(horizontal_gexf, "Horizontal coherence"),
  build_cluster_rows(vertical_gexf,   "Vertical extension")
) |>
  select(-network)

tbl_clusters <- cluster_df |>
  transmute(
    Network = family,
    Wave    = wave,
    Cluster = cluster,
    Nodes   = fmt_int(n_nodes),
    Edges   = fmt_int(n_edges),
    Density = fmt_3(density),
    Diameter = ifelse(is.na(diameter), "-", fmt_1(diameter)),
    `Average path length` = ifelse(is.na(avg_path), "-", fmt_1(avg_path)),
    `Avg. local clustering coeff.` = fmt_3(avg_lcc)
  )

gt_clusters <- tbl_clusters |>
  gt(groupname_col = "Network") |>
  tab_header(title = "Defence-Focused vs. Normalization-Focused subgraph comparison") |>
  cols_align(align = "left",   columns = c(Wave, Cluster)) |>
  cols_align(align = "center", columns = -c(Wave, Cluster)) |>
  style_gt()
save_gt(gt_clusters, out_dir, "table_coherence_clusters")


# ---- 7. Echo final values to the console ----------------------------------
cat("\n--- Horizontal coherence network structure ---\n")
print(tbl_horizontal)

cat("\n--- Vertical extension network structure ---\n")
print(tbl_vertical)

cat("\n--- Defence-Focused vs. Normalization-Focused subgraph comparison ---\n")
print(tbl_clusters)

message("\nSaved to ", normalizePath(out_dir), ":")
message("  table_network_structure_horizontal.{html,png,tex,docx}")
message("  table_network_structure_vertical.{html,png,tex,docx}")
message("  table_coherence_clusters.{html,png,tex,docx}")

