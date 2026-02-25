###############################################################################
# IDS 570 (Freedom Option) — Text as Data Exploration
# Corpus: 2024 US election discourse (pre-election vs post-election)
#
# What I’m trying to do:
# - I built a small corpus (n = 20) of news/opinion/research write-ups about the
#   2024 election, split into "pre" (before Election Day) and "post" (after).
# - The goal is exploratory: map how language shifts from uncertainty/anticipation
#   to explanation/diagnosis after results are known.
#
# Methods (mirroring the structured prompt):
# 1) TF–IDF (lexical distinctiveness)
# 2) Pearson correlation similarity between documents
# 3) Syntactic complexity (two texts, chosen from results of 1 & 2)
#
# Folder structure (relative to project root):
# - metadata.csv
# - ids570_corpus_2024/
#     - pre/   (10 .txt)
#     - post/  (10 .txt)
#
# Notes on cleaning choices:
# - These are modern English texts, so there’s no "long S" normalization.
# - I’m not doing aggressive spelling normalization; I want to preserve each outlet’s
#   style. The only “normalization” is standard text cleaning at tokenization.
###############################################################################

# -----------------------------
# 0) Setup
# -----------------------------

# Packages (install if needed)
pkgs <- c(
  "tidyverse",
  "readr",
  "readtext",
  "quanteda",
  "quanteda.textstats",
  "quanteda.textplots",
  "stringr",
  "lubridate",
  "ggplot2",
  "udpipe"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(readr)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stringr)
library(lubridate)
library(ggplot2)
library(udpipe)

set.seed(570)

# Helpful: keep plots readable
theme_set(theme_minimal())

# -----------------------------
# 1) Load metadata + texts
# -----------------------------

# Read metadata
# Your CSV may look “comma-less” in some viewers if it’s tab-separated.
# This tries comma first; if it fails, it falls back to tab.
meta_path <- "metadata.csv"

meta_try_csv <- tryCatch(
  read_csv(meta_path, show_col_types = FALSE),
  error = function(e) NULL
)

meta <- if (!is.null(meta_try_csv) && ncol(meta_try_csv) >= 3) {
  meta_try_csv
} else {
  read_tsv(meta_path, show_col_types = FALSE)
}

# Standardize column names (in case of capitalization differences)
meta <- meta %>%
  rename_with(~ str_to_lower(.x))

# Expected columns:
# doc_id | category | outlet | date
stopifnot(all(c("doc_id", "category", "outlet", "date") %in% names(meta)))

# Parse dates where possible (leave N/A as NA)
# I’m forgiving here because a couple entries may be "N/A" or blank.
meta <- meta %>%
  mutate(
    date = na_if(date, "N/A"),
    date = na_if(date, ""),
    date_parsed = suppressWarnings(mdy(date)),
    # If mdy fails, try ymd
    date_parsed = if_else(is.na(date_parsed), suppressWarnings(ymd(date)), date_parsed),
    # Clean category labels
    category = str_to_lower(category),
    category = if_else(category %in% c("pre", "post"), category, category),
    # Keep outlet tidy
    outlet = str_squish(outlet)
  )

# Read in text files (pre + post folders)
pre_files  <- list.files("ids570_corpus_2024/pre",  pattern = "\\.txt$", full.names = TRUE)
post_files <- list.files("ids570_corpus_2024/post", pattern = "\\.txt$", full.names = TRUE)

all_files <- c(pre_files, post_files)
if (length(all_files) != 20) {
  message("Heads up: I expected 20 files, but found ", length(all_files), ".")
}

texts <- readtext(all_files)

# Derive doc_id from filenames (must match metadata's doc_id)
# Example file: ".../pre_01_abc_fewer_polls.txt" -> doc_id = "pre_01_abc_fewer_polls.txt"
texts <- texts %>%
  mutate(doc_id = basename(doc_id))

# Join metadata
docs <- texts %>%
  left_join(meta, by = "doc_id")

# Quick checks
if (any(is.na(docs$category))) {
  warning("Some documents did not match metadata.csv by doc_id. ",
          "Check that filenames in metadata.csv exactly match the .txt filenames.")
}

# Build quanteda corpus with docvars
corp <- corpus(docs, text_field = "text")
docvars(corp, "category") <- docs$category
docvars(corp, "outlet")   <- docs$outlet
docvars(corp, "date")     <- docs$date_parsed

summary(corp)

# -----------------------------
# 2) Tokenization + DFM
# -----------------------------

# Cleaning choices (light-touch):
# - Remove punctuation, symbols, numbers
# - Lowercase
# - Remove stopwords
# - Remove very short tokens
# - Optional: remove a small set of common election boilerplate words (kept minimal)
custom_stop <- c(
  # super generic terms that can drown out signal
  "said", "say", "says", "one", "two", "also", "would"
)

toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = c(stopwords("en"), custom_stop)) %>%
  tokens_keep(min_nchar = 3)

# Document-feature matrix
dfm_counts <- dfm(toks)

# Optional trimming of very rare terms for stability in Pearson correlations
# (this is especially useful with mixed outlets/templates)
dfm_trimmed <- dfm_trim(dfm_counts, min_termfreq = 5)

# -----------------------------
# 3) Approach 1 — TF–IDF (lexical distinctiveness)
# -----------------------------

# TF-IDF weights
dfm_tfidf <- dfm_tfidf(dfm_counts)

# Get top terms per document
top_tfidf_per_doc <- function(dfm_in, top_n = 15) {
  docnames_vec <- docnames(dfm_in)
  out <- map_dfr(docnames_vec, function(d) {
    vec <- dfm_in[d, ]
    top <- topfeatures(vec, n = top_n)
    tibble(
      doc_id = d,
      term = names(top),
      tfidf = as.numeric(top)
    )
  })
  out
}

tfidf_tbl <- top_tfidf_per_doc(dfm_tfidf, top_n = 15) %>%
  left_join(meta, by = "doc_id") %>%
  arrange(category, doc_id, desc(tfidf))

# Save table for write-up
write_csv(tfidf_tbl, "output_tfidf_top_terms.csv")

# Quick display: top terms for first few docs
tfidf_tbl %>% group_by(doc_id) %>% slice_head(n = 10) %>% ungroup() %>% print(n = 50)

# Optional: show a compact plot for a few selected documents
# (I’m keeping this minimal; tables are often clearer for TF–IDF.)
# If you want, pick a subset of docs and plot their top tf-idf terms.

# -----------------------------
# 4) Approach 2 — Pearson correlation similarity + heatmap
# -----------------------------

# Pairwise Pearson correlations between documents
sim_r <- textstat_simil(dfm_trimmed, margin = "documents", method = "correlation")
r_mat <- as.matrix(sim_r)
r_mat <- round(r_mat, 3)

# Convert to long format for ggplot heatmap
heat_df <- as.data.frame(r_mat) %>%
  rownames_to_column("doc_i") %>%
  pivot_longer(-doc_i, names_to = "doc_j", values_to = "r")

# Save heatmap plot
p_heat <- ggplot(heat_df, aes(x = doc_j, y = doc_i, fill = r)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Pearson Correlation Between Documents (DFM trimmed: min_termfreq = 5)",
    x = NULL, y = NULL, fill = "r"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank()
  )

ggsave("output_similarity_heatmap.png", p_heat, width = 10, height = 10, dpi = 300)

# Identify most similar + least similar pairs (excluding diagonal)
pairs_tbl <- heat_df %>%
  filter(doc_i != doc_j) %>%
  mutate(pair = map2_chr(doc_i, doc_j, ~ paste(sort(c(.x, .y)), collapse = " || "))) %>%
  group_by(pair) %>%
  summarize(
    doc_a = first(doc_i),
    doc_b = first(doc_j),
    r = first(r),
    .groups = "drop"
  )

most_similar <- pairs_tbl %>% arrange(desc(r)) %>% slice_head(n = 5)
least_similar <- pairs_tbl %>% arrange(r) %>% slice_head(n = 5)

write_csv(most_similar, "output_most_similar_pairs.csv")
write_csv(least_similar, "output_least_similar_pairs.csv")

most_similar
least_similar

# -----------------------------
# 5) Choose two texts for syntactic complexity
# -----------------------------

# My selection logic:
# - One text from the pre-election set and one from the post-election set
# - Preferably among extreme similarity/distance cases so the syntactic analysis
#   has something to “bite” on.
#
# Here I pick:
# - Pre: the pre-election doc involved in the most similar pair (first row)
# - Post: the post-election doc involved in the least similar pair (first row)
#
# If this picks two from same category, I fall back to selecting a top TF–IDF doc
# from each category.

pick_syntax_docs <- function(most_sim, least_sim, meta_df) {
  # Candidate from most similar
  a1 <- most_sim$doc_a[1]
  b1 <- most_sim$doc_b[1]
  # Candidate from least similar
  a2 <- least_sim$doc_a[1]
  b2 <- least_sim$doc_b[1]
  
  # Gather candidates
  candidates <- unique(c(a1, b1, a2, b2))
  
  meta_sub <- meta_df %>% filter(doc_id %in% candidates)
  
  # Try to pick one pre and one post from candidates
  pre_doc <- meta_sub %>% filter(category == "pre") %>% slice_head(n = 1) %>% pull(doc_id)
  post_doc <- meta_sub %>% filter(category == "post") %>% slice_head(n = 1) %>% pull(doc_id)
  
  # If missing, fallback: pick most distinctive TF–IDF doc from each category
  if (length(pre_doc) == 0 || is.na(pre_doc)) {
    pre_doc <- tfidf_tbl %>%
      filter(category == "pre") %>%
      group_by(doc_id) %>%
      summarize(max_tfidf = max(tfidf), .groups = "drop") %>%
      arrange(desc(max_tfidf)) %>%
      slice(1) %>%
      pull(doc_id)
  }
  
  if (length(post_doc) == 0 || is.na(post_doc)) {
    post_doc <- tfidf_tbl %>%
      filter(category == "post") %>%
      group_by(doc_id) %>%
      summarize(max_tfidf = max(tfidf), .groups = "drop") %>%
      arrange(desc(max_tfidf)) %>%
      slice(1) %>%
      pull(doc_id)
  }
  
  c(pre_doc = pre_doc, post_doc = post_doc)
}

syntax_choice <- pick_syntax_docs(most_similar, least_similar, meta)
syntax_choice

# Extract raw text for those two docs
get_doc_text <- function(d_id, docs_df) {
  docs_df %>% filter(doc_id == d_id) %>% pull(text) %>% first()
}

pre_text  <- get_doc_text(syntax_choice["pre_doc"], docs)
post_text <- get_doc_text(syntax_choice["post_doc"], docs)

# Save chosen docs for transparency
write_lines(pre_text,  "output_syntax_doc_pre.txt")
write_lines(post_text, "output_syntax_doc_post.txt")

# -----------------------------
# 6) Approach 3 — Syntactic Complexity (Week 05-style)
# -----------------------------

# I’m using udpipe for dependency parsing. This can be slow.
# The first time you run this, you’ll download a model.
# If it errors, it’s usually a memory/timeout thing; restarting R can help.

model_file <- "udpipe_model_english-ewt-2.5-191206.udpipe"
if (!file.exists(model_file)) {
  message("Downloading udpipe English model...")
  dl <- udpipe_download_model(language = "english-ewt")
  file.copy(dl$file_model, model_file)
}

ud_model <- udpipe_load_model(model_file)

# Helper: parse and compute syntactic complexity measures
# Measures (adapted for a practical implementation):
# - MLS: mean length of sentence (tokens per sentence)
# - C/S: clauses per sentence (approx by counting verbs as clause heads)
# - DC/C and DC/S: dependent clauses per clause / per sentence
# - Coord/C and Coord/S: coordination (cc) per clause / per sentence
# - CN/C and CN/S: complex nominals per clause / per sentence (approx via noun compounds + nominal modifiers)
#
# Note: exact “Week 05 framework” definitions vary; I’m using widely-used operationalizations
# that can be reproduced from dependency parses. I’ll explain this in my write-up.

compute_syntactic_profile <- function(text, model, doc_label = "doc") {
  anno <- udpipe_annotate(model, x = text)
  anno <- as.data.frame(anno)
  
  # Basic cleaning: remove empty tokens
  anno <- anno %>% filter(!is.na(token), token != "")
  
  # Sentence id
  # udpipe uses sentence_id; keep it stable
  # Tokens per sentence
  sent_stats <- anno %>%
    group_by(sentence_id) %>%
    summarize(
      tokens = n(),
      # Clause approximation: count verbs (upos == VERB or AUX) as clause heads
      clauses = sum(anno$upos[anno$sentence_id == first(sentence_id)] %in% c("VERB", "AUX")),
      # Dependent clause approximation: mark tokens with dep_rel == "advcl" or "ccomp" or "xcomp" or "acl"
      dep_clauses = sum(anno$dep_rel[anno$sentence_id == first(sentence_id)] %in% c("advcl", "ccomp", "xcomp", "acl", "relcl")),
      # Coordination: dependency relation "cc" (coordinating conjunction)
      coord = sum(anno$dep_rel[anno$sentence_id == first(sentence_id)] %in% c("cc")),
      # Complex nominals approximation:
      # - compound, amod (adj mod), nmod (nominal mod), appos
      complex_nominals = sum(anno$dep_rel[anno$sentence_id == first(sentence_id)] %in% c("compound", "amod", "nmod", "appos")),
      .groups = "drop"
    )
  
  # Compute document-level means / ratios
  MLS <- mean(sent_stats$tokens, na.rm = TRUE)
  
  # Avoid divide-by-zero in short texts
  total_sent <- nrow(sent_stats)
  total_clauses <- sum(sent_stats$clauses, na.rm = TRUE)
  if (total_clauses == 0) total_clauses <- NA_real_
  
  C_per_S <- mean(sent_stats$clauses, na.rm = TRUE)
  
  DC_per_C <- (sum(sent_stats$dep_clauses, na.rm = TRUE) / total_clauses)
  DC_per_S <- (sum(sent_stats$dep_clauses, na.rm = TRUE) / total_sent)
  
  Coord_per_C <- (sum(sent_stats$coord, na.rm = TRUE) / total_clauses)
  Coord_per_S <- (sum(sent_stats$coord, na.rm = TRUE) / total_sent)
  
  CN_per_C <- (sum(sent_stats$complex_nominals, na.rm = TRUE) / total_clauses)
  CN_per_S <- (sum(sent_stats$complex_nominals, na.rm = TRUE) / total_sent)
  
  # Pull example sentences:
  # Choose the sentence with highest dependent clauses as an illustrative "complex" example
  # and one with low dependent clauses as a contrast option.
  example_hi <- sent_stats %>% arrange(desc(dep_clauses), desc(tokens)) %>% slice(1) %>% pull(sentence_id)
  example_lo <- sent_stats %>% arrange(dep_clauses, tokens) %>% slice(1) %>% pull(sentence_id)
  
  reconstruct_sentence <- function(sent_id) {
    anno %>%
      filter(sentence_id == sent_id) %>%
      arrange(token_id) %>%
      pull(token) %>%
      paste(collapse = " ")
  }
  
  ex1 <- reconstruct_sentence(example_hi)
  ex2 <- reconstruct_sentence(example_lo)
  
  profile <- tibble(
    doc = doc_label,
    MLS = MLS,
    `Clauses_per_Sentence` = C_per_S,
    `Dep_Clauses_per_Clause` = DC_per_C,
    `Dep_Clauses_per_Sentence` = DC_per_S,
    `Coord_per_Clause` = Coord_per_C,
    `Coord_per_Sentence` = Coord_per_S,
    `Complex_Nominals_per_Clause` = CN_per_C,
    `Complex_Nominals_per_Sentence` = CN_per_S,
    example_sentence_high_dep = ex1,
    example_sentence_low_dep = ex2
  )
  
  list(profile = profile, sentence_stats = sent_stats, annotated = anno)
}

# Compute profiles for selected pre/post docs
pre_label  <- paste0(syntax_choice["pre_doc"],  " (pre)")
post_label <- paste0(syntax_choice["post_doc"], " (post)")

pre_syn  <- compute_syntactic_profile(pre_text,  ud_model, doc_label = pre_label)
post_syn <- compute_syntactic_profile(post_text, ud_model, doc_label = post_label)

syn_table <- bind_rows(pre_syn$profile, post_syn$profile)

# Save outputs
write_csv(syn_table, "output_syntactic_profile_table.csv")
write_csv(pre_syn$sentence_stats %>% mutate(doc = pre_label),  "output_sentence_stats_pre.csv")
write_csv(post_syn$sentence_stats %>% mutate(doc = post_label), "output_sentence_stats_post.csv")

syn_table

# -----------------------------
# 7) A couple of simple, helpful visuals for the report
# -----------------------------

# 7a) Document length distribution (tokens after cleaning)
doc_lengths <- tibble(
  doc_id = docnames(dfm_counts),
  tokens = ntoken(dfm_counts)
) %>%
  left_join(meta, by = "doc_id")

p_len <- ggplot(doc_lengths, aes(x = category, y = tokens)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Document Token Counts (after tokenization + stopword removal)",
    x = NULL, y = "Tokens"
  )

ggsave("output_doc_length_boxplot.png", p_len, width = 7, height = 5, dpi = 300)

# 7b) Syntactic profile bar chart (optional quick visual)
syn_long <- syn_table %>%
  select(doc, MLS, Clauses_per_Sentence, Dep_Clauses_per_Sentence,
         Coord_per_Sentence, Complex_Nominals_per_Sentence) %>%
  pivot_longer(-doc, names_to = "measure", values_to = "value")

p_syn <- ggplot(syn_long, aes(x = measure, y = value, fill = doc)) +
  geom_col(position = "dodge") +
  labs(
    title = "Syntactic Complexity Measures (two selected texts)",
    x = NULL, y = "Value"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("output_syntax_measures_bar.png", p_syn, width = 10, height = 5, dpi = 300)

# -----------------------------
# 8) Interpretive helpers (so I don’t forget what to write about)
# -----------------------------
# I’m saving a small “notes” file with the key outputs I’ll reference in my write-up:
notes <- c(
  "Key outputs saved:",
  "- output_tfidf_top_terms.csv (top TF–IDF terms per document)",
  "- output_similarity_heatmap.png (Pearson correlation heatmap)",
  "- output_most_similar_pairs.csv / output_least_similar_pairs.csv",
  "- output_syntactic_profile_table.csv (Week 05-style syntax measures)",
  "- output_syntax_doc_pre.txt / output_syntax_doc_post.txt (the two texts I parsed)",
  "- output_doc_length_boxplot.png (token count distribution)",
  "- output_syntax_measures_bar.png (syntax measures visual)"
)

write_lines(notes, "output_run_notes.txt")


# -----------------------------
# 9) Extra analysis to make this more informative
# -----------------------------
# At this point, TF–IDF + Pearson + 2-doc syntax gives me a good skeleton, but it still
# doesn’t fully answer what I actually care about: what *changes* between pre- and post-
# election discourse in a way that I can explain clearly in writing.
#
# So I’m adding three lightweight analyses that stay realistic for a 20-text corpus:
#   (A) Battleground state mention density (pre vs post)
#   (B) A simple "forecasting vs explanation" lexical index (pre vs post)
#   (C) Outlet vs category decomposition (how much of the pattern is just outlet effects?)
#
# Everything below saves:
# - per-document tables (so I can cite specific examples)
# - category summaries (so I can cite clean stats)
# - graphs (so I can drop them straight into the paper)

# -----------------------------
# 9A) Battleground state mention density (pre vs post)
# -----------------------------
# I’m explicitly counting mentions of key battleground states and normalizing by
# document length. This lets me see whether pre-election writing is more geographic /
# state-targeted than post-election writing.

battleground_states <- c(
  "arizona", "georgia", "michigan", "nevada", "north carolina",
  "pennsylvania", "wisconsin"
)

expanded_states <- c(
  battleground_states,
  "florida", "texas", "ohio", "minnesota", "new hampshire", "virginia"
)

# Helper: count regex matches in raw text (case-insensitive), including multi-word states
count_state_mentions <- function(text, state_vec) {
  counts <- sapply(state_vec, function(st) {
    pattern <- paste0("\\b", gsub(" ", "\\\\s+", st), "\\b")
    str_count(str_to_lower(text), regex(pattern, ignore_case = TRUE))
  })
  counts <- as.integer(counts)
  names(counts) <- state_vec
  counts
}

# ---- per-document densities (what I’ll cite / compare)
state_counts_tbl <- docs %>%
  transmute(
    doc_id,
    category,
    outlet,
    date_parsed,
    text_raw = text
  ) %>%
  mutate(
    n_words = str_count(text_raw, "\\S+"),
    battleground_mentions = map_dbl(text_raw, ~ sum(count_state_mentions(.x, battleground_states))),
    expanded_mentions = map_dbl(text_raw, ~ sum(count_state_mentions(.x, expanded_states))),
    battleground_per_1k = ifelse(n_words == 0, NA_real_, 1000 * battleground_mentions / n_words),
    expanded_per_1k = ifelse(n_words == 0, NA_real_, 1000 * expanded_mentions / n_words)
  )

# ---- summary stats I can cite in writing
state_summary <- state_counts_tbl %>%
  group_by(category) %>%
  summarize(
    n_docs = n(),
    mean_words = mean(n_words, na.rm = TRUE),
    median_words = median(n_words, na.rm = TRUE),
    mean_battleground_mentions = mean(battleground_mentions, na.rm = TRUE),
    median_battleground_mentions = median(battleground_mentions, na.rm = TRUE),
    mean_battleground_per_1k = mean(battleground_per_1k, na.rm = TRUE),
    median_battleground_per_1k = median(battleground_per_1k, na.rm = TRUE),
    mean_expanded_per_1k = mean(expanded_per_1k, na.rm = TRUE),
    median_expanded_per_1k = median(expanded_per_1k, na.rm = TRUE),
    .groups = "drop"
  )

print(state_summary)
write_csv(state_summary, "output_state_density_summary_by_category.csv")
write_csv(state_counts_tbl, "output_state_density_by_doc.csv")

# Graph 1: density distribution (pre vs post)
p_state_density <- ggplot(state_counts_tbl, aes(x = category, y = battleground_per_1k)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.7) +
  labs(
    title = "Battleground State Mention Density (per 1,000 words)",
    x = NULL,
    y = "Mentions per 1,000 words"
  )

ggsave("output_battleground_density_boxplot.png", p_state_density, width = 7, height = 5, dpi = 300)

# ---- state-level breakdown (share + absolute counts) with a FIXED share computation
state_long <- map_dfr(seq_len(nrow(docs)), function(i) {
  counts <- count_state_mentions(docs$text[i], battleground_states)
  stopifnot(length(counts) == length(battleground_states))
  
  tibble(
    doc_id = docs$doc_id[i],
    category = docs$category[i],
    outlet = docs$outlet[i],
    state = names(counts),
    mentions = as.integer(counts)
  )
}) %>%
  group_by(category, state) %>%
  summarize(
    total_mentions = sum(mentions, na.rm = TRUE),
    docs_with_mention = sum(mentions > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_mentions = as.numeric(total_mentions)) %>%
  group_by(category) %>%
  mutate(
    denom = sum(total_mentions, na.rm = TRUE),
    share = if (denom[1] == 0) 0 else total_mentions / denom[1]
  ) %>%
  ungroup() %>%
  select(-denom)

# Sanity check (should be ~1 per category)
print(
  state_long %>%
    group_by(category) %>%
    summarize(
      sum_total_mentions = sum(total_mentions),
      sum_share = sum(share),
      shares = paste(round(share, 3), collapse = ", "),
      .groups = "drop"
    )
)

print(state_long %>% arrange(category, desc(total_mentions)))
write_csv(state_long, "output_battleground_state_breakdown.csv")

# Graph 2a: share within category (this is the one you already wanted)
# Order states based on POST share so the bars are comparable across categories
state_order <- state_long %>%
  filter(category == "post") %>%
  arrange(share) %>%
  pull(state)

state_long$state <- factor(state_long$state, levels = state_order)

p_state_share <- ggplot(state_long, aes(x = state, y = share, fill = category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Which Battleground States Dominate Mentions? (Share Within Category)",
    x = NULL,
    y = "Share of total battleground mentions"
  )

ggsave("output_battleground_state_share.png", p_state_share, width = 9, height = 6, dpi = 300)

# Graph 2b: absolute mentions + docs coverage (I like this because it prevents share-only misreads)
p_state_abs <- ggplot(state_long, aes(x = state, y = total_mentions, fill = category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Battleground State Mentions (Absolute Counts) + Coverage",
    subtitle = "Total mentions (bars). Coverage is saved in the CSV as docs_with_mention.",
    x = NULL,
    y = "Total mentions"
  )

ggsave("output_battleground_state_total_mentions.png", p_state_abs, width = 9, height = 6, dpi = 300)

# -----------------------------
# 9B) Forecasting vs Explanation Lexical Index (pre vs post)
# -----------------------------
# This is a simple dictionary-based measure. The point isn’t to pretend it’s perfect—
# it’s to create an interpretable signal that matches my main research question.

forecast_terms <- c(
  "poll", "polls", "polling", "survey", "surveys", "forecast", "forecasting",
  "model", "models", "probability", "odds", "margin", "margins",
  "prediction", "predict", "estimate", "estimates", "error", "errors"
)

explanation_terms <- c(
  "because", "since", "due", "therefore", "thus", "why",
  "explain", "explained", "reason", "reasons", "cause", "causes",
  "result", "results", "led", "lead", "driven",
  "turnout", "coalition", "underestimated", "overestimated", "shift", "shifts"
)

dfm_all <- dfm_counts

feature_sum <- function(dfm_obj, features) {
  feats_present <- intersect(features, featnames(dfm_obj))
  if (length(feats_present) == 0) return(rep(0, ndoc(dfm_obj)))
  as.numeric(rowSums(dfm_select(dfm_obj, pattern = feats_present)))
}

forecast_ct <- feature_sum(dfm_all, forecast_terms)
explain_ct  <- feature_sum(dfm_all, explanation_terms)
doc_tok_ct  <- ntoken(dfm_all)

lex_index_tbl <- tibble(
  doc_id = docnames(dfm_all),
  category = docvars(corp, "category"),
  outlet = docvars(corp, "outlet"),
  tokens = doc_tok_ct,
  forecast_terms = forecast_ct,
  explanation_terms = explain_ct,
  forecast_per_1k = ifelse(tokens == 0, NA_real_, 1000 * forecast_terms / tokens),
  explanation_per_1k = ifelse(tokens == 0, NA_real_, 1000 * explanation_terms / tokens),
  explanation_minus_forecast = explanation_per_1k - forecast_per_1k
)

lex_summary <- lex_index_tbl %>%
  group_by(category) %>%
  summarize(
    n_docs = n(),
    mean_forecast_per_1k = mean(forecast_per_1k, na.rm = TRUE),
    median_forecast_per_1k = median(forecast_per_1k, na.rm = TRUE),
    mean_explanation_per_1k = mean(explanation_per_1k, na.rm = TRUE),
    median_explanation_per_1k = median(explanation_per_1k, na.rm = TRUE),
    mean_diff = mean(explanation_minus_forecast, na.rm = TRUE),
    median_diff = median(explanation_minus_forecast, na.rm = TRUE),
    .groups = "drop"
  )

print(lex_summary)
write_csv(lex_index_tbl, "output_lexical_index_by_doc.csv")
write_csv(lex_summary, "output_lexical_index_summary_by_category.csv")

# Graph 3: Forecast vs Explanation scatter (with a simple 45-degree reference line)
p_fx_scatter <- ggplot(lex_index_tbl, aes(x = forecast_per_1k, y = explanation_per_1k)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(shape = category), size = 3, alpha = 0.85) +
  labs(
    title = "Forecasting vs Explanation Language (per 1,000 tokens)",
    subtitle = "Points above the dashed line use more explanation than forecasting language.",
    x = "Forecasting / polling terms per 1,000 tokens",
    y = "Explanation / causal terms per 1,000 tokens"
  )

ggsave("output_forecast_vs_explanation_scatter.png", p_fx_scatter, width = 9, height = 6, dpi = 300)

# Graph 4: Net shift distribution (explanation - forecast)
p_fx_diff <- ggplot(lex_index_tbl, aes(x = category, y = explanation_minus_forecast)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.7) +
  labs(
    title = "Net Shift Toward Explanation (Explanation minus Forecasting)",
    x = NULL,
    y = "Per-1,000 token difference"
  )

ggsave("output_explanation_minus_forecast_boxplot.png", p_fx_diff, width = 7, height = 5, dpi = 300)

# -----------------------------
# 9C) Outlet vs Category: am I just re-discovering outlet style?
# -----------------------------
# I want to know whether the pre/post differences I’m seeing are consistent within outlets
# (where I have repeats), or whether they’re mostly outlet/template effects.

outlet_counts <- lex_index_tbl %>%
  count(outlet, sort = TRUE)

print(outlet_counts)

lex_by_outlet <- lex_index_tbl %>%
  inner_join(outlet_counts %>% filter(n >= 2), by = "outlet") %>%
  group_by(outlet, category) %>%
  summarize(
    n_docs = n(),
    mean_diff = mean(explanation_minus_forecast, na.rm = TRUE),
    mean_forecast_per_1k = mean(forecast_per_1k, na.rm = TRUE),
    mean_explanation_per_1k = mean(explanation_per_1k, na.rm = TRUE),
    .groups = "drop"
  )

print(lex_by_outlet)
write_csv(lex_by_outlet, "output_lexical_index_by_outlet_category.csv")

# Graph 5: net explanation shift by outlet (only outlets with repeats)
p_outlet <- ggplot(lex_by_outlet, aes(x = outlet, y = mean_diff, fill = category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Net Shift Toward Explanation by Outlet (Outlets with 2+ Documents)",
    x = NULL,
    y = "Mean (explanation - forecast) per 1,000 tokens"
  )

ggsave("output_outlet_net_shift.png", p_outlet, width = 9, height = 6, dpi = 300)

# -----------------------------
# 9D) Save updated notes for the paper
# -----------------------------
extra_notes <- c(
  "",
  "Extra outputs added (Section 9):",
  "- output_state_density_summary_by_category.csv",
  "- output_state_density_by_doc.csv",
  "- output_battleground_density_boxplot.png",
  "- output_battleground_state_breakdown.csv",
  "- output_battleground_state_share.png",
  "- output_battleground_state_total_mentions.png",
  "- output_lexical_index_by_doc.csv",
  "- output_lexical_index_summary_by_category.csv",
  "- output_forecast_vs_explanation_scatter.png",
  "- output_explanation_minus_forecast_boxplot.png",
  "- output_lexical_index_by_outlet_category.csv",
  "- output_outlet_net_shift.png"
)

write_lines(c(notes, extra_notes), "output_run_notes.txt")

message("Section 9 added. Extra analyses + plots saved.")
message("Done. Outputs written to project directory.")