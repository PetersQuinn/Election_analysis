# From Uncertainty to Explanation

### A Text-as-Data Exploration of Pre- and Post-Election Discourse in 2024

## Overview

This project analyzes a small corpus of 20 political texts surrounding the 2024 U.S. presidential election. The corpus is divided into:

* **10 pre-election articles** (published before Election Day)
* **10 post-election articles** (published after results were known)

The central research question is:

> **How does political discourse shift from anticipatory uncertainty before an election to causal explanation after the results are known?**

Rather than comparing ideological positions, this project focuses on **temporal framing** — how language changes before vs. after electoral uncertainty resolves.

This is an exploratory NLP project completed under the “Freedom Option” assignment format in IDS 570 (Text as Data).

---

## Corpus Construction

* All documents are saved as individual `.txt` files.
* Metadata is stored in `metadata.csv`.
* Folder structure:

```
metadata.csv
ids570_corpus_2024/
    pre/
    post/
solutionTotal.R
```

### Sources

Texts were selected from center and left-leaning outlets and research institutions, including:

* ABC News / FiveThirtyEight
* The Guardian
* Gallup
* Brookings
* PRRI
* Politico
* AP News
* Brennan Center
* Northwestern IPR
* American Progress
* Independent Center

The goal was not partisan comparison, but **pre- vs post-election framing**.

---

## Methods

This project uses a multi-method exploratory pipeline:

### 1️⃣ TF–IDF (Lexical Distinctiveness)

* Identifies distinctive vocabulary within each document.
* Helps detect whether pre-election texts emphasize uncertainty and polling, while post-election texts emphasize explanation and evaluation.

Outputs:

* `output_tfidf_top_terms.csv`

---

### 2️⃣ Pearson Correlation Similarity

* Documents are represented as vectors in vocabulary space.
* Pairwise Pearson correlations are computed.
* Heatmap visualizes clustering patterns.

Outputs:

* `output_similarity_heatmap.png`
* `output_most_similar_pairs.csv`
* `output_least_similar_pairs.csv`

---

### 3️⃣ Syntactic Complexity (Dependency Parsing)

Using `udpipe`, two texts (one pre, one post) are analyzed for:

* Mean Length of Sentence (MLS)
* Clauses per sentence
* Dependent clauses per clause / sentence
* Coordination
* Complex nominals

This tests whether post-election discourse becomes more explanatory and structurally dense.

Outputs:

* `output_syntactic_profile_table.csv`
* `output_syntax_measures_bar.png`
* Sentence-level stats for transparency

---

## Additional Analyses (Section 9)

To move beyond general lexical patterns, three targeted analyses were added.

---

### A) Battleground State Mention Density

Counts mentions of major battleground states (AZ, GA, MI, NV, NC, PA, WI), normalized per 1,000 words.

This tests whether:

* Pre-election writing is more geographically targeted.
* Post-election writing becomes less state-specific and more national/coalitional.

Outputs:

* `output_state_density_summary_by_category.csv`
* `output_battleground_density_boxplot.png`
* `output_battleground_state_share.png`
* `output_battleground_state_total_mentions.png`

---

### B) Forecasting vs. Explanation Lexical Index

Constructed a simple dictionary-based index:

* **Forecasting language**: poll, forecast, model, probability, margin, error, etc.
* **Explanation language**: because, reason, cause, turnout, coalition, underestimated, etc.

For each document:

* Forecast terms per 1,000 tokens
* Explanation terms per 1,000 tokens
* Net difference (explanation − forecast)

This directly quantifies the shift from anticipation to explanation.

Outputs:

* `output_lexical_index_summary_by_category.csv`
* `output_forecast_vs_explanation_scatter.png`
* `output_explanation_minus_forecast_boxplot.png`

---

### C) Outlet vs. Category Decomposition

Tests whether observed patterns are simply outlet style or genuinely pre/post shifts.

Only outlets with multiple documents are included.

Outputs:

* `output_lexical_index_by_outlet_category.csv`
* `output_outlet_net_shift.png`

---

## Key Findings (High-Level)

* **Pre-election discourse** tends to emphasize polling, forecasts, margins, and conditional uncertainty.
* **Post-election discourse** shifts toward evaluation, explanation, turnout analysis, and causal framing.
* Battleground mentions are more concentrated in pre-election texts.
* Lexical index confirms a measurable shift from forecasting language toward explanatory language after results are known.
* Outlet-level checks suggest that the temporal shift is not solely an outlet artifact.

---

## Reproducibility

To reproduce:

1. Clone the repository.
2. Ensure `metadata.csv` and `ids570_corpus_2024/` folder are present.
3. Open the project in RStudio.
4. Run:

```r
source("solutionTotal.R")
```

All outputs (CSV tables + PNG figures) will be written to the project directory.

Required packages include:

* tidyverse
* readtext
* quanteda
* quanteda.textstats
* stringr
* lubridate
* udpipe
* ggplot2

---

## Limitations

* Small corpus (n = 20)
* Restricted ideological scope
* Dictionary-based lexical index is heuristic, not exhaustive
* Syntactic measures approximate clause structure via dependency parsing

This is an exploratory project designed to generate interpretable patterns rather than causal claims.

---

## Author

Quinn Peters
IDS 570 — Text as Data
Duke University
