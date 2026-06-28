##Mapping the individual studies to reviews 
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

phase_table <- read_excel("Phases_Overview_final.xlsx", sheet = "Main phases")
study_table <- read_excel("all_primary_studies271225.xlsx", sheet = "Sheet1")

make_join_id <- function(x) {
  x %>%
    str_to_lower() %>%
    str_squish() %>%
    str_replace_all("[^a-z0-9äöüøéèáàóòíìñ]", "")
}

phase_lookup <- phase_table %>%
  separate_rows(Study_IDs, sep = ";|\\n") %>%
  mutate(
    Study_IDs = str_squish(Study_IDs),
    join_id = make_join_id(Study_IDs)
  ) %>%
  filter(!is.na(join_id), join_id != "") %>%
  distinct(Main_Phase, join_id)

study_table_clean <- study_table %>%
  mutate(
    SR_ID = str_squish(SR_ID),
    join_id = make_join_id(SR_ID)
  )

mapped_all_phases <- study_table_clean %>%
  inner_join(
    phase_lookup,
    by = "join_id",
    relationship = "many-to-many"
  ) %>%
  select(Main_Phase, SR_ID, everything(), -join_id)

View(mapped_all_phases)

write_xlsx(mapped_all_phases, "mapped_studies_by_phase.xlsx")

##Cleaning the IDs 
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

studien <- read_excel("mapped_studies_by_phase.xlsx", sheet = "Phase 3")

studien <- studien %>%
  rename(study_id = ...3) %>%
  mutate(
    SR_ID = str_squish(SR_ID),
    Study_ID = study_id %>%
      as.character() %>%
      str_to_lower() %>%
      str_remove("^https?://(dx\\.)?doi\\.org/") %>%
      str_remove("^(doi|oi|do)\\s*:?\\s*") %>%
      str_trim(),
    
    Study_ID = if_else(
      str_detect(Study_ID, "10\\.\\d{4,9}/"),
      str_extract(Study_ID, "10\\.\\d{4,9}/.+"),
      str_extract(Study_ID, "\\b\\d{5,9}\\b")
    )
  )


studien <- studien %>%
  mutate(
    first_author = first_author %>%
      str_remove("^\\s*\\d+\\.") %>%          
      str_replace_all("et al\\.|et al", "") %>%
      str_replace_all("[,;].*", "") %>%
      str_squish()
  )


studien <- studien %>%
  mutate(
    study_year = str_extract(
      paste(first_author, study_year),
      "(19|20)\\d{2}"
    )
  )

studien_clean <- studien %>%
  select(SR_ID, Study_ID, first_author, study_year)

# fehlende DOIs
studien_clean %>% filter(is.na(Study_ID))

# mehrere Jahre?
studien_clean %>% filter(str_length(study_year) != 4)

# Autoren leer?
studien_clean %>% filter(first_author == "")


writexl::write_xlsx(studien_clean, "Overlap_clean_Phase3.xlsx")

##Calculating the CCA
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)
library(widyr)
library(ggplot2)

df <- read_xlsx("Overlap_clean_Phase3.xlsx")

# 2) Spaltennamen vereinheitlichen

df <- df %>%
  rename_with(~ str_replace_all(tolower(.x), " ", "_"))

df_clean <- df %>%
  mutate(
    study_id_raw = study_id,
    
    study_id = tolower(study_id),
    
    # DOI-URLs entfernen
    study_id = str_remove(study_id, "^https?://(dx\\.)?doi\\.org/"),
    
    # Präfixe entfernen (doi:, oi:, do)
    study_id = str_remove(study_id, "^(doi|oi|do)\\s*:?\\s*"),
    
    study_id = str_trim(study_id)
  ) %>%
  
  # PMCID ausschließen
  filter(!str_detect(study_id, "^pmc")) %>%
  
  # pro SR nur eine Nennung je Studie
  distinct(sr_id, study_id, .keep_all = TRUE)

df_clean <- df_clean %>%
  mutate(
    study_label = paste0(
      str_extract(first_author, "^[^,;]+"),
      " (",
      study_year,
      ")"
    )
  )

study_overlap <- df_clean %>%
  group_by(study_id, study_label) %>%
  summarise(
    n_SRs = n_distinct(sr_id),
    SRs = paste(sort(unique(sr_id)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_SRs))

citation_matrix <- df_clean %>%
  mutate(value = 1L) %>%
  select(study_label, sr_id, value) %>%
  pivot_wider(
    names_from = sr_id,
    values_from = value,
    values_fn = max,          # WICHTIG
    values_fill = 0
  ) %>%
  arrange(desc(rowSums(across(-study_label))))

citation_matrix <- citation_matrix %>%
  mutate(
    n_SRs = rowSums(across(-study_label))
  ) %>%
  relocate(n_SRs, .after = study_label)


citation_matrix <- df_clean %>%
  mutate(value = 1) %>%
  select(sr_id, study_id, value) %>%
  pivot_wider(
    names_from = sr_id,
    values_from = value,
    values_fill = 0
  )

# Grundzahlen
c <- n_distinct(df_clean$sr_id)
r <- n_distinct(df_clean$study_id)
N <- nrow(df_clean)

c <- n_distinct(df_clean$sr_id)       # Anzahl SRs
r <- n_distinct(df_clean$study_id)    # Anzahl Primärstudien
N <- nrow(df_clean)                   # Gesamt-Nennungen

CCA <- (N - r) / (r * c - r)
CCA_percent <- CCA * 100

sr_pairwise <- df_clean %>%
  pairwise_count(
    sr_id,
    study_id,
    sort = TRUE
  )

sr_sizes <- df_clean %>%
  count(sr_id, name = "n_studies")

# Anteil stark überlappender Studien (≥3 SRs)
high_overlap_share <- df_clean %>%
  count(study_id) %>%
  filter(n >= 3) %>%
  nrow() / r

# Fehlende Metriken berechnen
mean_SRs_per_study <- N / r
unique_share <- (df_clean %>% count(study_id) %>% filter(n == 1) %>% nrow()) / r

# Jetzt erst das Tibble erstellen
overlap_metrics <- tibble(
  n_SRs = c,
  n_unique_studies = r,
  total_mentions = N,
  CCA = CCA,
  CCA_percent = CCA_percent,
  mean_SRs_per_study = mean_SRs_per_study,
  unique_share = unique_share,
  high_overlap_share = high_overlap_share
)

overlap_only <- study_overlap %>%
  filter(n_SRs >= 2)

pairwise_cca <- sr_pairwise %>%
  left_join(sr_sizes, by = c("item1" = "sr_id")) %>%
  rename(n1 = n_studies) %>%
  left_join(sr_sizes, by = c("item2" = "sr_id")) %>%
  rename(n2 = n_studies) %>%
  mutate(
    CCA = n / (n1 + n2 - n)
  ) %>%
  arrange(desc(CCA))

ggplot(pairwise_cca, aes(item1, item2, fill = CCA)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Pairwise CCA") +
  theme_minimal() +
  labs(
    x = "Systematic Review",
    y = "Systematic Review",
    title = "Pairwise overlap between systematic reviews (CCA)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


write_xlsx(
  list(
    "Citation matrix" = citation_matrix,
    "Overlap_per_study" = study_overlap,
    "Overlap_only_n>=2" = overlap_only,
    "Pariwise_overlap_abs" = sr_pairwise,
    "Clean_data" = df_clean,
    "Overlap_metrics" = tibble(
      n_SRs = c, 
      n_unique_studies = r, 
      total_mentions = N, 
      CCA = CCA, 
      CCA_percent = CCA_percent
    )
  ),
  "Overlap_Phase3.xlsx"
)


