###############################################################################
# Scoping review analysis: Low-value care and de-implementation phases
#
# Purpose:
# This script cleans the extracted mapping data and creates the descriptive
# tables used in the thesis analysis. The analyses include:
# 1. Number of studies per de-implementation phase
# 2. Studies covering several phases
# 3. LVC practices, medical fields, and healthcare settings
# 4. Single-category vs multiple-category coding per phase
# 5. Stakeholder involvement
# 6. Descriptive mapping tables and selected figures
###############################################################################


# 1. Packages ------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(writexl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# 2. File paths ----------------------------------------------------------------

input_file <- "/Users/elisakohler/Scoping Review LVC/Analysis/Codes/Mapping_final.xlsx"

clean_data_file <- "/Users/elisakohler/Scoping Review LVC/Analysis/Codes/cleandata3.xlsx"

phase_results_file <- "/Users/elisakohler/Scoping Review LVC/Analysis/Results/Phases_Overview_final.xlsx"

descriptive_results_file <- "/Users/elisakohler/Scoping Review LVC/Analysis/Results/descriptive_final.xlsx"


# 3. Import data and rename columns --------------------------------------------

raw_data <- read_excel(input_file)

# The original Excel file contains long column names. They are renamed here to
# shorter names that are easier to use throughout the analysis.
column_names <- c(
  Country = "Country in which the study conducted",
  Study_ID = "Study ID",
  Funding = "Declaration of Funding",
  Design = "Study design",
  Aim = "Aim of the study",
  Number_studies = "Number of included studies",
  Quality = "Quality Assessment",
  Term = "Used term",
  LVC = "LVC practice",
  Setting = "Healthcare setting",
  Field = "Medical field",
  Phase_0.1 = "Identification of practices which are potentially of low value",
  Phase_0.1_Level = "Level of Identification",
  Phase_0.1_Method = "Method to identify practices of low value",
  Phase_0.3 = "Identification of prevalence",
  Phase_0.3_Method = "Method for investigating prevalence",
  Phase_1 = "Investigated methods for prioritisation of LVC",
  Phase_1_Method = "Method of prioritisation",
  Phase_1_Level = "Level",
  Phase_1.2 = "Investigation of methods to identify LVC in clinical practice",
  Phase_1.2_Method = "Methods to identify LVC in clinical practices",
  Phase_2.1 = "Investigation of determinants",
  Phase_2.1_Framework = "Framework used for identification of determinants",
  Phase_2.2 = "Name of the investigated intervention",
  Phase_2.2_type = "Intervention type",
  Phase_2.2_target = "Target group (of the intervention)",
  Phase_2.2_framework = "Framework used (intervention)",
  Phase_3 = "Outcomes",
  Phase_4.1 = "Investigations on how to sustain de-implementation efforts",
  Phase_4.2 = "Investigate on how to spread de-implementation efforts",
  Phase_4.1_determinants = "Determinants for sustainment",
  Stake_involvement = "Stakeholderinvolvement",
  Stake_type = "Stakeholder",
  Stake_phase = "Stage of involvement",
  Framework = "Collection of frameworks used in the field of de-implementation",
  Framework_names = "Names of the used frameworks",
  Framework_purpose = "Purpose of the framework",
  Developed_Framework = "Name of the developed framework",
  Initiative = "Investigation of de-implementation initiatives",
  Initiative_level = "Level of initiative",
  Initiative_name = "Name/Aim of the initiatives"
)

data <- raw_data %>%
  rename(any_of(column_names))


# 4. Clean data ----------------------------------------------------------------

# Empty cells, explicit "not investigated" entries, and missing values are
# harmonised to one category: "Not investigated".
# "Other: " is also standardised to "Other:" so that later recoding works.
data_clean <- data %>%
  mutate(
    across(
      everything(),
      ~ {
        x <- as.character(.x)
        x <- str_replace_all(x, "Other:\\s*", "Other:")
        if_else(
          is.na(x) | str_trim(x) == "" | str_to_lower(x) == "not investigated",
          "Not investigated",
          x
        )
      }
    )
  )

write_xlsx(data_clean, path = clean_data_file)


# 5. Helper objects and functions ----------------------------------------------

not_investigated_values <- c(
  "Not investigated",
  "not investigated",
  "No",
  "no"
)

# A long version of all phase variables is used as the basis for several
# analyses. Sub-phases such as Phase_0.1 and Phase_0.3 are additionally mapped
# to their main phase, for example Phase_0.
phase_long <- data_clean %>%
  pivot_longer(
    cols = starts_with("Phase_"),
    names_to = "Phase_Raw",
    values_to = "Content"
  ) %>%
  filter(!Content %in% not_investigated_values) %>%
  mutate(Main_Phase = str_extract(Phase_Raw, "Phase_\\d")) %>%
  filter(Main_Phase %in% paste0("Phase_", 0:4))

# Each study should only be counted once per main phase, even if it contains
# several sub-phase variables within the same phase.
study_phase <- phase_long %>%
  distinct(Study_ID, Main_Phase)

# Splits semicolon-separated variables such as LVC, Field, and Setting into one
# row per study and category. This makes category counting transparent.
split_categories <- function(clean_data, variable, is_setting = FALSE) {
  clean_data %>%
    select(Study_ID, {{ variable }}) %>%
    separate_rows({{ variable }}, sep = ";") %>%
    mutate(category_value = str_trim({{ variable }})) %>%
    mutate(
      category_value = case_when(
        is_setting &
          str_detect(category_value, "^Other:") &
          str_detect(category_value, regex("Hospital", ignore_case = TRUE)) ~ "Hospital",
        is_setting &
          str_detect(category_value, "^Other:") &
          str_detect(category_value, regex("Outpatient", ignore_case = TRUE)) ~ "Outpatient",
        str_detect(category_value, "^Other:") ~ "Other",
        TRUE ~ category_value
      )
    ) %>%
    select(Study_ID, {{ variable }} := category_value) %>%
    filter(!{{ variable }} %in% c("Not investigated", "", NA)) %>%
    distinct()
}

# Counts categories and stores the contributing study IDs. This is useful for
# checking the result behind each number in the output tables.
count_category_by_study <- function(data_long, variable_name) {
  data_long %>%
    group_by(.data[[variable_name]]) %>%
    summarise(
      n_studies = n_distinct(Study_ID),
      Study_IDs = paste(sort(unique(Study_ID)), collapse = ";"),
      .groups = "drop"
    ) %>%
    arrange(desc(n_studies))
}

# Identifies whether each study covers only one category or several categories.
# Studies with one category are shown under that category; studies with more
# than one category are grouped as "Multiple".
single_or_multiple_by_phase <- function(data_long, variable_name) {
  per_study <- data_long %>%
    group_by(Study_ID) %>%
    summarise(
      n_categories = n_distinct(.data[[variable_name]]),
      category = case_when(
        n_categories == 1 ~ first(.data[[variable_name]]),
        n_categories > 1 ~ "Multiple",
        TRUE ~ NA_character_
      ),
      .groups = "drop"
    )

  study_phase %>%
    left_join(per_study, by = "Study_ID") %>%
    filter(!is.na(category)) %>%
    group_by(Main_Phase, category) %>%
    summarise(
      n_studies = n_distinct(Study_ID),
      Study_IDs = paste(sort(unique(Study_ID)), collapse = ";"),
      .groups = "drop"
    ) %>%
    arrange(Main_Phase, desc(n_studies), category)
}

# Converts the long single/multiple phase table into a wide table that is easier
# to read in Excel.
make_phase_wide_table <- function(phase_table) {
  phase_table %>%
    select(Main_Phase, category, n_studies) %>%
    pivot_wider(
      names_from = Main_Phase,
      values_from = n_studies,
      values_fill = 0
    ) %>%
    arrange(category)
}

# Produces three outputs:
# 1. categories per study
# 2. number of studies with isolated vs multiple categories
# 3. isolated categories and their frequencies
multiple_selection_summary <- function(data_long, variable_name) {
  per_study <- data_long %>%
    group_by(Study_ID) %>%
    summarise(
      n_categories = n_distinct(.data[[variable_name]]),
      categories = paste(sort(unique(.data[[variable_name]])), collapse = "; "),
      .groups = "drop"
    )

  summary_table <- per_study %>%
    count(n_categories, name = "n_studies") %>%
    mutate(
      type = case_when(
        n_categories == 1 ~ "Isolated",
        n_categories > 1 ~ "Multiple",
        TRUE ~ NA_character_
      )
    )

  isolated <- per_study %>%
    filter(n_categories == 1) %>%
    count(categories, name = "n_studies") %>%
    arrange(desc(n_studies))

  list(
    per_study = per_study,
    summary = summary_table,
    isolated = isolated
  )
}


# 6. Phase analyses -------------------------------------------------------------

# Counts all investigated sub-phase variables.
individual_phases <- phase_long %>%
  group_by(Phase_Raw) %>%
  summarise(
    Amount = n(),
    Study_IDs = paste(sort(unique(Study_ID)), collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(Phase_Raw)

# Counts studies per main phase. A study is counted only once per main phase.
main_phases <- study_phase %>%
  group_by(Main_Phase) %>%
  summarise(
    Amount = n_distinct(Study_ID),
    Study_IDs = paste(sort(unique(Study_ID)), collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(Main_Phase)

# Identifies studies that cover more than one main phase.
studies_multiple_phases <- study_phase %>%
  group_by(Study_ID) %>%
  summarise(
    Number_of_phases = n_distinct(Main_Phase),
    Included_phases = paste(sort(unique(Main_Phase)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(Number_of_phases > 1) %>%
  arrange(desc(Number_of_phases))

# Counts how often each combination of phases occurs.
phase_combinations <- study_phase %>%
  group_by(Study_ID) %>%
  summarise(
    Phase_combination = paste(sort(unique(Main_Phase)), collapse = " & "),
    .groups = "drop"
  ) %>%
  count(Phase_combination, name = "n_studies") %>%
  arrange(desc(n_studies))


# 7. LVC practice, medical field, and healthcare setting ------------------------

lvc_long <- split_categories(data_clean, LVC)
field_long <- split_categories(data_clean, Field)
setting_long <- split_categories(data_clean, Setting, is_setting = TRUE)

lvc_by_study <- count_category_by_study(lvc_long, "LVC")
field_by_study <- count_category_by_study(field_long, "Field")
setting_by_study <- count_category_by_study(setting_long, "Setting")

lvc_field_map <- lvc_long %>%
  left_join(field_long, by = "Study_ID", relationship = "many-to-many") %>%
  count(LVC, Field, name = "n") %>%
  arrange(desc(n))

lvc_multiple <- multiple_selection_summary(lvc_long, "LVC")
field_multiple <- multiple_selection_summary(field_long, "Field")
setting_multiple <- multiple_selection_summary(setting_long, "Setting")

lvc_table <- lvc_multiple$isolated
field_table <- field_multiple$isolated
setting_table <- setting_multiple$isolated


# 8. Single vs multiple categories per phase ------------------------------------

lvc_by_phase_single_multiple <- single_or_multiple_by_phase(lvc_long, "LVC")
field_by_phase_single_multiple <- single_or_multiple_by_phase(field_long, "Field")
setting_by_phase_single_multiple <- single_or_multiple_by_phase(setting_long, "Setting")

lvc_by_phase_wide <- make_phase_wide_table(lvc_by_phase_single_multiple)
field_by_phase_wide <- make_phase_wide_table(field_by_phase_single_multiple)
setting_by_phase_wide <- make_phase_wide_table(setting_by_phase_single_multiple)


# 9. Stakeholder analyses -------------------------------------------------------

stake_phase_clean <- data_clean %>%
  select(Study_ID, Stake_phase) %>%
  separate_rows(Stake_phase, sep = ";") %>%
  mutate(Stake_phase = str_trim(Stake_phase)) %>%
  mutate(
    Stake_Main_Phase = case_when(
      str_detect(Stake_phase, "Phase 0") ~ "Phase_0",
      str_detect(Stake_phase, "Phase 1") ~ "Phase_1",
      str_detect(Stake_phase, "Phase 2") ~ "Phase_2",
      str_detect(Stake_phase, "Phase 3") ~ "Phase_3",
      str_detect(Stake_phase, "Phase 4") ~ "Phase_4",
      str_detect(Stake_phase, "Feedback") ~ "Phase_3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Stake_Main_Phase)) %>%
  distinct(Study_ID, Stake_Main_Phase)

stake_phase_summary <- stake_phase_clean %>%
  count(Stake_Main_Phase, name = "n_studies") %>%
  arrange(Stake_Main_Phase)

stake_type_clean <- data_clean %>%
  select(Study_ID, Stake_type) %>%
  separate_rows(Stake_type, sep = ";") %>%
  mutate(Stake_type = str_trim(Stake_type)) %>%
  mutate(
    Stakeholder = case_when(
      str_detect(Stake_type, regex("patient|public|relative", ignore_case = TRUE)) ~
        "Patients / Public / Relatives",
      str_detect(Stake_type, regex("nurse|doctor|pharmacist|healthcare professional|midwife|physician", ignore_case = TRUE)) ~
        "Healthcare professionals",
      str_detect(Stake_type, regex("manager|leader|organisation", ignore_case = TRUE)) ~
        "Managers / Organisations",
      str_detect(Stake_type, regex("policy|politic", ignore_case = TRUE)) ~
        "Policy makers",
      str_detect(Stake_type, regex("researcher", ignore_case = TRUE)) ~
        "Researchers",
      TRUE ~ "Other"
    )
  ) %>%
  distinct(Study_ID, Stakeholder)

stake_matrix <- stake_phase_clean %>%
  left_join(stake_type_clean, by = "Study_ID", relationship = "many-to-many") %>%
  distinct(Study_ID, Stake_Main_Phase, Stakeholder)

stake_summary <- stake_matrix %>%
  count(Stake_Main_Phase, Stakeholder, name = "n_studies") %>%
  arrange(Stake_Main_Phase, desc(n_studies))

stake_percent <- stake_matrix %>%
  group_by(Stake_Main_Phase) %>%
  mutate(Total = n_distinct(Study_ID)) %>%
  ungroup() %>%
  count(Stake_Main_Phase, Stakeholder, Total, name = "n_studies") %>%
  mutate(Percent = round(n_studies / Total * 100, 1))


# 10. Export phase and category results -----------------------------------------

write_xlsx(
  list(
    "Individual phases" = individual_phases,
    "Main phases" = main_phases,
    "Multiple phases" = studies_multiple_phases,
    "Phase combinations" = phase_combinations,
    "LVC by study" = lvc_by_study,
    "Field by study" = field_by_study,
    "Setting by study" = setting_by_study,
    "LVC x Field" = lvc_field_map,
    "LVC isolated" = lvc_table,
    "Field isolated" = field_table,
    "Setting isolated" = setting_table,
    "LVC by phase" = lvc_by_phase_wide,
    "Field by phase" = field_by_phase_wide,
    "Setting by phase" = setting_by_phase_wide,
    "LVC by phase IDs" = lvc_by_phase_single_multiple,
    "Field by phase IDs" = field_by_phase_single_multiple,
    "Setting by phase IDs" = setting_by_phase_single_multiple,
    "Stakeholder phase" = stake_phase_summary,
    "Stakeholder x type" = stake_percent
  ),
  path = phase_results_file
)


# 11. Descriptive mapping tables ------------------------------------------------

descriptive_mapping <- function(clean_data, variable) {
  total_studies <- n_distinct(clean_data$Study_ID)

  clean_data %>%
    separate_rows({{ variable }}, sep = ";") %>%
    mutate({{ variable }} := str_to_lower(str_trim({{ variable }}))) %>%
    filter(!{{ variable }} %in% str_to_lower(not_investigated_values)) %>%
    distinct(Study_ID, {{ variable }}) %>%
    count({{ variable }}, name = "n_studies", sort = TRUE) %>%
    mutate(Percent = round(n_studies / total_studies * 100, 2))
}

mapping_matrix <- function(clean_data, variable) {
  clean_data %>%
    separate_rows({{ variable }}, sep = ";") %>%
    mutate({{ variable }} := str_to_lower(str_trim({{ variable }}))) %>%
    filter(!{{ variable }} %in% str_to_lower(not_investigated_values)) %>%
    distinct(Study_ID, {{ variable }}) %>%
    mutate(value = 1) %>%
    pivot_wider(
      names_from = {{ variable }},
      values_from = value,
      values_fill = 0
    )
}

cross_table <- function(clean_data, variable, phase_variable) {
  data_filtered <- clean_data %>%
    filter(!.data[[phase_variable]] %in% not_investigated_values)

  total_studies <- n_distinct(data_filtered$Study_ID)

  result <- data_filtered %>%
    separate_rows({{ variable }}, sep = ";") %>%
    mutate({{ variable }} := str_to_lower(str_trim({{ variable }}))) %>%
    filter(!{{ variable }} %in% str_to_lower(not_investigated_values)) %>%
    distinct(Study_ID, {{ variable }}) %>%
    count({{ variable }}, name = "n_studies", sort = TRUE) %>%
    mutate(Percent = round(n_studies / total_studies * 100, 2))

  result %>%
    bind_rows(
      summarise(
        result,
        across(where(is.character), ~ "Total"),
        n_studies = total_studies,
        Percent = 100
      )
    )
}

descriptive_matrix_results <- list(
  Design = mapping_matrix(data_clean, Design),
  Country = mapping_matrix(data_clean, Country),
  Year = mapping_matrix(data_clean, Year),
  Funding = mapping_matrix(data_clean, Funding),
  Quality = mapping_matrix(data_clean, Quality),
  Term = mapping_matrix(data_clean, Term),
  LVC = mapping_matrix(data_clean, LVC),
  Setting = mapping_matrix(data_clean, Setting),
  Field = mapping_matrix(data_clean, Field),
  Stakeholder_phase = mapping_matrix(data_clean, Stake_phase),
  Stake_type = mapping_matrix(data_clean, Stake_type)
)

descriptive_frequency_results <- list(
  Design_frequency = descriptive_mapping(data_clean, Design),
  Country_frequency = descriptive_mapping(data_clean, Country),
  Year_frequency = descriptive_mapping(data_clean, Year),
  Funding_frequency = descriptive_mapping(data_clean, Funding),
  Quality_frequency = descriptive_mapping(data_clean, Quality),
  Term_frequency = descriptive_mapping(data_clean, Term),
  LVC_frequency = descriptive_mapping(data_clean, LVC),
  Setting_frequency = descriptive_mapping(data_clean, Setting),
  Field_frequency = descriptive_mapping(data_clean, Field),
  Stakeholder_phase_frequency = descriptive_mapping(data_clean, Stake_phase),
  Stake_type_frequency = descriptive_mapping(data_clean, Stake_type)
)

phase_specific_results <- list(
  Field_Phase_2_2 = cross_table(data_clean, Field, "Phase_2.2_type"),
  LVC_Phase_2_2 = cross_table(data_clean, LVC, "Phase_2.2_type"),
  Setting_Phase_2_2 = cross_table(data_clean, Setting, "Phase_2.2_type"),
  LVC_Phase_0_1 = cross_table(data_clean, LVC, "Phase_0.1")
)

all_descriptive_results <- c(
  descriptive_matrix_results,
  descriptive_frequency_results,
  phase_specific_results
)

write_xlsx(
  all_descriptive_results,
  path = descriptive_results_file
)


# 12. Figures ------------------------------------------------------------------

# Stakeholder heatmap
stakeholder_heatmap_data <- stake_summary %>%
  group_by(Stakeholder) %>%
  mutate(total = sum(n_studies)) %>%
  ungroup() %>%
  mutate(Stakeholder = reorder(Stakeholder, total))

ggplot(
  stakeholder_heatmap_data,
  aes(
    x = Stake_Main_Phase,
    y = Stakeholder,
    fill = n_studies
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n_studies), size = 4) +
  scale_fill_gradient(low = "white", high = "#2C7FB8") +
  labs(
    x = "De-implementation phase",
    y = "Stakeholder group",
    fill = "Number of studies"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Stakeholder stacked bar chart
stakeholder_palette <- c(
  "Healthcare professionals" = "cyan3",
  "Patients / Public / Relatives" = "darkolivegreen",
  "Managers / Organisations" = "darkslategrey",
  "Policy makers" = "deepskyblue4",
  "Researchers" = "deeppink4",
  "Other" = "#D9D9D9"
)

ggplot(
  stake_summary,
  aes(
    x = Stake_Main_Phase,
    y = n_studies,
    fill = Stakeholder
  )
) +
  geom_col(position = "stack") +
  scale_fill_manual(values = stakeholder_palette) +
  labs(
    y = "Number of studies",
    x = "De-implementation phase",
    fill = "Stakeholder group"
  ) +
  theme_minimal(base_size = 12)

# Country map
country_count <- data_clean %>%
  select(Study_ID, Country) %>%
  separate_rows(Country, sep = ";") %>%
  mutate(Country = str_to_lower(str_trim(Country))) %>%
  filter(!Country %in% str_to_lower(not_investigated_values)) %>%
  distinct(Study_ID, Country) %>%
  count(Country, name = "n_studies") %>%
  arrange(desc(n_studies))

country_map_ready <- country_count %>%
  mutate(
    Country = str_remove(Country, "^other:"),
    Country = case_when(
      Country == "united states" ~ "united states of america",
      Country == "brasil" ~ "brazil",
      Country == "singapur" ~ "singapore",
      TRUE ~ Country
    )
  )

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(name = str_to_lower(name))

map_data <- world %>%
  left_join(country_map_ready, by = c("name" = "Country"))

ggplot(map_data) +
  geom_sf(aes(fill = n_studies), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "cividis",
    na.value = "grey95",
    name = "Number of studies"
  ) +
  labs(title = "Geographical distribution of included studies") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
