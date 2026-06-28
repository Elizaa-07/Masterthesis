library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Read sheets
phase_table <- read_excel("Phases_Overview_final.xlsx", sheet = "Main phases")
study_table <- read_excel("all_primary_studies271225.xlsx")

# Clean review names so the join works reliably
phase_table <- phase_table %>%
  mutate(Study_IDs = str_squish(Study_IDs))

study_table <- study_table %>%
  mutate(SR_ID = str_squish(SR_ID))

# Map every individual study to the phase of its review
mapped_studies <- study_table %>%
  left_join(
    phase_table %>% select(Phase, SR_ID),
    by = "SR_ID" = ""
  ) %>%
  select(Phase, SR_ID, everything())

# Check result
View(mapped_studies)

write_xlsx(mapped_studies, "mapped_studies_by_phase.xlsx")