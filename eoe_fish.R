library(dplyr)
library(googledrive)
library(googlesheets4)

# retrieve google sheet of fish and habitat snorkels
eoe_snorkels_ss = '1Wsmwk1i1Kku50LadTTBJsWqs1L6K6rV3eEb98-SWaKA'
# retrieve metadata of this google sheet, each sub-sheet name and ID
mainstem_data <- googlesheets4::gs4_get(ss = eoe_snorkels_ss)

mainstem_fish_sheets <- mainstem_data$sheets %>%
  filter(grepl("Fish", name))

mainstem_habitat_sheets <- mainstem_data$sheets %>%
  filter(grepl("Habitat", name))

eoe.snorkel <- read.csv('./EoE_fish_snorkel_mainstem - MainstemFishSnorkel.csv') %>%
  filter(!duplicated(paste0(species, size_cm))) %>%
  arrange(species) %>%
  select(species, size_cm)
