library(dplyr)
library(googledrive)
library(googlesheets4)

# retrieve google sheet of fish and habitat snorkels
eoe_snorkels_ss = '1Wsmwk1i1Kku50LadTTBJsWqs1L6K6rV3eEb98-SWaKA'
# retrieve metadata of this google sheet, each sub-sheet name and ID
mainstem_data <- googlesheets4::gs4_get(ss = eoe_snorkels_ss)

# just fish data sheets
mainstem_fish_sheets <- mainstem_data$sheets %>%
  filter(grepl("Fish", name))

# just habitat data sheets
mainstem_habitat_sheets <- mainstem_data$sheets %>%
  filter(grepl("Habitat", name))

# species name and size ranges
eoe.snorkel <- read.csv('./EoE_fish_snorkel_mainstem - MainstemFishSnorkel.csv') %>%
  filter(!duplicated(paste0(species, size_cm))) %>%
  arrange(species) %>%
  select(species, size_cm)

# pile fish data into one dataframe
for(sheet in mainstem_fish_sheets$name) {

  this_sheet <- googlesheets4::read_sheet(ss = eoe_snorkels_ss, sheet = sheet) %>%
    mutate(
      date = lubridate::date(date),
      time_minutes = as.character(time_minutes)
    )
  print(colnames(this_sheet))

  if(!exists('eoe_fish_df')){
    eoe_fish_df <- this_sheet
  } else {
    res <- try(eoe_fish_df <- bind_rows(eoe_fish_df, this_sheet))
    if(any(class(res) == 'try-error')) {
      warning(paste(sheet, 'failed to bind'))
      next
    }
  }
}
## rm(eoe_fish_df)

eoe_fish_data <- eoe_fish_df %>%
  filter(!is.na(date))

googlesheets4::write_sheet(eoe_fish_data, ss = "https://docs.google.com/spreadsheets/d/1S60cJ4mvPsbA50NsWkZbuwwygiREbqiFxlGubKmHp6A/edit#gid=0")
