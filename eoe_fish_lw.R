library(dplyr)
library(googledrive)
library(googlesheets4)

# script compiling raw fish length and wieght data and creating LW relationships

# Wiyot Tribe and Stillwater Pikeminnow E-Fishing Data
# 2019
# retrieve google sheet of
wiyot_pm_ss = 'https://docs.google.com/spreadsheets/d/1xczbBv6_wocfRypV1B5FLoVp3jIns1RNvc30Apbx2ZY/edit#gid=0'
# retrieve metadata of this google sheet, each sub-sheet name and ID
wiyot_pm_ss_info <- googlesheets4::gs4_get(ss = wiyot_pm_ss)
wiyot_pm_id <- wiyot_pm_ss_info$spreadsheet_id

wiyot_pm_sheet <- googlesheets4::read_sheet(ss = wiyot_pm_id)

wiyot_pm_df_2019 <- wiyot_pm_sheet %>%
  select(
    site = `Site ID`,
    date = Date,
    fish_id = `Fish ID  (MMDDYY-XXX)`,
    species = Species,
    length_mm = `Length (mm)1`,
    weight_g = `Weight (g)`
  ) %>%
  mutate(
    site = site[[1]]
  )

# 2019
# retrieve google sheet of
wiyot_pm_ss = 'https://docs.google.com/spreadsheets/d/1dbOYtd3Dk-FgJUGAe_E6JIJS22z-kHsCe6LVqKIEvs4/edit#gid=0'
# retrieve metadata of this google sheet, each sub-sheet name and ID
wiyot_pm_ss_info <- googlesheets4::gs4_get(ss = wiyot_pm_ss)
wiyot_pm_id <- wiyot_pm_ss_info$spreadsheet_id

wiyot_pm_sheet <- googlesheets4::read_sheet(ss = wiyot_pm_id)

wiyot_pm_df_2021 <- wiyot_pm_sheet %>%
  select(
    site = `Site ID`,
    date = Date,
    fish_id = `Fish ID  (MMDDYY-XXX)`,
    species = Species,
    length_mm = `Length (mm, standard)`,
    weight_g = `Weight (g)`
  ) %>%
  mutate(
    site = site[[1]]
  )

# no weights! just using 2019 then

# using the 2019 length and weight data, let's fit a power model
x = wiyot_pm_df_2019$length_mm
y = wiyot_pm_df_2019$weight_g


#fit the model
model <- lm(log(y)~ log(x))

#view the output of the model
mod_sum = summary(model)
intercept = mod_sum$coefficients[1,1]
slope = mod_sum$coefficients[2,1]

# power regression: ln(y) = intercept + slope*ln(x)
## log(y) = intercept + slope*log(x)
## y = e ^ intercept + slope * log(x)
## y = exp(intercept)x^slope
## y = ax^b
a = exp(intercept)
b = slope
# simplified ax^b equation and plt
plot(x, y, xlab = "Length (mm)", ylab = "Weight (g)")
title('SF Eel River Pike Minnow Length to Weight\n Wiyot and Stillwater e-Fishing Data')
curve(a*x^slope, from=0, to=1000, , xlab="x", ylab="y", add = TRUE)

# upload df to gdrive
googlesheets4::write_sheet(eoe_fish_data,
                           ss = "https://docs.google.com/spreadsheets/d/1S60cJ4mvPsbA50NsWkZbuwwygiREbqiFxlGubKmHp6A/edit#gid=0",
                           sheet = 'eoe_snorkel_data')
