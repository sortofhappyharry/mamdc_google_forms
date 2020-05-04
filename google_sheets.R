library(googlesheets4)
library(tidyverse)

url = "https://docs.google.com/spreadsheets/d/131xLLp-8__Umonl5tMhs4FX2n5Hzp2YoNjGrhPGw4Hg/edit#gid=542938029"

ssid = as_sheets_id(url)

sheet_names = gs4_get(ssid)$sheets$name

sheet = read_sheet(url)

dat = data.frame(x = 1:3, y = 2:4)

sheet_write(dat, ss = ssid, sheet = "New Test Sheet")

#########
my_sheet_name = sheet_names[[6]]

sheet = read_sheet(ssid, sheet = my_sheet_name)

dat_to_append = sheet[1,]

sheet_append(ss = ssid, sheet = my_sheet_name, data = dat_to_append)