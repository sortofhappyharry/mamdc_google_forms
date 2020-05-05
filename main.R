library(tidyverse)
library(googlesheets4)
library(lubridate)
library(futile.logger)

source('utils.R')
source('clients.R')
source('transfer_rows.R')
source('match_client.R')

copy_url = "https://docs.google.com/spreadsheets/d/1oW9KMlelcXxWPEW-nXEAbL8dbqhI2E6TNDzlij5d96k/edit#gid=92826597"
real_url = "https://docs.google.com/spreadsheets/d/1_vt2VwoPj7DUbGyjd3U8LP3ZOprAAlREwxFu2NeyRWM/edit#gid=542938029"
CLIENT_FILE = 'clients.csv'
OLD_CLIENT_FILE = 'old_client_file.csv'

copy_ssid = as_sheets_id(copy_url)
real_ssid = as_sheets_id(real_url)

# sheet_names = gs4_get(copy_ssid)$sheets$name

# To initialize old database

if(!exists())


transfer_rows(copy_ssid,
              intake_sheet = "MAM DC Intake Response v2",
	          sheet_in_use = "Active Sheet",
	          completed_tasks =  "Completed Tasks"          
	          )

